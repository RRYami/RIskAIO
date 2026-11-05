(** Market state + risk factors (ADR-007). Port of [engine/market.py].

    Factors (1-day shocks, oldest first):
    - [EQ:<instrument_id>] — log return of the close;
    - [FX:<ccy>] — log return of EUR per unit (EUR is the numeraire, no factor);
    - [IR:<curve_id>:<tenor>] — absolute change of the zero rate on the Q14
      grid, where [curve_id] is e.g. [SWAP:EUR], [GOV:DE:EUR],
      [ISSUER:<issuer>:<ccy>] or [RATING:<rating>:<ccy>].

    Only the factors a portfolio needs are built. Zero curves interpolate
    linearly on the grid and extrapolate flat (Q14).

    Divergence: instrument/price tables are association lists, not frames. As-of
    curve groups are checked to hold the full 12-tenor grid explicitly (Python
    fails later on the shape mismatch instead). *)

open Risk_core

exception Market_data_error of string

let tenor_years = Array.of_list (List.map snd Marketdata.tenors)
let n_tenors = Array.length tenor_years

(** [(n, 12)] weights: z(t) is W times z_grid, linear inside, flat outside. *)
let interp_weights times =
  Array.map
    (fun t ->
      let tc =
        Float.min (Float.max t tenor_years.(0)) tenor_years.(n_tenors - 1)
      in
      let j = ref 0 in
      while !j < n_tenors && tenor_years.(!j) < tc do
        incr j
      done;
      let hi = Int.min (Int.max !j 1) (n_tenors - 1) in
      let lo = hi - 1 in
      let frac =
        (tc -. tenor_years.(lo)) /. (tenor_years.(hi) -. tenor_years.(lo))
      in
      let w = Array.make n_tenors 0. in
      w.(lo) <- 1. -. frac;
      w.(hi) <- w.(hi) +. frac;
      w)
    times

type market_state = {
  as_of : Calendar.date;
  prices : (string * float) list;
  fx : (string * float) list;  (** EUR per unit; EUR = 1 *)
  curves : (string * float array) list;  (** curve_id -> zero rates on grid *)
  vols : (string * float) list;
  spreads : (string * float) list;
  dividends : (string * float) list;
      (** continuous yield per priced instrument *)
}

let market_state (b : Marketdata.bundle) as_of =
  let prices =
    List.filter_map
      (fun (r : Marketdata.price_row) ->
        if Calendar.equal r.date as_of then
          Some (r.instrument_id, Money.float_of r.close)
        else None)
      b.prices
  in
  let fx =
    List.filter_map
      (fun (r : Marketdata.fx_row) ->
        if Calendar.equal r.date as_of then
          Some (r.currency, Money.float_of r.rate_to_eur)
        else None)
      b.fx
  in
  let crows =
    List.filter
      (fun (r : Marketdata.curve_row) -> Calendar.equal r.date as_of)
      b.curves
    |> List.sort (fun (a : Marketdata.curve_row) (b : Marketdata.curve_row) ->
        let c = String.compare a.curve_id b.curve_id in
        if c <> 0 then c else Float.compare a.tenor_years b.tenor_years)
  in
  if prices = [] || fx = [] || crows = [] then
    raise
      (Market_data_error
         ("market-data bundle has no as-of data for " ^ Calendar.to_string as_of));
  let curves =
    let rec group acc cur rows =
      match rows with
      | [] -> (
          match cur with
          | None -> []
          | Some c -> [ (c, Array.of_list (List.rev acc)) ])
      | (r : Marketdata.curve_row) :: rest -> (
          match cur with
          | None -> group [ r.zero_rate ] (Some r.curve_id) rest
          | Some c when c = r.curve_id -> group (r.zero_rate :: acc) cur rest
          | Some c ->
              (c, Array.of_list (List.rev acc))
              :: group [ r.zero_rate ] (Some r.curve_id) rest)
    in
    group [] None crows
  in
  List.iter
    (fun (c, g) ->
      if Array.length g <> n_tenors then
        raise
          (Market_data_error
             (Printf.sprintf "as-of curve %s has %d tenors, expected %d" c
                (Array.length g) n_tenors)))
    curves;
  let vols =
    List.map
      (fun (r : Marketdata.vol_row) -> (r.instrument_id, r.atm_vol_30d))
      b.vols
  in
  let spreads =
    List.map
      (fun (r : Marketdata.spread_row) -> (r.instrument_id, r.spread))
      b.spreads
  in
  let dividends =
    List.map
      (fun (r : Marketdata.dividend_row) -> (r.instrument_id, r.dividend_yield))
      b.dividends
  in
  { as_of; prices; fx; curves; vols; spreads; dividends }

type factor_set = {
  names : string array;
  index : (string * int) list;
  history : float array array;  (** (n_returns, n_factors) *)
  return_dates : Calendar.date array;  (** end date of each 1-day return *)
}

let size fs = Array.length fs.names

let factor_index fs name =
  match List.assoc_opt name fs.index with
  | None -> raise (Market_data_error ("unknown factor " ^ name))
  | Some i -> i

let eq fs iid = factor_index fs ("EQ:" ^ iid)

let fx fs ccy =
  if ccy = Marketdata.base_currency then None
  else Some (factor_index fs ("FX:" ^ ccy))

let ir fs curve_id =
  Array.init n_tenors (fun k ->
      factor_index fs
        (Printf.sprintf "IR:%s:%s" curve_id (fst (List.nth Marketdata.tenors k))))

(** Split [IR:<curve_id>:<tenor>] into [(curve_id, tenor)]. [curve_id] may
    itself contain ':' (e.g. GOV:DE:EUR), so split off the last component. *)
let split_ir name =
  if String.length name <= 3 || String.sub name 0 3 <> "IR:" then
    invalid_arg ("not an IR factor: " ^ name);
  let rest = String.sub name 3 (String.length name - 3) in
  match String.rindex_opt rest ':' with
  | None -> invalid_arg ("bad IR factor: " ^ name)
  | Some i ->
      ( String.sub rest 0 i,
        String.sub rest (i + 1) (String.length rest - i - 1) )

(** Pivot dated rows to a dense matrix over [keys]. Returns end-dates + returns.
    Single pass over [rows] (per-key filtering would be keys × rows). *)
let wide table_name rows get_key get_val get_date keys ~log_returns =
  let tbl = Hashtbl.create (4 * List.length keys) in
  List.iter
    (fun r ->
      let k = get_key r in
      let e = (get_date r, get_val r) in
      Hashtbl.replace tbl k
        (e :: (Hashtbl.find_opt tbl k |> Option.value ~default:[])))
    rows;
  let per_key =
    List.map
      (fun k -> (k, Hashtbl.find_opt tbl k |> Option.value ~default:[]))
      keys
  in
  let missing =
    List.filter_map (fun (k, m) -> if m = [] then Some k else None) per_key
  in
  if missing <> [] then
    raise
      (Market_data_error
         (Printf.sprintf "no %s history for [%s]" table_name
            (String.concat "; " missing)));
  let dates =
    List.concat_map (fun (_, m) -> List.map fst m) per_key
    |> List.sort_uniq Calendar.compare
  in
  let n = List.length dates and k = List.length keys in
  let mat = Array.make_matrix n k Float.nan in
  let date_pos =
    List.mapi (fun i d -> (d, i)) dates |> List.to_seq |> Hashtbl.of_seq
    |> fun h d -> Hashtbl.find h d
  in
  List.iteri
    (fun j (_, m) ->
      List.iter
        (fun (d, v) ->
          if Float.is_nan mat.(date_pos d).(j) then mat.(date_pos d).(j) <- v
          else
            raise
              (Market_data_error
                 (Printf.sprintf "duplicate %s row for %s on %s" table_name
                    (List.nth keys j) (Calendar.to_string d))))
        m)
    per_key;
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j v ->
          if Float.is_nan v then
            raise
              (Market_data_error
                 (Printf.sprintf "gaps in %s history (%s on %s)" table_name
                    (List.nth keys j)
                    (Calendar.to_string (List.nth dates i)))))
        row)
    mat;
  let rets =
    Array.init (n - 1) (fun i ->
        Array.init k (fun j ->
            if log_returns then Float.log (mat.(i + 1).(j) /. mat.(i).(j))
            else mat.(i + 1).(j) -. mat.(i).(j)))
  in
  (List.tl dates, rets)

(** Factor history: the last [history_days] daily returns ([None] = all).
    [curve_ids] are curve_id keys (SWAP:.., GOV:.., ISSUER:.., RATING:..). *)
let build_factors (bundle : Marketdata.bundle) ~equity_ids ~fx_ccys ~curve_ccys
    ~history_days =
  let eq = List.sort_uniq String.compare equity_ids in
  let fx =
    List.sort_uniq String.compare fx_ccys
    |> List.filter (fun c -> c <> Marketdata.base_currency)
  in
  let ir_ccys = List.sort_uniq String.compare curve_ccys in
  let names = ref [] in
  let blocks = ref [] in
  let dates = ref None in
  let add block_dates block block_names =
    (match !dates with
    | None -> dates := Some block_dates
    | Some d ->
        if
          List.length d <> List.length block_dates
          || not (List.for_all2 Calendar.equal d block_dates)
        then
          raise
            (Market_data_error "market-data frames do not share one date axis"));
    names := !names @ block_names;
    blocks := !blocks @ [ block ]
  in
  (if eq <> [] then
     let d, m =
       wide "close" bundle.prices
         (fun (r : Marketdata.price_row) -> r.instrument_id)
         (fun r -> Money.float_of r.Marketdata.close)
         (fun r -> r.Marketdata.date)
         eq ~log_returns:true
     in
     add d m (List.map (fun i -> "EQ:" ^ i) eq));
  (if fx <> [] then
     let d, m =
       wide "rate_to_eur" bundle.fx
         (fun (r : Marketdata.fx_row) -> r.currency)
         (fun r -> Money.float_of r.Marketdata.rate_to_eur)
         (fun r -> r.Marketdata.date)
         fx ~log_returns:true
     in
     add d m (List.map (fun c -> "FX:" ^ c) fx));
  List.iter
    (fun cid ->
      let rows =
        List.filter
          (fun (r : Marketdata.curve_row) ->
            r.curve_id = cid
            ||
            match String.split_on_char ':' cid with
            | [ "SWAP"; ccy ] -> r.curve_id = ccy (* legacy currency-keyed bundles *)
            | _ -> false)
          bundle.curves
      in
      let tenors = List.map fst Marketdata.tenors in
      let d, m =
        wide "zero_rate" rows
          (fun (r : Marketdata.curve_row) -> r.tenor)
          (fun r -> r.Marketdata.zero_rate)
          (fun r -> r.Marketdata.date)
          tenors ~log_returns:false
      in
      add d m (List.map (fun t -> "IR:" ^ cid ^ ":" ^ t) tenors))
    ir_ccys;
  match !dates with
  | None ->
      {
        names = [||];
        index = [];
        history = Array.make_matrix (Option.value history_days ~default:0) 0 0.;
        return_dates = [||];
      }
  | Some ds ->
      (* hstack the blocks column-wise *)
      let rows = List.length ds in
      let hist =
        Array.init rows (fun i ->
            Array.concat (List.map (fun blk -> blk.(i)) !blocks))
      in
      let available = rows in
      let h =
        match history_days with
        | None -> available
        | Some want ->
            if available < want then
              raise
                (Market_data_error
                   (Printf.sprintf "need %d daily returns, snapshot has %d" want
                      available));
            want
      in
      let names_arr = Array.of_list !names in
      {
        names = names_arr;
        index = List.mapi (fun i n -> (n, i)) !names;
        history = Array.sub hist (rows - h) h;
        return_dates = Array.sub (Array.of_list ds) (rows - h) h;
      }

(** The [n] returns ending at row [end_] (inclusive) — same factors, shorter
    history. *)
let factor_window fs ~end_ ~n =
  let len = Array.length fs.return_dates in
  if not (0 <= end_ - n + 1 && end_ - n + 1 <= end_ && end_ < len) then
    raise
      (Market_data_error
         (Printf.sprintf
            "window of %d returns ending at row %d is outside history" n end_));
  {
    fs with
    history = Array.sub fs.history (end_ - n + 1) n;
    return_dates = Array.sub fs.return_dates (end_ - n + 1) n;
  }
