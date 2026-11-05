(** Book construction + full revaluation. Port of [engine/pricing.py] (ADR-007).

    [Pricer.values] returns position values in the portfolio base currency,
    shape (n_scenarios, n_positions). Shocks are factor moves ([Market]).
    [horizon_days] ages every dated instrument consistently (ADR-014): options
    and futures lose [h] business days of time to expiry (/252, Q6); bonds and
    FX forwards move their valuation date [h] TARGET2 business days forward —
    cashflows inside the horizon are received as cash (no reinvestment), the
    rest discounted over shortened times on the (shocked) curve. Missing market
    data fails the build with the full list — a position is never silently
    dropped.

    Divergence: instrument ids are strings (36-char UUID text, as stored); the
    Python side uses [UUID] objects — same values, different type. *)

open Risk_core

exception Book_error of string list

exception Spec_error of string
(** internal: one holding is unpriceable *)

let linear_types = [ Refdata.Equity; Refdata.Etf; Refdata.Fund ]
let bond_types = [ Refdata.Gov_bond; Refdata.Corp_bond ]
let year_days = 365.
let trading_days = 252.

(** Row-wise dot with a fixed summation order (chunk-size independence). *)
let rowdot a v =
  Array.map
    (fun row ->
      let s = ref 0. in
      Array.iteri (fun j x -> s := !s +. (x *. v.(j))) row;
      !s)
    a

type position_info = {
  index : int;
  instrument_id : string;
  inst_type : Refdata.inst_type;
  currency : string;
  quantity : float;
  issuer : string;
  sector : string;
  country : string;
  portfolio : string;  (** constituent code in aggregate runs *)
}

type option_spec = {
  pos : int;
  underlying : string;
  strike : float;
  days_to_expiry : int;
  sigma : float;
  call : bool;
  style : string;
  contracts : float;  (** quantity × multiplier *)
  currency : string;
  dividend_yield : float;
}

type future_spec = {
  pos : int;
  underlying : string;
  contracts : float;  (** quantity × multiplier *)
  days_to_expiry : int;
  dividend_yield : float;
  currency : string;
}

type bond_spec = {
  pos : int;
  times : float array;
  cashflows : float array;  (** per unit par *)
  spread : float;
  face : float;  (** qty × par_value *)
  currency : string;  (** for FX conversion *)
  curve : string;  (** curve_id: GOV:.. / ISSUER:.. / RATING:.. *)
  freq : int;  (** coupon_frequency, for periodic YTM *)
}

type fx_forward_spec = {
  pos : int;
  contracts : float;
  buy_ccy : string;
  buy_notional : float;
  sell_ccy : string;
  sell_notional : float;
  t : float;
}

type book = {
  positions : position_info list;
  base_currency : string;
  linear : (int * string * float) list;  (** pos, iid, qty *)
  futures : future_spec list;
  options : option_spec list;
  bonds : bond_spec list;
  cash : int list;
  fx_forwards : fx_forward_spec list;
}

let equity_ids b =
  List.sort_uniq String.compare
    (List.map (fun (_, i, _) -> i) b.linear
    @ List.map (fun (f : future_spec) -> f.underlying) b.futures
    @ List.map (fun (o : option_spec) -> o.underlying) b.options)

let currencies b =
  List.sort_uniq String.compare
    (b.base_currency
     :: List.map (fun (p : position_info) -> p.currency) b.positions
    @ List.concat_map
        (fun (f : fx_forward_spec) -> [ f.buy_ccy; f.sell_ccy ])
        b.fx_forwards)

(** Curve ids shocked as IR factors: derivatives on SWAP:<ccy>, bonds on
    their resolved GOV/ISSUER/RATING curve, FX forwards on SWAP legs. *)
let swap_curve_id ccy = "SWAP:" ^ ccy

let curve_ids b =
  List.sort_uniq String.compare
    (List.map (fun (o : option_spec) -> swap_curve_id o.currency) b.options
    @ List.map (fun (bd : bond_spec) -> bd.curve) b.bonds
    @ List.map (fun (f : future_spec) -> swap_curve_id f.currency) b.futures
    @ List.concat_map
        (fun (f : fx_forward_spec) ->
          [ swap_curve_id f.buy_ccy; swap_curve_id f.sell_ccy ])
        b.fx_forwards)

(* --- terms access --- *)

let term_string terms k =
  match List.assoc_opt k terms with
  | Some (`String s) -> s
  | _ -> raise (Spec_error ("missing string term " ^ k))

let term_float terms k =
  match List.assoc_opt k terms with
  | Some (`Float f) -> f
  | Some (`Int i) -> float_of_int i
  | Some (`String s) -> (
      try float_of_string (String.trim s)
      with Failure _ -> raise (Spec_error ("bad numeric term " ^ k)))
  | _ -> raise (Spec_error ("missing numeric term " ^ k))

let term_int terms k =
  match List.assoc_opt k terms with
  | Some (`Int i) -> i
  | Some (`Float f) -> int_of_float f
  | _ -> raise (Spec_error ("missing int term " ^ k))

let term_float_alias terms keys =
  let rec go = function
    | [] -> raise (Spec_error ("missing numeric term " ^ List.hd keys))
    | k :: rest -> (
        match List.assoc_opt k terms with
        | Some (`Float f) -> f
        | Some (`Int i) -> float_of_int i
        | Some (`String s) -> (
            try float_of_string (String.trim s)
            with Failure _ ->
              if rest = [] then raise (Spec_error ("bad numeric term " ^ k))
              else go rest)
        | _ -> if rest = [] then raise (Spec_error ("missing numeric term " ^ k)) else go rest)
  in
  go keys

(** Curve-id sanitiser: ':' is the IR-factor separator, so strip it from
    free-text issuers/ratings. *)
let sanitize s =
  String.map (fun c -> if c = ':' then '-' else c) (String.trim s)

let gov_curve_id ~country ~ccy =
  Printf.sprintf "GOV:%s:%s" (sanitize country) (String.trim ccy)

let issuer_curve_id ~issuer ~ccy =
  Printf.sprintf "ISSUER:%s:%s" (sanitize issuer) (String.trim ccy)

let rating_curve_id ~rating ~ccy =
  Printf.sprintf "RATING:%s:%s" (sanitize rating) (String.trim ccy)

(** Curve grid with legacy fallback: new ids are SWAP:<ccy>; old bundles key
    curves by bare <ccy>. Bonds (GOV/ISSUER/RATING) have no fallback. *)
let find_curve_grid state curve_id =
  match List.assoc_opt curve_id state.Market.curves with
  | Some g -> g
  | None -> (
      match String.split_on_char ':' curve_id with
      | [ "SWAP"; ccy ] -> (
          match List.assoc_opt ccy state.Market.curves with
          | Some g -> g
          | None -> raise (Spec_error ("no zero curve for " ^ curve_id)))
      | _ -> raise (Spec_error ("no zero curve for " ^ curve_id)))

let has_curve state curve_id =
  try
    ignore (find_curve_grid state curve_id);
    true
  with Spec_error _ -> false

(* --- bonds --- *)

let divmod_floor a b =
  let q = a / b and r = a mod b in
  if r <> 0 && r < 0 <> (b < 0) then (q - 1, r + b) else (q, r)

let add_months (d : Calendar.date) months =
  let y, m = divmod_floor (d.m - 1 + months) 12 in
  let year = d.y + y and month = m + 1 in
  let rec try_day = function
    | [] ->
        raise
          (Spec_error
             (Printf.sprintf "cannot shift %s by %d months"
                (Calendar.to_string d) months))
    | day :: rest -> (
        try Calendar.make year month day
        with Calendar.Invalid_date _ -> try_day rest)
  in
  try_day [ d.d; 30; 29; 28 ]

(** Future cashflows per unit par: equal coupons + redemption. *)
let bond_cashflows terms as_of =
  let maturity = Calendar.of_string (term_string terms "maturity") in
  if Calendar.compare maturity as_of <= 0 then
    raise (Spec_error ("bond matured on " ^ Calendar.to_string maturity));
  (match List.assoc_opt "issue_date" terms with
  | None -> ()
  | Some (`String s) -> (
      let issue =
        try Calendar.of_string (String.trim s)
        with _ -> raise (Spec_error ("bad term issue_date: " ^ s))
      in
      if Calendar.compare issue maturity >= 0 then
        raise (Spec_error "issue_date must be before maturity");
      if Calendar.compare issue as_of > 0 then
        raise (Spec_error "issue_date is after as_of"))
  | _ -> raise (Spec_error "bad term issue_date"));
  let freq = term_int terms "coupon_frequency" in
  if not (List.mem freq [ 1; 2; 4; 12 ]) then
    raise (Spec_error (Printf.sprintf "unsupported coupon_frequency %d" freq));
  let coupon = term_float terms "coupon_rate" /. float_of_int freq in
  let step = 12 / freq in
  let dates = ref [] in
  let k = ref 0 in
  let go = ref true in
  while !go do
    let d = add_months maturity (-(!k * step)) in
    if Calendar.compare d as_of > 0 then (
      dates := d :: !dates;
      incr k)
    else go := false
  done;
  let dates = !dates in
  let times =
    Array.of_list
      (List.map
         (fun d ->
           float_of_int (Calendar.to_ordinal d - Calendar.to_ordinal as_of)
           /. year_days)
         dates)
  in
  let n = List.length dates in
  let cfs = Array.make n coupon in
  cfs.(n - 1) <- cfs.(n - 1) +. 1.;
  (times, cfs)

(** (issuer, sector, country); derivatives inherit their underlying's
    dimensions. *)
let dims (inst : Refdata.instrument_record) under =
  let src = match under with None -> inst.terms | Some u -> u.Refdata.terms in
  let get k =
    match List.assoc_opt k src with
    | Some (`String s) when s <> "" -> s
    | _ -> ""
  in
  let or_ v fb = if v = "" then fb () else v in
  ( or_ (get "issuer") (fun () ->
        Refdata.string_of_inst_type inst.inst_type ^ ":" ^ inst.currency),
    or_ (get "sector") (fun () ->
        String.lowercase_ascii (Refdata.string_of_inst_type inst.inst_type)),
    or_ (get "country") (fun () -> "n/a") )

let need_term ok what = if not ok then raise (Spec_error what)

let state_find what m k =
  match List.assoc_opt k m with
  | None -> raise (Spec_error ("no " ^ what ^ " for " ^ k))
  | Some v -> v

let add_spec (book : book) pos (inst : Refdata.instrument_record)
    (under : Refdata.instrument_record option) qty (state : Market.market_state)
    =
  let t = inst.terms and key = inst.instrument_id in
  if List.mem inst.inst_type linear_types then (
    ignore (state_find "price" state.prices key);
    { book with linear = book.linear @ [ (pos, key, qty) ] })
  else if inst.inst_type = Refdata.Future then (
    let under =
      match under with
      | None -> raise (Spec_error "future without underlying")
      | Some u -> u
    in
    let u = under.Refdata.instrument_id in
    ignore (state_find "underlying price" state.prices u);
    ignore (state_find "underlying dividend yield" state.dividends u);
    let expiry = Calendar.of_string (term_string t "expiry") in
    if Calendar.compare expiry state.as_of <= 0 then
      raise (Spec_error ("expired on " ^ Calendar.to_string expiry));
    let spec =
      {
        pos;
        underlying = u;
        contracts = qty *. term_float t "multiplier";
        days_to_expiry = Calendar.business_days_between state.as_of expiry;
        dividend_yield =
          state_find "underlying dividend yield" state.dividends u;
        currency = inst.currency;
      }
    in
    { book with futures = book.futures @ [ spec ] })
  else if inst.inst_type = Refdata.Listed_option then (
    let under =
      match under with
      | None -> raise (Spec_error "option without underlying")
      | Some u -> u
    in
    let u = under.Refdata.instrument_id in
    ignore (state_find "underlying price" state.prices u);
    ignore (state_find "underlying vol" state.vols u);
    ignore (state_find "underlying dividend yield" state.dividends u);
    let expiry = Calendar.of_string (term_string t "expiry") in
    let spec =
      {
        pos;
        underlying = u;
        strike = term_float t "strike";
        days_to_expiry = Calendar.business_days_between state.as_of expiry;
        sigma = state_find "underlying vol" state.vols u;
        call = term_string t "call_put" = "CALL";
        style = term_string t "style";
        contracts = qty *. term_float t "multiplier";
        currency = inst.currency;
        dividend_yield =
          state_find "underlying dividend yield" state.dividends u;
      }
    in
    { book with options = book.options @ [ spec ] })
  else if List.mem inst.inst_type bond_types then (
    let country = term_string t "country" in
    let issuer = term_string t "issuer" in
    let bond_rating = term_string t "bond_rating" in
    let par = term_float_alias t [ "par_value"; "notional" ] in
    if not (par > 0.) then raise (Spec_error "par_value must be > 0");
    let curve =
      match inst.inst_type with
      | Refdata.Gov_bond ->
          let cid = gov_curve_id ~country ~ccy:inst.currency in
          (match List.assoc_opt cid state.Market.curves with
          | Some _ -> cid
          | None ->
              raise
                (Spec_error
                   (Printf.sprintf "no gov curve %s for %s" cid key)))
      | Refdata.Corp_bond ->
          let iid = issuer_curve_id ~issuer ~ccy:inst.currency in
          if List.assoc_opt iid state.Market.curves <> None then iid
          else
            let rid = rating_curve_id ~rating:bond_rating ~ccy:inst.currency in
            (match List.assoc_opt rid state.Market.curves with
            | Some _ -> rid
            | None ->
                raise
                  (Spec_error
                     (Printf.sprintf
                        "no issuer curve %s nor rating curve %s for %s" iid rid
                        key)))
      | _ -> assert false
    in
    (* Residual basis over the issuer/rating/gov curve; 0 when the curve
       already carries the credit component. Optional for migration. *)
    let spread =
      match List.assoc_opt key state.spreads with Some s -> s | None -> 0.
    in
    let times, cfs = bond_cashflows t state.as_of in
    let freq = term_int t "coupon_frequency" in
    let spec =
      {
        pos;
        times;
        cashflows = cfs;
        spread;
        face = qty *. par;
        currency = inst.currency;
        curve;
        freq;
      }
    in
    { book with bonds = book.bonds @ [ spec ] })
  else if inst.inst_type = Refdata.Cash then
    { book with cash = book.cash @ [ pos ] }
  else if inst.inst_type = Refdata.Fx_forward then (
    let settle = Calendar.of_string (term_string t "settlement_date") in
    if Calendar.compare settle state.as_of <= 0 then
      raise (Spec_error ("settled on " ^ Calendar.to_string settle));
    let spec =
      {
        pos;
        contracts = qty;
        buy_ccy = term_string t "buy_currency";
        buy_notional = term_float t "buy_notional";
        sell_ccy = term_string t "sell_currency";
        sell_notional = term_float t "sell_notional";
        t =
          float_of_int
            (Calendar.to_ordinal settle - Calendar.to_ordinal state.as_of)
          /. year_days;
      }
    in
    { book with fx_forwards = book.fx_forwards @ [ spec ] })
  else
    raise
      (Spec_error
         ("unsupported type " ^ Refdata.string_of_inst_type inst.inst_type))

let build_book (holdings : Holdings.holding_row list)
    (instruments : (string * Refdata.instrument_record) list)
    (state : Market.market_state) base_currency row_portfolios =
  let book =
    ref
      {
        positions = [];
        base_currency;
        linear = [];
        futures = [];
        options = [];
        bonds = [];
        cash = [];
        fx_forwards = [];
      }
  in
  let problems = ref [] in
  List.iteri
    (fun idx (row : Holdings.holding_row) ->
      let iid = String.trim row.instrument_id in
      let qty = Money.float_of row.quantity in
      match List.assoc_opt iid instruments with
      | None ->
          problems := !problems @ [ iid ^ ": not in the pinned secmaster view" ]
      | Some inst -> (
          let under =
            match inst.inst_type with
            | Refdata.Future | Refdata.Listed_option -> (
                let ukey = term_string inst.terms "underlying_instrument_id" in
                match List.assoc_opt ukey instruments with
                | None ->
                    problems :=
                      !problems @ [ iid ^ ": underlying " ^ ukey ^ " unknown" ];
                    None
                | Some u -> Some u)
            | _ -> None
          in
          let under_missing =
            (match inst.inst_type with
              | Refdata.Future | Refdata.Listed_option -> true
              | _ -> false)
            && under = None
          in
          if not under_missing then
            let issuer, sector, country = dims inst under in
            let pos = List.length !book.positions in
            try
              let nb = add_spec !book pos inst under qty state in
              book := nb;
              let portfolio =
                match row_portfolios with
                | None -> ""
                | Some l -> (
                    match List.nth_opt l idx with None -> "" | Some s -> s)
              in
              book :=
                {
                  !book with
                  positions =
                    !book.positions
                    @ [
                        {
                          index = pos;
                          instrument_id = iid;
                          inst_type = inst.inst_type;
                          currency = inst.currency;
                          quantity = qty;
                          issuer;
                          sector;
                          country;
                          portfolio;
                        };
                      ];
                }
            with
            | Spec_error msg | Invalid_argument msg ->
                problems :=
                  !problems
                  @ [
                      Printf.sprintf "%s (%s, holding row %d): %s" iid
                        (Refdata.string_of_inst_type inst.inst_type)
                        idx msg;
                    ]
            | Calendar.Invalid_date msg ->
                problems :=
                  !problems
                  @ [
                      Printf.sprintf "%s (%s, holding row %d): %s" iid
                        (Refdata.string_of_inst_type inst.inst_type)
                        idx msg;
                    ]))
    holdings;
  List.iter
    (fun ccy ->
      if List.assoc_opt ccy state.fx = None then
        problems := !problems @ [ "no FX rate for " ^ ccy ])
    (currencies !book);
  List.iter
    (fun cid ->
      if not (has_curve state cid) then
        problems := !problems @ [ "no zero curve for " ^ cid ])
    (curve_ids !book);
  if !problems <> [] then raise (Book_error !problems);
  !book

(* --- pricer --- *)

type pricer = {
  book : book;
  state : Market.market_state;
  factors : Market.factor_set;
  n_pos : int;
  lin_pos : int array;
  lin_f : int array;
  lin_v0 : float array;
  pos_ccy : string array;
      (** per position; FX forwards value directly in EUR *)
  fut_f0 : float array;  (** aligned with book.futures *)
  fx_ccys : string list;
  rolled :
    (int, float * (float array * float array array * float) list) Hashtbl.t;
}

let shocked_curve grid cols shocks_row scale =
  Array.init (Array.length grid) (fun t ->
      grid.(t) +. (shocks_row.(cols.(t)) *. scale))

let future_price state factors (f : future_spec) shocks_row curve_row h =
  let s =
    List.assoc f.underlying state.Market.prices
    *. exp shocks_row.(Market.eq factors f.underlying)
  in
  let t = float_of_int (Int.max (f.days_to_expiry - h) 0) /. trading_days in
  let w = (Market.interp_weights [| Float.max t 1e-9 |]).(0) in
  let r = rowdot [| curve_row |] w in
  s *. exp ((r.(0) -. f.dividend_yield) *. t)

let make_pricer book state factors =
  let n_pos = List.length book.positions in
  let lin = Array.of_list book.linear in
  let lin_pos = Array.map (fun (p, _, _) -> p) lin in
  let lin_f = Array.map (fun (_, i, _) -> Market.eq factors i) lin in
  let lin_v0 =
    Array.map (fun (_, i, q) -> q *. List.assoc i state.Market.prices) lin
  in
  let pos_ccy =
    Array.init n_pos (fun p ->
        let info = List.nth book.positions p in
        info.currency)
  in
  List.iter
    (fun (f : fx_forward_spec) -> pos_ccy.(f.pos) <- Marketdata.base_currency)
    book.fx_forwards;
  let fx_ccys = List.sort_uniq String.compare (Array.to_list pos_ccy) in
  let pricer =
    {
      book;
      state;
      factors;
      n_pos;
      lin_pos;
      lin_f;
      lin_v0;
      pos_ccy;
      fut_f0 = [||];
      fx_ccys;
      rolled = Hashtbl.create 4;
    }
  in
  let zero = [| Array.make (Market.size factors) 0. |] in
  let fut_f0 =
    List.map
      (fun (f : future_spec) ->
        let cid = swap_curve_id f.currency in
        let cols =
          try Market.ir factors cid
          with Market.Market_data_error _ -> Market.ir factors f.currency
        in
        let grid = find_curve_grid state cid in
        let curve_row =
          Array.init (Array.length grid) (fun t ->
              grid.(t) +. zero.(0).(cols.(t)))
        in
        let v = future_price state factors f zero.(0) curve_row 0 in
        v)
      book.futures
  in
  { pricer with fut_f0 = Array.of_list fut_f0 }

(** Shocked zero curves over a row range: (hi-lo, 12) per curve_id. *)
let shocked_curves pricer shocks ~lo ~n ~scale =
  let cols_of cid =
    try Market.ir pricer.factors cid
    with Market.Market_data_error _ -> (
      match String.split_on_char ':' cid with
      | [ "SWAP"; ccy ] -> Market.ir pricer.factors ccy
      | _ -> raise (Market.Market_data_error ("unknown factor IR:" ^ cid)))
  in
  List.map
    (fun cid ->
      let grid = find_curve_grid pricer.state cid in
      let cols = cols_of cid in
      let m =
        Array.init n (fun r -> shocked_curve grid cols shocks.(lo + r) scale)
      in
      (cid, m))
    (curve_ids pricer.book)

let fx_levels pricer shocks ~lo ~n ~scale =
  List.map
    (fun ccy ->
      let level = List.assoc ccy pricer.state.Market.fx in
      let arr =
        match Market.fx pricer.factors ccy with
        | None -> Array.make n level
        | Some j ->
            Array.init n (fun r -> level *. exp (shocks.(lo + r).(j) *. scale))
      in
      (ccy, arr))
    pricer.fx_ccys

let roll pricer h =
  match Hashtbl.find_opt pricer.rolled h with
  | Some r -> r
  | None ->
      let dt =
        float_of_int
          (Calendar.to_ordinal
             (Calendar.add_business_days pricer.state.Market.as_of h)
          - Calendar.to_ordinal pricer.state.Market.as_of)
        /. year_days
      in
      let per_bond =
        List.map
          (fun (b : bond_spec) ->
            let paid = Array.map (fun t -> t <= dt) b.times in
            let keep i = not paid.(i) in
            let idx =
              Array.init (Array.length b.times) Fun.id
              |> Array.to_list |> List.filter keep
            in
            let times =
              Array.of_list (List.map (fun i -> b.times.(i) -. dt) idx)
            in
            let w = Market.interp_weights times in
            let received = ref 0. in
            Array.iteri
              (fun i p -> if p then received := !received +. b.cashflows.(i))
              paid;
            (times, w, !received))
          pricer.book.bonds
      in
      Hashtbl.replace pricer.rolled h (dt, per_bond);
      (dt, per_bond)

type workspace = { mutable out : float array array }
(** Per-domain scratch buffers (P7): reused across chunks so big runs stop
    churning the major heap. Sized exactly per call — never reused across
    different shapes — since readers fold over physical rows; a wider leftover
    buffer from another book would silently corrupt totals (the P7d server runs
    many books in one process). Each domain warms its own via [Domain.DLS]. *)

let workspace_key : workspace Domain.DLS.key =
  Domain.DLS.new_key (fun () -> { out = [||] })

let domain_workspace () = Domain.DLS.get workspace_key

(** [n × n_pos] scratch output, reused across same-shape calls. Exact-size: any
    shape change reallocates. *)
let ws_out pricer n =
  let ws = domain_workspace () in
  let a = ws.out in
  let cols_ok =
    n = 0 || (Array.length a > 0 && Array.length a.(0) = pricer.n_pos)
  in
  if Array.length a <> n || not cols_ok then (
    ws.out <- (if n = 0 then [||] else Array.make_matrix n pricer.n_pos 0.);
    ws.out)
  else a

(** Position values into [out] over shock rows [lo,hi), scaled, in base
    currency (conversion applied in place). [out] must be at least
    [(hi-lo) × n_pos]. Pure apart from the [out] writes and the (pre-warmed)
    roll cache. *)
let values_into pricer shocks ~lo ~hi ?(horizon_days = 0) ?(vol_shift = [])
    ?(scale = 1.0) ?(vol_sigma = None) ~out () =
  let n = hi - lo in
  let k = Market.size pricer.factors in
  for i = lo to hi - 1 do
    if Array.length shocks.(i) <> k then
      invalid_arg
        (Printf.sprintf "shocks row %d has %d factors, expected %d" i
           (Array.length shocks.(i))
           k)
  done;
  if Array.length pricer.lin_pos > 0 then
    for r = 0 to n - 1 do
      Array.iteri
        (fun j p ->
          out.(r).(p) <-
            pricer.lin_v0.(j)
            *. exp (shocks.(lo + r).(pricer.lin_f.(j)) *. scale))
        pricer.lin_pos
    done;
  let curves = shocked_curves pricer shocks ~lo ~n ~scale in
  let curve_of cid = List.assoc cid curves in
  List.iter2
    (fun (f : future_spec) f0 ->
      let eq_j = Market.eq pricer.factors f.underlying in
      let s0 = List.assoc f.underlying pricer.state.Market.prices in
      let t =
        float_of_int (Int.max (f.days_to_expiry - horizon_days) 0)
        /. trading_days
      in
      let w = (Market.interp_weights [| Float.max t 1e-9 |]).(0) in
      let crow = curve_of (swap_curve_id f.currency) in
      for r = 0 to n - 1 do
        let s = s0 *. exp (shocks.(lo + r).(eq_j) *. scale) in
        let rr = ref 0. in
        for tt = 0 to Array.length crow.(r) - 1 do
          rr := !rr +. (crow.(r).(tt) *. w.(tt))
        done;
        let fp = s *. exp ((!rr -. f.dividend_yield) *. t) in
        out.(r).(f.pos) <- f.contracts *. (fp -. f0)
      done)
    pricer.book.futures
    (Array.to_list pricer.fut_f0);
  List.iteri
    (fun oi (o : option_spec) ->
      let eq_j = Market.eq pricer.factors o.underlying in
      let s0 = List.assoc o.underlying pricer.state.Market.prices in
      let t =
        float_of_int (Int.max (o.days_to_expiry - horizon_days) 0)
        /. trading_days
      in
      let w = (Market.interp_weights [| Float.max t 1e-9 |]).(0) in
      let crow = curve_of (swap_curve_id o.currency) in
      let s =
        Array.init n (fun r -> s0 *. exp (shocks.(lo + r).(eq_j) *. scale))
      in
      let rr =
        Array.init n (fun r ->
            let acc = ref 0. in
            for tt = 0 to Array.length crow.(r) - 1 do
              acc := !acc +. (crow.(r).(tt) *. w.(tt))
            done;
            !acc)
      in
      let base_shift =
        match List.assoc_opt o.underlying vol_shift with
        | None -> 0.
        | Some v -> v
      in
      let sg =
        Array.init n (fun r ->
            Float.max
              ((match vol_sigma with None -> o.sigma | Some m -> m.(r).(oi))
              +. base_shift)
              1e-6)
      in
      let px =
        Options.option_price_v ~style:o.style ~s ~k:o.strike ~t ~r:rr
          ~q:o.dividend_yield ~sigma:sg ~call:o.call
      in
      for r = 0 to n - 1 do
        out.(r).(o.pos) <- o.contracts *. px.(r)
      done)
    pricer.book.options;
  let dt, rolled = roll pricer horizon_days in
  List.iter2
    (fun (b : bond_spec) (times, w, received) ->
      let crow = curve_of b.curve in
      let n_cf = Array.length times in
      let remaining =
        Array.sub b.cashflows (Array.length b.cashflows - n_cf) n_cf
      in
      for r = 0 to n - 1 do
        let pv =
          if n_cf = 0 then 0.
          else
            let s = ref 0. in
            for c = 0 to n_cf - 1 do
              let z = ref 0. in
              for tt = 0 to Array.length crow.(r) - 1 do
                z := !z +. (crow.(r).(tt) *. w.(c).(tt))
              done;
              s := !s +. (exp (-.(!z +. b.spread) *. times.(c)) *. remaining.(c))
            done;
            !s
        in
        out.(r).(b.pos) <- b.face *. (pv +. received)
      done)
    pricer.book.bonds rolled;
  List.iter
    (fun p ->
      let q = (List.nth pricer.book.positions p).quantity in
      for r = 0 to n - 1 do
        out.(r).(p) <- q
      done)
    pricer.book.cash;
  let fx = fx_levels pricer shocks ~lo ~n ~scale in
  let fx_of ccy = List.assoc ccy fx in
  List.iter
    (fun (fwd : fx_forward_spec) ->
      let t = Float.max (fwd.t -. dt) 0. in
      let w = (Market.interp_weights [| Float.max t 1e-9 |]).(0) in
      let buy_curve = curve_of (swap_curve_id fwd.buy_ccy)
      and sell_curve = curve_of (swap_curve_id fwd.sell_ccy) in
      let buy_fx = fx_of fwd.buy_ccy and sell_fx = fx_of fwd.sell_ccy in
      for r = 0 to n - 1 do
        let rb = ref 0. and rs = ref 0. in
        for tt = 0 to Array.length buy_curve.(r) - 1 do
          rb := !rb +. (buy_curve.(r).(tt) *. w.(tt));
          rs := !rs +. (sell_curve.(r).(tt) *. w.(tt))
        done;
        let buy = fwd.buy_notional *. exp (-. !rb *. t) *. buy_fx.(r) in
        let sell = fwd.sell_notional *. exp (-. !rs *. t) *. sell_fx.(r) in
        out.(r).(fwd.pos) <- fwd.contracts *. (buy -. sell)
      done)
    pricer.book.fx_forwards;
  let base = fx_of pricer.book.base_currency in
  let conv = Array.map (fun ccy -> fx_of ccy) pricer.pos_ccy in
  for r = 0 to n - 1 do
    for p = 0 to pricer.n_pos - 1 do
      out.(r).(p) <- out.(r).(p) *. conv.(p).(r) /. base.(r)
    done
  done

(** Position values in base currency, shape (n_scenarios, n_positions). *)
let values pricer shocks ?(horizon_days = 0) ?(vol_shift = []) ?(scale = 1.0)
    ?(vol_sigma = None) () =
  let n = Array.length shocks in
  if n = 0 then [||]
  else
    let out = Array.make_matrix n pricer.n_pos 0. in
    values_into pricer shocks ~lo:0 ~hi:n ~horizon_days ~vol_shift ~scale
      ~vol_sigma ~out ();
    out

let base_values pricer =
  let k = Market.size pricer.factors in
  (values pricer [| Array.make k 0. |] ()).(0)

(** Bond analytics (YTM family), periodic with coupon frequency m.
    Price per unit par at yield y: Σ CF/(1+y/m)^(m·t), t=Actual/365.
    YTM solves periodic price = engine local PV (continuous curve+spread).
    Mac/Mod/Convexity are YTM-weighted; effective convexity is bump-based. *)

let bond_price_at_y times cfs m y =
  let base = 1. +. y /. float_of_int m in
  if base <= 0. then Float.infinity
  else
    let s = ref 0. in
    for i = 0 to Array.length times - 1 do
      s :=
        !s +. cfs.(i) /. (base ** (float_of_int m *. times.(i)))
    done;
    !s

let bond_ytm times cfs m target =
  if not (Float.is_finite target && target > 0.) then Float.nan
  else if Array.length times = 0 then Float.nan
  else
    let f y = bond_price_at_y times cfs m y -. target in
    let lo = -.float_of_int m +. 1e-6 and hi = ref 10. in
    let flo = f lo in
    if not (flo > 0.) then Float.nan
    else (
      while f !hi > 0. && !hi < 1e6 do
        hi := !hi *. 2.
      done;
      if f !hi > 0. then Float.nan
      else
        try Quotes.brent f lo !hi ~tol:1e-12 ~maxiter:100
        with Invalid_argument _ -> Float.nan)

let bond_mac_mod_conv times cfs m y =
  if not (Float.is_finite y) then (Float.nan, Float.nan, Float.nan)
  else
    let base = 1. +. y /. float_of_int m in
    if base <= 0. then (Float.nan, Float.nan, Float.nan)
    else
      let price = bond_price_at_y times cfs m y in
      if not (price > 0.) then (Float.nan, Float.nan, Float.nan)
      else
        let mac = ref 0. and conv = ref 0. in
        for i = 0 to Array.length times - 1 do
          let t = times.(i) in
          let df = cfs.(i) /. (base ** (float_of_int m *. t)) /. price in
          mac := !mac +. t *. df;
          conv := !conv +. t *. (t +. 1. /. float_of_int m) *. df
        done;
        let macv = !mac in
        let modv = macv /. base in
        let convv = !conv /. (base *. base) in
        (macv, modv, convv)

type exposures = {
  market_value : float array;
  exposure : float array;
  delta_equivalent : float array;
  dv01 : float array;
  ytm : float array;
  mac : float array;
  mod_ytm : float array;
  convexity : float array;
  convexity_eff : float array;
  fx_exposure : (string * float) list;
}

(** Per-position exposure measures at the as-of state (base currency). *)
let exposures pricer =
  let k = Market.size pricer.factors in
  let zero = [| Array.make k 0. |] in
  let v0 = (values pricer zero ()).(0) in
  let fx = fx_levels pricer zero ~lo:0 ~n:1 ~scale:1.0 in
  let fx0 ccy = (List.assoc ccy fx).(0) in
  let base = fx0 pricer.book.base_currency in
  let exposure = Array.copy v0 in
  let delta_eq = Array.make pricer.n_pos 0. in
  (match pricer.book.options with
  | [] -> ()
  | opts ->
      let n_opt = List.length opts in
      let bumps = Array.make_matrix (2 * n_opt) k 0. in
      List.iteri
        (fun kk (o : option_spec) ->
          let j = Market.eq pricer.factors o.underlying in
          bumps.(2 * kk).(j) <- Float.log 1.01;
          bumps.((2 * kk) + 1).(j) <- Float.log 0.99)
        opts;
      let bumped = values pricer bumps () in
      List.iteri
        (fun kk (o : option_spec) ->
          let dv = bumped.(2 * kk).(o.pos) -. bumped.((2 * kk) + 1).(o.pos) in
          delta_eq.(o.pos) <- dv /. 0.02;
          exposure.(o.pos) <- delta_eq.(o.pos))
        opts);
  List.iter2
    (fun (f : future_spec) f0 ->
      exposure.(f.pos) <- f.contracts *. f0 *. fx0 f.currency /. base)
    pricer.book.futures
    (Array.to_list pricer.fut_f0);
  let dv01 = Array.make pricer.n_pos 0. in
  let ytm = Array.make pricer.n_pos Float.nan in
  let mac = Array.make pricer.n_pos Float.nan in
  let mod_ytm = Array.make pricer.n_pos Float.nan in
  let convexity = Array.make pricer.n_pos Float.nan in
  let convexity_eff = Array.make pricer.n_pos Float.nan in
  let dy = 0.0001 in
  List.iter
    (fun cid ->
      let cols =
        try Market.ir pricer.factors cid
        with Market.Market_data_error _ -> (
          match String.split_on_char ':' cid with
          | [ "SWAP"; ccy ] -> Market.ir pricer.factors ccy
          | _ -> raise (Market.Market_data_error ("unknown factor IR:" ^ cid)))
      in
      let bump_up = Array.make_matrix 1 k 0. in
      let bump_dn = Array.make_matrix 1 k 0. in
      Array.iter
        (fun j ->
          bump_up.(0).(j) <- dy;
          bump_dn.(0).(j) <- -.dy)
        cols;
      let b_up = (values pricer bump_up ()).(0) in
      let b_dn = (values pricer bump_dn ()).(0) in
      List.iter
        (fun (b : bond_spec) ->
          if b.curve = cid then (
            dv01.(b.pos) <- v0.(b.pos) -. b_up.(b.pos);
            let p0 = v0.(b.pos) in
            if Float.is_finite p0 && p0 <> 0. then
              convexity_eff.(b.pos) <-
                (b_up.(b.pos) +. b_dn.(b.pos) -. 2. *. p0)
                /. (p0 *. dy *. dy)))
        pricer.book.bonds)
    (curve_ids pricer.book);
  List.iter
    (fun (b : bond_spec) ->
      let fx_ccy = fx0 b.currency in
      if b.face <> 0. && fx_ccy <> 0. then (
        let target = v0.(b.pos) *. base /. (b.face *. fx_ccy) in
        let y = bond_ytm b.times b.cashflows b.freq target in
        ytm.(b.pos) <- y;
        let macv, modv, convv = bond_mac_mod_conv b.times b.cashflows b.freq y in
        mac.(b.pos) <- macv;
        mod_ytm.(b.pos) <- modv;
        convexity.(b.pos) <- convv))
    pricer.book.bonds;
  let fx_tot = Hashtbl.create 8 in
  let add ccy v =
    Hashtbl.replace fx_tot ccy
      (v +. (Hashtbl.find_opt fx_tot ccy |> Option.value ~default:0.))
  in
  List.iter
    (fun (info : position_info) ->
      if
        info.inst_type <> Refdata.Future && info.inst_type <> Refdata.Fx_forward
      then add info.currency exposure.(info.index))
    pricer.book.positions;
  List.iter
    (fun (fwd : fx_forward_spec) ->
      let w = (Market.interp_weights [| fwd.t |]).(0) in
      List.iter
        (fun (ccy, notional, sign) ->
          let grid = find_curve_grid pricer.state (swap_curve_id ccy) in
          let df = exp (-.(rowdot [| grid |] w).(0) *. fwd.t) in
          add ccy
            (sign *. fwd.contracts *. notional *. df
            *. List.assoc ccy pricer.state.Market.fx
            /. base))
        [
          (fwd.buy_ccy, fwd.buy_notional, 1.);
          (fwd.sell_ccy, fwd.sell_notional, -1.);
        ])
    pricer.book.fx_forwards;
  {
    market_value = v0;
    exposure;
    delta_equivalent = delta_eq;
    dv01;
    ytm;
    mac;
    mod_ytm;
    convexity;
    convexity_eff;
    fx_exposure =
      Hashtbl.fold (fun c v acc -> (c, v) :: acc) fx_tot []
      |> List.sort (fun (a, _) (b, _) -> String.compare a b);
  }
