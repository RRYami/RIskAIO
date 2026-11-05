(** Dev playground: inspect calculations and benchmark the engine.

    Non-production diagnostic tool (no stability promises, Linux-leaning for
    peak-RSS reads). Subcommands:

    book --fixture DIR Position table, factor inventory, base values, NAV. price
    --fixture DIR --id IID [--spot-bump F] [--vol-bump V] [--horizon N]
    One-position shock lab: base vs shocked value plus bump Greeks. option
    --style european|american --right call|put --spot S --strike K --years T
    --rate R --div Q --vol SIG [--price P] Direct single-option pricer: Black vs
    BS-2002 plus bump Greeks; with --price P it inverts to implied vol instead
    (European needs OTM, American handles ITM). bench-run --fixture DIR [--mc N]
    run_engine stages with per-stage timing, Gc allocation deltas and peak RSS
    (VmHWM).

    Memory numbers come from [Gc.stat]/[Gc.allocated_bytes] (words are 8 bytes
    on 64-bit) plus VmHWM from /proc/self/status. *)

open Risk_core
open Risk_engine

(* --- fixture loading (same bridge as bin/main.ml) --- *)

let load_fixture dir =
  let read name = Yojson.Basic.from_file (Filename.concat dir name) in
  let manifest = read "manifest.json" in
  let open Yojson.Basic.Util in
  let as_of = manifest |> member "as_of" |> to_string |> Calendar.of_string in
  let holdings =
    Holdings.holdings_of_file (Filename.concat dir "holdings.json")
  in
  let bundle =
    Marketdata.bundle_of_file (Filename.concat dir "marketdata.json")
  in
  let config = Config.of_yojson (read "config.json") in
  let instruments =
    Refdata.instruments_of_file (Filename.concat dir "instruments.json")
  in
  (holdings, instruments, bundle, config, as_of)

(* --- memory probing --- *)

type gc_snap = {
  minor : float;
  major : float;
  live : int;
  heap : int;
  top_heap : int;
  peak_rss_kb : int option;
}

let peak_rss_kb () =
  try
    let ic = open_in "/proc/self/status" in
    let rec go () =
      match input_line ic with
      | line -> (
          match String.split_on_char ':' line with
          | [ k; v ] when String.trim k = "VmHWM" -> (
              close_in ic;
              match String.split_on_char ' ' (String.trim v) with
              | n :: _ -> Some (int_of_string (String.trim n))
              | [] -> None)
          | _ -> go ())
      | exception End_of_file ->
          close_in ic;
          None
    in
    go ()
  with _ -> None

let gc_snapshot () =
  let s = Gc.stat () in
  {
    minor = s.minor_words;
    major = s.major_words;
    live = s.live_words;
    heap = s.heap_words;
    top_heap = s.top_heap_words;
    peak_rss_kb = peak_rss_kb ();
  }

let mib_of_words w = float_of_int w *. 8. /. (1024. *. 1024.)

let print_gc_delta label (a : gc_snap) (b : gc_snap) =
  Printf.printf
    "  [gc] %-16s minor=%7.1fMB major=%7.1fMB live=%7.1fMB heap=%7.1fMB \
     topheap=%7.1fMB%s\n"
    label
    (mib_of_words (int_of_float (b.minor -. a.minor)))
    (mib_of_words (int_of_float (b.major -. a.major)))
    (mib_of_words b.live) (mib_of_words b.heap) (mib_of_words b.top_heap)
    (match (a.peak_rss_kb, b.peak_rss_kb) with
    | _, Some after -> Printf.sprintf " peak_rss=%dMB" (after / 1024)
    | _ -> "")

let timed label f =
  let g0 = gc_snapshot () in
  let t0 = Unix.gettimeofday () in
  let v = f () in
  let dt = Unix.gettimeofday () -. t0 in
  let g1 = gc_snapshot () in
  Printf.printf "%-28s %8.3f s\n%!" label dt;
  print_gc_delta label g0 g1;
  v

(* --- book --- *)

let cmd_book rest =
  let dir = ref "" and base = ref "EUR" in
  Arg.parse_argv
    (Array.of_list ("book" :: rest))
    [
      ("--fixture", Arg.Set_string dir, "DIR fixture directory");
      ("--base", Arg.Set_string base, "CCY base currency (def EUR)");
    ]
    (fun _ -> ())
    "dev book --fixture DIR [--base CCY]";
  if !dir = "" then (
    prerr_endline "dev book --fixture DIR [--base CCY]";
    exit 2);
  let holdings, instruments, bundle, _config, as_of = load_fixture !dir in
  let state = Market.market_state bundle as_of in
  let book = Pricing.build_book holdings instruments state !base None in
  let pricer =
    Pricing.make_pricer book state
      (Market.build_factors bundle ~equity_ids:(Pricing.equity_ids book)
         ~fx_ccys:(Pricing.currencies book)
         ~curve_ccys:(Pricing.curve_ids book)
         ~history_days:None)
  in
  let v0 = Pricing.base_values pricer in
  Printf.printf "book as_of=%s base=%s positions=%d\n"
    (Calendar.to_string as_of) !base
    (List.length book.positions);
  Printf.printf "%-4s %-36s %-12s %-4s %12s %14s\n" "idx" "instrument" "type"
    "ccy" "qty" "base_value";
  List.iter
    (fun (p : Pricing.position_info) ->
      Printf.printf "%-4d %-36s %-12s %-4s %12.4f %14.2f\n" p.index
        p.instrument_id
        (Risk_core.Refdata.string_of_inst_type p.inst_type)
        p.currency p.quantity v0.(p.index))
    book.positions;
  Printf.printf "underlyings: %s\n"
    (String.concat "," (Pricing.equity_ids book));
  Printf.printf "fx: %s | curves: %s\n"
    (String.concat "," (Pricing.currencies book))
    (String.concat "," (Pricing.curve_ids book));
  Printf.printf "factors: %d (%s)\n"
    (Market.size pricer.Pricing.factors)
    (String.concat "," (Array.to_list pricer.Pricing.factors.names));
  Printf.printf "nav: %.2f %s\n"
    (Array.fold_left ( +. ) 0. v0)
    book.Pricing.base_currency

(* --- price: one-position shock lab --- *)

let shock_for pricer underlying bump =
  let k = Market.size pricer.Pricing.factors in
  let sh = Array.make_matrix 1 k 0. in
  (try
     let j = Market.eq pricer.Pricing.factors underlying in
     sh.(0).(j) <- Float.log1p bump
   with Market.Market_data_error _ -> ());
  sh

let cmd_price rest =
  let dir = ref ""
  and id = ref ""
  and spot = ref 0.
  and vol = ref 0.
  and h = ref 0
  and base = ref "EUR" in
  Arg.parse_argv
    (Array.of_list ("price" :: rest))
    [
      ("--fixture", Arg.Set_string dir, "DIR fixture directory");
      ("--base", Arg.Set_string base, "CCY base currency (def EUR)");
      ("--id", Arg.Set_string id, "IID position instrument id");
      ("--spot-bump", Arg.Set_float spot, "F relative spot shock (def 0)");
      ("--vol-bump", Arg.Set_float vol, "V absolute vol shock (def 0)");
      ("--horizon", Arg.Set_int h, "N horizon days (def 0)");
    ]
    (fun _ -> ())
    "dev price --fixture DIR --id IID [opts]";
  if !dir = "" || !id = "" then (
    prerr_endline
      "dev price --fixture DIR --id IID [--spot-bump F] [--vol-bump V] \
       [--horizon N] [--base CCY]";
    exit 2);
  let holdings, instruments, bundle, _config, as_of = load_fixture !dir in
  let state = Market.market_state bundle as_of in
  let book = Pricing.build_book holdings instruments state !base None in
  let factors =
    Market.build_factors bundle ~equity_ids:(Pricing.equity_ids book)
      ~fx_ccys:(Pricing.currencies book)
      ~curve_ccys:(Pricing.curve_ids book)
      ~history_days:None
  in
  let pricer = Pricing.make_pricer book state factors in
  let pos =
    match
      List.find_opt
        (fun (p : Pricing.position_info) -> p.instrument_id = !id)
        book.positions
    with
    | None ->
        prerr_endline ("unknown position: " ^ !id);
        exit 2
    | Some p -> p.index
  in
  let opt =
    List.find_opt
      (fun (o : Pricing.option_spec) -> o.Pricing.pos = pos)
      book.options
  in
  let und =
    match opt with
    | Some o -> o.Pricing.underlying
    | None -> (
        match
          List.find_opt
            (fun (p : Pricing.position_info) -> p.index = pos)
            book.positions
        with
        | None -> ""
        | Some p -> p.instrument_id)
  in
  let v0 = (Pricing.base_values pricer).(pos) in
  let value ?(sb = 0.) ?(vb = 0.) () =
    let sh = shock_for pricer und sb in
    let out =
      match opt with
      | None -> Pricing.values pricer sh ~horizon_days:!h ()
      | Some _ ->
          let oi =
            let rec nth i = function
              | [] -> 0
              | (o : Pricing.option_spec) :: _ when o.Pricing.pos = pos -> i
              | _ :: rest -> nth (i + 1) rest
            in
            nth 0 book.options
          in
          let base =
            (List.find
               (fun (o : Pricing.option_spec) -> o.Pricing.pos = pos)
               book.options)
              .Pricing.sigma
          in
          let vs =
            [|
              Array.init (List.length book.options) (fun j ->
                  if j = oi then base +. vb else base);
            |]
          in
          Pricing.values pricer sh ~horizon_days:!h ~vol_sigma:(Some vs) ()
    in
    out.(0).(pos)
  in
  let v1 = value ~sb:!spot ~vb:!vol () in
  Printf.printf
    "position %s underlying=%s base=%.4f shocked=%.4f pnl=%.4f (h=%d)\n" !id und
    v0 v1 (v1 -. v0) !h;
  (* bump Greeks, central differences *)
  let d_px = value ~sb:0.01 () -. value ~sb:(-0.01) () in
  let s0 = try List.assoc und state.Market.prices with Not_found -> 1.0 in
  let dv = value ~sb:0.01 () -. (2. *. value ()) +. value ~sb:(-0.01) () in
  Printf.printf "delta=%12.4f gamma=%12.6f (per unit spot, S=%.4f)\n"
    (d_px /. 0.02 /. s0)
    (dv /. (0.01 *. s0 *. (0.01 *. s0)))
    s0;
  match opt with
  | None -> ()
  | Some _ ->
      let vv = value ~vb:0.01 () -. value ~vb:(-0.01) () in
      Printf.printf "vega=%12.4f (per 1.00 vol)\n" (vv /. 0.02)

(* --- option: direct single-option pricer --- *)

let parse_style s =
  match String.lowercase_ascii (String.trim s) with
  | "european" | "e" -> `European
  | "american" | "a" -> `American
  | x ->
      raise
        (Invalid_argument ("bad --style: " ^ x ^ " (want european|american)"))

let parse_right s =
  match String.lowercase_ascii (String.trim s) with
  | "call" | "c" -> true
  | "put" | "p" -> false
  | x -> raise (Invalid_argument ("bad --right: " ^ x ^ " (want call|put)"))

let check_finite name x =
  if not (Float.is_finite x) then
    raise (Invalid_argument (name ^ " must be finite"))

let scalar_price ~american ~call ~s ~k ~t ~r ~q ~sigma =
  let b = r -. q in
  if american then Options.bs2002_1 ~s ~k ~t ~r ~b ~sigma ~call
  else Options.bs_1 ~s ~k ~t ~r ~b ~sigma ~call

(** Discounted intrinsic floor (European lower bound, as in quotes). *)
let dfloor ~call ~s ~k ~t ~r ~q =
  let f = s *. exp ((r -. q) *. t) and d = exp (-.r *. t) in
  if call then Float.max (d *. (f -. k)) 0. else Float.max (d *. (k -. f)) 0.

let iv_of_price ~american ~call ~s ~k ~t ~r ~q ~price =
  let right = if call then Quotes.Call else Quotes.Put in
  let qot : Quotes.quote =
    {
      date = "dev";
      underlying = "dev";
      expiry = "dev";
      right;
      strike = k;
      bid = price;
      ask = price;
      mid = price;
      spot = s;
      rate = r;
      div = q;
    }
  in
  if american then Quotes.iv_of_quote_amer qot ~t else Quotes.iv_of_quote qot ~t

let cmd_option rest =
  let style = ref "european"
  and right = ref "call"
  and s = ref Float.nan
  and k = ref Float.nan
  and t = ref Float.nan
  and r = ref 0.
  and q = ref 0.
  and sg = ref Float.nan
  and px = ref (-1.) in
  Arg.parse_argv
    (Array.of_list ("option" :: rest))
    [
      ("--style", Arg.Set_string style, "STYLE european|american (def european)");
      ("--right", Arg.Set_string right, "RIGHT call|put (def call)");
      ("--spot", Arg.Set_float s, "S spot (required)");
      ("--strike", Arg.Set_float k, "K strike (required)");
      ("--years", Arg.Set_float t, "T years to expiry (required)");
      ("--rate", Arg.Set_float r, "R rate (def 0)");
      ("--div", Arg.Set_float q, "Q dividend yield (def 0)");
      ("--vol", Arg.Set_float sg, "SIGMA vol (required to price)");
      ("--price", Arg.Set_float px, "P invert to implied vol instead");
    ]
    (fun _ -> ())
    "dev option --style S --right R --spot S --strike K --years T [opts]";
  if !s <> !s || !k <> !k || !t <> !t || ((not (!px > 0.)) && !sg <> !sg) then (
    prerr_endline
      "dev option --style european|american --right call|put --spot S --strike \
       K --years T [--rate R] [--div Q] [--vol SIG | --price P]";
    exit 2);
  try
    let american =
      match parse_style !style with `European -> false | `American -> true
    in
    let call = parse_right !right in
    List.iter
      (fun (n, x) -> check_finite n x)
      [ ("spot", !s); ("strike", !k); ("years", !t); ("rate", !r); ("div", !q) ];
    if not (!s > 0.) then raise (Invalid_argument "spot must be > 0");
    if not (!k > 0.) then raise (Invalid_argument "strike must be > 0");
    if not (!t >= 0.) then raise (Invalid_argument "years must be >= 0");
    let fwd = !s *. exp ((!r -. !q) *. !t) in
    let otm = if call then !k > fwd else !k < fwd in
    if !px > 0. then (
      if not (!t > 0.) then
        raise (Invalid_argument "years must be > 0 to invert");
      let iv =
        iv_of_price ~american ~call ~s:!s ~k:!k ~t:!t ~r:!r ~q:!q ~price:!px
      in
      let chk =
        scalar_price ~american ~call ~s:!s ~k:!k ~t:!t ~r:!r ~q:!q ~sigma:iv
      in
      Printf.printf "style=%s right=%s S=%.4f K=%.4f T=%.4f r=%.4f q=%.4f\n"
        !style !right !s !k !t !r !q;
      Printf.printf "target=%.6f otm=%b\n" !px otm;
      Printf.printf "iv=%.6f reprice=%.6f\n" iv chk)
    else (
      check_finite "vol" !sg;
      if !t > 0. && not (!sg > 0.) then
        raise (Invalid_argument "vol must be > 0 when years > 0");
      (* at t = 0 both models return intrinsic without touching sigma *)
      let sig_use = if !t > 0. then !sg else 0.2 in
      let p =
        scalar_price ~american ~call ~s:!s ~k:!k ~t:!t ~r:!r ~q:!q
          ~sigma:sig_use
      in
      let fl = dfloor ~call ~s:!s ~k:!k ~t:!t ~r:!r ~q:!q in
      Printf.printf
        "style=%s right=%s S=%.4f K=%.4f T=%.4f r=%.4f q=%.4f sigma=%.4f\n"
        !style !right !s !k !t !r !q !sg;
      Printf.printf "forward=%.4f otm=%b floor=%.6f\n" fwd otm fl;
      Printf.printf "price=%.6f time-value=%.6f\n" p (p -. fl);
      if !t > 0. then (
        let bump = 0.01 *. !s in
        let up =
          scalar_price ~american ~call ~s:(!s +. bump) ~k:!k ~t:!t ~r:!r ~q:!q
            ~sigma:!sg
        in
        let dn =
          scalar_price ~american ~call ~s:(!s -. bump) ~k:!k ~t:!t ~r:!r ~q:!q
            ~sigma:!sg
        in
        let md =
          scalar_price ~american ~call ~s:!s ~k:!k ~t:!t ~r:!r ~q:!q ~sigma:!sg
        in
        let vh = 0.01 in
        let sg_lo = Float.max (!sg -. vh) 1e-6 in
        let v_up =
          scalar_price ~american ~call ~s:!s ~k:!k ~t:!t ~r:!r ~q:!q
            ~sigma:(!sg +. vh)
        in
        let v_dn =
          scalar_price ~american ~call ~s:!s ~k:!k ~t:!t ~r:!r ~q:!q
            ~sigma:sg_lo
        in
        Printf.printf "delta=%.6f (ds=1%% central)\n"
          ((up -. dn) /. (2. *. bump));
        Printf.printf "gamma=%.6f\n" ((up -. (2. *. md) +. dn) /. (bump *. bump));
        Printf.printf "vega=%.6f (per 1.00 vol)\n"
          ((v_up -. v_dn) /. (!sg +. vh -. sg_lo))))
  with
  | Invalid_argument msg | Failure msg ->
      prerr_endline ("dev option: " ^ msg);
      exit 1
  | Quotes.Quotes_error msg ->
      prerr_endline ("dev option: cannot invert: " ^ msg);
      exit 1

(* --- bench-run: stage breakdown with Gc deltas --- *)
let cmd_bench rest =
  let dir = ref "" and mc = ref 10_000 and base = ref "EUR" in
  Arg.parse_argv
    (Array.of_list ("bench-run" :: rest))
    [
      ("--fixture", Arg.Set_string dir, "DIR fixture directory");
      ("--mc", Arg.Set_int mc, "N MC scenarios (default 10000)");
      ("--base", Arg.Set_string base, "CCY base currency (def EUR)");
    ]
    (fun _ -> ())
    "dev bench-run --fixture DIR [--mc N] [--base CCY]";
  if !dir = "" then (
    prerr_endline "dev bench-run --fixture DIR [--mc N] [--base CCY]";
    exit 2);
  let holdings, instruments, bundle, config, as_of =
    timed "load fixtures" (fun () -> load_fixture !dir)
  in
  let state =
    timed "market_state" (fun () -> Market.market_state bundle as_of)
  in
  let book =
    timed "build_book" (fun () ->
        Pricing.build_book holdings instruments state !base None)
  in
  let factors =
    timed "build_factors" (fun () ->
        Market.build_factors bundle ~equity_ids:(Pricing.equity_ids book)
          ~fx_ccys:(Pricing.currencies book)
          ~curve_ccys:(Pricing.curve_ids book)
          ~history_days:(Some config.history_days))
  in
  let inputs : Run.engine_inputs =
    {
      holdings;
      instruments;
      bundle;
      as_of;
      base_currency = !base;
      row_portfolios = None;
      surf_fits = [];
    }
  in
  let n_pos = List.length book.positions in
  let res =
    timed "run historical" (fun () -> Run.run_engine inputs config ())
  in
  Printf.printf "hist: %d scenarios x %d positions\n"
    (Array.length factors.history)
    n_pos;
  ignore res;
  let mc_config : Config.run_config =
    { config with method_ = Config.Montecarlo; n_scenarios = !mc; seed = 0 }
  in
  let t0 = Unix.gettimeofday () in
  let g0 = gc_snapshot () in
  let mc_res = Run.run_engine inputs mc_config () in
  let dt = Unix.gettimeofday () -. t0 in
  let g1 = gc_snapshot () in
  let evals = !mc * n_pos in
  Printf.printf "%-28s %8.3f s  (%d evals, %.2f us/eval)\n%!" "run MC" dt evals
    (dt *. 1e6 /. float_of_int evals);
  print_gc_delta "run MC" g0 g1;
  ignore mc_res;
  match peak_rss_kb () with
  | None -> Printf.printf "peak_rss: n/a (non-Linux?)\n"
  | Some kb -> Printf.printf "peak_rss: %d MB\n" (kb / 1024)

let usage =
  "dev: engine playground (non-production diagnostics)\n\
   usage: dev (book|price|option|bench-run) [opts]\n\
  \  book --fixture DIR [--base CCY]\n\
  \  price --fixture DIR --id IID [--spot-bump F] [--vol-bump V] [--horizon N] \
   [--base CCY]\n\
  \  option --style european|american --right call|put --spot S --strike K \
   --years T [--rate R] [--div Q] [--vol SIG | --price P]\n\
  \  bench-run --fixture DIR [--mc N] [--base CCY]"

let () =
  match Array.to_list Sys.argv with
  | _ :: "book" :: rest -> cmd_book rest
  | _ :: "price" :: rest -> cmd_price rest
  | _ :: "option" :: rest -> cmd_option rest
  | _ :: "bench-run" :: rest -> cmd_bench rest
  | _ ->
      prerr_endline usage;
      exit 2
