(** Dev playground: inspect calculations and benchmark the engine.

    Non-production diagnostic tool (no stability promises, Linux-leaning for
    peak-RSS reads). Subcommands:

      book --fixture DIR
        Position table, factor inventory, base values, NAV.
      price --fixture DIR --id IID [--spot-bump F] [--vol-bump V] [--horizon N]
        One-position shock lab: base vs shocked value plus bump Greeks.
      bench-run --fixture DIR [--mc N]
        run_engine stages with per-stage timing, Gc allocation deltas and
        peak RSS (VmHWM).

    Memory numbers come from [Gc.stat]/[Gc.allocated_bytes] (words are
    8 bytes on 64-bit) plus VmHWM from /proc/self/status. *)

open Risk_core
open Risk_engine

(* --- fixture loading (same bridge as bin/main.ml) --- *)

let load_fixture dir =
  let read name = Yojson.Basic.from_file (Filename.concat dir name) in
  let manifest = read "manifest.json" in
  let open Yojson.Basic.Util in
  let as_of =
    manifest |> member "as_of" |> to_string |> Calendar.of_string
  in
  let holdings = Holdings.holdings_of_file (Filename.concat dir "holdings.json") in
  let bundle = Marketdata.bundle_of_file (Filename.concat dir "marketdata.json") in
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
    "  [gc] %-16s minor=%7.1fMB major=%7.1fMB live=%7.1fMB heap=%7.1fMB topheap=%7.1fMB%s\n"
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
  Arg.parse_argv (Array.of_list ("book" :: rest))
    [
      ("--fixture", Arg.Set_string dir, "DIR fixture directory");
      ("--base", Arg.Set_string base, "CCY base currency (def EUR)");
    ]
    (fun _ -> ()) "dev book --fixture DIR [--base CCY]";
  if !dir = "" then (prerr_endline "dev book --fixture DIR [--base CCY]"; exit 2);
  let holdings, instruments, bundle, _config, as_of = load_fixture !dir in
  let state = Market.market_state bundle as_of in
  let book = Pricing.build_book holdings instruments state !base None in
  let pricer = Pricing.make_pricer book state
      (Market.build_factors bundle ~equity_ids:(Pricing.equity_ids book)
         ~fx_ccys:(Pricing.currencies book)
         ~curve_ccys:(Pricing.curve_currencies book)
         ~history_days:None)
  in
  let v0 = Pricing.base_values pricer in
  Printf.printf "book as_of=%s base=%s positions=%d\n" (Calendar.to_string as_of)
    !base (List.length book.positions);
  Printf.printf "%-4s %-36s %-12s %-4s %12s %14s\n" "idx" "instrument" "type"
    "ccy" "qty" "base_value";
  List.iter
    (fun (p : Pricing.position_info) ->
      Printf.printf "%-4d %-36s %-12s %-4s %12.4f %14.2f\n" p.index
        p.instrument_id p.inst_type p.currency p.quantity v0.(p.index))
    book.positions;
  Printf.printf "underlyings: %s\n"
    (String.concat "," (Pricing.equity_ids book));
  Printf.printf "fx: %s | curves: %s\n"
    (String.concat "," (Pricing.currencies book))
    (String.concat "," (Pricing.curve_currencies book));
  Printf.printf "factors: %d (%s)\n" (Market.size pricer.Pricing.factors)
    (String.concat "," (Array.to_list pricer.Pricing.factors.names));
  Printf.printf "nav: %.2f %s\n" (Array.fold_left ( +. ) 0. v0)
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
  let dir = ref "" and id = ref "" and spot = ref 0. and vol = ref 0. and h = ref 0 and base = ref "EUR" in
  Arg.parse_argv (Array.of_list ("price" :: rest))
    [
      ("--fixture", Arg.Set_string dir, "DIR fixture directory");
      ("--base", Arg.Set_string base, "CCY base currency (def EUR)");
      ("--id", Arg.Set_string id, "IID position instrument id");
      ("--spot-bump", Arg.Set_float spot, "F relative spot shock (def 0)");
      ("--vol-bump", Arg.Set_float vol, "V absolute vol shock (def 0)");
      ("--horizon", Arg.Set_int h, "N horizon days (def 0)");
    ]
    (fun _ -> ()) "dev price --fixture DIR --id IID [opts]";
  if !dir = "" || !id = "" then (
    prerr_endline "dev price --fixture DIR --id IID [--spot-bump F] [--vol-bump V] [--horizon N] [--base CCY]";
    exit 2);
  let holdings, instruments, bundle, _config, as_of = load_fixture !dir in
  let state = Market.market_state bundle as_of in
  let book = Pricing.build_book holdings instruments state !base None in
  let factors =
    Market.build_factors bundle ~equity_ids:(Pricing.equity_ids book)
      ~fx_ccys:(Pricing.currencies book)
      ~curve_ccys:(Pricing.curve_currencies book) ~history_days:None
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
    List.find_opt (fun (o : Pricing.option_spec) -> o.Pricing.pos = pos)
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
  Printf.printf "position %s underlying=%s base=%.4f shocked=%.4f pnl=%.4f (h=%d)\n"
    !id und v0 v1 (v1 -. v0) !h;
  (* bump Greeks, central differences *)
  let d_px = value ~sb:0.01 () -. value ~sb:(-0.01) () in
  let s0 =
    try List.assoc und state.Market.prices with Not_found -> 1.0
  in
  let dv = value ~sb:0.01 () -. (2. *. value ()) +. value ~sb:(-0.01) () in
  Printf.printf "delta=%12.4f gamma=%12.6f (per unit spot, S=%.4f)\n"
    (d_px /. 0.02 /. s0)
    (dv /. (0.01 *. s0 *. (0.01 *. s0)))
    s0;
  (match opt with
  | None -> ()
  | Some _ ->
      let vv = value ~vb:0.01 () -. value ~vb:(-0.01) () in
      Printf.printf "vega=%12.4f (per 1.00 vol)\n" (vv /. 0.02))

(* --- bench-run: stage breakdown with Gc deltas --- *)

let cmd_bench rest =
  let dir = ref "" and mc = ref 10_000 and base = ref "EUR" in
  Arg.parse_argv (Array.of_list ("bench-run" :: rest))
    [
      ("--fixture", Arg.Set_string dir, "DIR fixture directory");
      ("--mc", Arg.Set_int mc, "N MC scenarios (default 10000)");
      ("--base", Arg.Set_string base, "CCY base currency (def EUR)");
    ]
    (fun _ -> ()) "dev bench-run --fixture DIR [--mc N] [--base CCY]";
  if !dir = "" then (prerr_endline "dev bench-run --fixture DIR [--mc N] [--base CCY]"; exit 2);
  let holdings, instruments, bundle, config, as_of =
    timed "load fixtures" (fun () -> load_fixture !dir)
  in
  let state = timed "market_state" (fun () -> Market.market_state bundle as_of) in
  let book =
    timed "build_book" (fun () ->
        Pricing.build_book holdings instruments state !base None)
  in
  let factors =
    timed "build_factors" (fun () ->
        Market.build_factors bundle ~equity_ids:(Pricing.equity_ids book)
          ~fx_ccys:(Pricing.currencies book)
          ~curve_ccys:(Pricing.curve_currencies book)
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
    timed "run historical"
      (fun () -> Run.run_engine inputs config ())
  in
  Printf.printf "hist: %d scenarios x %d positions\n"
    (Array.length factors.history) n_pos;
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
  Printf.printf "%-28s %8.3f s  (%d evals, %.2f us/eval)\n%!" "run MC"
    dt evals
    (dt *. 1e6 /. float_of_int evals);
  print_gc_delta "run MC" g0 g1;
  ignore mc_res;
  (match peak_rss_kb () with
  | None -> Printf.printf "peak_rss: n/a (non-Linux?)\n"
  | Some kb -> Printf.printf "peak_rss: %d MB\n" (kb / 1024))

let usage =
  "dev: engine playground (non-production diagnostics)\n\
   usage: dev (book|price|bench-run) [opts]\n\
  \  book --fixture DIR [--base CCY]\n\
  \  price --fixture DIR --id IID [--spot-bump F] [--vol-bump V] [--horizon N] [--base CCY]\n\
  \  bench-run --fixture DIR [--mc N] [--base CCY]"

let () =
  match Array.to_list Sys.argv with
  | _ :: "book" :: rest -> cmd_book rest
  | _ :: "price" :: rest -> cmd_price rest
  | _ :: "bench-run" :: rest -> cmd_bench rest
  | _ ->
      prerr_endline usage;
      exit 2
