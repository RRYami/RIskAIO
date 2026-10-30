(** risk-ai CLI: self-check + fixture parity entry point.

    P0/P2 scope: [check] runs a stdlib-only smoke test; [parity] validates a
    fixture dir from [python/export_fixtures.py] (JSON bridge) and will grow
    into the full golden-file comparison once [run_engine] lands (P4). *)

let version = "0.1.0"

let check () =
  let e = Risk_core.Calendar.easter_sunday 2026 in
  let e_ok = Risk_core.Calendar.to_string e = "2026-04-05" in
  let n = Risk_engine.Options.ndtr 0. in
  let n_ok = abs_float (n -. 0.5) < 1e-15 in
  Printf.printf "risk-ai %s\n" version;
  Printf.printf "easter-2026: %s %s\n"
    (Risk_core.Calendar.to_string e)
    (if e_ok then "OK" else "FAIL");
  Printf.printf "ndtr(0): %.17g %s\n" n (if n_ok then "OK" else "FAIL");
  if not (e_ok && n_ok) then exit 1

let read_json dir name = Yojson.Basic.from_file (Filename.concat dir name)

let parity dir =
  let manifest = read_json dir "manifest.json" in
  let open Yojson.Basic.Util in
  let gen = manifest |> member "generator" |> to_string in
  let as_of = manifest |> member "as_of" |> to_string in
  let holdings =
    Risk_core.Holdings.holdings_of_file (Filename.concat dir "holdings.json")
  in
  let bundle =
    Risk_core.Marketdata.bundle_of_file (Filename.concat dir "marketdata.json")
  in
  let config = Risk_engine.Config.of_yojson (read_json dir "config.json") in
  Risk_engine.Config.validate config;
  let as_of_date = Risk_core.Calendar.of_string as_of in
  let instruments =
    Risk_core.Refdata.instruments_of_file
      (Filename.concat dir "instruments.json")
  in
  let state = Risk_engine.Market.market_state bundle as_of_date in
  let book =
    Risk_engine.Pricing.build_book holdings instruments state "EUR" None
  in
  let factors =
    Risk_engine.Market.build_factors bundle
      ~equity_ids:(Risk_engine.Pricing.equity_ids book)
      ~fx_ccys:(Risk_engine.Pricing.currencies book)
      ~curve_ccys:(Risk_engine.Pricing.curve_currencies book)
      ~history_days:(Some config.history_days)
  in
  let pricer = Risk_engine.Pricing.make_pricer book state factors in
  let nav =
    Array.fold_left ( +. ) 0. (Risk_engine.Pricing.base_values pricer)
  in
  let fx_here =
    List.exists
      (fun (r : Risk_core.Marketdata.fx_row) ->
        Risk_core.Calendar.to_string r.date = as_of)
      bundle.fx
  in
  Printf.printf "generator: %s\nas_of: %s\n" gen as_of;
  Printf.printf "holdings: %d rows\n" (List.length holdings);
  Printf.printf
    "marketdata: %d prices, %d fx, %d curves, %d vols, %d spreads, %d dividends\n"
    (List.length bundle.prices)
    (List.length bundle.fx)
    (List.length bundle.curves)
    (List.length bundle.vols)
    (List.length bundle.spreads)
    (List.length bundle.dividends);
  Printf.printf "config: kind=%s method=%s horizons=%d scenarios=%d\n"
    (Risk_engine.Config.kind config)
    (match config.method_ with
    | Risk_engine.Config.Historical -> "historical"
    | Risk_engine.Config.Montecarlo -> "montecarlo")
    (List.length config.horizons)
    config.n_scenarios;
  Printf.printf "fx-on-as-of: %s\n" (if fx_here then "OK" else "MISSING");
  Printf.printf "model-nav: %.2f %s (%d positions, %d factors)\n" nav
    pricer.book.base_currency
    (List.length book.positions)
    (Risk_engine.Market.size factors);
  let res =
    Risk_engine.Run.run_engine
      Risk_engine.Run.
        {
          holdings;
          instruments;
          bundle;
          as_of = as_of_date;
          base_currency = "EUR";
          row_portfolios = None;
          surf_fits = [];
        }
      config ()
  in
  let var99_1d =
    List.find_opt
      (fun (r : Risk_engine.Run.result_row) ->
        r.measure = "var"
        && List.assoc_opt "confidence" r.slice = Some (`Float 0.99)
        && List.assoc_opt "horizon_days" r.slice = Some (`Int 1))
      res.rows
  in
  (match var99_1d with
  | None -> Printf.printf "var99/1d: MISSING\n"
  | Some r ->
      Printf.printf "var99/1d: %.2f %s (%d rows)\n" r.value
        (Option.value r.value_currency ~default:"?")
        (List.length res.rows));
  let bt_config : Risk_engine.Config.run_config =
    {
      config with
      history_days = 250;
      backtest = Some { days = 40; confidence = 0.99 };
    }
  in
  let bt =
    Risk_engine.Backtest.run_backtest
      Risk_engine.Run.
        {
          holdings;
          instruments;
          bundle;
          as_of = as_of_date;
          base_currency = "EUR";
          row_portfolios = None;
          surf_fits = [];
        }
      bt_config ()
  in
  let _, _, _, summary = Risk_engine.Backtest.summary bt in
  Printf.printf "backtest-40d: %s exceptions, traffic=%s\n"
    (match List.assoc_opt "exceptions" summary with
    | Some (`Int x) -> string_of_int x
    | _ -> "?")
    (match List.assoc_opt "traffic_light" summary with
    | Some (`String s) -> s
    | _ -> "?");
  if not fx_here then exit 1

let usage = "usage: risk-ai (check | parity --fixture DIR)"

let () =
  match Array.to_list Sys.argv with
  | _ :: "check" :: _ -> check ()
  | _ :: "parity" :: rest -> (
      match rest with
      | "--fixture" :: dir :: _ -> parity dir
      | _ ->
          prerr_endline usage;
          exit 2)
  | _ ->
      prerr_endline usage;
      exit 2
