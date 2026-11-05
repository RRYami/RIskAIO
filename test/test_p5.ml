(** P5 tests: backtest statistics (independent goldens) + full oracle comparison
    of run_backtest on mini-v1 fixtures. *)

open Risk_core
open Risk_engine

let assert_close ~rel ~abs_ name expected actual =
  let tol = Float.max (abs_float expected *. rel) abs_ in
  if abs_float (actual -. expected) > tol then
    Alcotest.failf "%s: expected %.17g, got %.17g (tol %.3g)" name expected
      actual tol

(* --- distribution goldens (independent of Python) --- *)

let test_chi2 () =
  Alcotest.check (Alcotest.float 1e-15) "sf0" 1. (Backtest.chi2_sf 0. 1);
  (* 95% critical values: P(X > 3.8414) = 0.05 for df 1, 5.9915 for df 2 *)
  Alcotest.check (Alcotest.float 1e-9) "df1-95" 0.05
    (Backtest.chi2_sf 3.841458820694124 1);
  Alcotest.check (Alcotest.float 1e-9) "df2-95" 0.05
    (Backtest.chi2_sf 5.991464547107979 2)

let test_binom () =
  (* 0.99^250, verified against scipy *)
  Alcotest.check (Alcotest.float 1e-12) "cdf0" 0.08105851616218143
    (Backtest.binom_cdf 0 250 0.01);
  Alcotest.check (Alcotest.float 1e-15) "cdf-all" 1.
    (Backtest.binom_cdf 250 250 0.01);
  Alcotest.check (Alcotest.float 1e-15) "cdf-neg" 0.
    (Backtest.binom_cdf (-1) 250 0.01)

let test_traffic () =
  Alcotest.check Alcotest.string "green" "green"
    (Backtest.traffic_light 250 0 0.01);
  Alcotest.check Alcotest.string "red" "red"
    (Backtest.traffic_light 250 249 0.01);
  Alcotest.check Alcotest.string "green4" "green"
    (Backtest.traffic_light 250 4 0.01)

let test_kupiec () =
  let t = Backtest.kupiec_pof 250 0 0.01 in
  Alcotest.check Alcotest.int "df" 1 t.df;
  Alcotest.check (Alcotest.float 1e-12) "lr" 5.025167926750726 t.statistic;
  Alcotest.check Alcotest.bool "p-range" true
    (t.p_value > 0.02 && t.p_value < 0.03);
  Alcotest.check_raises "n0"
    (Invalid_argument "need 0 <= exceptions <= observations, observations > 0")
    (fun () -> ignore (Backtest.kupiec_pof 0 0 0.01));
  Alcotest.check_raises "x>n"
    (Invalid_argument "need 0 <= exceptions <= observations, observations > 0")
    (fun () -> ignore (Backtest.kupiec_pof 10 11 0.01))

let test_christoffersen () =
  let t = Backtest.christoffersen_independence (Array.make 10 false) in
  Alcotest.check (Alcotest.float 1e-15) "lr0" 0. t.statistic;
  Alcotest.check (Alcotest.float 1e-15) "p1" 1. t.p_value;
  let alt = Array.init 6 (fun i -> i mod 2 = 0) in
  let t2 = Backtest.christoffersen_independence alt in
  Alcotest.check Alcotest.int "df" 1 t2.df;
  Alcotest.check Alcotest.bool "finite" true
    (Float.is_finite t2.statistic && Float.is_finite t2.p_value);
  Alcotest.check_raises "short"
    (Invalid_argument "need at least two observations") (fun () ->
      ignore (Backtest.christoffersen_independence [| true |]))

(* --- oracle --- *)

let load_dir base =
  match Sys.getenv_opt "RISK_AI_FIXTURES" with
  | None -> None
  | Some dir ->
      let p = Filename.concat dir base in
      if Sys.file_exists p then Some (dir, p) else None

let is_v2_fixtures dir =
  try
    let m = Yojson.Basic.from_file (Filename.concat dir "manifest.json") in
    let open Yojson.Basic.Util in
    match m |> member "terms_schema" with
    | `String s -> s <> "universe-v1"
    | _ -> false
  with _ -> false

let skip_if_v2 dir what =
  if is_v2_fixtures dir then (
    print_endline ("skip " ^ what ^ ": universe-v2 fixtures need regenerated oracle");
    Alcotest.skip ())

let build_inputs dir =
  let instruments =
    Refdata.instruments_of_file (Filename.concat dir "instruments.json")
  in
  let holdings =
    Holdings.holdings_of_file (Filename.concat dir "holdings.json")
  in
  let bundle =
    Marketdata.bundle_of_file (Filename.concat dir "marketdata.json")
  in
  let manifest = Yojson.Basic.from_file (Filename.concat dir "manifest.json") in
  let open Yojson.Basic.Util in
  let as_of = manifest |> member "as_of" |> to_string |> Calendar.of_string in
  let inputs : Run.engine_inputs =
    {
      holdings;
      instruments;
      bundle;
      as_of;
      base_currency = "EUR";
      row_portfolios = None;
      surf_fits = [];
    }
  in
  inputs

let bt_config () : Config.run_config =
  {
    Config.default with
    history_days = 250;
    backtest = Some { Config.days = 40; confidence = 0.99 };
  }

let num = function `Float x -> x | `Int i -> float_of_int i | _ -> Float.nan

let json_close rel a b =
  match (a, b) with
  | (`Float _ | `Int _), (`Float _ | `Int _) ->
      let x, y = (num a, num b) in
      abs_float (x -. y) <= Float.max (abs_float x *. rel) 1e-12
  | _ -> a = b

let check_assoc rel name want got =
  Alcotest.check
    (Alcotest.list Alcotest.string)
    (name ^ ":keys") (List.map fst want) (List.map fst got);
  List.iter
    (fun (k, v) ->
      if not (json_close rel v (List.assoc k got)) then
        Alcotest.failf "%s[%s]: %s vs %s" name k (Yojson.Basic.to_string v)
          (Yojson.Basic.to_string (List.assoc k got)))
    want

let test_backtest_oracle () =
  match load_dir "oracle_p5.json" with
  | None -> Alcotest.skip ()
  | Some (dir, oracle_path) ->
      skip_if_v2 dir "oracle_p5";
      let res = Backtest.run_backtest (build_inputs dir) (bt_config ()) () in
      let open Yojson.Basic.Util in
      let o = Yojson.Basic.from_file oracle_path in
      let want_result = o |> member "result" in
      let want_summary = want_result |> member "summary" |> to_assoc in
      let pof, ind, cc, got_s = Backtest.summary res in
      let scalar_keys =
        List.filter
          (fun (k, _) ->
            not
              (List.mem k
                 [
                   "kupiec_pof";
                   "christoffersen_independence";
                   "christoffersen_conditional_coverage";
                 ]))
          want_summary
      in
      check_assoc 1e-8 "summary" scalar_keys got_s;
      let check_lr name (w : Yojson.Basic.t) (t : Backtest.lr_test) =
        assert_close ~rel:1e-8 ~abs_:1e-12 (name ^ ":stat")
          (w |> member "statistic" |> to_number)
          t.statistic;
        assert_close ~rel:1e-8 ~abs_:1e-12 (name ^ ":p")
          (w |> member "p_value" |> to_number)
          t.p_value;
        Alcotest.check Alcotest.int (name ^ ":df")
          (w |> member "df" |> to_int)
          t.df
      in
      check_lr "kupiec" (want_summary |> List.assoc "kupiec_pof") pof;
      check_lr "indep"
        (want_summary |> List.assoc "christoffersen_independence")
        ind;
      check_lr "cc"
        (want_summary |> List.assoc "christoffersen_conditional_coverage")
        cc;
      let want_days = want_result |> member "days" |> to_list in
      Alcotest.check Alcotest.int "n-days" (List.length want_days)
        (List.length res.days);
      List.iter2
        (fun w (d : Backtest.backtest_day) ->
          Alcotest.check Alcotest.string "forecast"
            (w |> member "forecast_date" |> to_string)
            (Calendar.to_string d.forecast_date);
          Alcotest.check Alcotest.string "pnl-date"
            (w |> member "pnl_date" |> to_string)
            (Calendar.to_string d.pnl_date);
          assert_close ~rel:1e-6 ~abs_:1e-3 "var"
            (w |> member "var" |> to_number)
            d.var;
          assert_close ~rel:1e-6 ~abs_:1e-3 "pnl"
            (w |> member "pnl" |> to_number)
            d.pnl;
          Alcotest.check Alcotest.bool "exception"
            (w |> member "exception" |> to_bool)
            (Backtest.is_exception d))
        want_days res.days;
      let want_rows = o |> member "rows" |> to_list in
      let got_rows = Backtest.rows res in
      Alcotest.check Alcotest.int "n-rows" (List.length want_rows)
        (List.length got_rows);
      List.iteri
        (fun i w ->
          let g = List.nth got_rows i in
          Alcotest.check Alcotest.string
            (Printf.sprintf "row%d:view" i)
            (w |> member "view" |> to_string)
            g.Run.view;
          Alcotest.check Alcotest.string
            (Printf.sprintf "row%d:measure" i)
            (w |> member "measure" |> to_string)
            g.measure;
          assert_close ~rel:1e-6 ~abs_:1e-3
            (Printf.sprintf "row%d:value" i)
            (w |> member "value" |> to_number)
            g.value;
          check_assoc 1e-12
            (Printf.sprintf "row%d:slice" i)
            (w |> member "slice" |> to_assoc
            |> List.sort (fun (a, _) (b, _) -> String.compare a b))
            (List.sort (fun (a, _) (b, _) -> String.compare a b) g.slice))
        want_rows

let test_bt_cancel () =
  match load_dir "oracle_p5.json" with
  | None -> Alcotest.skip ()
  | Some (dir, _) -> (
      match
        try
          ignore
            (Backtest.run_backtest (build_inputs dir) (bt_config ())
               ~should_cancel:(Some (fun () -> true))
               ());
          None
        with Run.Engine_cancelled _ -> Some ()
      with
      | Some () -> ()
      | None -> Alcotest.fail "expected Engine_cancelled")

let test_bt_short_history () =
  match load_dir "oracle_p5.json" with
  | None -> Alcotest.skip ()
  | Some (dir, _) -> (
      let bad : Config.run_config =
        {
          (bt_config ()) with
          backtest = Some { Config.days = 299; confidence = 0.99 };
        }
      in
      match
        try
          ignore (Backtest.run_backtest (build_inputs dir) bad ());
          None
        with Backtest.Backtest_error _ -> Some ()
      with
      | Some () -> ()
      | None -> Alcotest.fail "expected Backtest_error")

let () =
  Alcotest.run "risk-ai-p5"
    [
      ( "stats",
        [
          Alcotest.test_case "chi2" `Quick test_chi2;
          Alcotest.test_case "binom" `Quick test_binom;
          Alcotest.test_case "traffic" `Quick test_traffic;
          Alcotest.test_case "kupiec" `Quick test_kupiec;
          Alcotest.test_case "christoffersen" `Quick test_christoffersen;
        ] );
      ( "oracle",
        [
          Alcotest.test_case "backtest" `Slow test_backtest_oracle;
          Alcotest.test_case "cancel" `Quick test_bt_cancel;
          Alcotest.test_case "short-history" `Quick test_bt_short_history;
        ] );
    ]
