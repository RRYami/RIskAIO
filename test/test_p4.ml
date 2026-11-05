(** P4 tests: run_engine vs the Python oracle on mini-v1 fixtures, plus MC
    determinism/diagnostics and unit checks (matmul, cancel, bad window). *)

open Risk_core
open Risk_engine

let assert_close ~rel ~abs_ name expected actual =
  let tol = Float.max (abs_float expected *. rel) abs_ in
  if
    abs_float (actual -. expected) > tol
    || not (Float.is_finite actual && Float.is_finite expected)
  then
    Alcotest.failf "%s: expected %.17g, got %.17g (tol %.3g)" name expected
      actual tol

(* --- matmul --- *)

let test_matmul () =
  let c =
    Stats.matmul
      [| [| 1.; 2. |]; [| 3.; 4. |] |]
      [| [| 5.; 6. |]; [| 7.; 8. |] |]
  in
  Alcotest.check (Alcotest.float 1e-12) "c00" 19. c.(0).(0);
  Alcotest.check (Alcotest.float 1e-12) "c01" 22. c.(0).(1);
  Alcotest.check (Alcotest.float 1e-12) "c10" 43. c.(1).(0);
  Alcotest.check (Alcotest.float 1e-12) "c11" 50. c.(1).(1)

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
  let config =
    Config.of_yojson
      (Yojson.Basic.from_file (Filename.concat dir "config.json"))
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
  (inputs, config)

let num = function `Float x -> x | `Int i -> float_of_int i | _ -> Float.nan

let json_equal_float_tol rel a b =
  match (a, b) with
  | (`Float _ | `Int _), (`Float _ | `Int _) ->
      let x, y = (num a, num b) in
      abs_float (x -. y) <= Float.max (abs_float x *. rel) 1e-12
  | _ -> a = b

let compare_slice rel name want got =
  let sort = List.sort (fun (a, _) (b, _) -> String.compare a b) in
  let w, g = (sort want, sort got) in
  Alcotest.check
    (Alcotest.list Alcotest.string)
    (name ^ ":slice-keys") (List.map fst w) (List.map fst g);
  List.iter2
    (fun (k, v) (_, v2) ->
      if not (json_equal_float_tol rel v v2) then
        Alcotest.failf "%s:slice[%s]: %s vs %s" name k
          (Yojson.Basic.to_string v)
          (Yojson.Basic.to_string v2))
    w g

let compare_row rel i (want : Yojson.Basic.t) (got : Run.result_row) =
  let open Yojson.Basic.Util in
  let s f = want |> member f in
  let opt f =
    match s f with
    | `Null -> None
    | `String x -> Some x
    | _ -> Alcotest.failf "row %d bad %s" i f
  in
  Alcotest.check Alcotest.string
    (Printf.sprintf "row%d:view" i)
    (want |> member "view" |> to_string)
    got.view;
  Alcotest.check
    (Alcotest.option Alcotest.string)
    (Printf.sprintf "row%d:dim" i)
    (opt "dimension") got.dimension;
  Alcotest.check
    (Alcotest.option Alcotest.string)
    (Printf.sprintf "row%d:key" i)
    (opt "key") got.key;
  Alcotest.check Alcotest.string
    (Printf.sprintf "row%d:measure" i)
    (want |> member "measure" |> to_string)
    got.measure;
  compare_slice rel (Printf.sprintf "row%d" i)
    (want |> member "slice" |> to_assoc)
    got.slice;
  (* Near-zero monetary P&L can cancel almost completely, making relative
     error unstable across summation orders. One thousandth of a currency
     unit remains financially immaterial; dimensionless rows stay strict. *)
  let abs_ = match got.value_currency with Some _ -> 1e-3 | None -> 1e-9 in
  assert_close ~rel ~abs_ (Printf.sprintf "row%d:value" i)
    (want |> member "value" |> to_number)
    got.value;
  Alcotest.check
    (Alcotest.option Alcotest.string)
    (Printf.sprintf "row%d:ccy" i)
    (opt "value_currency") got.value_currency

let test_run_oracle () =
  match load_dir "oracle_p4.json" with
  | None -> Alcotest.skip ()
  | Some (dir, oracle_path) ->
      skip_if_v2 dir "oracle_p4";
      let inputs, config = build_inputs dir in
      let res = Run.run_engine inputs config () in
      let open Yojson.Basic.Util in
      let o = Yojson.Basic.from_file oracle_path in
      let rows = o |> member "rows" |> to_list in
      Alcotest.check Alcotest.int "n-rows" (List.length rows)
        (List.length res.rows);
      List.iteri (fun i w -> compare_row 1e-7 i w (List.nth res.rows i)) rows;
      let pnl = o |> member "pnl" |> to_assoc in
      List.iter
        (fun (k, v) ->
          let got = List.assoc k res.pnl in
          let want = v |> to_list |> List.map to_number |> Array.of_list in
          Alcotest.check Alcotest.int
            ("pnl:" ^ k ^ ":len")
            (Array.length want) (Array.length got);
          Array.iteri
            (fun i e ->
              assert_close ~rel:1e-7 ~abs_:1e-3
                (Printf.sprintf "pnl:%s[%d]" k i)
                e got.(i))
            want)
        pnl;
      let diag = o |> member "diagnostics" |> to_assoc in
      let got_diag = res.diagnostics in
      (* The Python golden predates the volatility diagnostics added by SSVI;
         extra OCaml diagnostics are valid, but every golden key must remain. *)
      let missing_diag_keys =
        diag |> List.map fst |> List.filter (fun k -> not (List.mem_assoc k got_diag))
      in
      Alcotest.check
        (Alcotest.list Alcotest.string)
        "diag-keys-missing" [] missing_diag_keys;
      List.iter
        (fun (k, v) ->
          if not (json_equal_float_tol 1e-8 v (List.assoc k got_diag)) then
            Alcotest.failf "diag[%s]: %s vs %s" k (Yojson.Basic.to_string v)
              (Yojson.Basic.to_string (List.assoc k got_diag)))
        diag

let mc_small_config config : Config.run_config =
  { config with method_ = Config.Montecarlo; n_scenarios = 500; seed = 0 }

let test_mc_determinism () =
  match load_dir "oracle_p4.json" with
  | None -> Alcotest.skip ()
  | Some (dir, _) ->
      let inputs, config = build_inputs dir in
      let mc = mc_small_config config in
      let r1 = Run.run_engine inputs mc () in
      let r2 = Run.run_engine inputs mc () in
      Alcotest.check Alcotest.string "deterministic"
        (Yojson.Basic.to_string (`List (Run.canonical_rows r1.rows)))
        (Yojson.Basic.to_string (`List (Run.canonical_rows r2.rows)));
      Alcotest.check Alcotest.int "row-count" (List.length r1.rows)
        (List.length r2.rows)

(** Parallel chunks must be bit-identical to sequential: 5,000 scenarios over 10
    chunks takes the domain path (n ≥ 4000), 1 domain forces loops. *)
let test_parallel_determinism () =
  match load_dir "oracle_p4.json" with
  | None -> Alcotest.skip ()
  | Some (dir, _) ->
      let inputs, config = build_inputs dir in
      let big : Config.run_config =
        {
          config with
          method_ = Config.Montecarlo;
          n_scenarios = 5000;
          chunk_size = 500;
          seed = 3;
        }
      in
      let run dom =
        Unix.putenv "RISK_AI_DOMAINS" (string_of_int dom);
        Run.run_engine inputs big ()
      in
      let r_seq = run 1 in
      let r_par = run 8 in
      Unix.putenv "RISK_AI_DOMAINS" "";
      Alcotest.check Alcotest.string "par=seq"
        (Yojson.Basic.to_string (`List (Run.canonical_rows r_seq.rows)))
        (Yojson.Basic.to_string (`List (Run.canonical_rows r_par.rows)))

let test_mc_diagnostics () =
  match load_dir "oracle_p4.json" with
  | None -> Alcotest.skip ()
  | Some (dir, oracle_path) ->
      skip_if_v2 dir "oracle_p4:mc";
      let inputs, config = build_inputs dir in
      let res = Run.run_engine inputs (mc_small_config config) () in
      let open Yojson.Basic.Util in
      let o = Yojson.Basic.from_file oracle_path in
      let want = o |> member "mc_small" |> member "diagnostics" |> to_assoc in
      let got = res.diagnostics in
      Alcotest.check Alcotest.int "mc-factors"
        (want |> List.assoc "mc_factors" |> to_int)
        (got |> List.assoc "mc_factors" |> to_int);
      Alcotest.check Alcotest.bool "repaired"
        (want |> List.assoc "covariance_repaired" |> to_bool)
        (got |> List.assoc "covariance_repaired" |> to_bool);
      List.iter
        (fun (ccy, v) ->
          assert_close ~rel:1e-9 ~abs_:1e-12 ("explained:" ^ ccy) (to_number v)
            (got
            |> List.assoc "curve_variance_explained"
            |> to_assoc |> List.assoc ccy |> to_number))
        (want |> List.assoc "curve_variance_explained" |> to_assoc);
      assert_close ~rel:1e-9 ~abs_:1e-12 "min-eig"
        (want |> List.assoc "covariance_min_eigenvalue" |> to_number)
        (got |> List.assoc "covariance_min_eigenvalue" |> to_number)

let test_cancel () =
  match load_dir "oracle_p4.json" with
  | None -> Alcotest.skip ()
  | Some (dir, _) -> (
      let inputs, config = build_inputs dir in
      match
        try
          ignore
            (Run.run_engine inputs config
               ~should_cancel:(Some (fun () -> true))
               ());
          None
        with Run.Engine_cancelled _ -> Some ()
      with
      | Some () -> ()
      | None -> Alcotest.fail "expected Engine_cancelled")

let test_bad_window () =
  match load_dir "oracle_p4.json" with
  | None -> Alcotest.skip ()
  | Some (dir, _) -> (
      let inputs, config = build_inputs dir in
      let bad : Config.run_config =
        {
          config with
          historical_windows =
            [
              {
                id = "NOWHERE";
                start = Calendar.of_string "1999-01-04";
                end_ = Calendar.of_string "1999-02-01";
              };
            ];
        }
      in
      match
        try
          ignore (Run.run_engine inputs bad ());
          None
        with Run.Engine_config_error _ -> Some ()
      with
      | Some () -> ()
      | None -> Alcotest.fail "expected Engine_config_error")

let () =
  Alcotest.run "risk-ai-p4"
    [
      ("unit", [ Alcotest.test_case "matmul" `Quick test_matmul ]);
      ( "oracle",
        [
          Alcotest.test_case "run" `Slow test_run_oracle;
          Alcotest.test_case "mc-determinism" `Slow test_mc_determinism;
          Alcotest.test_case "par-determinism" `Slow test_parallel_determinism;
          Alcotest.test_case "mc-diagnostics" `Slow test_mc_diagnostics;
          Alcotest.test_case "cancel" `Quick test_cancel;
          Alcotest.test_case "bad-window" `Quick test_bad_window;
        ] );
    ]
