(** P3 tests: market factors + pricing book, including cross-language oracle
    comparison against RiskStartUp's engine on mini-v1 fixtures.

    The oracle file (fixtures/mini-v1/oracle_p3.json, generated outside this
    repo by running RiskStartUp's engine on the fixtures) holds
    factor history, base values, exposures and shocked valuations computed by
    the Python engine. Tolerances are relative: summation/BLAS order differs,
    BAW agrees to ~1e-10. *)

open Risk_core
open Risk_engine

let eps = Alcotest.float 1e-12

let assert_close ~rel ~abs_ name expected actual =
  let tol = Float.max (abs_float expected *. rel) abs_ in
  if abs_float (actual -. expected) > tol then
    Alcotest.failf "%s: expected %.17g, got %.17g (tol %.3g)" name expected
      actual tol

let assert_vec ~rel ~abs_ name expected actual =
  Alcotest.check Alcotest.int (name ^ ":length") (Array.length expected)
    (Array.length actual);
  Array.iteri
    (fun i e ->
      assert_close ~rel ~abs_ (Printf.sprintf "%s[%d]" name i) e actual.(i))
    expected

(* --- interp_weights --- *)

let test_interp () =
  let w = Market.interp_weights [| 1.0 |] in
  Alcotest.check eps "1Y-exact" 1. w.(0).(3);
  Alcotest.check eps "1Y-sum" 1. (Array.fold_left ( +. ) 0. w.(0));
  let below = (Market.interp_weights [| 0.0 |]).(0) in
  Alcotest.check eps "below-grid" 1. below.(0);
  let above = (Market.interp_weights [| 100.0 |]).(0) in
  Alcotest.check eps "above-grid" 1. above.(11);
  let mid = (Market.interp_weights [| 1.5 |]).(0) in
  Alcotest.check eps "mid-lo" 0.5 mid.(3);
  Alcotest.check eps "mid-hi" 0.5 mid.(4);
  let q = (Market.interp_weights [| 0.25 |]).(0) in
  Alcotest.check eps "3M-exact" 1. q.(1)

(* --- bond cashflows --- *)

let test_bond_cashflows () =
  let terms =
    [
      ("maturity", `String "2035-06-15");
      ("coupon_frequency", `Int 1);
      ("coupon_rate", `Float 0.025);
    ]
  in
  let as_of = Calendar.of_string "2026-09-22" in
  let times, cfs = Pricing.bond_cashflows terms as_of in
  Alcotest.check Alcotest.int "9-coupons" 9 (Array.length times);
  Alcotest.check eps "cf-sum"
    ((9. *. 0.025) +. 1.)
    (Array.fold_left ( +. ) 0. cfs);
  for i = 1 to Array.length times - 1 do
    Alcotest.check Alcotest.bool "increasing" true (times.(i) > times.(i - 1))
  done;
  let first =
    float_of_int
      (Calendar.to_ordinal (Calendar.of_string "2027-06-15")
      - Calendar.to_ordinal as_of)
    /. 365.
  in
  Alcotest.check eps "first-time" first times.(0)

(* --- factor_window --- *)

let test_factor_window () =
  let fs : Market.factor_set =
    {
      names = [| "a"; "b" |];
      index = [ ("a", 0); ("b", 1) ];
      history =
        [|
          [| 1.; 2. |]; [| 3.; 4. |]; [| 5.; 6. |]; [| 7.; 8. |]; [| 9.; 10. |];
        |];
      return_dates =
        Array.init 5 (fun i ->
            Calendar.of_ordinal
              (Calendar.to_ordinal (Calendar.of_string "2026-09-01") + i));
    }
  in
  let w = Market.factor_window fs ~end_:3 ~n:2 in
  Alcotest.check Alcotest.int "rows" 2 (Array.length w.history);
  Alcotest.check eps "row0" 5. w.history.(0).(0);
  Alcotest.check eps "row1" 8. w.history.(1).(1)

(* --- BLAS wiring: cross-checked against loop math, not against itself --- *)

(** Deterministic pseudo-random data (LCG; independent of Stats.rng). *)
let lcg_data seed rows cols =
  let s = ref seed in
  Array.init rows (fun _ ->
      Array.init cols (fun _ ->
          s := ((!s * 1103515245) + 12345) land 0x7fffffff;
          (float_of_int !s /. 2147483648.) -. 0.5))

let loop_matmul a b =
  let m = Array.length a
  and k = Array.length a.(0)
  and n = Array.length b.(0) in
  Array.init m (fun i ->
      Array.init n (fun j ->
          let s = ref 0. in
          for l = 0 to k - 1 do
            s := !s +. (a.(i).(l) *. b.(l).(j))
          done;
          !s))

let assert_mat_close name rel e a =
  Alcotest.check Alcotest.int (name ^ ":rows") (Array.length e) (Array.length a);
  Array.iteri
    (fun i row ->
      Alcotest.check Alcotest.int (name ^ ":cols")
        (Array.length e.(i))
        (Array.length row);
      Array.iteri
        (fun j x ->
          let tol = Float.max (abs_float e.(i).(j) *. rel) 1e-12 in
          if abs_float (x -. e.(i).(j)) > tol then
            Alcotest.failf "%s[%d][%d]: expected %.17g, got %.17g" name i j
              e.(i).(j)
              x)
        row)
    a

let test_blas_wiring () =
  (* 40×50 @ 50×60: above the BLAS threshold — must match loop math. *)
  let a = lcg_data 42 40 50 and b = lcg_data 1337 50 60 in
  assert_mat_close "gemm" 1e-9 (loop_matmul a b) (Stats.matmul a b);
  (* symmetric 40×40 eigensolve: orthonormality + residual (LAPACK path). *)
  let r = lcg_data 7 40 40 in
  let sym =
    Array.init 40 (fun i ->
        Array.init 40 (fun j -> 0.5 *. (r.(i).(j) +. r.(j).(i))))
  in
  let vals, vecs = Stats.eigh_sym sym in
  Alcotest.check Alcotest.int "eig-n" 40 (Array.length vals);
  for i = 0 to 39 do
    for j = 0 to 39 do
      let dot = ref 0. in
      for l = 0 to 39 do
        dot := !dot +. (vecs.(l).(i) *. vecs.(l).(j))
      done;
      let want = if i = j then 1. else 0. in
      if abs_float (!dot -. want) > 1e-9 then
        Alcotest.failf "orth[%d][%d]=%.17g" i j !dot
    done
  done;
  (* A·v = λ·v residual *)
  for t = 0 to 39 do
    let worst = ref 0. in
    for i = 0 to 39 do
      let av = ref 0. in
      for j = 0 to 39 do
        av := !av +. (sym.(i).(j) *. vecs.(j).(t))
      done;
      worst := Float.max !worst (abs_float (!av -. (vals.(t) *. vecs.(i).(t))))
    done;
    if !worst > 1e-8 then Alcotest.failf "eig-res[%d]=%.17g" t !worst
  done;
  (* 70×70 PD Cholesky (LAPACK path): L·Lᵀ reproduces the input. *)
  let q = lcg_data 99 70 70 in
  let pd =
    Array.init 70 (fun i ->
        Array.init 70 (fun j ->
            let s = ref 0. in
            for l = 0 to 69 do
              s := !s +. (q.(i).(l) *. q.(j).(l))
            done;
            !s +. if i = j then 70. else 0.))
  in
  let l = Stats.cholesky_lower pd in
  let recon =
    loop_matmul l (Array.init 70 (fun i -> Array.init 70 (fun j -> l.(j).(i))))
  in
  assert_mat_close "potrf" 1e-8 pd recon

(* --- oracle --- *)

let load_dir () =
  match Sys.getenv_opt "RISK_AI_FIXTURES" with
  | None -> None
  | Some dir ->
      let oracle = Filename.concat dir "oracle_p3.json" in
      if Sys.file_exists oracle then Some (dir, oracle) else None

let is_v2_oracle dir =
  try
    let m = Yojson.Basic.from_file (Filename.concat dir "manifest.json") in
    let open Yojson.Basic.Util in
    match m |> member "terms_schema" with
    | `String s -> s <> "universe-v1"
    | _ -> false
  with _ -> false

let build_all dir =
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
  let state = Market.market_state bundle as_of in
  let book = Pricing.build_book holdings instruments state "EUR" None in
  let factors =
    Market.build_factors bundle ~equity_ids:(Pricing.equity_ids book)
      ~fx_ccys:(Pricing.currencies book)
      ~curve_ccys:(Pricing.curve_ids book)
      ~history_days:(Some config.history_days)
  in
  (book, Pricing.make_pricer book state factors, state, factors, config)

let test_factors_oracle () =
  match load_dir () with
  | None -> Alcotest.skip ()
  | Some (dir, oracle_path) ->
      if is_v2_oracle dir then (
        print_endline "skip oracle_p3: universe-v2 fixtures need regenerated oracle";
        Alcotest.skip ())
      else
      let _, pricer, _, factors, _ = build_all dir in
      let _ = pricer in
      let open Yojson.Basic.Util in
      let o = Yojson.Basic.from_file oracle_path in
      let names = o |> member "factor_names" |> to_list |> List.map to_string in
      Alcotest.check
        (Alcotest.list Alcotest.string)
        "names" names
        (Array.to_list factors.names);
      let hist =
        o |> member "factor_history" |> to_list
        |> List.map (fun r -> r |> to_list |> List.map to_number)
      in
      Alcotest.check Alcotest.int "rows" (List.length hist)
        (Array.length factors.history);
      List.iteri
        (fun i row ->
          List.iteri
            (fun j e ->
              assert_close ~rel:0. ~abs_:1e-12
                (Printf.sprintf "hist[%d][%d]" i j)
                e
                factors.history.(i).(j))
            row)
        hist;
      let dates = o |> member "return_dates" |> to_list |> List.map to_string in
      Alcotest.check
        (Alcotest.list Alcotest.string)
        "return-dates" dates
        (Array.to_list (Array.map Calendar.to_string factors.return_dates))

let test_pricing_oracle () =
  match load_dir () with
  | None -> Alcotest.skip ()
  | Some (dir, oracle_path) ->
      if is_v2_oracle dir then (
        print_endline "skip oracle_p3: universe-v2 fixtures need regenerated oracle";
        Alcotest.skip ())
      else
      let book, pricer, _, _, _ = build_all dir in
      let open Yojson.Basic.Util in
      let o = Yojson.Basic.from_file oracle_path in
      let order =
        o |> member "position_order" |> to_list |> List.map to_string
      in
      Alcotest.check
        (Alcotest.list Alcotest.string)
        "position-order" order
        (List.map
           (fun (p : Pricing.position_info) -> p.instrument_id)
           book.positions);
      let v0 = Pricing.base_values pricer in
      assert_vec ~rel:1e-8 ~abs_:1e-9 "base"
        (Array.of_list
           (o |> member "base_values" |> to_list |> List.map to_number))
        v0;
      let expo = Pricing.exposures pricer in
      let oe = o |> member "exposures" in
      assert_vec ~rel:1e-8 ~abs_:1e-9 "exposure"
        (Array.of_list
           (oe |> member "exposure" |> to_list |> List.map to_number))
        expo.exposure;
      assert_vec ~rel:1e-8 ~abs_:1e-9 "delta"
        (Array.of_list
           (oe |> member "delta_equivalent" |> to_list |> List.map to_number))
        expo.delta_equivalent;
      assert_vec ~rel:1e-8 ~abs_:1e-9 "dv01"
        (Array.of_list (oe |> member "dv01" |> to_list |> List.map to_number))
        expo.dv01;
      List.iter
        (fun (ccy, v) ->
          let got = List.assoc ccy expo.fx_exposure in
          assert_close ~rel:1e-8 ~abs_:1e-9 ("fx:" ^ ccy) v got)
        (oe |> member "fx_exposure" |> to_assoc
        |> List.map (fun (k, v) -> (k, to_number v)));
      let shocks =
        Array.of_list
          (o |> member "shocks" |> to_list
          |> List.map (fun r ->
              Array.of_list (r |> to_list |> List.map to_number)))
      in
      (* Python's golden uses BAW while production uses BS-2002. Relative
         error is unstable for nearly worthless options, so allow one cent of
         model difference only on American-option shocked values. *)
      let assert_shocked name i j expected actual =
        let american =
          List.exists
            (fun (o : Pricing.option_spec) -> o.pos = j && o.style = "AMERICAN")
            book.options
        in
        assert_close ~rel:1e-6 ~abs_:(if american then 1e-2 else 1e-3)
          (Printf.sprintf "%s[%d][%d]" name i j)
          expected actual
      in
      let got0 = Pricing.values pricer shocks ~horizon_days:0 () in
      let want0 =
        Array.of_list
          (o |> member "values_h0" |> to_list
          |> List.map (fun r ->
              Array.of_list (r |> to_list |> List.map to_number)))
      in
      Array.iteri
        (fun i row ->
          Array.iteri (fun j e -> assert_shocked "h0" i j e got0.(i).(j)) row)
        want0;
      let got10 = Pricing.values pricer shocks ~horizon_days:10 () in
      let want10 =
        Array.of_list
          (o |> member "values_h10" |> to_list
          |> List.map (fun r ->
              Array.of_list (r |> to_list |> List.map to_number)))
      in
      Array.iteri
        (fun i row ->
          Array.iteri (fun j e -> assert_shocked "h10" i j e got10.(i).(j)) row)
        want10

let () =
  Alcotest.run "risk-ai-p3"
    [
      ( "market",
        [
          Alcotest.test_case "interp" `Quick test_interp;
          Alcotest.test_case "bond-cashflows" `Quick test_bond_cashflows;
          Alcotest.test_case "factor-window" `Quick test_factor_window;
          Alcotest.test_case "blas-wiring" `Quick test_blas_wiring;
        ] );
      ( "oracle",
        [
          Alcotest.test_case "factors" `Quick test_factors_oracle;
          Alcotest.test_case "pricing" `Slow test_pricing_oracle;
        ] );
    ]
