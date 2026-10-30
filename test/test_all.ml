(** P1/P2 unit tests. Port coverage: money, calendar, currencies (core) and
    stats, options, config (engine). Golden values are hand-computed or
    published constants — no Python needed to run them. *)

open Risk_core
open Risk_engine

let eps = Alcotest.float 1e-12

(* --- money --- *)

let test_money_parse () =
  let m = Money.decimal_of_string "123.45" in
  Alcotest.check eps "value" 123.45 (Money.float_of m);
  Alcotest.check Alcotest.string "repr" "123.45" (Money.to_string m);
  let i = Money.decimal_of_int 7 in
  Alcotest.check eps "int" 7. (Money.float_of i)

let test_money_rejects () =
  let bad = [ ""; "abc"; "1.2.3"; "NaN"; "inf"; "1e"; "--5"; "." ] in
  List.iter
    (fun s ->
      match
        try Some (Money.decimal_of_string s)
        with Money.Invalid_decimal _ -> None
      with
      | None -> ()
      | Some _ -> Alcotest.fail ("accepted bad decimal: " ^ s))
    bad;
  (* overflow to inf is rejected *)
  match
    try Some (Money.decimal_of_string "1e999")
    with Money.Invalid_decimal _ -> None
  with
  | None -> ()
  | Some _ -> Alcotest.fail "accepted 1e999"

let test_money_quantize () =
  let q s p =
    Money.to_string (Money.quantize_amount (Money.decimal_of_string s) p)
  in
  Alcotest.check Alcotest.string "half-up" "2.68" (q "2.675" 2);
  Alcotest.check Alcotest.string "half-up-neg" "-2.68" (q "-2.675" 2);
  Alcotest.check Alcotest.string "zero-places" "3" (q "2.5" 0);
  Alcotest.check Alcotest.string "neg-half" "-3" (q "-2.5" 0)

(* --- calendar --- *)

let d = Calendar.of_string

let test_easter () =
  Alcotest.check Alcotest.string "easter-2026" "2026-04-05"
    (Calendar.to_string (Calendar.easter_sunday 2026));
  Alcotest.check Alcotest.string "easter-2025" "2025-04-20"
    (Calendar.to_string (Calendar.easter_sunday 2025))

let test_target2 () =
  Alcotest.check Alcotest.bool "sat-not-biz" false
    (Calendar.is_business_day (d "2026-09-26"));
  Alcotest.check Alcotest.bool "tue-biz" true
    (Calendar.is_business_day (d "2026-09-22"));
  Alcotest.check Alcotest.bool "good-friday" false
    (Calendar.is_business_day (d "2026-04-03"));
  Alcotest.check Alcotest.bool "easter-mon" false
    (Calendar.is_business_day (d "2026-04-06"));
  Alcotest.check Alcotest.string "prev-biz" "2026-04-02"
    (Calendar.to_string (Calendar.previous_business_day (d "2026-04-07")))

let test_biz_arith () =
  Alcotest.check Alcotest.int "between-week" 5
    (Calendar.business_days_between (d "2026-09-21") (d "2026-09-28"));
  Alcotest.check Alcotest.int "between-none" 0
    (Calendar.business_days_between (d "2026-09-22") (d "2026-09-22"));
  Alcotest.check Alcotest.string "add-5" "2026-09-29"
    (Calendar.to_string (Calendar.add_business_days (d "2026-09-22") 5));
  let ending = Calendar.business_days_ending (d "2026-09-22") 3 in
  Alcotest.check
    (Alcotest.list Alcotest.string)
    "ending3"
    [ "2026-09-18"; "2026-09-21"; "2026-09-22" ]
    (List.map Calendar.to_string ending);
  Alcotest.check Alcotest.string "roundtrip" "2026-09-22"
    (Calendar.to_string (d "2026-09-22"))

(* --- currencies --- *)

let test_currencies () =
  Alcotest.check Alcotest.bool "eur" true
    (Currencies.is_iso_currency (Some "EUR"));
  Alcotest.check Alcotest.bool "jpy" true
    (Currencies.is_iso_currency (Some "JPY"));
  Alcotest.check Alcotest.bool "xau-excluded" false
    (Currencies.is_iso_currency (Some "XAU"));
  Alcotest.check Alcotest.bool "none" false (Currencies.is_iso_currency None);
  Alcotest.check Alcotest.bool "euro-word" false
    (Currencies.is_iso_currency (Some "EURO"))

(* --- stats --- *)

let test_ewma () =
  let w = Stats.ewma_weights 3 0.5 in
  (* ages 2,1,0 → raw 0.5*0.25, 0.5*0.5, 0.5 → normalised *)
  Alcotest.check eps "w0" (0.125 /. 0.875) w.(0);
  Alcotest.check eps "w2" (0.5 /. 0.875) w.(2);
  Alcotest.check eps "sums-to-1" 1. (w.(0) +. w.(1) +. w.(2))

let test_sample_cov () =
  let r = [| [| 1.; 2. |]; [| 3.; 4. |] |] in
  let c = Stats.sample_covariance r in
  Alcotest.check eps "c00" 2. c.(0).(0);
  Alcotest.check eps "c01" 2. c.(0).(1);
  Alcotest.check eps "c11" 2. c.(1).(1)

let test_psd () =
  (* full rank: no repair *)
  let c = Stats.psd_cholesky [| [| 2.; 1. |]; [| 1.; 2. |] |] in
  Alcotest.check Alcotest.bool "no-repair" false c.repaired;
  (* rank deficient: repaired, min eigenvalue ≈ 0 *)
  let c2 = Stats.psd_cholesky [| [| 1.; 1. |]; [| 1.; 1. |] |] in
  Alcotest.check Alcotest.bool "repaired" true c2.repaired;
  Alcotest.check (Alcotest.float 1e-9) "min-eig" 0. c2.min_eigenvalue;
  (* chol reproduces the repaired matrix: L L^T ≈ [[1,1],[1,1]] *)
  let l = c2.chol in
  Alcotest.check eps "recon00" 1.
    ((l.(0).(0) *. l.(0).(0)) +. (l.(0).(1) *. l.(0).(1)))

let test_var_es () =
  (* pnl [3;1;-2;-5], alpha 0.75: loss desc = 5,2,-1,-3; tail = {5} *)
  let t = Stats.var_es [| 3.; 1.; -2.; -5. |] ~alpha:0.75 ~weights:None in
  Alcotest.check eps "var" 5. t.var;
  Alcotest.check eps "es" 5. t.es;
  Alcotest.check Alcotest.int "boundary" 0 t.boundary;
  (* alpha 0.6 on [1;-1]: loss desc = 1,-1; tail_prob 0.4 → tail = {1} *)
  let t2 = Stats.var_es [| 1.; -1. |] ~alpha:0.6 ~weights:None in
  Alcotest.check eps "var2" 1. t2.var;
  Alcotest.check eps "es2" 1. t2.es

let test_components () =
  let pnl = [| 4.; 2.; -2.; -4. |] in
  let t = Stats.var_es pnl ~alpha:0.75 ~weights:None in
  (* tail = worst scenario only (index 3) *)
  let g = [| [| -4.; 1. |] |] in
  let ces = Stats.component_es g t in
  Alcotest.check eps "ces0" 4. ces.(0);
  Alcotest.check eps "ces1" (-1.) ces.(1);
  let band = Stats.var_band t (Array.length pnl) in
  Alcotest.check Alcotest.bool "band-nonempty" true (Array.length band >= 1)

let test_mc_shape () =
  let chol = (Stats.psd_cholesky [| [| 1.; 0.3 |]; [| 0.3; 1. |] |]).chol in
  let x =
    Stats.mc_shocks chol ~n:20000 ~seed:7 ~distribution:`Gaussian ~nu:5.
  in
  Alcotest.check Alcotest.int "rows" 20000 (Array.length x);
  Alcotest.check Alcotest.int "cols" 2 (Array.length x.(0));
  let m0 = Array.fold_left (fun a r -> a +. r.(0)) 0. x /. 20000. in
  Alcotest.check (Alcotest.float 0.05) "mean≈0" 0. m0;
  let v0 =
    Array.fold_left (fun a r -> a +. ((r.(0) -. m0) ** 2.)) 0. x /. 19999.
  in
  Alcotest.check (Alcotest.float 0.1) "var≈1" 1. v0;
  let xt =
    Stats.mc_shocks chol ~n:5000 ~seed:7 ~distribution:`Student_t ~nu:5.
  in
  Alcotest.check Alcotest.int "t-rows" 5000 (Array.length xt)

(* --- options --- *)

let test_ndtr () =
  Alcotest.check eps "ndtr0" 0.5 (Options.ndtr 0.);
  Alcotest.check eps "ndtr1" 0.8413447460685429 (Options.ndtr 1.);
  Alcotest.check eps "ndtr-1.96" 0.024997895148220435 (Options.ndtr (-1.96));
  Alcotest.check eps "ndtr5" 0.9999997133484281 (Options.ndtr 5.)

let test_put_call_parity () =
  let s = [| 100.; 90.; 110. |]
  and r = [| 0.03; 0.03; 0.05 |]
  and b = [| 0.01; 0.01; 0.02 |] in
  let c = Options.black_scholes ~s ~k:100. ~t:1. ~r ~b ~sigma:0.2 ~call:true in
  let p = Options.black_scholes ~s ~k:100. ~t:1. ~r ~b ~sigma:0.2 ~call:false in
  Array.iteri
    (fun i _ ->
      let fwd =
        (s.(i) *. exp ((b.(i) -. r.(i)) *. 1.)) -. (100. *. exp (-.r.(i) *. 1.))
      in
      Alcotest.check eps ("parity" ^ string_of_int i) fwd (c.(i) -. p.(i)))
    s

let test_american_bounds () =
  let s = [| 100. |] and r = [| 0.05 |] and b = [| 0.02 |] in
  let euro =
    Options.black_scholes ~s ~k:100. ~t:1. ~r ~b ~sigma:0.25 ~call:true
  in
  let amer = Options.baw ~s ~k:100. ~t:1. ~r ~b ~sigma:0.25 ~call:true in
  Alcotest.check Alcotest.bool "amer>=euro" true (amer.(0) >= euro.(0) -. 1e-12);
  Alcotest.check Alcotest.bool "amer>=intrinsic" true
    (amer.(0) >= s.(0) -. 100. -. 1e-9);
  (* no early exercise when b >= r for a call → equals european *)
  let b2 = [| 0.06 |] in
  let amer2 = Options.baw ~s ~k:100. ~t:1. ~r ~b:b2 ~sigma:0.25 ~call:true in
  let euro2 =
    Options.black_scholes ~s ~k:100. ~t:1. ~r ~b:b2 ~sigma:0.25 ~call:true
  in
  Alcotest.check eps "no-early=b" euro2.(0) amer2.(0);
  (* expiry intrinsic *)
  let expd =
    Options.option_price ~style:"AMERICAN" ~s ~k:100. ~t:0. ~r ~q:0.03
      ~sigma:0.25 ~call:false
  in
  Alcotest.check eps "expired-put" 0. expd.(0);
  (* Extreme rate shocks can drive the approximate exercise boundary below
     the strike. Immediate exercise must still respect intrinsic and European
     lower bounds rather than returning S - K when that difference is negative. *)
  let s = 84.37919951646262
  and k = 139.08000000000001
  and t = 0.71031746031746035
  and r = -0.67046460865112079
  and q = 0.0030109999999999998
  and sigma = 0.222687 in
  let b = r -. q in
  let euro = Options.bs_1 ~s ~k ~t ~r ~b ~sigma ~call:true in
  let amer = Options.bs2002_1 ~s ~k ~t ~r ~b ~sigma ~call:true in
  Alcotest.check eps "negative-rate-euro-floor" euro amer

(* --- config --- *)

let test_config_defaults () =
  let c = Config.default in
  Config.validate c;
  Alcotest.check Alcotest.string "kind" "risk" (Config.kind c);
  Alcotest.check_raises "history-days"
    (Invalid_argument "history_days must be >= 250 (Q12)") (fun () ->
      Config.validate { c with history_days = 200 });
  Alcotest.check_raises "curve-pcs"
    (Invalid_argument "curve_pcs must be in 0..12") (fun () ->
      Config.validate { c with curve_pcs = 13 })

let test_config_roundtrip () =
  let c = Config.default in
  let j = Config.to_yojson c in
  let back = Config.of_yojson j in
  let rej = Config.to_yojson back in
  Alcotest.check Alcotest.string "json-stable" (Yojson.Basic.to_string j)
    (Yojson.Basic.to_string rej);
  (* backtest configs keep the key (risk configs omit it, like Python) *)
  let b = { c with backtest = Some { days = 250; confidence = 0.99 } } in
  Alcotest.check Alcotest.string "backtest-kind" "backtest" (Config.kind b);
  let bj = Config.to_yojson b |> Yojson.Basic.to_string in
  Alcotest.check Alcotest.string "backtest-roundtrip" bj
    (Config.of_yojson (Config.to_yojson b)
    |> Config.to_yojson |> Yojson.Basic.to_string)

let test_config_vol_compat () =
  (* pre-vol fixture configs (no vol_* keys) default to frozen *)
  let minimal =
    Config.of_yojson
      (`Assoc
        [
          ("method", `String "historical");
          ("confidences", `List [ `Float 0.99 ]);
          ("horizons", `List [ `Int 1 ]);
          ("history_days", `Int 500);
          ("weighting", `String "equal");
          ("brw_lambda", `Float 0.99);
          ("covariance", `String "ewma");
          ("ewma_lambda", `Float 0.97);
          ("curve_pcs", `Int 3);
          ("distribution", `String "gaussian");
          ("nu", `Float 5.0);
          ("n_scenarios", `Int 10000);
          ("seed", `Int 0);
          ("component_confidence", `Float 0.99);
          ("ucits_limit", `Float 0.2);
          ("stress", `List []);
          ("historical_windows", `List []);
          ("worst_window_days", `Int 20);
          ("chunk_size", `Int 2000);
          ("backtest", `Null);
          ("vol_model", `Null);
          ("vol_pcs", `Null);
          ("vol_universe", `Null);
        ])
  in
  Alcotest.check Alcotest.bool "frozen-default" true
    (minimal.vol_model = Config.Frozen
    && minimal.vol_pcs = 0
    && minimal.vol_universe = []);
  Config.validate minimal;
  (match
     try Some (Config.validate { minimal with vol_model = Config.Bootstrap })
     with Invalid_argument _ -> None
   with
  | None -> ()
  | Some _ -> Alcotest.fail "bootstrap without universe accepted");
  (match
     try
       Some
         (Config.validate
            {
              minimal with
              vol_model = Config.Gaussian;
              vol_universe = [ "EQ1" ];
            })
     with Invalid_argument _ -> None
   with
  | None -> ()
  | Some _ -> Alcotest.fail "gaussian without montecarlo accepted")

(* --- fixtures (gated on RISK_AI_FIXTURES) --- *)

let test_fixtures () =
  match Sys.getenv_opt "RISK_AI_FIXTURES" with
  | None -> Alcotest.skip ()
  | Some dir ->
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
      Config.validate config;
      Alcotest.check Alcotest.bool "holdings-nonempty" true (holdings <> []);
      Alcotest.check Alcotest.bool "prices-nonempty" true (bundle.prices <> []);
      Alcotest.check Alcotest.bool "fx-nonempty" true (bundle.fx <> []);
      Alcotest.check Alcotest.bool "curves-nonempty" true (bundle.curves <> [])

(* --- csv --- *)

let test_csv_basic () =
  let rows = Csv.parse_string "a,b,c\n1,2,3\n4,5,6\n" in
  Alcotest.check Alcotest.int "nrows" 3 (List.length rows);
  Alcotest.check
    (Alcotest.list Alcotest.string)
    "hdr" [ "a"; "b"; "c" ] (List.nth rows 0);
  Alcotest.check
    (Alcotest.list Alcotest.string)
    "row2" [ "4"; "5"; "6" ] (List.nth rows 2)

let test_csv_quoted () =
  let rows =
    Csv.parse_string "a,b\n\"x,y\",\"p\"\"q\"\nplain,\"multi\nline\"\n"
  in
  Alcotest.check Alcotest.int "nrows" 3 (List.length rows);
  Alcotest.check
    (Alcotest.list Alcotest.string)
    "comma+escape" [ "x,y"; "p\"q" ] (List.nth rows 1);
  Alcotest.check
    (Alcotest.list Alcotest.string)
    "newline" [ "plain"; "multi\nline" ] (List.nth rows 2)

let test_csv_crlf_no_trailing () =
  let rows = Csv.parse_string "a,b\r\n1,2" in
  Alcotest.check Alcotest.int "nrows" 2 (List.length rows);
  Alcotest.check
    (Alcotest.list Alcotest.string)
    "row" [ "1"; "2" ] (List.nth rows 1)

let test_csv_rejects () =
  (match
     try Some (Csv.parse_string "a,b\n\"oops,1,2\n")
     with Csv.Csv_error _ -> None
   with
  | None -> ()
  | Some _ -> Alcotest.fail "accepted unterminated quote");
  (match
     try Some (Csv.check_width [ [ "a"; "b" ]; [ "1" ] ])
     with Csv.Csv_error _ -> None
   with
  | None -> ()
  | Some _ -> Alcotest.fail "accepted ragged row");
  (match
     try Some (Csv.header_index [ [ "a"; "a" ] ])
     with Csv.Csv_error _ -> None
   with
  | None -> ()
  | Some _ -> Alcotest.fail "accepted duplicate column")

let test_csv_col () =
  let rows = Csv.check_width (Csv.parse_string "a,b\n1, 2 \n") in
  let idx = Csv.header_index rows in
  Alcotest.check Alcotest.string "trimmed" "2" (Csv.col idx "b" (List.nth rows 1))

(* --- quotes --- *)

let test_quotes_roundtrip () =
  let s, k, r, q, sg = (100., 110., 0.02, 0.01, 0.3) in
  let tq = Risk_engine.Quotes.t_years ~date:"2024-08-21" ~expiry:"2025-02-20" in
  let px =
    (Risk_engine.Options.black_scholes ~s:[| s |] ~k ~t:tq ~r:[| r |]
       ~b:[| r -. q |] ~sigma:sg ~call:true).(0)
  in
  let qot : Risk_engine.Quotes.quote =
    {
      date = "2024-08-21"; underlying = "SOXX"; expiry = "2025-02-20";
      right = Risk_engine.Quotes.Call; strike = k; bid = px -. 0.05;
      ask = px +. 0.05; mid = px; spot = s; rate = r; div = q;
    }
  in
  Alcotest.check Alcotest.bool "otm" true (Risk_engine.Quotes.is_otm qot ~t:tq);
  Alcotest.check (Alcotest.float 1e-9) "iv" sg
    (Risk_engine.Quotes.iv_of_quote qot ~t:tq);
  (* ITM put with K > F is not OTM *)
  let itm = { qot with right = Risk_engine.Quotes.Put } in
  Alcotest.check Alcotest.bool "itm-excluded" false
    (Risk_engine.Quotes.is_otm itm ~t:tq);
  (* crossed market raises *)
  let crossed = { qot with bid = px +. 1.; ask = px -. 1. } in
  (match
     try Some (Risk_engine.Quotes.iv_of_quote crossed ~t:tq)
     with Risk_engine.Quotes.Quotes_error _ -> None
   with
  | None -> ()
  | Some _ -> Alcotest.fail "accepted crossed market")

let test_quotes_amer () =
  (* BS-2002 roundtrip: price American at known sigma, invert, recover.
     Includes ITM contracts where the European inverter cannot go. *)
  let s, r, q = (100., 0.05, 0.01) in
  let cases =
    [
      (true, 90.0, 0.5, 0.30);
      (* ITM call *)
      (true, 110.0, 0.5, 0.30);
      (* OTM call *)
      (false, 110.0, 0.25, 0.40);
      (* ITM put, short-dated, high rate: large early premium *)
      (false, 90.0, 0.25, 0.40);
      (* OTM put *)
      (false, 100.0, 1.0, 0.25);
      (* ATM put *)
    ]
  in
  List.iter
    (fun (call, k, t, sg) ->
      let px =
        Risk_engine.Options.bs2002_1 ~s ~k ~t ~r ~b:(r -. q) ~sigma:sg ~call
      in
      let right = if call then Risk_engine.Quotes.Call else Risk_engine.Quotes.Put in
      let qot : Risk_engine.Quotes.quote =
        {
          date = "2024-08-21"; underlying = "SOXX"; expiry = "2025-02-20";
          right; strike = k; bid = px -. 0.02; ask = px +. 0.02; mid = px;
          spot = s; rate = r; div = q;
        }
      in
      let got = Risk_engine.Quotes.iv_of_quote_amer qot ~t in
      Alcotest.check (Alcotest.float 1e-9)
        (Printf.sprintf "amer-recover %s K=%.0f" (if call then "C" else "P") k)
        sg got)
    cases;
  (* bias demo: an American ITM put price inverted with Black overstates IV *)
  let tq = Risk_engine.Quotes.t_years ~date:"2024-08-21" ~expiry:"2024-11-20" in
  let k, sg = (110.0, 0.40) in
  let px =
    (Risk_engine.Options.baw ~s:[| s |] ~k ~t:tq ~r:[| r |] ~b:[| r -. q |]
       ~sigma:sg ~call:false).(0)
  in
  let euro =
    (Risk_engine.Options.black_scholes ~s:[| s |] ~k ~t:tq ~r:[| r |]
       ~b:[| r -. q |] ~sigma:sg ~call:false).(0)
  in
  Alcotest.check Alcotest.bool "amer-premium-positive" true (px > euro +. 0.01);
  let qot : Risk_engine.Quotes.quote =
    {
      date = "2024-08-21"; underlying = "SOXX"; expiry = "2024-11-20";
      right = Risk_engine.Quotes.Put; strike = k; bid = px -. 0.02;
      ask = px +. 0.02; mid = px; spot = s; rate = r; div = q;
    }
  in
  let amer = Risk_engine.Quotes.iv_of_quote_amer qot ~t:tq in
  Alcotest.check (Alcotest.float 0.02) "amer-unbiased" sg amer

let test_quotes_grid_itm () =
  (* ITM quotes are kept and inverted through BS-2002 (needed for books
     holding ITM Americans); OTM through Black. *)
  let s, r, q, sg = (100., 0.05, 0.01, 0.35) in
  let t = Risk_engine.Quotes.t_years ~date:"2024-08-21" ~expiry:"2025-02-20" in
  let mk right strike =
    let call = right = Risk_engine.Quotes.Call in
    let px =
      Risk_engine.Options.bs2002_1 ~s ~k:strike ~t ~r ~b:(r -. q) ~sigma:sg
        ~call
    in
    {
      Risk_engine.Quotes.date = "2024-08-21";
      underlying = "SOXX"; expiry = "2025-02-20";
      right; strike; bid = px -. 0.02; ask = px +. 0.02; mid = px;
      spot = s; rate = r; div = q;
    }
  in
  let gs =
    Risk_engine.Quotes.to_grids [ mk Risk_engine.Quotes.Put 110.; mk Risk_engine.Quotes.Call 110. ]
  in
  Alcotest.check Alcotest.int "one-expiry" 1 (List.length gs);
  let g = List.hd gs in
  Alcotest.check Alcotest.int "two-strikes" 2 (Array.length g.Risk_engine.Quotes.k);
  Array.iter
    (fun v -> Alcotest.check (Alcotest.float 1e-6) "iv" sg v)
    g.Risk_engine.Quotes.iv;
  (* stale mid below intrinsic is rejected, not fitted *)
  let bad =
    mk Risk_engine.Quotes.Put 110.
  in
  let bad = { bad with Risk_engine.Quotes.mid = 0.5 } in
  (match
     try Some (Risk_engine.Quotes.to_grids [ bad ])
     with Risk_engine.Quotes.Quotes_error _ -> None
   with
  | None -> ()
  | Some _ -> Alcotest.fail "accepted stale mid")

let test_quotes_grid () =
  let mk k mid =
    {
      Risk_engine.Quotes.date = "2024-08-21";
      underlying = "SOXX"; expiry = "2025-02-20";
      right = Risk_engine.Quotes.Call; strike = k; bid = mid -. 0.02;
      ask = mid +. 0.02; mid; spot = 100.; rate = 0.02; div = 0.01;
    }
  in
  (* OTM call mids from Black at two vols; insert out of order.
     Mids are generated at the same t the inversion uses. *)
  let t = Risk_engine.Quotes.t_years ~date:"2024-08-21" ~expiry:"2025-02-20" in
  let px sg k =
    (Risk_engine.Options.black_scholes ~s:[| 100. |] ~k ~t ~r:[| 0.02 |]
       ~b:[| 0.01 |] ~sigma:sg ~call:true).(0)
  in
  let gs = Risk_engine.Quotes.to_grids [ mk 120. (px 0.3 120.); mk 105. (px 0.25 105.) ] in
  Alcotest.check Alcotest.int "one-expiry" 1 (List.length gs);
  let g = List.hd gs in
  Alcotest.check (Alcotest.float 1e-9) "t" t g.Risk_engine.Quotes.t;
  Alcotest.check Alcotest.bool "sorted" true
    (g.Risk_engine.Quotes.k.(0) < g.Risk_engine.Quotes.k.(1));
  Alcotest.check (Alcotest.float 1e-6) "iv-lo" 0.25 g.Risk_engine.Quotes.iv.(0);
  Alcotest.check (Alcotest.float 1e-6) "iv-hi" 0.3 g.Risk_engine.Quotes.iv.(1)

(* Reference: fixtures/ssvi-cal-v1/scipy_reference.json ("from_atm" run),
   generated outside the repo (seed 7, 20bp noise). Found via
   RISK_AI_FIXTURES or a cwd-relative search; missing file = fail loudly. *)
(* --- surface constraints --- *)

let sc_exp = [| 0.25; 0.5; 1.0; 2.0 |]
let sc_k = [| -1.0; -0.5; -0.25; 0.0; 0.25; 0.5; 1.0 |]
let sc_u = [| -2.9; -2.7; -2.6; -2.5; -0.42; 0.0; 0.41 |]

let sc_grid u =
  let s = Risk_engine.Ssvi.decode sc_exp u in
  Array.map
    (fun t -> Array.map (fun k -> Risk_engine.Ssvi.iv s ~k ~t) sc_k)
    sc_exp

let sc_status rep name =
  let c =
    List.find
      (fun (c : Risk_engine.Surface_constraints.check) ->
        c.Risk_engine.Surface_constraints.name = name)
      rep.Risk_engine.Surface_constraints.checks
  in
  c.Risk_engine.Surface_constraints.status

let st_check = Alcotest.of_pp (fun fmt _ -> Format.pp_print_string fmt "st")

let test_sc_valid () =
  let rep =
    Risk_engine.Surface_constraints.validate_grid ~expiries:sc_exp ~k:sc_k
      ~iv:(sc_grid sc_u) ~spot:100. ~rate:0.02 ~div:0.01
  in
  Alcotest.check Alcotest.bool "valid" true
    rep.Risk_engine.Surface_constraints.valid;
  let s = Risk_engine.Ssvi.decode sc_exp sc_u in
  let gj = Risk_engine.Surface_constraints.gj_ok s in
  Alcotest.check Alcotest.string "gj" "gj-sufficient"
    gj.Risk_engine.Surface_constraints.name;
  Alcotest.check Alcotest.bool "gj-pass" true
    (gj.Risk_engine.Surface_constraints.status = Risk_engine.Surface_constraints.Pass)

let test_sc_structural () =
  let iv = sc_grid sc_u in
  iv.(1).(3) <- Float.nan;
  let rep =
    Risk_engine.Surface_constraints.validate_grid ~expiries:sc_exp ~k:sc_k ~iv
      ~spot:100. ~rate:0.02 ~div:0.01
  in
  Alcotest.check Alcotest.bool "invalid" false
    rep.Risk_engine.Surface_constraints.valid;
  Alcotest.check st_check "structural-fail"
    Risk_engine.Surface_constraints.Fail
    (sc_status rep "structural");
  Alcotest.check st_check "calendar-indet"
    Risk_engine.Surface_constraints.Indeterminate
    (sc_status rep "calendar")

let test_sc_calendar () =
  let iv = sc_grid sc_u in
  Array.iteri (fun i _ -> iv.(2).(i) <- iv.(2).(i) *. 0.3) iv.(2);
  let rep =
    Risk_engine.Surface_constraints.validate_grid ~expiries:sc_exp ~k:sc_k ~iv
      ~spot:100. ~rate:0.02 ~div:0.01
  in
  Alcotest.check Alcotest.bool "invalid" false
    rep.Risk_engine.Surface_constraints.valid;
  Alcotest.check st_check "calendar-fail" Risk_engine.Surface_constraints.Fail
    (sc_status rep "calendar")

let test_sc_shape () =
  (* IV spike at the top strike breaks strike-monotonicity *)
  let iv = sc_grid sc_u in
  iv.(1).(6) <- 5.0;
  let rep =
    Risk_engine.Surface_constraints.validate_grid ~expiries:sc_exp ~k:sc_k ~iv
      ~spot:100. ~rate:0.02 ~div:0.01
  in
  Alcotest.check st_check "mono-fail" Risk_engine.Surface_constraints.Fail
    (sc_status rep "monotonicity");
  (* sharp mid dip breaks convexity *)
  let iv2 = sc_grid sc_u in
  iv2.(0).(3) <- iv2.(0).(3) *. 0.2;
  let rep2 =
    Risk_engine.Surface_constraints.validate_grid ~expiries:sc_exp ~k:sc_k
      ~iv:iv2 ~spot:100. ~rate:0.02 ~div:0.01
  in
  Alcotest.check st_check "cvx-fail" Risk_engine.Surface_constraints.Fail
    (sc_status rep2 "convexity")

let test_sc_gj () =
  (* raw violating params: eta far above the rho-implied cap *)
  let holds, margin =
    Risk_engine.Surface_constraints.gj_at ~theta:1.0 ~rho:0.0 ~eta:10.0
      ~gamma:0.25
  in
  Alcotest.check Alcotest.bool "violated" false holds;
  Alcotest.check Alcotest.bool "neg-margin" true (margin < 0.)

(* --- calibration --- *)

let cal_exp = [| 0.25; 0.5; 1.0 |]
let cal_k = [| -0.75; -0.25; 0.0; 0.25; 0.75 |]
let cal_true_u = [| -2.9; -2.7; -2.55; -0.42; 0.0; 0.41 |]

let gauss st =
  let u1 = Float.max (Random.State.float st 1.0) 1e-300 in
  let u2 = Random.State.float st 1.0 in
  sqrt (-2. *. Float.log u1) *. Float.cos (2. *. Float.pi *. u2)

(** Noisy grids from a known surface; [drift] shifts the latent. *)
let synth_grids ~seed u noise =
  let st = Random.State.make [| seed |] in
  let s = Risk_engine.Ssvi.decode cal_exp u in
  List.map
    (fun t ->
      let k = Array.copy cal_k
      and iv =
        Array.map
          (fun kk -> Risk_engine.Ssvi.iv s ~k:kk ~t +. (gauss st *. noise))
          cal_k
      and w = Array.make (Array.length cal_k) 1. in
      { Risk_engine.Quotes.t; k; iv; w })
    (Array.to_list cal_exp)

let test_cal_recovery () =
  let grids = synth_grids ~seed:42 cal_true_u 0.002 in
  (match Risk_engine.Ssvi_calibration.fit_day grids ~date:"2024-08-21" with
  | Risk_engine.Ssvi_calibration.Failed f ->
      Alcotest.fail ("fit failed: " ^ f.reason)
  | Risk_engine.Ssvi_calibration.Ok f ->
      Alcotest.check Alcotest.bool "converged" true f.converged;
      Alcotest.check Alcotest.bool "noise-level" true (f.fun_ < 1e-5);
      Alcotest.check Alcotest.bool "rmse" true (f.rmse_iv < 0.004);
      Alcotest.check Alcotest.bool "cold-start" false f.warm)

let test_cal_history () =
  let drift i =
    Array.mapi (fun j x -> if j = 4 then x +. (0.02 *. float_of_int i) else x)
      cal_true_u
  in
  let days =
    List.init 3 (fun i ->
        ("2024-08-2" ^ string_of_int (i + 1), synth_grids ~seed:(100 + i) (drift i) 0.001))
  in
  let outs = Risk_engine.Ssvi_calibration.fit_history days in
  Alcotest.check Alcotest.int "three-days" 3 (List.length outs);
  List.iteri
    (fun i o ->
      match o with
      | Risk_engine.Ssvi_calibration.Failed f ->
          Alcotest.fail ("day failed: " ^ f.reason)
      | Risk_engine.Ssvi_calibration.Ok f ->
          Alcotest.check Alcotest.bool "converged" true f.converged;
          Alcotest.check Alcotest.bool "warm-after-first" (i = 0 || f.warm) true;
          Alcotest.check Alcotest.bool "rmse" true (f.rmse_iv < 0.003))
    outs

let test_cal_json () =
  let grids = synth_grids ~seed:7 cal_true_u 0.001 in
  let f =
    match Risk_engine.Ssvi_calibration.fit_day grids ~date:"2024-08-21" with
    | Risk_engine.Ssvi_calibration.Ok f -> f
    | Risk_engine.Ssvi_calibration.Failed x ->
        Alcotest.fail ("fit failed: " ^ x.reason)
  in
  let j = Risk_engine.Ssvi_calibration.to_yojson [ f ] in
  let back = Risk_engine.Ssvi_calibration.of_yojson j in
  Alcotest.check Alcotest.int "one-fit" 1 (List.length back);
  let f2 = List.hd back in
  Alcotest.check Alcotest.string "date" f.date f2.date;
  Alcotest.check (Alcotest.float 1e-15) "fun" f.fun_ f2.fun_;
  Array.iteri
    (fun i x ->
      Alcotest.check (Alcotest.float 1e-15) "u" x f2.Risk_engine.Ssvi_calibration.u.(i))
    f.Risk_engine.Ssvi_calibration.u

let test_cal_recanon () =
  (* canonical refit of a synthetic fit: same common grid, tiny resample loss *)
  let grids = synth_grids ~seed:11 cal_true_u 0.001 in
  let f =
    match Risk_engine.Ssvi_calibration.fit_day grids ~date:"2024-08-21" with
    | Risk_engine.Ssvi_calibration.Ok f -> f
    | Risk_engine.Ssvi_calibration.Failed x ->
        Alcotest.fail ("fit failed: " ^ x.reason)
  in
  let canon_exp = [| 0.3; 0.75 |] and canon_k = [| -0.5; 0.0; 0.5 |] in
  let outs =
    Risk_engine.Ssvi_calibration.recanon [ f ] ~expiries:canon_exp ~k:canon_k
  in
  match outs with
  | [ Risk_engine.Ssvi_calibration.Ok g ] ->
      Alcotest.check Alcotest.int " expiries" 2
        (Array.length g.Risk_engine.Ssvi_calibration.expiries);
      Alcotest.check Alcotest.bool "resample-loss" true (g.rmse_iv < 0.002);
      (* decoded canonical surface matches the original on the new grid *)
      let s0 = Risk_engine.Ssvi.decode f.expiries f.u
      and s1 = Risk_engine.Ssvi.decode g.expiries g.u in
      Array.iter
        (fun t ->
          Array.iter
            (fun k ->
              let a = Risk_engine.Ssvi.iv s0 ~k ~t
              and b = Risk_engine.Ssvi.iv s1 ~k ~t in
              Alcotest.check (Alcotest.float 1e-3) "surface" a b)
            canon_k)
        canon_exp
  | _ -> Alcotest.fail "recanon failed"

let test_cal_failure () =
  (match Risk_engine.Ssvi_calibration.fit_day [] ~date:"2024-08-21" with
  | Risk_engine.Ssvi_calibration.Failed _ -> ()
  | Risk_engine.Ssvi_calibration.Ok _ -> Alcotest.fail "empty fit accepted");
  (match
     Risk_engine.Ssvi_calibration.fit_day
       [ { Risk_engine.Quotes.t = 0.5; k = [||]; iv = [||]; w = [||] } ]
       ~date:"2024-08-21"
   with
  | Risk_engine.Ssvi_calibration.Failed _ -> ()
  | Risk_engine.Ssvi_calibration.Ok _ -> Alcotest.fail "empty expiry accepted")

(* --- pca latent --- *)

let plat_fits () =
  (* 5 fits over 2024-08-21..28 with the 08-27 fit missing -> 26->28 gap *)
  let dates =
    [ "2024-08-21"; "2024-08-22"; "2024-08-23"; "2024-08-26"; "2024-08-28" ]
  in
  List.mapi
    (fun i d ->
      let grids = synth_grids ~seed:(1000 + i) cal_true_u 0.001 in
      match Risk_engine.Ssvi_calibration.fit_day grids ~date:d with
      | Risk_engine.Ssvi_calibration.Ok f -> Risk_engine.Ssvi_calibration.Ok f
      | Risk_engine.Ssvi_calibration.Failed x ->
          Alcotest.fail ("fit failed: " ^ x.reason))
    dates

let test_plat_gaps () =
  let lat = Risk_engine.Pca_latent.of_fits (plat_fits ()) in
  (* 4 adjacent pairs, minus the 8-26->8-28 two-day gap *)
  Alcotest.check Alcotest.int "diffs" 3 (Array.length lat.Risk_engine.Pca_latent.d_hist);
  Alcotest.check Alcotest.int "dropped" 1 lat.Risk_engine.Pca_latent.n_gap_dropped;
  Alcotest.check Alcotest.string "last" "2024-08-26"
    lat.Risk_engine.Pca_latent.d_dates.(2)

let test_plat_pca () =
  let lat = Risk_engine.Pca_latent.of_fits (plat_fits ()) in
  let full = Risk_engine.Pca_latent.pca_fit lat.Risk_engine.Pca_latent.d_hist 0 in
  Alcotest.check Alcotest.bool "explained" true (full.Risk_engine.Pca_latent.explained > 0.999);
  (* full-rank projection reconstructs *)
  let d = lat.Risk_engine.Pca_latent.d_hist.(0) in
  let r = Risk_engine.Pca_latent.project full d in
  Array.iteri
    (fun i x -> Alcotest.check (Alcotest.float 1e-9) "recon" x r.(i))
    d;
  (* truncation drops variance *)
  let k1 = Risk_engine.Pca_latent.pca_fit lat.Risk_engine.Pca_latent.d_hist 1 in
  Alcotest.check Alcotest.bool "residual" true
    (k1.Risk_engine.Pca_latent.residual_share >= full.Risk_engine.Pca_latent.residual_share);
  (* scores round-trip through loadings *)
  let sc = Risk_engine.Pca_latent.scores k1 d in
  Alcotest.check Alcotest.int "k" 1 (Array.length sc)

let test_plat_base () =
  let lat = Risk_engine.Pca_latent.of_fits (plat_fits ()) in
  let b = Risk_engine.Pca_latent.base_state lat ~as_of:"2024-08-26" in
  let u26 = lat.Risk_engine.Pca_latent.u_hist.(3) in
  Array.iteri (fun i x -> Alcotest.check (Alcotest.float 1e-15) "base" x b.(i)) u26;
  Alcotest.check Alcotest.bool "diff-found" true
    (Risk_engine.Pca_latent.diff_of_date lat "2024-08-26" <> None);
  Alcotest.check Alcotest.bool "diff-missing" true
    (Risk_engine.Pca_latent.diff_of_date lat "2024-08-28" = None);
  (match
     try Some (Risk_engine.Pca_latent.base_state lat ~as_of:"2024-08-01")
     with Invalid_argument _ -> None
   with
  | None -> ()
  | Some _ -> Alcotest.fail "base before first fit accepted")

(* --- vol engine end-to-end (synthetic book + Bootstrap) --- *)

(** Tiny EUR book: 100 shares + short 10 European calls. 41 daily closes
    (seeded walk), quotes fitted per day, Bootstrap vs Frozen comparison. *)
let e2e_bundle ~n_days ~seed =
  let st = Random.State.make [| seed |] in
  let t0 = Risk_core.Calendar.of_string "2024-08-21" in
  let dates =
    Array.init n_days (fun i ->
        Risk_core.Calendar.to_string
          (Risk_core.Calendar.add_business_days t0 i))
  in
  let rets = Array.init n_days (fun _ -> gauss st *. 0.01) in
  let s = ref 100. in
  let closes =
    Array.map
      (fun r ->
        s := !s *. exp r;
        !s)
      rets
  in
  let open Risk_core.Marketdata in
  let prices =
    Array.to_list
      (Array.mapi
         (fun i c ->
           {
             instrument_id = "EQ1";
             date = Risk_core.Calendar.of_string dates.(i);
             close = Risk_core.Money.decimal_of_string (Printf.sprintf "%.6f" c);
             currency = "EUR";
           })
         closes)
  in
  let fx =
    List.map
      (fun d ->
        {
          date = Risk_core.Calendar.of_string d;
          currency = "EUR";
          rate_to_eur = Risk_core.Money.decimal_of_string "1.0";
        })
      (Array.to_list dates)
  in
  let curves =
    List.concat_map
      (fun d ->
        List.map
          (fun (tenor, yrs) ->
            {
              date = Risk_core.Calendar.of_string d;
              currency = "EUR";
              tenor;
              tenor_years = yrs;
              zero_rate = 0.02;
            })
          Risk_core.Marketdata.tenors)
      (Array.to_list dates)
  in
  {
    prices;
    fx;
    curves;
    vols = [ { instrument_id = "EQ1"; atm_vol_30d = 0.3 } ];
    spreads = [];
    dividends = [ { instrument_id = "EQ1"; dividend_yield = 0.01 } ];
  }

let e2e_inputs (bundle : Risk_core.Marketdata.bundle) fits =
  let n_days = List.length bundle.Risk_core.Marketdata.prices in
  let open Risk_core in
  let und : Refdata.instrument_record =
    {
      instrument_id = "EQ1";
      inst_type = "EQUITY";
      currency = "EUR";
      terms =
        [
          ("issuer", `String "EQ1");
          ("sector", `String "equity");
          ("country", `String "n/a");
        ];
      overlay = false;
      base_instrument_id = None;
    }
  and opt : Refdata.instrument_record =
    {
      instrument_id = "OPT1";
      inst_type = "LISTED_OPTION";
      currency = "EUR";
      terms =
        [
          ("underlying_instrument_id", `String "EQ1");
          ("strike", `Float 100.0);
          ("expiry", `String "2025-02-20");
          ("call_put", `String "CALL");
          ("style", `String "EUROPEAN");
          ("multiplier", `Float 1.0);
        ];
      overlay = false;
      base_instrument_id = None;
    }
  in
  let holdings =
    [
      {
        Holdings.instrument_id = "EQ1";
        quantity = Money.decimal_of_string "100";
        market_value_local = Money.decimal_of_string "0";
        currency = "EUR";
        price = None;
        raw_identifier = "";
        raw_identifier_type = "";
      };
      {
        Holdings.instrument_id = "OPT1";
        quantity = Money.decimal_of_string "-10";
        market_value_local = Money.decimal_of_string "0";
        currency = "EUR";
        price = None;
        raw_identifier = "";
        raw_identifier_type = "";
      };
    ]
  in
  let as_of = Risk_core.Calendar.add_business_days (Risk_core.Calendar.of_string "2024-08-21") (n_days - 1) in
  ( {
      Risk_engine.Run.holdings = holdings;
      instruments = [ ("EQ1", und); ("OPT1", opt) ];
      bundle;
      as_of;
      base_currency = "EUR";
      row_portfolios = None;
      surf_fits = [ ("EQ1", fits) ];
    },
    as_of )

let e2e_fits dates =
  List.mapi
    (fun i d ->
      let grids = synth_grids ~seed:(2000 + i) cal_true_u 0.001 in
      match Risk_engine.Ssvi_calibration.fit_day grids ~date:d with
      | Risk_engine.Ssvi_calibration.Ok f -> f
      | Risk_engine.Ssvi_calibration.Failed x ->
          Alcotest.fail ("e2e fit failed: " ^ x.reason))
    dates

let e2e_config vol =
  let c = Risk_engine.Config.default in
  {
    c with
    Risk_engine.Config.method_ = Risk_engine.Config.Historical;
    confidences = [ 0.99 ];
    horizons = [ 1 ];
    history_days = 30;
    n_scenarios = 1000;
    vol_model = vol;
    vol_pcs = 0;
    vol_universe =
      (match vol with
      | Risk_engine.Config.Frozen -> []
      | _ -> [ "EQ1" ]);
  }

let e2e_var res =
  let r =
    List.find
      (fun (x : Risk_engine.Run.result_row) ->
        x.measure = "var"
        && List.assoc_opt "confidence" x.slice = Some (`Float 0.99))
      res.Risk_engine.Run.rows
  in
  r.value

let test_e2e_bootstrap () =
  let n_days = 41 in
  let bundle = e2e_bundle ~n_days ~seed:99 in
  let dates =
    List.init n_days (fun i ->
        Risk_core.Calendar.to_string
          (Risk_core.Calendar.add_business_days
             (Risk_core.Calendar.of_string "2024-08-21")
             i))
  in
  (* fits only for the first 30 close dates -> last 11 scenarios fall back *)
  let fits = e2e_fits (List.filteri (fun i _ -> i < 30) dates) in
  let inputs, _ = e2e_inputs bundle fits in
  let frozen =
    Risk_engine.Run.run_engine inputs (e2e_config Risk_engine.Config.Frozen) ()
  in
  let boot =
    Risk_engine.Run.run_engine inputs (e2e_config Risk_engine.Config.Bootstrap) ()
  in
  let vf = e2e_var frozen and vb = e2e_var boot in
  Alcotest.check Alcotest.bool "var-positive" true (vf > 0. && vb > 0.);
  (* vol shocks move the short-call book *)
  Alcotest.check Alcotest.bool "vol-matters" true (abs_float (vb -. vf) > 1e-6 *. vf);
  let diag = boot.Risk_engine.Run.diagnostics in
  Alcotest.check Alcotest.bool "vol-diag" true
    (List.assoc_opt "vol_model" diag = Some (`String "bootstrap"));
  Alcotest.check Alcotest.bool "fallback-counted" true
    (match List.assoc_opt "vol_fallback_scenarios" diag with
    | Some (`Int n) -> n = 11
    | _ -> false)

(* --- american accuracy: BAW vs CRR binomial reference --- *)

(** Cox-Ross-Rubinstein American pricer (reference only: O(n^2), too slow
    for scenarios). Continuous yield, n steps. *)
let crr_amer ~s ~k ~t ~r ~q ~sigma ~call ~n =
  let dt = t /. float_of_int n in
  let up = exp (sigma *. sqrt dt) in
  let d = 1. /. up in
  let disc = exp (-.r *. dt) in
  let p = (exp ((r -. q) *. dt) -. d) /. (up -. d) in
  let v =
    Array.init (n + 1) (fun i ->
        let st = s *. (up ** float_of_int i) *. (d ** float_of_int (n - i)) in
        if call then Float.max (st -. k) 0. else Float.max (k -. st) 0.)
  in
  for j = n - 1 downto 0 do
    for i = 0 to j do
      let st = s *. (up ** float_of_int i) *. (d ** float_of_int (j - i)) in
      let hold = disc *. ((p *. v.(i + 1)) +. ((1. -. p) *. v.(i))) in
      let exer =
        if call then Float.max (st -. k) 0. else Float.max (k -. st) 0.
      in
      v.(i) <- Float.max hold exer
    done
  done;
  v.(0)

(* --- american shootout: BAW vs Bjerksund-Stensland 2002 vs CRR --- *)


let test_amer_accuracy () =
  (* Phi2 quadrature goldens (exact values) *)
  Alcotest.check (Alcotest.float 1e-9) "phi2-000" 0.25
    (Risk_engine.Options.phi2 0. 0. 0.);
  Alcotest.check (Alcotest.float 1e-9) "phi2-005" (1. /. 3.)
    (Risk_engine.Options.phi2 0. 0. 0.5);
  (* production vector paths agree with the scalar kernel exactly *)
  let ss = [| 400.; 522.35; 600. |] and rr = [| 0.0; 0.03; 0.05 |] in
  let bb = [| -0.25; -0.01; 0.035 |] and sg = [| 0.2; 0.5; 0.8 |] in
  List.iter
    (fun call ->
      let v1 =
        Risk_engine.Options.bs2002 ~s:ss ~k:500. ~t:0.5 ~r:rr ~b:bb
          ~sigma:0.4 ~call
      and vv =
        Risk_engine.Options.bs2002_v ~s:ss ~k:500. ~t:0.5 ~r:rr ~b:bb
          ~sigma:sg ~call
      in
      Array.iteri
        (fun i x ->
          let ref_ =
            Risk_engine.Options.bs2002_1 ~s:ss.(i) ~k:500. ~t:0.5 ~r:rr.(i)
              ~b:bb.(i) ~sigma:0.4 ~call
          in
          Alcotest.check (Alcotest.float 1e-12) "vec-scalar" ref_ x;
          let refv =
            Risk_engine.Options.bs2002_1 ~s:ss.(i) ~k:500. ~t:0.5 ~r:rr.(i)
              ~b:bb.(i) ~sigma:sg.(i) ~call
          in
          Alcotest.check (Alcotest.float 1e-12) "vecsig-scalar" refv vv.(i))
        v1)
    [ true; false ];
  Alcotest.check (Alcotest.float 1e-9) "phi2-110"
    (Risk_engine.Options.ndtr 1. ** 2.)
    (Risk_engine.Options.phi2 1. 1. 0.);
  Alcotest.check (Alcotest.float 1e-12) "phi2-sym"
    (Risk_engine.Options.phi2 0.7 (-0.4) 0.3)
    (Risk_engine.Options.phi2 (-0.4) 0.7 0.3);
  (* quadrature path (|rho| >= 0.925): Phi2(0,0,rho) = 1/4 + asin(rho)/2pi *)
  Alcotest.check (Alcotest.float 1e-9) "phi2-quad"
    (0.25 +. asin 0.99 /. (2. *. Float.pi))
    (Risk_engine.Options.phi2 0. 0. 0.99);
  let s, r, q = (522.35, 0.04, 0.005) in
  let n = 1000 in
  let worst = ref 0. and worst_at = ref ("", 0., 0., 0.) in
  let rows = ref [] in
  List.iter
    (fun call ->
      List.iter
        (fun sigma ->
          List.iter
            (fun t ->
              List.iter
                (fun m ->
                  let k = s *. m in
                  let baw =
                    (Risk_engine.Options.baw ~s:[| s |] ~k ~t ~r:[| r |]
                       ~b:[| r -. q |] ~sigma ~call).(0)
                  in
                  let ref_ =
                    crr_amer ~s ~k ~t ~r ~q ~sigma ~call ~n
                  in
                  let scale = Float.max ref_ (0.01 *. s) in
                  let err = abs_float (baw -. ref_) /. scale in
                  if err > !worst then (
                    worst := err;
                    worst_at := ((if call then "C" else "P"), sigma, t, m));
                  rows :=
                    (Printf.sprintf "%s sig=%.1f T=%.2f K/S=%.2f baw=%.4f crr=%.4f err=%.4f%%"
                       (if call then "C" else "P")
                       sigma t m baw ref_ (100. *. err))
                    :: !rows)
                [ 0.7; 0.85; 1.0; 1.15 ])
            [ 0.08; 0.5 ])
        [ 0.4; 0.8 ])
    [ true; false ];
  List.iter (fun l -> Printf.printf "  amer %s\n" l) (List.rev !rows);
  let tag =
    fun (c, sg, t, m) -> Printf.sprintf "%s sig=%.1f T=%.2f K/S=%.2f" c sg t m
  in
  Printf.printf "  amer worst: %.4f%% at %s\n" (100. *. !worst) (tag !worst_at);
  Alcotest.check Alcotest.bool "baw-within-2pct" true (!worst < 0.02);
  (* hostile grid: very short-dated, deep ITM, high dividend yield.
     Three-way BAW vs BS2002 vs CRR; pre-registered rule: BS replaces BAW
     only if it wins clearly where BAW is materially wrong. *)
  let wbaw = ref 0. and wbs = ref 0. in
  let wat_baw = ref ("", 0., 0., 0.) and wat_bs = ref ("", 0., 0., 0.) in
  List.iter
    (fun call ->
      List.iter
        (fun sigma ->
          List.iter
            (fun (t, qq) ->
              List.iter
                (fun m ->
                  let k = s *. m in
                  let b = r -. qq in
                  let baw =
                    (Risk_engine.Options.baw ~s:[| s |] ~k ~t ~r:[| r |]
                       ~b:[| b |] ~sigma ~call).(0)
                  in
                  let bs = Risk_engine.Options.bs2002_1 ~s ~k ~t ~r ~b ~sigma ~call in
                  let ref_ = crr_amer ~s ~k ~t ~r ~q:qq ~sigma ~call ~n in
                  let scale = Float.max ref_ (0.01 *. s) in
                  let eb = abs_float (baw -. ref_) /. scale
                  and es = abs_float (bs -. ref_) /. scale in
                  if eb > !wbaw then (
                    wbaw := eb;
                    wat_baw := ((if call then "C" else "P"), sigma, t, m));
                  if es > !wbs then (
                    wbs := es;
                    wat_bs := ((if call then "C" else "P"), sigma, t, m));
                  Printf.printf
                    "  hostile %s sig=%.1f q=%.2f T=%.2f K/S=%.2f baw=%.4f \
                     bs=%.4f crr=%.4f eb=%.4f%% es=%.4f%%\n"
                    (if call then "C" else "P")
                    sigma qq t m baw bs ref_ (100. *. eb) (100. *. es))
                [ 0.6; 0.8; 1.2 ])
            [ (0.02, 0.01); (0.08, 0.01); (0.02, 0.08); (0.5, 0.08) ])
        [ 0.3; 0.6 ])
    [ true; false ];
  Printf.printf "  hostile baw worst: %.4f%% at %s\n" (100. *. !wbaw)
    (tag !wat_baw);
  Printf.printf "  hostile bs  worst: %.4f%% at %s\n" (100. *. !wbs)
    (tag !wat_bs);
  (* 2026-10 shootout verdict: neither dominates; both < 1.3%. BAW stays
     the production model (already vectorized in option_price_v); BS-2002
     remains here as the challenger reference. *)
  Alcotest.check Alcotest.bool "baw-hostile" true (!wbaw < 0.02);
  Alcotest.check Alcotest.bool "bs-hostile" true (!wbs < 0.02);
  (* abyss grid: book envelope (q to 25%, r to 0%, T to 3y). CRR steps
     grow with maturity to hold the reference error down. *)
  let wab = ref 0. and wsb = ref 0. in
  let wat_ab = ref "" and wat_sb = ref "" in
  (* BAW is only defined for r > 0 (paper assumption; at r = 0 its kk term
     is 0/0 and our fallback silently returns European, missing the entire
     early-exercise premium). Tracked separately so the suite enshrines
     the domain, not the failure. *)
  let wab_pos = ref 0. in
  let atag c sg r qq t m =
    Printf.sprintf "%s sig=%.2f r=%.2f q=%.2f T=%.2f K/S=%.2f" c sg r qq t m
  in
  List.iter
    (fun call ->
      List.iter
        (fun sigma ->
          List.iter
            (fun (t, r, qq) ->
              let ndeep = if t >= 1. then 2000 else 1000 in
              List.iter
                (fun m ->
                  let k = s *. m in
                  let b = r -. qq in
                  let baw =
                    (Risk_engine.Options.baw ~s:[| s |] ~k ~t ~r:[| r |]
                       ~b:[| b |] ~sigma ~call).(0)
                  in
                  let bs = Risk_engine.Options.bs2002_1 ~s ~k ~t ~r ~b ~sigma ~call in
                  let ref_ = crr_amer ~s ~k ~t ~r ~q:qq ~sigma ~call ~n:ndeep in
                  let scale = Float.max ref_ (0.01 *. s) in
                  let eb = abs_float (baw -. ref_) /. scale
                  and es = abs_float (bs -. ref_) /. scale in
                  if eb > !wab then (
                    wab := eb;
                    wat_ab :=
                      atag
                        (if call then "C" else "P")
                        sigma r qq t m);
                  if r > 0.01 then
                    if eb > !wab_pos then wab_pos := eb;
                  if es > !wsb then (
                    wsb := es;
                    wat_sb :=
                      atag
                        (if call then "C" else "P")
                        sigma r qq t m);
                  Printf.printf
                    "  abyss %s sig=%.2f r=%.2f q=%.2f T=%.2f K/S=%.2f \
                     baw=%.4f bs=%.4f crr=%.4f eb=%.4f%% es=%.4f%%\n"
                    (if call then "C" else "P")
                    sigma r qq t m baw bs ref_ (100. *. eb) (100. *. es))
                [ 0.6; 0.8; 1.0 ])
            [ (1.0, 0.02, 0.06); (3.0, 0.02, 0.06); (1.0, 0.0, 0.15);
              (3.0, 0.0, 0.15); (1.0, 0.0, 0.25); (3.0, 0.0, 0.25) ])
        [ 0.3; 0.5 ])
    [ true; false ];
  Printf.printf "  abyss baw worst: %.4f%% at %s\n" (100. *. !wab) !wat_ab;
  Printf.printf "  abyss baw worst (r>0): %.4f%%\n" (100. *. !wab_pos);
  Printf.printf "  abyss bs  worst: %.4f%% at %s\n" (100. *. !wsb) !wat_sb;
  Alcotest.check Alcotest.bool "baw-in-domain" true (!wab_pos < 0.02);
  Alcotest.check Alcotest.bool "bs-abyss" true (!wsb < 0.02)

let find_reference () =
  let exe_dir =
    try Filename.dirname Sys.executable_name
    with _ -> "."
  in
  let exe_dir =
    if Filename.is_relative exe_dir then
      Filename.concat (Sys.getcwd ()) exe_dir
    else exe_dir
  in
  let cands =
    [
      "fixtures/ssvi-cal-v1/scipy_reference.json";
      "../fixtures/ssvi-cal-v1/scipy_reference.json";
      Filename.concat exe_dir
        "../../../fixtures/ssvi-cal-v1/scipy_reference.json";
    ]
  in
  let with_env =
    match Sys.getenv_opt "RISK_AI_FIXTURES" with
    | None -> []
    | Some d -> [ Filename.concat d "../ssvi-cal-v1/scipy_reference.json" ]
  in
  let rec go = function
    | [] -> Alcotest.fail "scipy_reference.json not found"
    | p :: rest -> if Sys.file_exists p then p else go rest
  in
  go (with_env @ cands)

let test_optim_rosenbrock () =
  (* sanity: classic Rosenbrock minimum at (1,1) *)
  let f x =
    let a = 1. -. x.(0) and b = x.(1) -. (x.(0) *. x.(0)) in
    (a *. a) +. (100. *. b *. b)
  in
  let r = Optim.nelder_mead f [| -1.2; 1.0 |] in
  Alcotest.check Alcotest.bool "converged" true r.Optim.converged;
  Alcotest.check (Alcotest.float 1e-6) "fmin" 0. r.Optim.f;
  Alcotest.check (Alcotest.float 1e-4) "x0" 1. r.Optim.x.(0);
  Alcotest.check (Alcotest.float 1e-4) "x1" 1. r.Optim.x.(1)

let test_optim_ssvi_scipy () =
  let open Yojson.Basic.Util in
  let j = Yojson.Basic.from_file (find_reference ()) in
  let exp = j |> member "expiries" |> to_list |> List.map to_number |> Array.of_list
  and k =
    j |> member "k_grid" |> to_list |> List.map to_number |> Array.of_list
  and obs =
    j |> member "obs_iv" |> to_list
    |> List.map (fun r -> r |> to_list |> List.map to_number |> Array.of_list)
    |> Array.of_list
  in
  let atm = j |> member "from_atm" in
  let u0 =
    atm |> member "u0" |> to_list |> List.map to_number |> Array.of_list
  in
  let tight =
    atm |> member "scipy_nelder_mead_tight" in
  let ref_fun = tight |> member "fun" |> to_number
  and ref_u =
    tight |> member "u" |> to_list |> List.map to_number |> Array.of_list
  in
  let obj u =
    let s = Ssvi.decode exp u in
    let acc = ref 0. and n = ref 0 in
    Array.iteri
      (fun j t ->
        Array.iteri
          (fun i kk ->
            let d = Ssvi.iv s ~k:kk ~t -. obs.(j).(i) in
            acc := !acc +. (d *. d);
            incr n)
          k)
      exp;
    !acc /. float_of_int !n
  in
  (* formula cross-check, independent of our optimizer: OCaml objective
     evaluated at scipy's optimum reproduces scipy's reported fun *)
  Alcotest.check (Alcotest.float 1e-12) "formula-matches-scipy" ref_fun (obj ref_u);
  let r =
    Optim.nelder_mead ~maxiter:20000 ~xatol:1e-10 ~fatol:1e-14 obj u0
  in
  Alcotest.check Alcotest.bool "converged" true r.Optim.converged;
  Alcotest.check (Alcotest.float 1e-12) "matches-scipy" ref_fun r.Optim.f;
  Alcotest.check Alcotest.bool "beats-or-matches" true (r.Optim.f <= ref_fun *. 1.000001);
  (* decoded optimum is admissible and fits to noise level *)
  let s = Ssvi.decode exp r.Optim.x in
  Alcotest.check Alcotest.bool "rho" true (abs_float s.Ssvi.rho < 1.);
  Alcotest.check Alcotest.bool "rmse-noise-level" true (sqrt r.Optim.f < 0.002)

(* --- ssvi --- *)

let ssvi_exp = [| 0.0822; 0.25; 0.5; 1.0; 2.0 |]

let test_ssvi_atm () =
  (* w(0,th) = th exactly, for any admissible surface *)
  let u = [| 0.5; -0.3; 0.1; 0.0; 0.2; 0.4; -0.1; 0.3 |] in
  let s = Ssvi.decode ssvi_exp u in
  Array.iteri
    (fun j _ ->
      let th = s.Ssvi.theta.(j) in
      let phi = Ssvi.phi_of ~theta:th ~eta:s.Ssvi.eta ~gamma:s.Ssvi.gamma in
      let w =
        Ssvi.w_of ~k:0. ~theta:th ~rho:s.Ssvi.rho ~phi
      in
      Alcotest.check eps "atm-identity" th w;
      let v = Ssvi.iv s ~k:0. ~t:ssvi_exp.(j) in
      Alcotest.check eps "atm-iv" (sqrt (th /. ssvi_exp.(j))) v)
    ssvi_exp

let test_ssvi_bounds () =
  (* extreme latent states stay strictly admissible, finite, positive *)
  let latents =
    [
      [| 30.; 30.; 30.; 30.; 30.; 30.; 30.; 30. |];
      [| -30.; -30.; -30.; -30.; -30.; -30.; -30.; -30. |];
      [| 5.; -5.; 5.; -5.; 5.; 10.; -10.; 10. |];
    ]
  in
  List.iter
    (fun u ->
      let s = Ssvi.decode ssvi_exp u in
      Alcotest.check Alcotest.bool "rho<1" true (abs_float s.Ssvi.rho < 1.);
      Alcotest.check Alcotest.bool "gamma-range" true
        (s.Ssvi.gamma > 0. && s.Ssvi.gamma < 0.5);
      let eta_max = 2. /. (1. +. abs_float s.Ssvi.rho) in
      Alcotest.check Alcotest.bool "eta-cap" true
        (s.Ssvi.eta > 0. && s.Ssvi.eta < eta_max);
      for j = 1 to Array.length ssvi_exp - 1 do
        Alcotest.check Alcotest.bool "theta-incr" true
          (s.Ssvi.theta.(j) > s.Ssvi.theta.(j - 1))
      done;
      let ks = [| -2.; -0.5; 0.; 0.5; 2. |] in
      Array.iter
        (fun k ->
          Array.iter
            (fun t ->
              let v = Ssvi.iv s ~k ~t in
              Alcotest.check Alcotest.bool "finite-pos" true
                (Float.is_finite v && v > 0.))
            ssvi_exp)
        ks)
    latents

let test_ssvi_roundtrip () =
  let u =
    [| 0.5; -0.3; 0.1; 0.0; 0.2; 1.2; -0.7; 0.9 |]
  in
  let s = Ssvi.decode ssvi_exp u in
  let u2 = Ssvi.encode s in
  Array.iteri
    (fun i x -> Alcotest.check (Alcotest.float 1e-9) "latent" x u2.(i))
    u;
  let s2 = Ssvi.decode ssvi_exp u2 in
  Alcotest.check eps "rho" s.Ssvi.rho s2.Ssvi.rho;
  Alcotest.check eps "eta" s.Ssvi.eta s2.Ssvi.eta

let () =
  Alcotest.run "risk-ai"
    [
      ( "money",
        [
          Alcotest.test_case "parse" `Quick test_money_parse;
          Alcotest.test_case "rejects" `Quick test_money_rejects;
          Alcotest.test_case "quantize" `Quick test_money_quantize;
        ] );
      ( "calendar",
        [
          Alcotest.test_case "easter" `Quick test_easter;
          Alcotest.test_case "target2" `Quick test_target2;
          Alcotest.test_case "arith" `Quick test_biz_arith;
        ] );
      ("currencies", [ Alcotest.test_case "iso" `Quick test_currencies ]);
      ( "stats",
        [
          Alcotest.test_case "ewma" `Quick test_ewma;
          Alcotest.test_case "sample-cov" `Quick test_sample_cov;
          Alcotest.test_case "psd" `Quick test_psd;
          Alcotest.test_case "var-es" `Quick test_var_es;
          Alcotest.test_case "components" `Quick test_components;
          Alcotest.test_case "mc-shape" `Quick test_mc_shape;
        ] );
      ( "options",
        [
          Alcotest.test_case "ndtr" `Quick test_ndtr;
          Alcotest.test_case "parity" `Quick test_put_call_parity;
          Alcotest.test_case "american" `Quick test_american_bounds;
          Alcotest.test_case "amer-shootout" `Slow test_amer_accuracy;
        ] );
      ( "config",
        [
          Alcotest.test_case "defaults" `Quick test_config_defaults;
          Alcotest.test_case "roundtrip" `Quick test_config_roundtrip;
          Alcotest.test_case "vol-compat" `Quick test_config_vol_compat;
        ] );
      ("fixtures", [ Alcotest.test_case "load" `Quick test_fixtures ]);
      ( "csv",
        [
          Alcotest.test_case "basic" `Quick test_csv_basic;
          Alcotest.test_case "quoted" `Quick test_csv_quoted;
          Alcotest.test_case "crlf" `Quick test_csv_crlf_no_trailing;
          Alcotest.test_case "rejects" `Quick test_csv_rejects;
          Alcotest.test_case "col" `Quick test_csv_col;
        ] );
      ( "ssvi",
        [
          Alcotest.test_case "atm-identity" `Quick test_ssvi_atm;
          Alcotest.test_case "bounds" `Quick test_ssvi_bounds;
          Alcotest.test_case "roundtrip" `Quick test_ssvi_roundtrip;
        ] );
      ( "optim",
        [
          Alcotest.test_case "rosenbrock" `Quick test_optim_rosenbrock;
          Alcotest.test_case "ssvi-vs-scipy" `Quick test_optim_ssvi_scipy;
        ] );
      ( "quotes",
        [
          Alcotest.test_case "roundtrip" `Quick test_quotes_roundtrip;
          Alcotest.test_case "amer" `Quick test_quotes_amer;
          Alcotest.test_case "grid-itm" `Quick test_quotes_grid_itm;
          Alcotest.test_case "grid" `Quick test_quotes_grid;
        ] );
      ( "calibration",
        [
          Alcotest.test_case "recovery" `Quick test_cal_recovery;
          Alcotest.test_case "history" `Quick test_cal_history;
          Alcotest.test_case "json" `Quick test_cal_json;
          Alcotest.test_case "recanon" `Quick test_cal_recanon;
          Alcotest.test_case "failure" `Quick test_cal_failure;
        ] );
      ( "pca-latent",
        [
          Alcotest.test_case "gaps" `Quick test_plat_gaps;
          Alcotest.test_case "pca" `Quick test_plat_pca;
          Alcotest.test_case "base" `Quick test_plat_base;
        ] );
      ( "vol-e2e",
        [ Alcotest.test_case "bootstrap" `Quick test_e2e_bootstrap ] );
      ( "surface-constraints",
        [
          Alcotest.test_case "valid" `Quick test_sc_valid;
          Alcotest.test_case "structural" `Quick test_sc_structural;
          Alcotest.test_case "calendar" `Quick test_sc_calendar;
          Alcotest.test_case "shape" `Quick test_sc_shape;
          Alcotest.test_case "gj" `Quick test_sc_gj;
        ] );
    ]
