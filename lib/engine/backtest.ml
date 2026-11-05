(** VaR backtesting (M6). Port of [engine/backtest.py].

    Pure, like [run_engine]. For each test date t (the last [n_days] business
    days with a next-day move in the bundle): 1-day VaR as of t vs the
    hypothetical P&L of the static book revalued on the actual t → t+1 move with
    a 1-day horizon roll. Vols, spreads and dividend yields are static
    (ADR-007). Tests: Kupiec POF (χ²₁), Christoffersen independence (χ²₁) and
    conditional coverage (χ²₂), Basel traffic light from the binomial CDF.

    Divergence: no scipy — χ² tails use [Options.erfc] (exact for df 1, 2) and
    the binomial CDF is a forward recurrence; accurate to ~1e-12 on the oracle.
    Python [ValueError]s map to [Invalid_argument]. *)

open Risk_core

exception Backtest_error of string

type lr_test = { statistic : float; p_value : float; df : int }

(** x·ln(y), with 0·ln(0) = 0 (scipy xlogy). *)
let xlogy x y = if x = 0. then 0. else x *. Float.log y

(** Chi-square survival for df 1 and 2 (scipy chi2.sf). *)
let chi2_sf x df =
  if x <= 0. then 1.
  else
    match df with
    | 1 -> Options.erfc (sqrt (x /. 2.))
    | 2 -> exp (-.x /. 2.)
    | d ->
        invalid_arg
          ("chi2_sf: only df 1 and 2 supported, got " ^ string_of_int d)

let lr_to_result lr df =
  let lr = if lr > 0. then lr else 0. in
  { statistic = lr; p_value = chi2_sf lr df; df }

(** Kupiec (1995) proportion of failures: H0 exception rate = p (p = 1 −
    confidence). *)
let kupiec_pof n x p =
  if n <= 0 || x < 0 || x > n then
    invalid_arg "need 0 <= exceptions <= observations, observations > 0";
  let phat = float_of_int x /. float_of_int n in
  let ll0 =
    xlogy (float_of_int (n - x)) (1. -. p) +. xlogy (float_of_int x) p
  in
  let ll1 =
    xlogy (float_of_int (n - x)) (1. -. phat) +. xlogy (float_of_int x) phat
  in
  lr_to_result (-2. *. (ll0 -. ll1)) 1

(** Christoffersen (1998) first-order Markov independence of exceptions. *)
let christoffersen_independence hits =
  let n = Array.length hits in
  if n < 2 then invalid_arg "need at least two observations";
  let n00 = ref 0 and n01 = ref 0 and n10 = ref 0 and n11 = ref 0 in
  for i = 0 to n - 2 do
    match (hits.(i), hits.(i + 1)) with
    | false, false -> incr n00
    | false, true -> incr n01
    | true, false -> incr n10
    | true, true -> incr n11
  done;
  let n00, n01, n10, n11 =
    (float_of_int !n00, float_of_int !n01, float_of_int !n10, float_of_int !n11)
  in
  let pi01 = if n00 +. n01 = 0. then 0. else n01 /. (n00 +. n01) in
  let pi11 = if n10 +. n11 = 0. then 0. else n11 /. (n10 +. n11) in
  let pi = (n01 +. n11) /. (n00 +. n01 +. n10 +. n11) in
  let ll0 = xlogy (n00 +. n10) (1. -. pi) +. xlogy (n01 +. n11) pi in
  let ll1 =
    xlogy n00 (1. -. pi01)
    +. xlogy n01 pi01
    +. xlogy n10 (1. -. pi11)
    +. xlogy n11 pi11
  in
  lr_to_result (-2. *. (ll0 -. ll1)) 1

(** P(X ≤ x), X ~ Binomial(n, p), by forward recurrence. *)
let binom_cdf x n p =
  if x < 0 then 0.
  else if x >= n then 1.
  else if p = 0. then 1.
  else if p = 1. then 0.
  else
    let q = 1. -. p in
    let term = ref (q ** float_of_int n) in
    let s = ref !term in
    for i = 0 to x - 1 do
      term := !term *. float_of_int (n - i) /. float_of_int (i + 1) *. p /. q;
      s := !s +. !term
    done;
    Float.min !s 1.

(** Basel zone: 250 obs at 99% → 0–4 / 5–9 / 10+. *)
let traffic_light n x p =
  let cum = binom_cdf x n p in
  if cum < 0.95 then "green" else if cum < 0.9999 then "yellow" else "red"

type backtest_day = {
  forecast_date : Calendar.date;  (** VaR as of this date *)
  pnl_date : Calendar.date;  (** hypothetical P&L is the move into this date *)
  var : float;
  pnl : float;
}

let is_exception d = -.d.pnl > d.var

type backtest_result = {
  confidence : float;
  method_ : string;
  base_currency : string;
  days : backtest_day list;
}

let observations r = List.length r.days
let exceptions r = List.filter is_exception r.days |> List.length

let summary r =
  let p = 1. -. r.confidence in
  let n = observations r and x = exceptions r in
  let pof = kupiec_pof n x p in
  let hits = Array.of_list (List.map is_exception r.days) in
  let ind = christoffersen_independence hits in
  let cc = lr_to_result (pof.statistic +. ind.statistic) 2 in
  let first = List.hd r.days and last = List.nth r.days (n - 1) in
  ( pof,
    ind,
    cc,
    [
      ("method", `String r.method_);
      ("confidence", `Float r.confidence);
      ("horizon_days", `Int 1);
      ("pnl", `String "hypothetical");
      ("base_currency", `String r.base_currency);
      ("start", `String (Calendar.to_string first.forecast_date));
      ("end", `String (Calendar.to_string last.pnl_date));
      ("observations", `Int n);
      ("exceptions", `Int x);
      ("expected_exceptions", `Float (float_of_int n *. p));
      ("exception_rate", `Float (float_of_int x /. float_of_int n));
      ("traffic_light", `String (traffic_light n x p));
    ] )

let lr_json t =
  `Assoc
    [
      ("statistic", `Float t.statistic);
      ("p_value", `Float t.p_value);
      ("df", `Int t.df);
    ]

(** [risk_results] rows for view [backtest]: four summary rows plus per-day
    [var]/[pnl] rows keyed by P&L date (dimension [day]). *)
let rows r =
  let pof, ind, cc, s = summary r in
  let get k = List.assoc k s in
  let common =
    List.map
      (fun k -> (k, List.assoc k s))
      [ "method"; "confidence"; "horizon_days" ]
  in
  let exc =
    {
      Run.view = "backtest";
      dimension = None;
      key = None;
      measure = "exceptions";
      slice =
        common
        @ [
            ("observations", get "observations");
            ("expected_exceptions", get "expected_exceptions");
            ("exception_rate", get "exception_rate");
            ("traffic_light", get "traffic_light");
            ("pnl", get "pnl");
            ("start", get "start");
            ("end", get "end");
          ];
      value = float_of_int (exceptions r);
      value_currency = None;
    }
  in
  let test_row measure t =
    {
      Run.view = "backtest";
      dimension = None;
      key = None;
      measure;
      slice = common @ [ ("p_value", `Float t.p_value); ("df", `Int t.df) ];
      value = t.statistic;
      value_currency = None;
    }
  in
  let day_rows =
    List.concat_map
      (fun d ->
        let key = Calendar.to_string d.pnl_date in
        let meta =
          common
          @ [
              ("forecast_date", `String (Calendar.to_string d.forecast_date));
              ("exception", `Bool (is_exception d));
            ]
        in
        [
          {
            Run.view = "backtest";
            dimension = Some "day";
            key = Some key;
            measure = "var";
            slice = meta;
            value = d.var;
            value_currency = Some r.base_currency;
          };
          {
            Run.view = "backtest";
            dimension = Some "day";
            key = Some key;
            measure = "pnl";
            slice = meta;
            value = d.pnl;
            value_currency = Some r.base_currency;
          };
        ])
      r.days
  in
  [
    exc;
    test_row "kupiec_pof" pof;
    test_row "christoffersen_independence" ind;
    test_row "christoffersen_conditional_coverage" cc;
  ]
  @ day_rows

let as_json r =
  let pof, ind, cc, s = summary r in
  `Assoc
    [
      ( "summary",
        `Assoc
          (s
          @ [
              ("kupiec_pof", lr_json pof);
              ("christoffersen_independence", lr_json ind);
              ("christoffersen_conditional_coverage", lr_json cc);
            ]) );
      ( "days",
        `List
          (List.map
             (fun d ->
               `Assoc
                 [
                   ( "forecast_date",
                     `String (Calendar.to_string d.forecast_date) );
                   ("pnl_date", `String (Calendar.to_string d.pnl_date));
                   ("var", `Float d.var);
                   ("pnl", `Float d.pnl);
                   ("exception", `Bool (is_exception d));
                 ])
             r.days) );
    ]

(** Backtest the last [config.backtest.days] days of [inputs.bundle] up to
    [inputs.as_of] at [config.backtest.confidence]. Needs
    [config.history_days + days] daily returns. [should_cancel] is polled before
    each day. *)
let run_backtest (inputs : Run.engine_inputs) (config : Config.run_config)
    ?(should_cancel = None) () =
  let bt =
    match config.backtest with
    | None -> { Config.days = 250; confidence = 0.99 }
    | Some b -> b
  in
  let n_days, confidence = (bt.days, bt.confidence) in
  let rp = inputs.row_portfolios in
  let book0 =
    Pricing.build_book inputs.holdings inputs.instruments
      (Market.market_state inputs.bundle inputs.as_of)
      inputs.base_currency rp
  in
  let full =
    Market.build_factors inputs.bundle ~equity_ids:(Pricing.equity_ids book0)
      ~fx_ccys:(Pricing.currencies book0)
      ~curve_ccys:(Pricing.curve_ids book0)
      ~history_days:None
  in
  let dates = full.return_dates in
  if
    Array.length dates = 0
    || not (Calendar.equal dates.(Array.length dates - 1) inputs.as_of)
  then
    raise
      (Backtest_error
         ("bundle history does not end on " ^ Calendar.to_string inputs.as_of));
  let need = config.history_days + n_days in
  if Array.length dates < need then
    raise
      (Backtest_error
         (Printf.sprintf
            "need %d daily returns (%d history + %d test days), bundle has %d"
            need config.history_days n_days (Array.length dates)));
  let days = ref [] in
  let first = Array.length dates - n_days - 1 in
  for e = first to Array.length dates - 2 do
    (match should_cancel with
    | None -> ()
    | Some f -> if f () then raise Run.(Engine_cancelled "backtest cancelled"));
    let t = dates.(e) in
    let state = Market.market_state inputs.bundle t in
    let book =
      Pricing.build_book inputs.holdings inputs.instruments state
        inputs.base_currency rp
    in
    let factors = Market.factor_window full ~end_:e ~n:config.history_days in
    let pricer = Pricing.make_pricer book state factors in
    let v0 = Pricing.base_values pricer in
    let shocks, weights, _, _ = Run.one_day_shocks factors config () in
    let total = Run.total_pnl pricer v0 shocks 1 config.chunk_size None 1. () in
    let var = (Stats.var_es total ~alpha:confidence ~weights).var in
    let move = full.history.(e + 1) in
    let vals = Pricing.values pricer [| move |] ~horizon_days:1 () in
    let pnl =
      Array.fold_left ( +. ) 0. vals.(0) -. Array.fold_left ( +. ) 0. v0
    in
    days :=
      !days @ [ { forecast_date = t; pnl_date = dates.(e + 1); var; pnl } ]
  done;
  {
    confidence;
    method_ =
      (match config.method_ with
      | Config.Historical -> "historical"
      | Config.Montecarlo -> "montecarlo");
    base_currency = inputs.base_currency;
    days = !days;
  }
