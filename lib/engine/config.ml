(** Run configuration + stress scenario definitions. Port of
    [RiskStartUp/src/riskplatform/engine/config.py] (ADR-007, decisions-log
    2026-09-24).

    Everything that changes results lives here and serialises canonically
    ([to_yojson]), so a manifest can hash it ([config_hash]).

    Engine-first note: [historical_window.start]/[end_] are
    {!Risk_core.Calendar.date} values (the Python form uses [datetime.date]);
    JSON keeps ISO strings. *)

type shock_kind = Equity | Fx | Rates | Vol
type scope = All | Currency | Sector | Instrument

type shock = {
  kind : shock_kind;
  size : float;
  scope : scope;
  target : string option;
}

type stress_scenario = { id : string; label : string; shocks : shock list }

(** Starter library of 4 scenarios (decisions-log 2026-09-24). *)
let starter_library =
  [
    {
      id = "EQ_DOWN_20";
      label = "Equities −20%";
      shocks = [ { kind = Equity; size = -0.20; scope = All; target = None } ];
    };
    {
      id = "RATES_UP_100";
      label = "Rates +100bp parallel";
      shocks = [ { kind = Rates; size = 100.0; scope = All; target = None } ];
    };
    {
      id = "EUR_UP_10";
      label = "EUR +10% vs all";
      shocks =
        [ { kind = Fx; size = (1. /. 1.1) -. 1.; scope = All; target = None } ];
    };
    {
      id = "RISK_OFF";
      label =
        "Risk-off: equities −15%, rates −50bp, vol +10pts, safe havens +5% vs \
         EUR";
      shocks =
        [
          { kind = Equity; size = -0.15; scope = All; target = None };
          { kind = Rates; size = -50.0; scope = All; target = None };
          { kind = Vol; size = 10.0; scope = All; target = None };
          { kind = Fx; size = 0.05; scope = Currency; target = Some "USD" };
          { kind = Fx; size = 0.05; scope = Currency; target = Some "CHF" };
          { kind = Fx; size = 0.05; scope = Currency; target = Some "JPY" };
        ];
    };
  ]

type historical_window = {
  id : string;
  start : Risk_core.Calendar.date;
  end_ : Risk_core.Calendar.date;
}

type backtest_config = { days : int; confidence : float }
type method_ = Historical | Montecarlo
type weighting = Equal | Brw
type covariance = Ewma | Sample
type distribution = Gaussian | Student_t

(** Implied-vol scenario model (SSVI surface shocks).
    - [Frozen]: per-option [sigma] stays at the as-of ATM value (status quo).
    - [Bootstrap]: historical latent-surface diffs replayed on their own
      dates (exact equity/vol dependence, requires [surf_fits]).
    - [Gaussian]: joint Gaussian over market factors and retained latent
      PCs, Monte Carlo only (requires [surf_fits]). *)
type vol_model = Frozen | Bootstrap | Gaussian

type run_config = {
  method_ : method_;
  confidences : float list;
  horizons : int list;
  history_days : int;
  weighting : weighting;
  brw_lambda : float;
  covariance : covariance;
  ewma_lambda : float;
  curve_pcs : int;
  distribution : distribution;
  nu : float;
  n_scenarios : int;
  seed : int;
  component_confidence : float;
  ucits_limit : float;
  stress : stress_scenario list;
  historical_windows : historical_window list;
  worst_window_days : int;
  chunk_size : int;
  backtest : backtest_config option;
  vol_model : vol_model;
  vol_pcs : int;
  vol_universe : string list;  (** underlying ids with SSVI surfaces *)
}

let default =
  {
    method_ = Historical;
    confidences = [ 0.95; 0.975; 0.99 ];
    horizons = [ 1; 10; 20 ];
    history_days = 500;
    weighting = Equal;
    brw_lambda = 0.99;
    covariance = Ewma;
    ewma_lambda = 0.97;
    curve_pcs = 3;
    distribution = Gaussian;
    nu = 5.0;
    n_scenarios = 10_000;
    seed = 0;
    component_confidence = 0.99;
    ucits_limit = 0.20;
    stress = starter_library;
    historical_windows = [];
    worst_window_days = 20;
    chunk_size = 2_000;
    backtest = None;
    vol_model = Frozen;
    vol_pcs = 0;
    vol_universe = [];
  }

let kind c = match c.backtest with None -> "risk" | Some _ -> "backtest"

let validate_backtest b =
  if b.days < 2 then invalid_arg "backtest days must be >= 2";
  if not (0.5 < b.confidence && b.confidence < 1.0) then
    invalid_arg "backtest confidence must be in (0.5, 1)"

let validate c =
  (match c.backtest with None -> () | Some b -> validate_backtest b);
  if c.history_days < 250 then invalid_arg "history_days must be >= 250 (Q12)";
  if
    c.confidences = []
    || List.exists (fun x -> not (0.5 < x && x < 1.0)) c.confidences
  then invalid_arg "confidences must be in (0.5, 1)";
  if c.horizons = [] || List.exists (fun h -> h < 1) c.horizons then
    invalid_arg "horizons must be >= 1 day";
  if c.curve_pcs < 0 || c.curve_pcs > 12 then
    invalid_arg "curve_pcs must be in 0..12";
  if c.method_ = Montecarlo && c.n_scenarios < 100 then
    invalid_arg "n_scenarios must be >= 100";
  if c.vol_pcs < 0 then invalid_arg "vol_pcs must be >= 0";
  (match c.vol_model with
  | Frozen -> ()
  | Bootstrap | Gaussian ->
      if c.vol_universe = [] then
        invalid_arg "vol_model needs a non-empty vol_universe");
  (match c.vol_model with
  | Gaussian ->
      if c.method_ <> Montecarlo then
        invalid_arg "Gaussian vol_model needs method=montecarlo"
  | _ -> ())

(* --- JSON (mirrors RunConfig.as_json / from_json) --- *)

let shock_kind_to_string = function
  | Equity -> "equity"
  | Fx -> "fx"
  | Rates -> "rates"
  | Vol -> "vol"

let shock_kind_of_string = function
  | "equity" -> Equity
  | "fx" -> Fx
  | "rates" -> Rates
  | "vol" -> Vol
  | s -> invalid_arg ("unknown shock kind: " ^ s)

let scope_to_string = function
  | All -> "all"
  | Currency -> "currency"
  | Sector -> "sector"
  | Instrument -> "instrument"

let scope_of_string = function
  | "all" -> All
  | "currency" -> Currency
  | "sector" -> Sector
  | "instrument" -> Instrument
  | s -> invalid_arg ("unknown scope: " ^ s)

let vol_model_to_string = function
  | Frozen -> "frozen"
  | Bootstrap -> "bootstrap"
  | Gaussian -> "gaussian"

let vol_model_of_string = function
  | "frozen" -> Frozen
  | "bootstrap" -> Bootstrap
  | "gaussian" -> Gaussian
  | s -> invalid_arg ("unknown vol_model: " ^ s)

open Yojson.Basic.Util

let shock_to_yojson s =
  `Assoc
    [
      ("kind", `String (shock_kind_to_string s.kind));
      ("size", `Float s.size);
      ("scope", `String (scope_to_string s.scope));
      ("target", match s.target with None -> `Null | Some t -> `String t);
    ]

let shock_of_yojson j =
  {
    kind = j |> member "kind" |> to_string |> shock_kind_of_string;
    size = j |> member "size" |> to_number;
    scope =
      (match j |> member "scope" with
      | `Null -> All
      | s -> s |> to_string |> scope_of_string);
    target =
      (match j |> member "target" with
      | `Null -> None
      | s -> Some (to_string s));
  }

let scenario_to_yojson (s : stress_scenario) =
  `Assoc
    [
      ("id", `String s.id);
      ("label", `String s.label);
      ("shocks", `List (List.map shock_to_yojson s.shocks));
    ]

let scenario_of_yojson j =
  {
    id = j |> member "id" |> to_string;
    label = j |> member "label" |> to_string;
    shocks = j |> member "shocks" |> to_list |> List.map shock_of_yojson;
  }

let window_to_yojson (w : historical_window) =
  `Assoc
    [
      ("id", `String w.id);
      ("start", `String (Risk_core.Calendar.to_string w.start));
      ("end", `String (Risk_core.Calendar.to_string w.end_));
    ]

let window_of_yojson j =
  {
    id = j |> member "id" |> to_string;
    start = j |> member "start" |> to_string |> Risk_core.Calendar.of_string;
    end_ = j |> member "end" |> to_string |> Risk_core.Calendar.of_string;
  }

let to_yojson c =
  let backtest =
    match c.backtest with
    | None -> []
    | Some b ->
        [
          ( "backtest",
            `Assoc
              [ ("days", `Int b.days); ("confidence", `Float b.confidence) ] );
        ]
  in
  `Assoc
    ([
       ( "method",
         `String
           (match c.method_ with
           | Historical -> "historical"
           | Montecarlo -> "montecarlo") );
       ("confidences", `List (List.map (fun x -> `Float x) c.confidences));
       ("horizons", `List (List.map (fun h -> `Int h) c.horizons));
       ("history_days", `Int c.history_days);
       ( "weighting",
         `String (match c.weighting with Equal -> "equal" | Brw -> "brw") );
       ("brw_lambda", `Float c.brw_lambda);
       ( "covariance",
         `String (match c.covariance with Ewma -> "ewma" | Sample -> "sample")
       );
       ("ewma_lambda", `Float c.ewma_lambda);
       ("curve_pcs", `Int c.curve_pcs);
       ( "distribution",
         `String
           (match c.distribution with
           | Gaussian -> "gaussian"
           | Student_t -> "student_t") );
       ("nu", `Float c.nu);
       ("n_scenarios", `Int c.n_scenarios);
       ("seed", `Int c.seed);
       ("component_confidence", `Float c.component_confidence);
       ("ucits_limit", `Float c.ucits_limit);
       ("stress", `List (List.map scenario_to_yojson c.stress));
       ( "historical_windows",
         `List (List.map window_to_yojson c.historical_windows) );
       ("worst_window_days", `Int c.worst_window_days);
       ("chunk_size", `Int c.chunk_size);
       ("vol_model", `String (vol_model_to_string c.vol_model));
       ("vol_pcs", `Int c.vol_pcs);
       ("vol_universe", `List (List.map (fun s -> `String s) c.vol_universe));
     ]
    @ backtest)

let of_yojson j =
  let method_ =
    match j |> member "method" |> to_string with
    | "historical" -> Historical
    | "montecarlo" -> Montecarlo
    | s -> invalid_arg ("unknown method: " ^ s)
  in
  let weighting =
    match j |> member "weighting" |> to_string with
    | "equal" -> Equal
    | "brw" -> Brw
    | s -> invalid_arg ("unknown weighting: " ^ s)
  in
  let covariance =
    match j |> member "covariance" |> to_string with
    | "ewma" -> Ewma
    | "sample" -> Sample
    | s -> invalid_arg ("unknown covariance: " ^ s)
  in
  let distribution : distribution =
    match j |> member "distribution" |> to_string with
    | "gaussian" -> Gaussian
    | "student_t" -> Student_t
    | s -> invalid_arg ("unknown distribution: " ^ s)
  in
  let backtest =
    match j |> member "backtest" with
    | `Null -> None
    | b ->
        Some
          {
            days = b |> member "days" |> to_int;
            confidence = b |> member "confidence" |> to_number;
          }
  in
  {
    method_;
    confidences = j |> member "confidences" |> to_list |> List.map to_number;
    horizons = j |> member "horizons" |> to_list |> List.map to_int;
    history_days = j |> member "history_days" |> to_int;
    weighting;
    brw_lambda = j |> member "brw_lambda" |> to_number;
    covariance;
    ewma_lambda = j |> member "ewma_lambda" |> to_number;
    curve_pcs = j |> member "curve_pcs" |> to_int;
    distribution;
    nu = j |> member "nu" |> to_number;
    n_scenarios = j |> member "n_scenarios" |> to_int;
    seed = j |> member "seed" |> to_int;
    component_confidence = j |> member "component_confidence" |> to_number;
    ucits_limit = j |> member "ucits_limit" |> to_number;
    stress = j |> member "stress" |> to_list |> List.map scenario_of_yojson;
    historical_windows =
      j |> member "historical_windows" |> to_list |> List.map window_of_yojson;
    worst_window_days = j |> member "worst_window_days" |> to_int;
    chunk_size = j |> member "chunk_size" |> to_int;
    backtest;
    vol_model =
      (match j |> member "vol_model" with
      | `Null -> Frozen
      | s -> s |> to_string |> vol_model_of_string);
    vol_pcs =
      (match j |> member "vol_pcs" with
      | `Null -> 0
      | v -> to_int v);
    vol_universe =
      (match j |> member "vol_universe" with
      | `Null -> []
      | v -> v |> to_list |> List.map to_string);
  }
