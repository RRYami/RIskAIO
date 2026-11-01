(** Engine entry point: [run_engine inputs config]. Port of [engine/run.py].

    Pure computation. Flow per horizon h (ADR-014): 1. 1-day factor shocks
    (historical window or Monte Carlo draws) × √h; 2. chunked full revaluation →
    portfolio P&L per scenario (base currency); 3. VaR/ES per confidence; for
    the component confidence, position P&L is recomputed only on tail + VaR-band
    scenarios and aggregated per dimension (Euler ES, band VaR); 4. once:
    exposures, UCITS absolute VaR (99%/20d as % of model NAV), stress scenarios.

    Divergence: instrument ids are strings (as stored); [should_cancel] is an
    optional [unit -> bool] thunk polled between chunks. *)

open Risk_core

exception Engine_config_error of string
exception Engine_cancelled of string

type engine_inputs = {
  holdings : Holdings.holding_row list;
  instruments : (string * Refdata.instrument_record) list;
  bundle : Marketdata.bundle;
  as_of : Calendar.date;
  base_currency : string;
  row_portfolios : string list option;  (** per holding row, aggregate runs *)
  surf_fits : (string * Ssvi_calibration.fit list) list;
      (** underlying id → dated SSVI fits, ascending; [] = frozen vol *)
}

type result_row = {
  view : string;  (** summary | component | exposures | stress *)
  dimension : string option;
  key : string option;
  measure : string;
  slice : (string * Yojson.Basic.t) list;
  value : float;
  value_currency : string option;
}

type engine_result = {
  rows : result_row list;
  pnl : (string * float array) list;  (** "h1", "h10", ... per-scenario P&L *)
  diagnostics : (string * Yojson.Basic.t) list;
}

let dimensions = [ "position"; "issuer"; "sector"; "country"; "currency" ]

let as_json r =
  `Assoc
    [
      ("view", `String r.view);
      ("dimension", match r.dimension with None -> `Null | Some d -> `String d);
      ("key", match r.key with None -> `Null | Some k -> `String k);
      ("measure", `String r.measure);
      ("slice", `Assoc r.slice);
      ("value", `Float r.value);
      ( "value_currency",
        match r.value_currency with None -> `Null | Some c -> `String c );
    ]

let canonical_rows rows = List.map as_json rows

(* --- scenarios --- *)

let estimate_cov (config : Config.run_config) returns =
  match config.covariance with
  | Config.Ewma -> Stats.ewma_covariance returns config.ewma_lambda
  | Config.Sample -> Stats.sample_covariance returns

(** Replace each currency's 12 tenor factors by top-k principal components. *)
let pca_curves (factors : Market.factor_set) k estimate =
  let ir_cols : (string * int array) list =
    let by_ccy = Hashtbl.create 4 in
    Array.iteri
      (fun j name ->
        if String.length name > 3 && String.sub name 0 3 = "IR:" then
          let ccy = List.nth (String.split_on_char ':' name) 1 in
          Hashtbl.replace by_ccy ccy
            (j :: (Hashtbl.find_opt by_ccy ccy |> Option.value ~default:[])))
      factors.names;
    Hashtbl.fold
      (fun ccy js acc -> (ccy, Array.of_list (List.sort Int.compare js)) :: acc)
      by_ccy []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  if k = 0 || ir_cols = [] then (factors.history, (fun x -> x), [])
  else
    let ir_set = Hashtbl.create 32 in
    List.iter
      (fun (_, cols) -> Array.iter (fun j -> Hashtbl.replace ir_set j ()) cols)
      ir_cols;
    let keep =
      Array.init (Market.size factors) Fun.id
      |> Array.to_list
      |> List.filter (fun j -> not (Hashtbl.mem ir_set j))
      |> Array.of_list
    in
    let n = Array.length factors.history in
    let ecol m j = Array.init (Array.length m) (fun i -> m.(i).(j)) in
    let layout = ref [] in
    let explained = ref [] in
    let parts =
      ref
        [
          Array.init n (fun i ->
              Array.init (Array.length keep) (fun a ->
                  factors.history.(i).(keep.(a))));
        ]
    in
    List.iter
      (fun (ccy, cols) ->
        let sub =
          Array.init n (fun i ->
              Array.init (Array.length cols) (fun a ->
                  factors.history.(i).(cols.(a))))
        in
        let vals, vecs = Stats.eigh_sym (estimate sub) in
        let m = Array.length vals in
        let order =
          Array.init m Fun.id |> Array.to_list
          |> List.sort (fun a b -> Float.compare vals.(b) vals.(a))
        in
        let top = List.filteri (fun i _ -> i < k) order |> Array.of_list in
        (* sign-normalise: largest loading positive *)
        let load = Array.init m (fun _ -> Array.make k 0.) in
        Array.iteri
          (fun c t ->
            let col = ecol vecs t in
            let amax = ref 0. and imax = ref 0 in
            Array.iteri
              (fun i v ->
                if abs_float v > abs_float !amax then (
                  amax := v;
                  imax := i))
              col;
            let s = if !amax >= 0. then 1. else -1. in
            for i = 0 to m - 1 do
              load.(i).(c) <- s *. vecs.(i).(t)
            done;
            ignore imax)
          top;
        let proj = Stats.matmul sub load in
        parts := !parts @ [ proj ];
        layout := !layout @ [ (cols, load) ];
        let total = Array.fold_left ( +. ) 0. vals in
        let kept =
          Array.fold_left ( +. ) 0.
            (Array.map (fun t -> Float.max vals.(t) 0.) top)
        in
        explained :=
          !explained @ [ (ccy, if total > 0. then kept /. total else 1.) ])
      ir_cols;
    let reduced =
      Array.init n (fun i -> Array.concat (List.map (fun p -> p.(i)) !parts))
    in
    let expand draws =
      let nd = Array.length draws in
      let full = Array.make_matrix nd (Market.size factors) 0. in
      let klen = Array.length keep in
      for i = 0 to nd - 1 do
        for a = 0 to klen - 1 do
          full.(i).(keep.(a)) <- draws.(i).(a)
        done;
        let off = ref klen in
        List.iter
          (fun (cols, load) ->
            for a = 0 to Array.length cols - 1 do
              let s = ref 0. in
              for c = 0 to k - 1 do
                s := !s +. (draws.(i).(!off + c) *. load.(a).(c))
              done;
              full.(i).(cols.(a)) <- !s
            done;
            off := !off + k)
          !layout
      done;
      full
    in
    (reduced, expand, !explained)

(* --- vol scenarios (SSVI surface shocks) --- *)

type vol_env = {
  lat : (string * Pca_latent.latent_set) list;
  pc : (string * Pca_latent.pca) list;
}

(** Build the vol environment from dated fits; [None] = frozen vol. Raises
    [Engine_config_error] on missing/short histories — never a silent fallback
    to frozen. *)
let vol_env_of_inputs inputs (config : Config.run_config) =
  match config.vol_model with
  | Config.Frozen -> None
  | Config.Bootstrap | Config.Gaussian -> (
      try
        let lat =
          List.map
            (fun u ->
              match List.assoc_opt u inputs.surf_fits with
              | None ->
                  raise (Engine_config_error ("vol: no surface fits for " ^ u))
              | Some fits ->
                  ( u,
                    Pca_latent.of_fits
                      (List.map (fun f -> Ssvi_calibration.Ok f) fits) ))
            config.vol_universe
        in
        let pc =
          List.map
            (fun (u, l) ->
              if Array.length l.Pca_latent.d_hist < 2 then
                raise
                  (Engine_config_error
                     ("vol: fewer than 2 one-day diffs for " ^ u));
              (u, Pca_latent.pca_fit l.Pca_latent.d_hist config.vol_pcs))
            lat
        in
        Some { lat; pc }
      with Invalid_argument msg -> raise (Engine_config_error ("vol: " ^ msg)))

(** Per-scenario latent states per surfaced underlying (n_scen × p), plus
    fallback count (scenarios reusing the base surface). Historical+Bootstrap
    pairs each history row with the latent diff of its own return date (exact
    dependence, plan §9A). Montecarlo+Gaussian draws jointly with the
    (curve-reduced) market factors over date-aligned rows (plan §9B). *)
let vol_states_hist env ~as_of return_dates n =
  let fallback = ref 0 in
  let states =
    List.map
      (fun (u, (lat : Pca_latent.latent_set)) ->
        let pc = List.assoc u env.pc in
        let base = Pca_latent.base_state lat ~as_of in
        let arr =
          Array.init n (fun i ->
              match
                Pca_latent.diff_of_date lat
                  (Calendar.to_string return_dates.(i))
              with
              | Some d -> Pca_latent.state_of_diff lat pc ~as_of d
              | None ->
                  incr fallback;
                  Array.copy base)
        in
        (u, arr))
      env.lat
  in
  (states, !fallback)

let one_day_shocks (factors : Market.factor_set) (config : Config.run_config)
    ?(vol : vol_env option = None) ?(as_of : string option = None) () =
  let need_as_of () =
    match as_of with
    | Some s -> s
    | None -> invalid_arg "one_day_shocks: vol needs as_of"
  in
  match config.method_ with
  | Config.Historical ->
      let weights =
        match config.weighting with
        | Config.Brw ->
            Some
              (Stats.ewma_weights
                 (Array.length factors.history)
                 config.brw_lambda)
        | Config.Equal -> None
      in
      let states, _fallback, vdiag =
        match vol with
        | None -> ([], 0, [])
        | Some _env -> (
            match config.vol_model with
            | Config.Frozen -> ([], 0, [])
            | Config.Gaussian ->
                raise
                  (Engine_config_error "vol: Gaussian needs method=montecarlo")
            | Config.Bootstrap ->
                let st, fb =
                  vol_states_hist _env ~as_of:(need_as_of ())
                    factors.return_dates
                    (Array.length factors.history)
                in
                (st, fb, [ ("vol_fallback_scenarios", `Int fb) ]))
      in
      ( factors.history,
        weights,
        ("n_scenarios", `Int (Array.length factors.history)) :: vdiag,
        states )
  | Config.Montecarlo -> (
      if Market.size factors = 0 then
        ( Array.make_matrix config.n_scenarios 0 0.,
          None,
          [ ("n_scenarios", `Int config.n_scenarios) ],
          [] )
      else
        let reduced, expand, explained =
          pca_curves factors config.curve_pcs (estimate_cov config)
        in
        let rd = Array.length reduced.(0) in
        let dist =
          match config.distribution with
          | Config.Gaussian -> `Gaussian
          | Config.Student_t -> `Student_t
        in
        let frozen_path () =
          let fac = Stats.psd_cholesky (estimate_cov config reduced) in
          let draws =
            Stats.mc_shocks fac.Stats.chol ~n:config.n_scenarios
              ~seed:config.seed ~distribution:dist ~nu:config.nu
          in
          ( expand draws,
            None,
            [
              ("n_scenarios", `Int config.n_scenarios);
              ("mc_factors", `Int rd);
              ("curve_pcs", `Int config.curve_pcs);
              ( "curve_variance_explained",
                `Assoc (List.map (fun (c, v) -> (c, `Float v)) explained) );
              ("covariance_repaired", `Bool fac.repaired);
              ("covariance_min_eigenvalue", `Float fac.min_eigenvalue);
            ],
            [] )
        in
        match vol with
        | None -> frozen_path ()
        | Some _ when config.vol_model = Config.Frozen -> frozen_path ()
        | Some env -> (
            match config.vol_model with
            | Config.Frozen -> assert false
            | Config.Bootstrap ->
                raise
                  (Engine_config_error "vol: Bootstrap needs method=historical")
            | Config.Gaussian ->
                let as_of = need_as_of () in
                (* date-aligned rows across factors and every surface *)
                let in_all d =
                  List.for_all
                    (fun (_, (lat : Pca_latent.latent_set)) ->
                      Pca_latent.diff_of_date lat d <> None)
                    env.lat
                in
                let aligned =
                  Array.init (Array.length factors.history) Fun.id
                  |> Array.to_list
                  |> List.filter (fun i ->
                      in_all (Calendar.to_string factors.return_dates.(i)))
                in
                if aligned = [] then
                  raise
                    (Engine_config_error
                       "vol: no date overlap between factors and surface fits");
                let per_u =
                  List.map
                    (fun (u, (lat : Pca_latent.latent_set)) ->
                      let pc = List.assoc u env.pc in
                      let scores =
                        List.map
                          (fun i ->
                            let d =
                              Option.get
                                (Pca_latent.diff_of_date lat
                                   (Calendar.to_string factors.return_dates.(i)))
                            in
                            Pca_latent.scores pc d)
                          aligned
                      in
                      (u, lat, pc, scores))
                    env.lat
                in
                let combined =
                  Array.of_list
                    (List.mapi
                       (fun a i ->
                         (* [i] is the factor-history row; [a] its position in
                            the aligned order shared by every score list *)
                         let row = ref (Array.copy reduced.(i)) in
                         List.iter
                           (fun (_, _, _, scores) ->
                             row := Array.append !row (List.nth scores a))
                           per_u;
                         !row)
                       aligned)
                in
                let fac = Stats.psd_cholesky (estimate_cov config combined) in
                let draws =
                  Stats.mc_shocks fac.Stats.chol ~n:config.n_scenarios
                    ~seed:config.seed ~distribution:dist ~nu:config.nu
                in
                let kd = Array.length combined.(0) - rd in
                (* score-column offset per underlying, in env.lat order *)
                let offsets =
                  let off = ref rd in
                  List.map
                    (fun (u, _, (pc : Pca_latent.pca), _) ->
                      let o = !off in
                      off := !off + pc.Pca_latent.k;
                      (u, o))
                    per_u
                in
                let states =
                  List.map
                    (fun ( u,
                           (lat : Pca_latent.latent_set),
                           (pc : Pca_latent.pca),
                           _ ) ->
                      let base = Pca_latent.base_state lat ~as_of in
                      let p = Array.length base in
                      let off = List.assoc u offsets in
                      let arr =
                        Array.init config.n_scenarios (fun r ->
                            Array.init p (fun a ->
                                let s = ref pc.Pca_latent.mean.(a) in
                                for j = 0 to pc.Pca_latent.k - 1 do
                                  s :=
                                    !s
                                    +. pc.Pca_latent.loadings.(a).(j)
                                       *. draws.(r).(off + j)
                                done;
                                base.(a) +. !s))
                      in
                      (u, arr))
                    per_u
                in
                let vdiag =
                  List.map
                    (fun (u, _, (pc : Pca_latent.pca), _) ->
                      ( "vol_" ^ u,
                        `Assoc
                          [
                            ("pcs", `Int pc.Pca_latent.k);
                            ("explained", `Float pc.Pca_latent.explained);
                            ( "residual_share",
                              `Float pc.Pca_latent.residual_share );
                          ] ))
                    per_u
                in
                ( expand (Array.map (fun row -> Array.sub row 0 rd) draws),
                  None,
                  [
                    ("n_scenarios", `Int config.n_scenarios);
                    ("mc_factors", `Int rd);
                    ("joint_factors", `Int (rd + kd));
                    ("n_aligned", `Int (List.length aligned));
                    ("curve_pcs", `Int config.curve_pcs);
                    ( "curve_variance_explained",
                      `Assoc (List.map (fun (c, v) -> (c, `Float v)) explained)
                    );
                    ("covariance_repaired", `Bool fac.repaired);
                    ("covariance_min_eigenvalue", `Float fac.min_eigenvalue);
                  ]
                  @ vdiag,
                  states )))

(** Per-scenario sigma matrix (n_scen × book.options order) from latent states,
    plus the count of options past the last expiry knot (flat theta
    extrapolation — the documented boundary policy, §6.4). Non-surfaced
    underlyings and expired options keep [o.sigma]. *)
let vol_sigmas (pricer : Pricing.pricer) (factors : Market.factor_set)
    (states : (string * float array array) list) (env : vol_env) shocks ~h
    ~scale =
  match pricer.Pricing.book.Pricing.options with
  | [] -> (None, 0)
  | opts ->
      let n = Array.length shocks in
      let n_opt = List.length opts in
      let clamped = ref 0 in
      let mat =
        Array.init n (fun r ->
            Array.init n_opt (fun oi ->
                let o = List.nth opts oi in
                match List.assoc_opt o.Pricing.underlying states with
                | None -> o.Pricing.sigma
                | Some u_arr -> (
                    let t_rem =
                      float_of_int (Int.max (o.Pricing.days_to_expiry - h) 0)
                      /. 252.
                    in
                    if t_rem <= 0. then o.Pricing.sigma
                    else
                      let lat = List.assoc o.Pricing.underlying env.lat in
                      let t_use =
                        if
                          t_rem
                          > lat.Pca_latent.expiries.(Array.length
                                                       lat.Pca_latent.expiries
                                                     - 1)
                        then (
                          incr clamped;
                          lat.Pca_latent.expiries.(Array.length
                                                     lat.Pca_latent.expiries
                                                   - 1))
                        else t_rem
                      in
                      let surf =
                        Ssvi.decode lat.Pca_latent.expiries u_arr.(r)
                      in
                      let s0 =
                        List.assoc o.Pricing.underlying
                          pricer.Pricing.state.Market.prices
                      in
                      let eq_j =
                        Market.eq pricer.Pricing.factors o.Pricing.underlying
                      in
                      let s = s0 *. exp (shocks.(r).(eq_j) *. scale) in
                      let grid =
                        List.assoc o.Pricing.currency
                          pricer.Pricing.state.Market.curves
                      in
                      let cols = Market.ir factors o.Pricing.currency in
                      let shocked =
                        Array.init (Array.length grid) (fun tt ->
                            grid.(tt) +. (shocks.(r).(cols.(tt)) *. scale))
                      in
                      let w =
                        (Market.interp_weights [| Float.max t_rem 1e-9 |]).(0)
                      in
                      let rr = ref 0. in
                      for tt = 0 to Array.length shocked - 1 do
                        rr := !rr +. (shocked.(tt) *. w.(tt))
                      done;
                      let fwd =
                        s *. exp ((!rr -. o.Pricing.dividend_yield) *. t_rem)
                      in
                      let k = Float.log (o.Pricing.strike /. fwd) in
                      try Ssvi.iv surf ~k ~t:t_use
                      with Ssvi.Ssvi_error _ -> o.Pricing.sigma)))
      in
      (Some mat, !clamped)

(** Domain count for chunk parallelism. [RISK_AI_DOMAINS] overrides (0/1 =
    sequential); default is the machine's recommendation. *)
let n_domains () =
  match Sys.getenv_opt "RISK_AI_DOMAINS" with
  | Some s -> ( try Int.max 1 (int_of_string (String.trim s)) with _ -> 1)
  | None -> ( try Domain.recommended_domain_count () with _ -> 1)

(** Portfolio P&L per scenario for [shocks × scale] (√h applied per chunk so a
    scaled copy of the full shock matrix never exists — same values). Chunks
    write disjoint slices of [total], so multi-domain execution is bit-identical
    to sequential. The bond-roll cache is warmed up front: [Hashtbl] mutation is
    not domain-safe. *)
let total_pnl pricer v0 shocks h chunk should_cancel scale ?(vol_sigma = None)
    () =
  let n = Array.length shocks in
  let v0_sum = Array.fold_left ( +. ) 0. v0 in
  let total = Array.make n 0. in
  ignore (Pricing.roll pricer h);
  let process lo hi =
    (match should_cancel with
    | None -> ()
    | Some f -> if f () then raise (Engine_cancelled "run cancelled"));
    let out = Pricing.ws_out pricer (hi - lo) in
    (* vol_sigma is absolute-indexed; slice to the chunk window *)
    let vs =
      Option.map (fun m -> Array.init (hi - lo) (fun r -> m.(lo + r))) vol_sigma
    in
    Pricing.values_into pricer shocks ~lo ~hi ~horizon_days:h ~vol_shift:[]
      ~scale ~vol_sigma:vs ~out ();
    for r = 0 to hi - lo - 1 do
      total.(lo + r) <- Array.fold_left ( +. ) 0. out.(r) -. v0_sum
    done
  in
  let bounds =
    let rec go i acc =
      if i >= n then List.rev acc
      else go (i + chunk) ((i, Int.min (i + chunk) n) :: acc)
    in
    go 0 []
  in
  let domains = n_domains () in
  (if domains <= 1 || List.length bounds <= 1 || n < 4000 then
     List.iter (fun (lo, hi) -> process lo hi) bounds
   else
     let groups = Array.init domains (fun _ -> []) in
     List.iteri
       (fun i b -> groups.(i mod domains) <- b :: groups.(i mod domains))
       bounds;
     let groups =
       Array.to_list (Array.map List.rev groups)
       |> List.filter (fun g -> g <> [])
     in
     let workers =
       List.map
         (fun g ->
           Domain.spawn (fun () -> List.iter (fun (lo, hi) -> process lo hi) g))
         (List.tl groups)
     in
     List.iter (fun (lo, hi) -> process lo hi) (List.hd groups);
     List.iter Domain.join workers);
  total

(* --- components --- *)

let dimension_key (p : Pricing.position_info) dim =
  match dim with
  | "portfolio" -> p.portfolio
  | "position" -> p.instrument_id
  | "issuer" -> p.issuer
  | "sector" -> p.sector
  | "country" -> p.country
  | "currency" -> p.currency
  | d -> invalid_arg ("unknown component dimension: " ^ d)

let components pricer (book : Pricing.book) v0 one_day scale total h alpha
    weights slice_ dimensions ?(vol_sigma_full = None) () =
  let tail = Stats.var_es total ~alpha ~weights in
  let band = Stats.var_band tail (Array.length total) in
  let needed =
    Array.append tail.Stats.tail_index band
    |> Array.to_list |> List.sort_uniq Int.compare |> Array.of_list
  in
  let at = Hashtbl.create (Array.length needed) in
  Array.iteri (fun r s -> Hashtbl.replace at s r) needed;
  let sub =
    Array.map (fun s -> Array.map (fun x -> x *. scale) one_day.(s)) needed
  in
  let m = Array.length needed in
  let out = Pricing.ws_out pricer m in
  let vol_sub =
    Option.map
      (fun mat -> Array.init m (fun r -> mat.(needed.(r))))
      vol_sigma_full
  in
  Pricing.values_into pricer sub ~lo:0 ~hi:m ~horizon_days:h ~vol_shift:[]
    ~scale:1.0 ~vol_sigma:vol_sub ~out ();
  let v0_arr = v0 in
  let pos_pnl =
    Array.init m (fun r -> Array.mapi (fun j v -> v -. v0_arr.(j)) out.(r))
  in
  let pick idxs = Array.map (fun s -> pos_pnl.(Hashtbl.find at s)) idxs in
  let tail_rows = pick tail.Stats.tail_index in
  let band_rows = pick band in
  let base = book.base_currency in
  List.concat_map
    (fun dim ->
      let keys =
        List.map
          (fun (p : Pricing.position_info) -> dimension_key p dim)
          book.positions
        |> List.sort_uniq String.compare
      in
      let col =
        List.mapi (fun j k -> (k, j)) keys |> List.to_seq |> Hashtbl.of_seq
        |> fun h k -> Hashtbl.find h k
      in
      let group =
        Array.init (List.length book.positions) (fun _ ->
            Array.make (List.length keys) 0.)
      in
      List.iter
        (fun (p : Pricing.position_info) ->
          group.(p.index).(col (dimension_key p dim)) <- 1.)
        book.positions;
      let ces = Stats.component_es (Stats.matmul tail_rows group) tail in
      let cvar = Stats.component_var (Stats.matmul band_rows group) tail in
      List.concat_map
        (fun k ->
          let j = col k in
          [
            {
              view = "component";
              dimension = Some dim;
              key = Some k;
              measure = "es";
              slice = slice_;
              value = ces.(j);
              value_currency = Some base;
            };
            {
              view = "component";
              dimension = Some dim;
              key = Some k;
              measure = "var";
              slice = slice_;
              value = cvar.(j);
              value_currency = Some base;
            };
          ])
        keys)
    dimensions

(* --- stress --- *)

let in_scope (shock : Config.shock) ~currency ~sector ~instrument_id =
  match shock.scope with
  | Config.All -> true
  | Config.Currency -> currency = Option.value shock.target ~default:""
  | Config.Sector -> sector = Option.value shock.target ~default:""
  | Config.Instrument -> instrument_id = Option.value shock.target ~default:""

let term_str terms k =
  match List.assoc_opt k terms with Some (`String s) -> s | _ -> ""

let hypothetical (scenario : Config.stress_scenario)
    (factors : Market.factor_set) instruments (book : Pricing.book) =
  let n = Market.size factors in
  let vec = Array.make n 0. in
  let vol_shift = Hashtbl.create 4 in
  let bump_vol u v =
    Hashtbl.replace vol_shift u
      (v +. (Hashtbl.find_opt vol_shift u |> Option.value ~default:0.))
  in
  List.iter
    (fun (shock : Config.shock) ->
      Array.iteri
        (fun j name ->
          let kind, rest =
            match String.split_on_char ':' name with
            | k :: r -> (k, String.concat ":" r)
            | [] -> ("", "")
          in
          match kind with
          | "EQ" when shock.kind = Config.Equity -> (
              match List.assoc_opt rest instruments with
              | None ->
                  raise
                    (Engine_config_error ("stress: unknown instrument " ^ rest))
              | Some (inst : Refdata.instrument_record) ->
                  if
                    in_scope shock ~currency:inst.currency
                      ~sector:(term_str inst.terms "sector")
                      ~instrument_id:rest
                  then vec.(j) <- vec.(j) +. Float.log1p shock.size)
          | "FX" when shock.kind = Config.Fx ->
              if
                shock.scope = Config.All
                || shock.scope = Config.Currency
                   && rest = Option.value shock.target ~default:""
              then vec.(j) <- vec.(j) +. Float.log1p shock.size
          | "IR" when shock.kind = Config.Rates ->
              let ccy = List.hd (String.split_on_char ':' rest) in
              if
                shock.scope = Config.All
                || shock.scope = Config.Currency
                   && ccy = Option.value shock.target ~default:""
              then vec.(j) <- vec.(j) +. (shock.size /. 10_000.)
          | _ -> ())
        factors.names;
      if shock.kind = Config.Vol then
        List.iter
          (fun (o : Pricing.option_spec) ->
            match List.assoc_opt o.underlying instruments with
            | None -> ()
            | Some (inst : Refdata.instrument_record) ->
                if
                  in_scope shock ~currency:inst.currency
                    ~sector:(term_str inst.terms "sector")
                    ~instrument_id:o.underlying
                then bump_vol o.underlying (shock.size /. 100.))
          book.options)
    scenario.shocks;
  (vec, Hashtbl.fold (fun k v acc -> (k, v) :: acc) vol_shift [])

let stress_rows pricer (book : Pricing.book) v0 nav
    (factors : Market.factor_set) instruments (config : Config.run_config) =
  let base = book.base_currency in
  let v0_sum = Array.fold_left ( +. ) 0. v0 in
  let rows = ref [] in
  let emit key pnl meta =
    let pct = if nav = 0. then Float.nan else pnl /. nav in
    rows :=
      {
        view = "stress";
        dimension = None;
        key = Some key;
        measure = "pnl_pct_nav";
        slice = meta;
        value = pct;
        value_currency = None;
      }
      :: {
           view = "stress";
           dimension = None;
           key = Some key;
           measure = "pnl";
           slice = meta;
           value = pnl;
           value_currency = Some base;
         }
      :: !rows
  in
  List.iter
    (fun (sc : Config.stress_scenario) ->
      let vec, vol_shift = hypothetical sc factors instruments book in
      let vals =
        Pricing.values pricer [| vec |] ~horizon_days:0 ~vol_shift ()
      in
      let pnl = Array.fold_left ( +. ) 0. vals.(0) -. v0_sum in
      emit sc.id pnl
        [ ("kind", `String "hypothetical"); ("label", `String sc.label) ])
    config.stress;
  let dates = factors.return_dates in
  List.iter
    (fun (w : Config.historical_window) ->
      let mask =
        Array.init (Array.length dates) (fun i ->
            Calendar.compare w.start dates.(i) < 0
            && Calendar.compare dates.(i) w.end_ <= 0)
      in
      if not (Array.exists Fun.id mask) then
        raise
          (Engine_config_error
             ("historical window " ^ w.id ^ " is outside the snapshot history"));
      let vec = Array.make (Market.size factors) 0. in
      Array.iteri
        (fun i take ->
          if take then
            Array.iteri (fun j v -> vec.(j) <- vec.(j) +. v) factors.history.(i))
        mask;
      let vals = Pricing.values pricer [| vec |] ~horizon_days:0 () in
      let pnl = Array.fold_left ( +. ) 0. vals.(0) -. v0_sum in
      emit w.id pnl
        [
          ("kind", `String "historical");
          ("start", `String (Calendar.to_string w.start));
          ("end", `String (Calendar.to_string w.end_));
        ])
    config.historical_windows;
  let d = config.worst_window_days in
  if d > 0 && Array.length factors.history >= d then (
    let n = Array.length factors.history and k = Market.size factors in
    let csum = Array.make_matrix (n + 1) k 0. in
    for i = 0 to n - 1 do
      for j = 0 to k - 1 do
        csum.(i + 1).(j) <- csum.(i).(j) +. factors.history.(i).(j)
      done
    done;
    let windows =
      Array.init
        (n - d + 1)
        (fun i -> Array.init k (fun j -> csum.(i + d).(j) -. csum.(i).(j)))
    in
    let pnl_w = total_pnl pricer v0 windows 0 config.chunk_size None 1.0 () in
    let worst = ref 0 in
    for i = 1 to Array.length pnl_w - 1 do
      if pnl_w.(i) < pnl_w.(!worst) then worst := i
    done;
    emit
      ("WORST_" ^ string_of_int d ^ "D")
      pnl_w.(!worst)
      [
        ("kind", `String "historical_worst");
        ("days", `Int d);
        ("start", `String (Calendar.to_string dates.(!worst)));
        ("end", `String (Calendar.to_string dates.(!worst + d - 1)));
      ]);
  List.rev !rows

(* --- exposures --- *)

let exposure_rows pricer (book : Pricing.book) =
  let e = Pricing.exposures pricer in
  let base = book.base_currency in
  (* cons-accumulate + one rev at the end: (@) per row is quadratic. *)
  let rows = ref [] in
  let push r = rows := r :: !rows in
  List.iter
    (fun (p : Pricing.position_info) ->
      let key = p.instrument_id and i = p.index in
      let meta =
        [ ("type", `String (Refdata.string_of_inst_type p.inst_type)) ]
      in
      push
        {
          view = "exposures";
          dimension = Some "position";
          key = Some key;
          measure = "market_value";
          slice = meta;
          value = e.market_value.(i);
          value_currency = Some base;
        };
      push
        {
          view = "exposures";
          dimension = Some "position";
          key = Some key;
          measure = "exposure";
          slice = meta;
          value = e.exposure.(i);
          value_currency = Some base;
        };
      if p.inst_type = Refdata.Listed_option then
        push
          {
            view = "exposures";
            dimension = Some "position";
            key = Some key;
            measure = "delta_equivalent";
            slice = meta;
            value = e.delta_equivalent.(i);
            value_currency = Some base;
          };
      if e.dv01.(i) <> 0. then (
        let mv = e.market_value.(i) in
        push
          {
            view = "exposures";
            dimension = Some "position";
            key = Some key;
            measure = "dv01";
            slice = meta;
            value = e.dv01.(i);
            value_currency = Some base;
          };
        push
          {
            view = "exposures";
            dimension = Some "position";
            key = Some key;
            measure = "mod_duration";
            slice = meta;
            value = (if mv = 0. then 0. else e.dv01.(i) *. 10_000. /. mv);
            value_currency = None;
          }))
    book.positions;
  List.iter
    (fun (ccy, v) ->
      push
        {
          view = "exposures";
          dimension = Some "currency";
          key = Some ccy;
          measure = "fx_exposure";
          slice = [];
          value = v;
          value_currency = Some base;
        })
    (List.sort (fun (a, _) (b, _) -> String.compare a b) e.fx_exposure);
  let tot k v =
    push
      {
        view = "exposures";
        dimension = Some "total";
        key = None;
        measure = k;
        slice = [];
        value = v;
        value_currency = Some base;
      }
  in
  tot "gross" (Array.fold_left (fun a v -> a +. abs_float v) 0. e.exposure);
  tot "net" (Array.fold_left ( +. ) 0. e.exposure);
  tot "dv01" (Array.fold_left ( +. ) 0. e.dv01);
  List.rev !rows

(* --- entry point --- *)

let run_engine (inputs : engine_inputs) (config : Config.run_config)
    ?(should_cancel = None) () =
  let state = Market.market_state inputs.bundle inputs.as_of in
  let rp = inputs.row_portfolios in
  let book =
    Pricing.build_book inputs.holdings inputs.instruments state
      inputs.base_currency rp
  in
  let dims =
    (match rp with None -> [] | Some _ -> [ "portfolio" ]) @ dimensions
  in
  let factors =
    Market.build_factors inputs.bundle ~equity_ids:(Pricing.equity_ids book)
      ~fx_ccys:(Pricing.currencies book)
      ~curve_ccys:(Pricing.curve_currencies book)
      ~history_days:(Some config.history_days)
  in
  let pricer = Pricing.make_pricer book state factors in
  let v0 = Pricing.base_values pricer in
  let nav = Array.fold_left ( +. ) 0. v0 in
  let base = inputs.base_currency in
  let as_of_s = Calendar.to_string inputs.as_of in
  let venv = vol_env_of_inputs inputs config in
  let one_day, weights, diag, states =
    one_day_shocks factors config ~vol:venv ~as_of:(Some as_of_s) ()
  in
  let n_clamped = ref 0 in
  (* batches accumulated with cons + one concat at the end:
     (@) per batch is quadratic in the row count. *)
  let batches =
    ref
      [
        [
          {
            view = "summary";
            dimension = None;
            key = None;
            measure = "nav";
            slice = [];
            value = nav;
            value_currency = Some base;
          };
        ];
      ]
  in
  let pnl = ref [] in
  List.iter
    (fun h ->
      let scale = sqrt (float_of_int h) in
      let vsigma =
        match venv with
        | None -> None
        | Some env ->
            if states = [] then None
            else
              let m, nc =
                vol_sigmas pricer factors states env one_day ~h ~scale
              in
              n_clamped := !n_clamped + nc;
              m
      in
      let total =
        total_pnl pricer v0 one_day h config.chunk_size should_cancel scale
          ~vol_sigma:vsigma ()
      in
      pnl := ("h" ^ string_of_int h, total) :: !pnl;
      List.iter
        (fun alpha ->
          let tail = Stats.var_es total ~alpha ~weights in
          let meta =
            [
              ( "method",
                `String
                  (match config.method_ with
                  | Config.Historical -> "historical"
                  | Config.Montecarlo -> "montecarlo") );
              ("confidence", `Float alpha);
              ("horizon_days", `Int h);
            ]
          in
          batches :=
            [
              {
                view = "summary";
                dimension = None;
                key = None;
                measure = "var";
                slice = meta;
                value = tail.var;
                value_currency = Some base;
              };
              {
                view = "summary";
                dimension = None;
                key = None;
                measure = "es";
                slice = meta;
                value = tail.es;
                value_currency = Some base;
              };
            ]
            :: !batches;
          if alpha = 0.99 && h = 20 then
            let ucits = if nav = 0. then Float.nan else tail.var /. nav in
            batches :=
              [
                {
                  view = "summary";
                  dimension = None;
                  key = None;
                  measure = "ucits_var_pct_nav";
                  slice =
                    meta
                    @ [
                        ("limit", `Float config.ucits_limit);
                        ("breach", `Bool (ucits > config.ucits_limit));
                      ];
                  value = ucits;
                  value_currency = None;
                };
              ]
              :: !batches)
        (List.sort_uniq Float.compare config.confidences);
      let cmeta =
        [
          ( "method",
            `String
              (match config.method_ with
              | Config.Historical -> "historical"
              | Config.Montecarlo -> "montecarlo") );
          ("confidence", `Float config.component_confidence);
          ("horizon_days", `Int h);
        ]
      in
      batches :=
        components pricer book v0 one_day scale total h
          config.component_confidence weights cmeta dims ~vol_sigma_full:vsigma
          ()
        :: !batches)
    (List.sort_uniq Int.compare config.horizons);
  batches := exposure_rows pricer book :: !batches;
  batches :=
    stress_rows pricer book v0 nav factors inputs.instruments config :: !batches;
  let vdiag =
    ("vol_model", `String (Config.vol_model_to_string config.vol_model))
    :: ("vol_pcs", `Int config.vol_pcs)
    :: ("vol_clamped_options", `Int !n_clamped)
    ::
    (match venv with
    | None -> []
    | Some env ->
        ( "vol_universe",
          `List (List.map (fun s -> `String s) config.vol_universe) )
        :: List.map
             (fun (u, (pc : Pca_latent.pca)) ->
               ( "vol_" ^ u,
                 `Assoc
                   [
                     ("pcs", `Int pc.Pca_latent.k);
                     ("explained", `Float pc.Pca_latent.explained);
                     ("residual_share", `Float pc.Pca_latent.residual_share);
                   ] ))
             env.pc
        @ List.map
            (fun (u, (lat : Pca_latent.latent_set)) ->
              ("vol_n_diffs_" ^ u, `Int (Array.length lat.Pca_latent.d_hist)))
            env.lat)
  in
  let diag =
    diag @ vdiag
    @ [
        ( "method",
          `String
            (match config.method_ with
            | Config.Historical -> "historical"
            | Config.Montecarlo -> "montecarlo") );
        ("n_positions", `Int (List.length book.positions));
        ("n_factors", `Int (Market.size factors));
        ( "history_start",
          match factors.return_dates with
          | [||] -> `Null
          | a -> `String (Calendar.to_string a.(0)) );
        ( "history_end",
          match factors.return_dates with
          | [||] -> `Null
          | a -> `String (Calendar.to_string a.(Array.length a - 1)) );
      ]
  in
  {
    rows = List.concat (List.rev !batches);
    pnl = List.rev !pnl;
    diagnostics = diag;
  }
