(** Independent static-arbitrage validation (plan §5).

    Two layers, never conflated (§5.5):

    - [validate_grid] checks a *sampled* IV grid with Black prices: finite
      inputs, calendar monotonicity of total variance, strike monotonicity of
      calls, butterfly convexity, and call-price bounds. These are
      finite-grid diagnostics — passing them never proves absence of
      arbitrage between nodes or outside the domain.
    - [gj_ok] checks the Gatheral–Jacquier *sufficient* conditions of the
      SSVI model itself over a wide log-theta domain. Failing a sufficient
      condition does not prove a surface is arbitrageable; it only withholds
      the analytical guarantee.

    Tolerances are absolute with documented units: variance units for
    calendar ([tol_w]), price units scaled by discount×forward for
    monotonicity/bounds, and 1/forward units for the second divided
    difference. *)

type status = Pass | Fail | Indeterminate

type check = {
  name : string;
  status : status;
  n_viol : int;
  worst : float;
  where : (float * float) option;  (** (T, k) of the worst violation *)
  note : string;
}

type report = { checks : check list; valid : bool }

let pass name note =
  { name; status = Pass; n_viol = 0; worst = 0.; where = None; note }

let fail1 name n_viol worst where note =
  { name; status = Fail; n_viol; worst; where; note }

let indeterminate name note =
  {
    name;
    status = Indeterminate;
    n_viol = 0;
    worst = Float.nan;
    where = None;
    note;
  }

let tol_w = 1e-9

let is_sorted a =
  let n = Array.length a in
  let rec go i = i + 1 >= n || (a.(i) < a.(i + 1) && go (i + 1)) in
  n < 2 || go 0

(** European call at (S, K, t) with carry b = r − q, vol sigma. *)
let call_price ~spot ~strike ~t ~rate ~div sigma =
  (Options.black_scholes ~s:[| spot |] ~k:strike ~t ~r:[| rate |]
     ~b:[| rate -. div |] ~sigma ~call:true).(0)

let strike_of fwd t ki = fwd t *. exp ki

let validate_grid ~expiries ~k ~iv ~spot ~rate ~div =
  let m = Array.length expiries and n = Array.length k in
  let structural =
    if m = 0 || n < 3 then
      fail1 "structural" 1 Float.nan None "need >=1 expiry and >=3 strikes"
    else if not (is_sorted expiries) then
      fail1 "structural" 1 Float.nan None "expiries not strictly increasing"
    else if not (is_sorted k) then
      fail1 "structural" 1 Float.nan None "moneyness not strictly increasing"
    else if not (Array.for_all (fun t -> t > 0.) expiries) then
      fail1 "structural" 1 Float.nan None "non-positive expiry"
    else if Array.exists (fun row -> Array.length row <> n) iv then
      fail1 "structural" 1 Float.nan None "ragged iv grid"
    else
      let bad = ref 0 and worst = ref 0. and where = ref None in
      Array.iteri
        (fun j row ->
          Array.iteri
            (fun i v ->
              if not (Float.is_finite v && v > 0.) then (
                incr bad;
                worst := Float.max !worst 1.;
                where := Some (expiries.(j), k.(i))))
            row)
        iv;
      if !bad > 0 then
        fail1 "structural" !bad !worst !where "non-finite/non-positive iv"
      else pass "structural" "rectangular, sorted, finite, positive"
  in
  if structural.status <> Pass then
    let rest =
      List.map
        (fun nm -> indeterminate nm "skipped: structural failure")
        [ "calendar"; "monotonicity"; "convexity"; "bounds" ]
    in
    { checks = structural :: rest; valid = false }
  else
    let w j i =
      let t = expiries.(j) in
      t *. iv.(j).(i) *. iv.(j).(i)
    in
    let fwd t = spot *. exp ((rate -. div) *. t) in
    let disc t = exp (-.rate *. t) in
    let cprice j i =
      let t = expiries.(j) in
      call_price ~spot ~strike:(strike_of fwd t k.(i)) ~t ~rate ~div iv.(j).(i)
    in
    (* calendar: w non-decreasing in T at fixed k *)
    let ncal = ref 0 and wcal = ref 0. and whcal = ref None in
    for j = 1 to m - 1 do
      for i = 0 to n - 1 do
        let d = w (j - 1) i -. w j i in
        if d > tol_w then (
          incr ncal;
          if d > !wcal then (
            wcal := d;
            whcal := Some (expiries.(j), k.(i))))
      done
    done;
    (* monotonicity: adjacent call steps non-increasing *)
    let nmon = ref 0 and wmon = ref 0. and whmon = ref None in
    for j = 0 to m - 1 do
      let tol = 1e-9 *. disc expiries.(j) *. fwd expiries.(j) in
      for i = 1 to n - 1 do
        let d = cprice j i -. cprice j (i - 1) in
        if d > tol then (
          incr nmon;
          if d > !wmon then (
            wmon := d;
            whmon := Some (expiries.(j), k.(i))))
      done
    done;
    (* convexity: second divided difference >= -tol *)
    let ncvx = ref 0 and wcvx = ref 0. and whcvx = ref None in
    for j = 0 to m - 1 do
      let kk i = strike_of fwd expiries.(j) k.(i) in
      let tol = 1e-9 *. disc expiries.(j) /. fwd expiries.(j) in
      for i = 1 to n - 2 do
        let h1 = kk i -. kk (i - 1) and h2 = kk (i + 1) -. kk i in
        let d1 = (cprice j i -. cprice j (i - 1)) /. h1
        and d2 = (cprice j (i + 1) -. cprice j i) /. h2 in
        let dd = (d2 -. d1) /. (0.5 *. (h1 +. h2)) in
        if dd < -.tol then (
          incr ncvx;
          if -.dd > !wcvx then (
            wcvx := -.dd;
            whcvx := Some (expiries.(j), k.(i))))
      done
    done;
    (* bounds: max(D(F−K),0) ≤ C ≤ D·F *)
    let nbnd = ref 0 and wbnd = ref 0. and whbnd = ref None in
    for j = 0 to m - 1 do
      let tol = 1e-9 *. disc expiries.(j) *. fwd expiries.(j) in
      for i = 0 to n - 1 do
        let strike = strike_of fwd expiries.(j) k.(i) and c = cprice j i in
        let lo =
          Float.max (disc expiries.(j) *. (fwd expiries.(j) -. strike)) 0.
        and hi = disc expiries.(j) *. fwd expiries.(j) in
        let v = Float.max (lo -. c) (c -. hi) in
        if v > tol then (
          incr nbnd;
          if v > !wbnd then (
            wbnd := v;
            whbnd := Some (expiries.(j), k.(i))))
      done
    done;
    let checks =
      [
        structural;
        (if !ncal = 0 then pass "calendar" "total variance non-decreasing in T"
         else fail1 "calendar" !ncal !wcal !whcal
           "total variance decreases across expiries");
        (if !nmon = 0 then pass "monotonicity" "calls decrease in strike"
         else fail1 "monotonicity" !nmon !wmon !whmon
           "call price increases in strike");
        (if !ncvx = 0 then pass "convexity" "calls convex in strike"
         else fail1 "convexity" !ncvx !wcvx !whcvx
           "negative second divided difference");
        (if !nbnd = 0 then pass "bounds" "DF-bounds hold"
         else fail1 "bounds" !nbnd !wbnd !whbnd "call outside DF-bounds");
      ]
    in
    { checks; valid = List.for_all (fun c -> c.status = Pass) checks }

(** Gatheral–Jacquier sufficient conditions at one knot variance:
    θ·φ·(1+|ρ|) < 4 and θ·φ²·(1+|ρ|) ≤ 4. Returns (holds, worst margin:
    positive = satisfied by this much, negative = violated). *)
let gj_at ~theta ~rho ~eta ~gamma =
  let phi = Ssvi.phi_of ~theta ~eta ~gamma in
  let s = 1. +. abs_float rho in
  let m1 = 4. -. (theta *. phi *. s) and m2 = 4. -. (theta *. phi *. phi *. s) in
  (m1 > 0. && m2 >= 0., Float.min m1 m2)

(** [gj_ok] over the knot thetas plus a log grid 1e-10..1e4 (§6.5): the
    analytical guarantee must hold over the whole theta domain, not just at
    traded maturities. *)
let gj_ok s =
  let thetas =
    let grid =
      Array.init 200 (fun i -> 10. ** (-10. +. (14. *. float_of_int i /. 199.)))
    in
    Array.append s.Ssvi.theta grid
  in
  let bad = ref 0 and worst = ref Float.infinity in
  Array.iter
    (fun th ->
      let holds, margin =
        gj_at ~theta:th ~rho:s.Ssvi.rho ~eta:s.Ssvi.eta ~gamma:s.Ssvi.gamma
      in
      if not holds then incr bad;
      worst := Float.min !worst margin)
    thetas;
  if !bad = 0 then pass "gj-sufficient" "GJ conditions hold on theta domain"
  else
    fail1 "gj-sufficient" !bad (-. !worst) None
      "GJ sufficient condition violated (no proof of arbitrage)"
