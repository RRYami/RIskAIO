(** Constrained SSVI volatility surfaces (plan §6).

    Gatheral–Jacquier Surface SVI, restricted power-law smile (§6.2):
    [w(k,θ) = θ/2 · (1 + ρ·φ·k + √((φ·k+ρ)² + 1−ρ²))],
    [φ(θ) = η / (θ^γ · (1+θ)^(1−γ))], with [σ(k,T) = √(w/T)].
    Here [θ] is ATM *total variance* (so [w(0,θ) = θ] exactly), [k] is
    log-forward moneyness [log(K/F)].

    Admissibility is by construction (§6.3–6.4): simulation works in the
    unconstrained latent vector
    [a_0..a_{m-1}; u_ρ; u_η; u_γ] (m = number of expiry knots) and [decode]
    maps it through [tanh]/[sigmoid]/[softplus] into
    [-1 < ρ < 1], [0 < γ < 0.5], [0 < η < η_max(ρ) = 2/(1+|ρ|)] and a strictly
    positive, non-decreasing ATM total-variance ladder. Raw parameters are
    never simulated or clipped — use [decode], never manual assembly.

    Safety margins [eps_rho]/[eps_eta] keep strict inequalities away from the
    boundary; [theta_floor]/[incr_floor] keep the ladder positive in finite
    precision. Theta between knots is piecewise-linear (monotone by
    construction — never a spline that could dip). *)

exception Ssvi_error of string

(** Strict-inequality safety margins (documented, §6.3). *)
let eps_rho = 1e-6
let eps_eta = 1e-6

(** ATM total-variance floors (in variance units; 1e-8 ≈ 1bp vol at 1M). *)
let theta_floor = 1e-8
let incr_floor = 1e-10

let sigmoid x =
  if x >= 0. then 1. /. (1. +. exp (-.x))
  else
    let e = exp x in
    e /. (1. +. e)

let logit p =
  if not (p > 0. && p < 1.) then invalid_arg "ssvi: logit outside (0,1)";
  Float.log (p /. (1. -. p))

let softplus x =
  if x > 30. then x else if x < -30. then exp x else Float.log1p (exp x)

let softplus_inv y =
  if not (y > 0.) then invalid_arg "ssvi: softplus_inv of non-positive";
  if y > 30. then y else y +. Float.log1p (-.exp (-.y))

(** Clamp a unit-interval value away from the exact boundaries so the
    admissible decode is total: [sigmoid] underflows to exactly 0.0 (and
    rounds to exactly 1.0) for extreme latent inputs, which would otherwise
    produce gamma = 0 or eta = 0 and trip [phi_of]'s guards mid-optimization.
    The cut is at 1e-12 — far outside any economically meaningful region. *)
let clamp01 x = Float.min (Float.max x 1e-12) (1. -. 1e-12)

let atanh_capped x =
  if not (x > -1. && x < 1.) then invalid_arg "ssvi: atanh outside (-1,1)";
  0.5 *. Float.log ((1. +. x) /. (1. -. x))

type surface = {
  expiries : float array;  (** year fractions, strictly increasing *)
  theta : float array;  (** ATM total variance per knot, increasing *)
  rho : float;
  eta : float;
  gamma : float;
}

(** [φ(θ)] for the restricted power law. [theta <= 0] is rejected. *)
let phi_of ~theta ~eta ~gamma =
  if not (theta > 0.) then invalid_arg "ssvi: phi needs theta > 0";
  if not (eta > 0.) then invalid_arg "ssvi: phi needs eta > 0";
  if not (gamma > 0. && gamma < 0.5) then
    invalid_arg "ssvi: phi needs 0 < gamma < 0.5";
  let t = Float.max theta 1e-300 in
  eta /. ((t ** gamma) *. ((1. +. t) ** (1. -. gamma)))

(** Raw total variance at one (k, θ knot value). *)
let w_of ~k ~theta ~rho ~phi =
  let pk = phi *. k in
  0.5 *. theta
  *. (1. +. (rho *. pk) +. sqrt ((pk +. rho) ** 2. +. (1. -. (rho *. rho))))

(** ATM total variance at expiry [t] by monotone piecewise-linear interp. *)
let theta_at s t =
  let m = Array.length s.expiries in
  if m = 0 then invalid_arg "ssvi: empty surface";
  if t <= s.expiries.(0) then s.theta.(0)
  else if t >= s.expiries.(m - 1) then s.theta.(m - 1)
  else
    let j = ref 0 in
    while !j + 1 < m && s.expiries.(!j + 1) < t do
      incr j
    done;
    let t0 = s.expiries.(!j) and t1 = s.expiries.(!j + 1) in
    let w = (t -. t0) /. (t1 -. t0) in
    s.theta.(!j) +. (w *. (s.theta.(!j + 1) -. s.theta.(!j)))

(** Implied vol at log-forward moneyness [k], expiry [t] (years). *)
let iv s ~k ~t =
  if not (t > 0.) then raise (Ssvi_error "expiry must be positive");
  if not (Float.is_finite k) then raise (Ssvi_error "moneyness not finite");
  let th = theta_at s t in
  let phi = phi_of ~theta:th ~eta:s.eta ~gamma:s.gamma in
  let w = w_of ~k ~theta:th ~rho:s.rho ~phi in
  if not (w > 0. && Float.is_finite w) then
    raise (Ssvi_error "non-positive total variance");
  sqrt (w /. t)

(** Admissible decode of the latent vector
    [[a_0..a_{m-1}; u_ρ; u_η; u_γ]] (§6.3–6.4). ρ is decoded first since
    [η_max] depends on it. *)
let decode expiries u =
  let m = Array.length expiries in
  if m = 0 then invalid_arg "ssvi: no expiries";
  if Array.length u <> m + 3 then
    invalid_arg "ssvi: latent length must be n_expiries + 3";
  for i = 1 to m - 1 do
    if not (expiries.(i) > expiries.(i - 1)) then
      invalid_arg "ssvi: expiries must be strictly increasing"
  done;
  let theta = Array.make m 0. in
  theta.(0) <- softplus u.(0) +. theta_floor;
  for j = 1 to m - 1 do
    theta.(j) <- theta.(j - 1) +. softplus u.(j) +. incr_floor
  done;
  let rho = (1. -. eps_rho) *. Float.tanh u.(m) in
  let gamma = 0.5 *. clamp01 (sigmoid u.(m + 2)) in
  let eta_max = 2. /. (1. +. abs_float rho) in
  let eta = (1. -. eps_eta) *. eta_max *. clamp01 (sigmoid u.(m + 1)) in
  { expiries = Array.copy expiries; theta; rho; eta; gamma }

(** Inverse of [decode]: admissible surface → unconstrained latent. *)
let encode s =
  let m = Array.length s.expiries in
  let u = Array.make (m + 3) 0. in
  u.(0) <- softplus_inv (s.theta.(0) -. theta_floor);
  for j = 1 to m - 1 do
    u.(j) <- softplus_inv (s.theta.(j) -. s.theta.(j - 1) -. incr_floor)
  done;
  u.(m) <- atanh_capped (s.rho /. (1. -. eps_rho));
  let eta_max = 2. /. (1. +. abs_float s.rho) in
  u.(m + 1) <- logit (s.eta /. ((1. -. eps_eta) *. eta_max));
  u.(m + 2) <- logit (s.gamma /. 0.5);
  u
