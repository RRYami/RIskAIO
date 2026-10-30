(** Option pricing (Q6): Black-Scholes (EUROPEAN), Bjerksund-Stensland 2002
    (AMERICAN). Port of [engine/options.py].

    BAW (Barone-Adeseh & Whaley) is retained as a tested alternative
    ([baw]/[baw_v]) but is NOT the engine path: its formula divides by
    [1 - exp(-rT)], undefined at r = 0, and the degenerate fallback silently
    returns European — missing the entire early-exercise premium (up to 87%
    underpricing measured on deep-ITM calls at r = 0, q = 25%). BS-2002 was
    validated at <= 1.7% vs 1000-step CRR across the book envelope
    (q to 25%, r to 0%, T to 3y; see amer-shootout test).

    Divergences:
    - Arrays are [float array] with one element per scenario; the caller
      broadcasts scalars (no numpy broadcasting).
    - No scipy: [ndtr] is built on a stdlib-only [erf] (Taylor for |x|<=1,
      Laplace continued fraction via Lentz for |x|>1), accurate to ~1e-15 —
      verified against published normal-CDF goldens in [test/test_all.ml];
      [phi2] (bivariate normal) is adaptive Simpson on Owen's 1D integral.
    - The BAW critical price is scalar per (r, b) pair with per-call
      convergence, so results are independent of batching (stronger than the
      ADR-007 chunk-independence requirement, same property).
    - Degenerate early-exercise parameters (q = 0 / non-finite, e.g. r <= 0)
      fall back to the European value instead of producing NaN. *)

let sqrt2 = sqrt 2.
let two_over_sqrt_pi = 1.1283791670955126

let rec erf_taylor_loop x2 n term sum =
  if n > 60 then sum
  else
    let term' =
      term *. -.x2
      *. float_of_int ((2 * n) - 1)
      /. (float_of_int n *. float_of_int ((2 * n) + 1))
    in
    let sum' = sum +. term' in
    if abs_float term' <= 1e-19 *. abs_float sum' then sum'
    else erf_taylor_loop x2 (n + 1) term' sum'

let erf_taylor x = two_over_sqrt_pi *. erf_taylor_loop (x *. x) 1 x x

(** erfc(x) for x > 1 via the Laplace continued fraction (Lentz). *)
let erfc_large x =
  let tiny = 1e-300 in
  let f = ref tiny in
  let c = ref tiny in
  let d = ref 0. in
  let j = ref 0 in
  let conv = ref false in
  while !j < 500 && not !conv do
    incr j;
    (* Laplace CF: 1/(x² + (1/2)/(1 + 1/(x² + (3/2)/(1 + ...)))),
       i.e. a₁ = 1, aⱼ = (j-1)/2 for j ≥ 2; b odd = x², b even = 1. *)
    let a = if !j = 1 then 1. else float_of_int (!j - 1) /. 2. in
    let b = if !j mod 2 = 1 then x *. x else 1. in
    d := b +. (a *. !d);
    if !d = 0. then d := tiny;
    d := 1. /. !d;
    c := b +. (a /. !c);
    if !c = 0. then c := tiny;
    let cd = !c *. !d in
    f := !f *. cd;
    if abs_float (cd -. 1.) < 1e-15 then conv := true
  done;
  x *. exp (-.x *. x) /. sqrt Float.pi *. !f

let erf x =
  let ax = abs_float x in
  if ax <= 1. then erf_taylor x
  else
    let e = 1. -. erfc_large ax in
    if x >= 0. then e else -.e

(** Complementary error function, accurate for large arguments (no cancellation,
    unlike [1 -. erf x]). *)
let rec erfc x =
  if x >= 0. then if x <= 1. then 1. -. erf_taylor x else erfc_large x
  else 2. -. erfc (-.x)

(** Standard normal CDF. *)
let ndtr x = 0.5 *. (1. +. erf (x /. sqrt2))

let d1_1 ~s ~k ~t ~b ~sigma =
  (Float.log (s /. k) +. ((b +. (0.5 *. sigma *. sigma)) *. t))
  /. (sigma *. sqrt t)

let bs_1 ~s ~k ~t ~r ~b ~sigma ~call =
  if t <= 0. then if call then Float.max (s -. k) 0. else Float.max (k -. s) 0.
  else (
    if sigma <= 0. then invalid_arg "options: sigma must be > 0";
    let sq = sqrt t in
    let d1 =
      (Float.log (s /. k) +. ((b +. (0.5 *. sigma *. sigma)) *. t))
      /. (sigma *. sq)
    in
    let d2 = d1 -. (sigma *. sq) in
    let carry = exp ((b -. r) *. t) and disc = exp (-.r *. t) in
    if call then (s *. carry *. ndtr d1) -. (k *. disc *. ndtr d2)
    else (k *. disc *. ndtr (-.d2)) -. (s *. carry *. ndtr (-.d1)))

let check_len name a b =
  if Array.length a <> Array.length b then
    invalid_arg ("options: array length mismatch in " ^ name)

let black_scholes ~s ~k ~t ~r ~b ~sigma ~call =
  check_len "r" s r;
  check_len "b" s b;
  Array.init (Array.length s) (fun i ->
      bs_1 ~s:s.(i) ~k ~t ~r:r.(i) ~b:b.(i) ~sigma ~call)

(** Scalar critical stock price for one (r, b) pair (200 bisection iters). *)
let critical_price_1 ~k ~t ~r ~b ~sigma ~call ~q =
  let carry = exp ((b -. r) *. t) in
  let f x =
    let euro = bs_1 ~s:x ~k ~t ~r ~b ~sigma ~call in
    let d1 = d1_1 ~s:x ~k ~t ~b ~sigma in
    if call then x -. k -. euro -. ((1. -. (carry *. ndtr d1)) *. x /. q)
    else k -. x -. euro +. ((1. -. (carry *. ndtr (-.d1))) *. x /. q)
  in
  let lo = ref 0. and hi = ref 0. in
  if call then (
    lo := k;
    hi := 2. *. k;
    for _ = 1 to 64 do
      if f !hi <= 0. then hi := !hi *. 2.
    done)
  else (
    lo := 1e-12 *. k;
    hi := k);
  let flo = ref (f !lo) in
  for _ = 1 to 200 do
    if !hi -. !lo > 1e-12 *. Float.max 1. !hi then
      let mid = 0.5 *. (!lo +. !hi) in
      let fm = f mid in
      let s_lo = Float.compare fm 0. = Float.compare !flo 0. in
      if s_lo then (
        lo := mid;
        flo := fm)
      else hi := mid
  done;
  0.5 *. (!lo +. !hi)

let baw ~s ~k ~t ~r ~b ~sigma ~call =
  check_len "r" s r;
  check_len "b" s b;
  let n = Array.length s in
  if t <= 0. then
    Array.init n (fun i ->
        if call then Float.max (s.(i) -. k) 0. else Float.max (k -. s.(i)) 0.)
  else (
    if sigma <= 0. then invalid_arg "options: sigma must be > 0";
    let euro = black_scholes ~s ~k ~t ~r ~b ~sigma ~call in
    let early_idx =
      Array.init n (fun i -> i)
      |> Array.to_list
      |> List.filter (fun i -> if call then b.(i) < r.(i) else r.(i) > 0.)
    in
    if early_idx = [] then euro
    else
      let out = Array.copy euro in
      (* Group by exact (r, b): critical prices depend on (r, b) only. *)
      let pairs =
        List.sort_uniq Stdlib.compare
          (List.map (fun i -> (r.(i), b.(i))) early_idx)
      in
      List.iter
        (fun (ru, bu) ->
          let idxs =
            List.filter (fun i -> r.(i) = ru && b.(i) = bu) early_idx
          in
          match idxs with
          | [] -> ()
          | i0 :: _ ->
              let ru = r.(i0) and bu = b.(i0) in
              let sig2 = sigma *. sigma in
              let m = 2. *. ru /. sig2 and n_ = 2. *. bu /. sig2 in
              let kk = 1. -. exp (-.ru *. t) in
              let root = sqrt (((n_ -. 1.) ** 2.) +. (4. *. m /. kk)) in
              let q =
                if call then 0.5 *. (-.(n_ -. 1.) +. root)
                else 0.5 *. (-.(n_ -. 1.) -. root)
              in
              if (not (Float.is_finite q)) || q = 0. then
                (* Degenerate (e.g. r <= 0): keep the European value. *)
                ()
              else
                let s_star =
                  critical_price_1 ~k ~t ~r:ru ~b:bu ~sigma ~call ~q
                in
                let d1 = d1_1 ~s:s_star ~k ~t ~b:bu ~sigma in
                let carry = exp ((bu -. ru) *. t) in
                let a =
                  if call then s_star /. q *. (1. -. (carry *. ndtr d1))
                  else -.(s_star /. q) *. (1. -. (carry *. ndtr (-.d1)))
                in
                List.iter
                  (fun i ->
                    let se = s.(i) in
                    let v =
                      if call then
                        if se >= s_star then se -. k
                        else euro.(i) +. (a *. ((se /. s_star) ** q))
                      else if se <= s_star then k -. se
                      else euro.(i) +. (a *. ((se /. s_star) ** q))
                    in
                    out.(i) <- v)
                  idxs)
        pairs;
      out)

(** Gauss-Legendre nodes/weights (positive halves; the loop covers both
    signs) for the Drezner-Wesolowsky/Genz central branch. Fixed 20-point:
    accurate to ~1e-14 for |rho| < 0.925, which covers every BS-2002 call
    (rho = +-sqrt(t1/t2) = +-0.786). *)
let dw_x =
  [|
    0.9931285991850949; 0.9639719272779138; 0.912234428251326;
    0.8391169718222188; 0.7463319064601508; 0.6360536807265152;
    0.5108670019508271; 0.3737060887154196; 0.2277858511416451;
    0.07652652113349731;
  |]

let dw_w =
  [|
    0.017614007139152118; 0.04060142980038694; 0.06267204833410906;
    0.08327674157670475; 0.10193011981724043; 0.11819453196151842;
    0.13168863844917663; 0.14209610931838205; 0.14917298647260375;
    0.15275338713072585;
  |]

(** Upper tail L(h,k;r) = P(X>=h, Y>=k) via the DW/Genz central branch.
    Deterministic cost (~40 exp evals); no iteration. This is what BS-2002's
    Psi terms need (NAG's cbnd). *)
let bvn_upper h k r =
  let hk = h *. k in
  let hs = 0.5 *. (h *. h +. k *. k) in
  let ang = 0.5 *. asin r in
  let acc = ref 0. in
  Array.iteri
    (fun i x ->
      let w = dw_w.(i) in
      let s1 = sin (ang *. (1. -. x)) in
      acc := !acc +. w *. exp ((s1 *. hk -. hs) /. (1. -. s1 *. s1));
      let s2 = sin (ang *. (1. +. x)) in
      acc := !acc +. w *. exp ((s2 *. hk -. hs) /. (1. -. s2 *. s2)))
    dw_x;
  Float.min
    (Float.max
       (!acc *. ang /. (2. *. Float.pi) +. ndtr (-.h) *. ndtr (-.k))
       0.)
    1.

(** 1D-quadrature reference for |rho| >= 0.925 (never hit by BS-2002, whose
    rho is +-0.786; kept total and tested via the Phi2(0,0,rho) identity).
    Phi2(h,k,rho) = integral_{-inf}^{h} phi(x) Phi((k-rho x)/s) dx,
    truncated to [-8, min(h,12)] (phi tails outside contribute < 1e-15). *)
let phi2_quad h k rho =
  let s = sqrt (1. -. rho *. rho) in
  let hi = Float.min h 12. in
  let f x =
    exp (-0.5 *. x *. x) /. sqrt (2. *. Float.pi)
    *. ndtr ((k -. rho *. x) /. s)
  in
  let rec adaptive a b fa fm fb whole tol depth =
    let m = 0.5 *. (a +. b) in
    let lm = 0.5 *. (a +. m) and rm = 0.5 *. (m +. b) in
    let flm = f lm and frm = f rm in
    let left = (m -. a) /. 6. *. (fa +. (4. *. flm) +. fm)
    and right = (b -. m) /. 6. *. (fm +. (4. *. frm) +. fb) in
    let delta = left +. right -. whole in
    if depth <= 0 || abs_float delta <= 15. *. tol then
      left +. right +. delta /. 15.
    else
      adaptive a m fa flm fm left (0.5 *. tol) (depth - 1)
      +. adaptive m b fm frm fb right (0.5 *. tol) (depth - 1)
  in
  let lo = -8. in
  let fa = f lo and fb = f hi and fm = f (0.5 *. (lo +. hi)) in
  let whole = (hi -. lo) /. 6. *. (fa +. (4. *. fm) +. fb) in
  adaptive lo hi fa fm fb whole 1e-11 16

(** Bivariate normal CDF. DW central branch below 0.925, quadrature above. *)
let phi2 h k rho =
  if abs_float rho >= 1. -. 1e-12 then (
    if rho > 0. then ndtr (Float.min h k)
    else Float.max (ndtr h +. ndtr k -. 1.) 0.)
  else if h <= -8. then 0.
  else if abs_float rho < 0.925 then
    let l = bvn_upper h k rho in
    Float.min (Float.max (ndtr h +. ndtr k -. 1. +. l) 0.) 1.
  else phi2_quad h k rho

(** Upper tail P(X>a, Y>b). Direct DW evaluation in the central region;
    1 - Phi(a) - Phi(b) + Phi2(a,b,rho) above it. *)
let upper_tail a b rho =
  if abs_float rho < 0.925 then bvn_upper a b rho
  else
    let u = 1. -. ndtr a -. ndtr b +. phi2 a b rho in
    Float.min (Float.max u 0.) 1.

(** BS-2002 phi term (paper eq. 7). *)
let bs_phi ~fs ~t ~gamma ~h ~i ~r ~b ~v =
  let v2 = v *. v in
  let d1 = -.((Float.log (fs /. h) +. (b +. (gamma -. 0.5) *. v2) *. t) /. (v *. sqrt t)) in
  let d2 = d1 -. 2. *. Float.log (i /. fs) /. (v *. sqrt t) in
  let lambda = -.r +. gamma *. b +. 0.5 *. gamma *. (gamma -. 1.) *. v2 in
  let kappa = 2. *. b /. v2 +. (2. *. gamma -. 1.) in
  exp (lambda *. t) *. (fs ** gamma)
  *. (ndtr d1 -. ((i /. fs) ** kappa *. ndtr d2))

(** BS-2002 psi term (bivariate). *)
let bs_psi ~fs ~t2 ~gamma ~h ~i2 ~i1 ~t1 ~r ~b ~v =
  let v2 = v *. v in
  let st1 = v *. sqrt t1 and st2 = v *. sqrt t2 in
  let bg1 = (b +. (gamma -. 0.5) *. v2) *. t1
  and bg2 = (b +. (gamma -. 0.5) *. v2) *. t2 in
  let d1 = (Float.log (fs /. i1) +. bg1) /. st1
  and d2 = (Float.log (i2 *. i2 /. (fs *. i1)) +. bg1) /. st1
  and d3 = (Float.log (fs /. i1) -. bg1) /. st1
  and d4 = (Float.log (i2 *. i2 /. (fs *. i1)) -. bg1) /. st1 in
  let e1 = (Float.log (fs /. h) +. bg2) /. st2
  and e2 = (Float.log (i2 *. i2 /. (fs *. h)) +. bg2) /. st2
  and e3 = (Float.log (i1 *. i1 /. (fs *. h)) +. bg2) /. st2
  and e4 = (Float.log (fs *. i1 *. i1 /. (h *. i2 *. i2)) +. bg2) /. st2 in
  let tau = sqrt (t1 /. t2) in
  let lambda = -.r +. gamma *. b +. 0.5 *. gamma *. (gamma -. 1.) *. v2 in
  let kappa = 2. *. b /. v2 +. (2. *. gamma -. 1.) in
  exp (lambda *. t2) *. (fs ** gamma)
  *. (upper_tail d1 e1 tau
     -. ((i2 /. fs) ** kappa *. upper_tail d2 e2 tau)
     -. ((i1 /. fs) ** kappa *. upper_tail d3 e3 (-.tau))
     +. ((i1 /. i2) ** kappa *. upper_tail d4 e4 (-.tau)))

(** Scalar BS-2002 American call (NAG s30qcc / paper Prop. 1). Lower bound
    floored at the European value; immediate exercise above the boundary.
    For tiny sigma the alpha/kappa powers are 0·inf indeterminate forms
    (underflowed i^{-β} times overflowed s^{β}); there the time value is
    economically zero, so a non-finite main value falls back to
    max(intrinsic, euro) — never NaN. *)
let rec bs2002_1 ~s ~k ~t ~r ~b ~sigma ~call =
  let euro = bs_1 ~s ~k ~t ~r ~b ~sigma ~call in
  let intr = if call then Float.max (s -. k) 0. else Float.max (k -. s) 0. in
  let put_via_symmetry () =
    if r -. b <= 0. then
      bs_1 ~s ~k ~t ~r ~b ~sigma ~call:false
    else
      let c =
        bs2002_1 ~s:k ~k:s ~t ~r:(r -. b) ~b:(-.b) ~sigma ~call:true
      in
      Float.max c (bs_1 ~s ~k ~t ~r ~b ~sigma ~call:false)
  in
  if t <= 0. then intr
  else if call then (
    if b >= r then euro
    else
      let v2 = sigma *. sigma in
      let t1 = 0.5 *. (sqrt 5. -. 1.) *. t in
      let beta =
        (0.5 -. b /. v2)
        +. sqrt ((b /. v2 -. 0.5) ** 2. +. (2. *. r /. v2))
      in
      if not (beta > 1.) then euro
      else
        let binf = beta /. (beta -. 1.) *. k in
        let b0 = Float.max k (r /. (r -. b) *. k) in
        if not (binf > b0) then euro
        else
          let h t_ =
            -.(b *. t_ +. 2. *. sigma *. sqrt t_)
            *. (k *. k /. ((binf -. b0) *. b0))
          in
          let i1 = b0 +. (binf -. b0) *. (1. -. exp (h t1))
          and i2 = b0 +. (binf -. b0) *. (1. -. exp (h t)) in
          let alpha x = (x -. k) *. (x ** (-.beta)) in
          if s >= i2 then Float.max intr euro
          else
            let v =
              alpha i2 *. (s ** beta)
              -. (alpha i2 *. bs_phi ~fs:s ~t:t1 ~gamma:beta ~h:i2 ~i:i2 ~r ~b ~v:sigma)
              +. bs_phi ~fs:s ~t:t1 ~gamma:1. ~h:i2 ~i:i2 ~r ~b ~v:sigma
              -. bs_phi ~fs:s ~t:t1 ~gamma:1. ~h:i1 ~i:i2 ~r ~b ~v:sigma
              -. k *. bs_phi ~fs:s ~t:t1 ~gamma:0. ~h:i2 ~i:i2 ~r ~b ~v:sigma
              +. k *. bs_phi ~fs:s ~t:t1 ~gamma:0. ~h:i1 ~i:i2 ~r ~b ~v:sigma
              +. alpha i1 *. bs_phi ~fs:s ~t:t1 ~gamma:beta ~h:i1 ~i:i2 ~r ~b ~v:sigma
              -. alpha i1 *. bs_psi ~fs:s ~t2:t ~gamma:beta ~h:i1 ~i2 ~i1 ~t1 ~r ~b ~v:sigma
              +. bs_psi ~fs:s ~t2:t ~gamma:1. ~h:i1 ~i2 ~i1 ~t1 ~r ~b ~v:sigma
              -. bs_psi ~fs:s ~t2:t ~gamma:1. ~h:k ~i2 ~i1 ~t1 ~r ~b ~v:sigma
              -. k *. bs_psi ~fs:s ~t2:t ~gamma:0. ~h:i1 ~i2 ~i1 ~t1 ~r ~b ~v:sigma
              +. k *. bs_psi ~fs:s ~t2:t ~gamma:0. ~h:k ~i2 ~i1 ~t1 ~r ~b ~v:sigma
            in
            Float.max (if Float.is_finite v then v else intr) euro)
  else put_via_symmetry ()

(** Vector-spot BS-2002 (scalar sigma), mirroring [black_scholes]. *)
let bs2002 ~s ~k ~t ~r ~b ~sigma ~call =
  check_len "r" s r;
  check_len "b" s b;
  Array.init (Array.length s) (fun i ->
      bs2002_1 ~s:s.(i) ~k ~t ~r:r.(i) ~b:b.(i) ~sigma ~call)

(** Vector-sigma BS-2002 (per-scenario sigma), mirroring [baw_v].
    Every row is an independent closed-form eval (no shared solves,
    unlike BAW's critical-price grouping). *)
let bs2002_v ~s ~k ~t ~r ~b ~sigma ~call =
  check_len "r" s r;
  check_len "sigma" s sigma;
  if Array.exists (fun x -> x <= 0.) sigma then
    invalid_arg "options: sigma must be > 0";
  Array.init (Array.length s) (fun i ->
      bs2002_1 ~s:s.(i) ~k ~t ~r:r.(i) ~b:b.(i) ~sigma:sigma.(i) ~call)

(** Stock option with continuous dividend yield [q] (carry b = r − q); the style
    flag picks the model (Q6). *)
let option_price ~style ~s ~k ~t ~r ~q ~sigma ~call =
  check_len "r" s r;
  let b = Array.map (fun ri -> ri -. q) r in
  match style with
  | "AMERICAN" -> bs2002 ~s ~k ~t ~r ~b ~sigma ~call
  | "EUROPEAN" -> black_scholes ~s ~k ~t ~r ~b ~sigma ~call
  | _ -> invalid_arg ("unknown option style: " ^ style)

(** Vector-sigma variant: per-scenario [sigma.(i)] (SSVI scenario states).
    Falls back elementwise to the scalar path; American grouping is by exact
    (r, b) pairs as before, sigma varying freely within a group. *)
let black_scholes_v ~s ~k ~t ~r ~b ~sigma ~call =
  check_len "r" s r;
  check_len "sigma" s sigma;
  Array.init (Array.length s) (fun i ->
      bs_1 ~s:s.(i) ~k ~t ~r:r.(i) ~b:b.(i) ~sigma:sigma.(i) ~call)

let baw_v ~s ~k ~t ~r ~b ~sigma ~call =
  check_len "r" s r;
  check_len "sigma" s sigma;
  let n = Array.length s in
  if t <= 0. then
    Array.init n (fun i ->
        if call then Float.max (s.(i) -. k) 0. else Float.max (k -. s.(i)) 0.)
  else (
    if Array.exists (fun x -> x <= 0.) sigma then
      invalid_arg "options: sigma must be > 0";
    (* exact (r, b) groups, as in [baw]; sigma varies within the group and
       the critical price is solved per distinct sigma *)
    let early =
      Array.init n (fun i -> i)
      |> Array.to_list
      |> List.filter (fun i -> if call then b.(i) < r.(i) else r.(i) > 0.)
    in
    let euro =
      Array.init n (fun i ->
          bs_1 ~s:s.(i) ~k ~t ~r:r.(i) ~b:b.(i) ~sigma:sigma.(i) ~call)
    in
    if early = [] then euro
    else
      let out = Array.copy euro in
      let groups =
        List.sort_uniq Stdlib.compare (List.map (fun i -> (r.(i), b.(i))) early)
      in
      List.iter
        (fun (ru, bu) ->
          let idxs =
            List.filter (fun i -> r.(i) = ru && b.(i) = bu) early
          in
          (* one critical price per distinct sigma in the group *)
          let by_sig =
            List.sort_uniq Stdlib.compare
              (List.map (fun i -> sigma.(i)) idxs)
          in
          List.iter
            (fun sg ->
              let here = List.filter (fun i -> sigma.(i) = sg) idxs in
              let sig2 = sg *. sg in
              let m = 2. *. ru /. sig2 and n_ = 2. *. bu /. sig2 in
              let kk = 1. -. exp (-.ru *. t) in
              let root = sqrt (((n_ -. 1.) ** 2.) +. (4. *. m /. kk)) in
              let qq =
                if call then 0.5 *. (-.(n_ -. 1.) +. root)
                else 0.5 *. (-.(n_ -. 1.) -. root)
              in
              if Float.is_finite qq && qq <> 0. then (
                let s_star =
                  critical_price_1 ~k ~t ~r:ru ~b:bu ~sigma:sg ~call ~q:qq
                in
                let d1 = d1_1 ~s:s_star ~k ~t ~b:bu ~sigma:sg in
                let carry = exp ((bu -. ru) *. t) in
                let a =
                  if call then s_star /. qq *. (1. -. (carry *. ndtr d1))
                  else -.(s_star /. qq) *. (1. -. (carry *. ndtr (-.d1)))
                in
                List.iter
                  (fun i ->
                    let se = s.(i) in
                    let v =
                      if call then
                        if se >= s_star then se -. k
                        else euro.(i) +. (a *. ((se /. s_star) ** qq))
                      else if se <= s_star then k -. se
                      else euro.(i) +. (a *. ((se /. s_star) ** qq))
                    in
                    out.(i) <- v)
                  here))
            by_sig)
        groups;
      out)

let option_price_v ~style ~s ~k ~t ~r ~q ~sigma ~call =
  check_len "r" s r;
  check_len "sigma" s sigma;
  let b = Array.map (fun ri -> ri -. q) r in
  match style with
  | "AMERICAN" -> bs2002_v ~s ~k ~t ~r ~b ~sigma ~call
  | "EUROPEAN" -> black_scholes_v ~s ~k ~t ~r ~b ~sigma ~call
  | _ -> invalid_arg ("unknown option style: " ^ style)
