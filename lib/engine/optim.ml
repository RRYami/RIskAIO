(** Derivative-free optimization (stdlib only): Nelder–Mead simplex.

    Used for the daily SSVI calibration, which runs in the unconstrained
    latent space ([Ssvi.decode] maps to admissible parameters), so no bounded
    optimizer is needed. Coefficients default to the Gao–Han adaptive scheme
    ([ρ = 0.75 − 1/2n], [σ = 1 − 1/n]), which the scipy cross-check
    ([fixtures/ssvi-cal-v1/scipy_reference.json], from the ATM init) shows is
    required to reach the L-BFGS-B optimum from a cold start; the classic
    fixed [ρ = σ = 0.5] stalls on the theta-ladder landscape.

    Non-finite objective values are mapped to a large finite penalty so the
    simplex arithmetic never sees NaN; a non-finite value at the start point
    itself is an error. Stopping mirrors scipy: coordinate spread ([xatol])
    and function spread ([fatol]), checked together. *)

type result = { x : float array; f : float; nit : int; converged : bool }

let huge = 1e300

let nelder_mead ?(maxiter = 10000) ?(xatol = 1e-10) ?(fatol = 1e-12)
    ?(step = 0.05) ?(adaptive = true) f x0 =
  let n = Array.length x0 in
  if n = 0 then invalid_arg "optim: empty start point";
  let wrap x =
    let v = f x in
    if Float.is_finite v then v else huge
  in
  let f0 = f x0 in
  if not (Float.is_finite f0) then invalid_arg "optim: f(x0) not finite";
  (* initial simplex: x0 plus per-axis steps (scipy convention) *)
  let simplex = Array.init (n + 1) (fun i ->
      if i = 0 then (Array.copy x0, f0)
      else
        let x = Array.copy x0 in
        let d = step *. Float.max (abs_float x0.(i - 1)) 1. in
        let d = if x0.(i - 1) = 0. then 0.00025 else d in
        x.(i - 1) <- x.(i - 1) +. d;
        (x, wrap x))
  in
  let rho = if adaptive then 0.75 -. (1. /. (2. *. float_of_int n)) else 0.5 in
  let sigma = if adaptive then 1. -. (1. /. float_of_int n) else 0.5 in
  let sort () =
    Array.sort (fun (_, a) (_, b) -> Float.compare a b) simplex
  in
  let spread () =
    sort ();
    let _, fb = simplex.(0) and _, fw = simplex.(n) in
    let dx = ref 0. in
    for i = 1 to n do
      let x, _ = simplex.(i) and x0', _ = simplex.(0) in
      for j = 0 to n - 1 do
        dx := Float.max !dx (abs_float (x.(j) -. x0'.(j)))
      done
    done;
    (abs_float (fw -. fb), !dx)
  in
  let it = ref 0 in
  let conv = ref false in
  while !it < maxiter && not !conv do
    incr it;
    sort ();
    let xb, fb = simplex.(0) in
    let _, fn_1 = simplex.(n - 1) and xw, fw = simplex.(n) in
    (* centroid of the best n *)
    let c = Array.make n 0. in
    for i = 0 to n - 1 do
      let x, _ = simplex.(i) in
      for j = 0 to n - 1 do
        c.(j) <- c.(j) +. x.(j)
      done
    done;
    for j = 0 to n - 1 do
      c.(j) <- c.(j) /. float_of_int n
    done;
    let at t = Array.init n (fun j -> c.(j) +. (t *. (c.(j) -. xw.(j)))) in
    let xr = at 1. in
    let fr = wrap xr in
    if fr < fb then (
      (* expand *)
      let xe = Array.init n (fun j -> c.(j) +. (2. *. (xr.(j) -. c.(j)))) in
      let fe = wrap xe in
      simplex.(n) <- (if fe < fr then (xe, fe) else (xr, fr)))
    else if fr < fn_1 then simplex.(n) <- (xr, fr)
    else (
      (* contract toward the better of xr / worst *)
      let base = if fr < fw then xr else xw in
      let xc =
        Array.init n (fun j -> c.(j) +. (rho *. (base.(j) -. c.(j))))
      in
      let fc = wrap xc in
      if fc < Float.min fr fw then simplex.(n) <- (xc, fc)
      else
        (* shrink toward best *)
        for i = 1 to n do
          let x, _ = simplex.(i) in
          let xs =
            Array.init n (fun j -> xb.(j) +. (sigma *. (x.(j) -. xb.(j))))
          in
          simplex.(i) <- (xs, wrap xs)
        done);
    let df, dx = spread () in
    if df <= fatol && dx <= xatol then conv := true
  done;
  sort ();
  let x, fv = simplex.(0) in
  { x = Array.copy x; f = fv; nit = !it; converged = !conv }
