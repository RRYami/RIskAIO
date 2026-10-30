(** Covariance, draws and tail measures. Port of [engine/stats.py] (ADR-007;
    production versions of [reference/]).

    Divergences (all agreed/tolerance-OK):
    - Matrices are [float array array] (row-major), not numpy; no BLAS yet —
      Owl/LACAML vectorisation is a P6 perf step. Small contractions use
      fixed-order loops, preserving the chunk-size-independence property.
    - Eigensolver is a pure-OCaml cyclic Jacobi ([eigh_sym]) instead of LAPACK.
    - RNG is SplitMix64 + Box–Muller + Marsaglia–Tsang gamma, NOT PCG64, so
      Monte-Carlo draws match in distribution/shape but not bit-for-bit. The
      draw *order* (all normals row-major, then chi-squares) mirrors
      [mc_shocks], and seeds are plain ints (63-bit on 64-bit platforms).
    - A literal all-zero covariance is repaired to ~zero instead of raising
      (Python's [cholesky] would fail there); [repaired] is reported [true]. *)

type vec = float array

type mat = float array array
(** row-major: [m.(i).(j)] *)

let mat_zeros r c = Array.make_matrix r c 0.

(* --- BLAS backend (LACAML) --- *)

module La = Lacaml.D

(** Kernels below this FLOP count stay on fixed-order scalar loops
    (deterministic, no copy overhead); above it they use BLAS/LAPACK. *)
let blas_min_flops = 50_000

let mat_to_lacaml a =
  let m = Array.length a in
  let n = if m = 0 then 0 else Array.length a.(0) in
  let t = La.Mat.create m n in
  for i = 1 to m do
    for j = 1 to n do
      t.{i, j} <- a.(i - 1).(j - 1)
    done
  done;
  t

let mat_of_lacaml t =
  let m = La.Mat.dim1 t and n = La.Mat.dim2 t in
  Array.init m (fun i -> Array.init n (fun j -> t.{i + 1, j + 1}))

(** C = A times B via dgemm. All dims must be ≥ 1. *)
let gemm_nn a b = mat_of_lacaml (La.gemm (mat_to_lacaml a) (mat_to_lacaml b))

(* --- covariance --- *)

(** Oldest → newest, normalised; w ∝ λ^age (age 0 = newest). *)
let ewma_weights n lam =
  if not (0. < lam && lam < 1.) then invalid_arg "lambda must be in (0, 1)";
  if n < 1 then invalid_arg "n must be >= 1";
  let w =
    Array.init n (fun i -> (1. -. lam) *. (lam ** float_of_int (n - 1 - i)))
  in
  let s = Array.fold_left ( +. ) 0. w in
  Array.map (fun x -> x /. s) w

(** Weighted second moment: [(returns * w)^T @ returns], [returns] = (n, k). *)
let ewma_covariance returns lam =
  let n = Array.length returns in
  if n = 0 then invalid_arg "returns must be non-empty";
  let k = Array.length returns.(0) in
  let w = ewma_weights n lam in
  if n * k * k >= blas_min_flops && n > 0 && k > 0 then (
    (* A[i][j] = returns[i][j] * sqrt(w[i]); C = AᵀA via dsyrk, then mirror. *)
    let a = La.Mat.create n k in
    for i = 1 to n do
      let s = sqrt w.(i - 1) in
      for j = 1 to k do
        a.{i, j} <- returns.(i - 1).(j - 1) *. s
      done
    done;
    let c = mat_of_lacaml (La.syrk ~trans:`T a) in
    for i = 0 to k - 1 do
      for j = 0 to i - 1 do
        c.(i).(j) <- c.(j).(i)
      done
    done;
    c)
  else
    let c = mat_zeros k k in
    for i = 0 to n - 1 do
      let wi = w.(i) in
      for a = 0 to k - 1 do
        let ra = returns.(i).(a) *. wi in
        for b = 0 to k - 1 do
          c.(a).(b) <- c.(a).(b) +. (ra *. returns.(i).(b))
        done
      done
    done;
    c

(** Unbiased sample covariance (ddof=1). For [n < 2] returns a zero matrix (the
    engine guarantees ≥250 rows; numpy would return NaN). *)
let sample_covariance returns =
  let n = Array.length returns in
  if n = 0 then invalid_arg "returns must be non-empty";
  let k = Array.length returns.(0) in
  if n < 2 then mat_zeros k k
  else
    let mean = Array.make k 0. in
    for i = 0 to n - 1 do
      for a = 0 to k - 1 do
        mean.(a) <- mean.(a) +. returns.(i).(a)
      done
    done;
    for a = 0 to k - 1 do
      mean.(a) <- mean.(a) /. float_of_int n
    done;
    if n * k * k >= blas_min_flops then (
      let a = La.Mat.create n k in
      for i = 1 to n do
        for j = 1 to k do
          a.{i, j} <- returns.(i - 1).(j - 1) -. mean.(j - 1)
        done
      done;
      let c = mat_of_lacaml (La.syrk ~trans:`T a) in
      let denom = float_of_int (n - 1) in
      for i = 0 to k - 1 do
        for j = 0 to k - 1 do
          c.(i).(j) <- (if j >= i then c.(i).(j) else c.(j).(i)) /. denom
        done
      done;
      c)
    else
      let c = mat_zeros k k in
      let denom = float_of_int (n - 1) in
      for i = 0 to n - 1 do
        for a = 0 to k - 1 do
          let da = returns.(i).(a) -. mean.(a) in
          for b = 0 to k - 1 do
            c.(a).(b) <-
              c.(a).(b) +. (da *. (returns.(i).(b) -. mean.(b)) /. denom)
          done
        done
      done;
      c

(* --- symmetric eigensolver (cyclic Jacobi) --- *)

let off_norm a =
  let n = Array.length a in
  let s = ref 0. in
  for i = 0 to n - 1 do
    for j = 0 to i - 1 do
      s := !s +. (a.(i).(j) *. a.(i).(j))
    done
  done;
  sqrt !s

(** Cyclic Jacobi eigensolver (small matrices; LAPACK below dispatches here). *)
let jacobi_eigh a =
  let n = Array.length a in
  if n = 0 then ([||], [||])
  else
    let a = Array.map Array.copy a in
    let v =
      Array.init n (fun i -> Array.init n (fun j -> if i = j then 1. else 0.))
    in
    let converged = ref false in
    let sweep = ref 0 in
    while !sweep < 100 && not !converged do
      incr sweep;
      if off_norm a < 1e-15 then converged := true
      else
        for p = 0 to n - 2 do
          for q = p + 1 to n - 1 do
            let apq = a.(p).(q) in
            if abs_float apq > 1e-300 then (
              let app = a.(p).(p) and aqq = a.(q).(q) in
              let theta = (aqq -. app) /. (2. *. apq) in
              let t =
                (if theta >= 0. then 1. else -1.)
                /. (abs_float theta +. sqrt ((theta *. theta) +. 1.))
              in
              let c = 1. /. sqrt ((t *. t) +. 1.) in
              let s = t *. c in
              for k = 0 to n - 1 do
                if k <> p && k <> q then (
                  let akp = a.(k).(p) and akq = a.(k).(q) in
                  let nkp = (c *. akp) -. (s *. akq) in
                  let nkq = (s *. akp) +. (c *. akq) in
                  a.(k).(p) <- nkp;
                  a.(p).(k) <- nkp;
                  a.(k).(q) <- nkq;
                  a.(q).(k) <- nkq)
              done;
              a.(p).(p) <-
                (c *. c *. app) -. (2. *. c *. s *. apq) +. (s *. s *. aqq);
              a.(q).(q) <-
                (s *. s *. app) +. (2. *. c *. s *. apq) +. (c *. c *. aqq);
              a.(p).(q) <- 0.;
              a.(q).(p) <- 0.;
              for k = 0 to n - 1 do
                let vkp = v.(k).(p) and vkq = v.(k).(q) in
                v.(k).(p) <- (c *. vkp) -. (s *. vkq);
                v.(k).(q) <- (s *. vkp) +. (c *. vkq)
              done)
          done
        done
    done;
    (Array.init n (fun i -> a.(i).(i)), v)

let cholesky_loop a =
  let n = Array.length a in
  let l = mat_zeros n n in
  for i = 0 to n - 1 do
    for j = 0 to i do
      let s = ref a.(i).(j) in
      for k = 0 to j - 1 do
        s := !s -. (l.(i).(k) *. l.(j).(k))
      done;
      if i = j then (
        if !s <= 0. then failwith "cholesky: matrix not positive definite";
        l.(i).(j) <- sqrt !s)
      else l.(i).(j) <- !s /. l.(j).(j)
    done
  done;
  l

type factorisation = { chol : mat; repaired : bool; min_eigenvalue : float }

(** Eigenvalues + eigenvectors of a symmetric matrix. Returns [(vals, vecs)]
    with [vecs.(i).(j)] = component [i] of vector [j]. LAPACK dsyevd above 25×25
    (eigenvalues ascending); cyclic Jacobi below. *)
let eigh_sym a =
  let n = Array.length a in
  if n = 0 then ([||], [||])
  else if n > 25 then (
    let t = mat_to_lacaml a in
    for i = 1 to n do
      for j = i + 1 to n do
        t.{i, j} <- 0.5 *. (t.{i, j} +. t.{j, i})
      done
    done;
    let w = La.syevd ~vectors:true t in
    let vals = Array.init n (fun i -> w.{i + 1}) in
    let vecs =
      Array.init n (fun i -> Array.init n (fun j -> t.{i + 1, j + 1}))
    in
    (vals, vecs))
  else jacobi_eigh a

(** Lower Cholesky factor. LAPACK dpotrf above 64×64, scalar loops below. *)
let cholesky_lower a =
  let n = Array.length a in
  if n > 64 then (
    let t = mat_to_lacaml a in
    La.potrf ~up:false t;
    Array.init n (fun i ->
        Array.init n (fun j -> if j <= i then t.{i + 1, j + 1} else 0.)))
  else cholesky_loop a

(** PSD-clipped Cholesky: symmetrise → eigh → clip below [eps * max(maxval, 0)]
    → reconstruct → Cholesky. *)
let psd_cholesky ?(eps = 1e-12) cov =
  let n = Array.length cov in
  if n = 0 then
    { chol = [||]; repaired = false; min_eigenvalue = Float.infinity }
  else
    let c =
      Array.init n (fun i ->
          Array.init n (fun j -> 0.5 *. (cov.(i).(j) +. cov.(j).(i))))
    in
    let vals, vecs = eigh_sym c in
    let maxv = Array.fold_left Float.max 0. vals in
    let floor = Float.max (eps *. Float.max maxv 0.) 1e-300 in
    let minv = Array.fold_left Float.min Float.infinity vals in
    let repaired = minv < floor in
    let c2 =
      if not repaired then c
      else
        let clipped = Array.map (fun v -> Float.max v floor) vals in
        let m =
          if n > 64 then (
            (* (V·diag) @ Vᵀ via dgemm — the scalar loop is n³. *)
            let a = La.Mat.create n n in
            for i = 1 to n do
              for k = 1 to n do
                a.{i, k} <- vecs.(i - 1).(k - 1) *. clipped.(k - 1)
              done
            done;
            mat_of_lacaml (La.gemm a ~transb:`T (mat_to_lacaml vecs)))
          else
            let m = mat_zeros n n in
            for i = 0 to n - 1 do
              for j = 0 to n - 1 do
                let s = ref 0. in
                for k = 0 to n - 1 do
                  s := !s +. (vecs.(i).(k) *. clipped.(k) *. vecs.(j).(k))
                done;
                m.(i).(j) <- !s
              done
            done;
            m
        in
        Array.init n (fun i ->
            Array.init n (fun j -> 0.5 *. (m.(i).(j) +. m.(j).(i))))
    in
    { chol = cholesky_lower c2; repaired; min_eigenvalue = minv }

(* --- RNG (SplitMix64 + Box–Muller + Marsaglia–Tsang) --- *)

type rng = { mutable s : int64; mutable spare : float option }

let make_rng seed = { s = Int64.of_int seed; spare = None }

let next_u64 r =
  let z = Int64.add r.s 0x9E3779B97F4A7C15L in
  r.s <- z;
  let z =
    Int64.mul
      (Int64.logxor z (Int64.shift_right_logical z 30))
      0xBF58476D1CE4E5B9L
  in
  let z =
    Int64.mul
      (Int64.logxor z (Int64.shift_right_logical z 27))
      0x94D049BB133111EBL
  in
  Int64.logxor z (Int64.shift_right_logical z 31)

(** Uniform on the unit interval. *)
let uniform01 r =
  Int64.to_float (Int64.shift_right_logical (next_u64 r) 11)
  /. 9007199254740992.

let gaussian r =
  match r.spare with
  | Some z ->
      r.spare <- None;
      z
  | None ->
      let u1 = 1. -. uniform01 r in
      let u2 = uniform01 r in
      let rad = sqrt (-2. *. Float.log u1) in
      let th = 2. *. Float.pi *. u2 in
      r.spare <- Some (rad *. sin th);
      rad *. cos th

let rec gamma_ge1 r a =
  let d = a -. (1. /. 3.) in
  let c = 1. /. sqrt (9. *. d) in
  let x = gaussian r in
  let v = (1. +. (c *. x)) ** 3. in
  if v <= 0. then gamma_ge1 r a
  else
    let u = uniform01 r in
    if Float.log u < (0.5 *. x *. x) +. d -. (d *. v) +. (d *. Float.log v) then
      d *. v
    else gamma_ge1 r a

let gamma r a =
  if a < 1. then gamma_ge1 r (a +. 1.) *. (uniform01 r ** (1. /. a))
  else gamma_ge1 r a

let chisquare r nu = 2. *. gamma r (nu /. 2.)

(** 1-day factor shocks (horizon scaling is applied by the caller, ADR-014).
    [chol] is the lower Cholesky factor with [k] rows; returns (n, k). *)
let mc_shocks chol ~n ~seed ~distribution ~nu =
  (match distribution with
  | `Student_t when nu <= 2. -> invalid_arg "Student-t needs nu > 2"
  | _ -> ());
  let k = Array.length chol in
  let r = make_rng seed in
  let z = Array.init n (fun _ -> Array.init k (fun _ -> gaussian r)) in
  (* X = Z @ Cholᵀ, via dgemm past the threshold. *)
  let x =
    if n * k * k >= blas_min_flops && n > 0 && k > 0 then
      mat_of_lacaml (La.gemm (mat_to_lacaml z) ~transb:`T (mat_to_lacaml chol))
    else
      Array.init n (fun i ->
          Array.init k (fun j ->
              let s = ref 0. in
              for l = 0 to k - 1 do
                s := !s +. (z.(i).(l) *. chol.(j).(l))
              done;
              !s))
  in
  (match distribution with
  | `Gaussian -> ()
  | `Student_t ->
      for i = 0 to n - 1 do
        let f = sqrt ((nu -. 2.) /. chisquare r nu) in
        for j = 0 to k - 1 do
          x.(i).(j) <- x.(i).(j) *. f
        done
      done);
  x

(** Fixed-order matrix product (chunk-size independence, ADR-007). dgemm past
    the BLAS threshold; note the two paths can differ in the last bit (different
    summation order), like BLAS vs loops anywhere. *)
let matmul a b =
  let m = Array.length a in
  if m = 0 then [||]
  else
    let k = Array.length a.(0) in
    if Array.length b <> k then invalid_arg "matmul: inner dimensions differ";
    let n = if k = 0 then 0 else Array.length b.(0) in
    if m * n * k >= blas_min_flops && n > 0 && k > 0 then gemm_nn a b
    else
      Array.init m (fun i ->
          Array.init n (fun j ->
              let s = ref 0. in
              for l = 0 to k - 1 do
                s := !s +. (a.(i).(l) *. b.(l).(j))
              done;
              !s))

(* --- tail measures --- *)

type tail = {
  var : float;
  es : float;
  order : int array;  (** all scenarios, worst loss first (stable) *)
  boundary : int;  (** position of the VaR scenario in [order] *)
  tail_index : int array;
  tail_weight : vec;  (** sums to 1 *)
}

(** Exact-tail-weight VaR/ES (see reference [var_es]). *)
let var_es pnl ~alpha ~weights =
  if not (0.5 < alpha && alpha < 1.) then
    invalid_arg "alpha must be in (0.5, 1)";
  let n = Array.length pnl in
  if n = 0 then invalid_arg "pnl must be non-empty";
  let loss = Array.map (fun p -> -.p) pnl in
  let p =
    match weights with
    | None -> Array.make n (1. /. float_of_int n)
    | Some w ->
        if Array.length w <> n then invalid_arg "weights length must match pnl";
        w
  in
  let order = Array.init n (fun i -> i) in
  Array.stable_sort (fun i j -> Float.compare loss.(j) loss.(i)) order;
  let cum = Array.make n 0. in
  let acc = ref 0. in
  for k = 0 to n - 1 do
    acc := !acc +. p.(order.(k));
    cum.(k) <- !acc
  done;
  let tail_prob = 1. -. alpha in
  let rec find k =
    if k >= n then n
    else if cum.(k) >= tail_prob -. 1e-15 then k
    else find (k + 1)
  in
  let k = find 0 in
  if k >= n then invalid_arg "weights do not cover the tail probability";
  let idx = Array.sub order 0 (k + 1) in
  let w = Array.init (k + 1) (fun t -> p.(idx.(t))) in
  w.(k) <- (tail_prob -. if k > 0 then cum.(k - 1) else 0.);
  for t = 0 to k do
    w.(t) <- w.(t) /. tail_prob
  done;
  let es = ref 0. in
  for t = 0 to k do
    es := !es +. (w.(t) *. loss.(idx.(t)))
  done;
  {
    var = loss.(order.(k));
    es = !es;
    order;
    boundary = k;
    tail_index = idx;
    tail_weight = w;
  }

(** Scenarios within ±m ranks of the VaR boundary, m = max(1, n // 200)
    (ADR-007). *)
let var_band tail n =
  let m = Int.max 1 (n / 200) in
  let lo = Int.max 0 (tail.boundary - m) in
  let hi = Int.min n (tail.boundary + m + 1) in
  Array.sub tail.order lo (hi - lo)

(** Euler ES per group; [group_pnl_tail] = (len(tail_index), n_groups). Sums to
    ES. *)
let component_es group_pnl_tail tail =
  let n_groups = Array.length group_pnl_tail.(0) in
  Array.init n_groups (fun g ->
      let s = ref 0. in
      for t = 0 to Array.length tail.tail_index - 1 do
        s := !s +. (tail.tail_weight.(t) *. group_pnl_tail.(t).(g))
      done;
      -. !s)

(** Band-mean loss per group, rescaled to sum exactly to VaR (ADR-007). *)
let component_var group_pnl_band tail =
  let n_groups = Array.length group_pnl_band.(0) in
  let contrib =
    Array.init n_groups (fun g ->
        let s = ref 0. in
        for i = 0 to Array.length group_pnl_band - 1 do
          s := !s +. group_pnl_band.(i).(g)
        done;
        -.(!s /. float_of_int (Array.length group_pnl_band)))
  in
  let total = Array.fold_left ( +. ) 0. contrib in
  if total = 0. || not (Float.is_finite total) then Array.make n_groups 0.
  else Array.map (fun c -> c *. (tail.var /. total)) contrib
