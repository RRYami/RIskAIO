(** Latent SSVI-PCA + joint vol scenario construction.

    From dated calibration fits ([Ssvi_calibration.fit_history]) with a
    common expiry grid: [encode] each surface to unconstrained latent
    coordinates, difference consecutive days, and PCA the diffs (same
    covariance → [Stats.eigh_sym] → top-k sign-normalised recipe as
    [Run.pca_curves], so loadings are comparable). Scenarios reconstruct
    through [Ssvi.decode], hence admissible by construction.

    [vol_pcs = 0] (or >= full rank) reproduces raw diffs exactly; smaller k
    PCA-filters them. Dropped residual variance is reported
    ([residual_share]) and surfaced in engine diagnostics — never silently
    ignored (plan §8).

    Date alignment: fit dates are ISO strings; factor history rows carry
    [Market] return dates. A latent diff ending on date d pairs with the
    factor row whose return date is d. Diffs spanning >1 business day
    (failed fits, holidays) are excluded and counted ([n_gap_dropped]);
    scenarios without a fit pair fall back to the base surface and are
    counted by the caller. *)

type latent_set = {
  expiries : float array;
  dates : string array;  (** fit dates, ascending *)
  u_hist : float array array;  (** latent levels per date *)
  d_hist : float array array;  (** 1-day diffs, aligned to d_dates *)
  d_dates : string array;
  n_gap_dropped : int;  (** diffs spanning >1 business day, excluded *)
}

let of_fits fits =
  let ok =
    List.filter_map
      (function
        | Ssvi_calibration.Ok f -> Some f
        | Ssvi_calibration.Failed _ -> None)
      fits
  in
  match ok with
  | [] -> invalid_arg "pca_latent: no successful fits"
  | first :: _ ->
      let m = Array.length first.Ssvi_calibration.expiries in
      List.iter
        (fun (f : Ssvi_calibration.fit) ->
          if Array.length f.Ssvi_calibration.expiries <> m then
            invalid_arg "pca_latent: expiry grid changed across fits";
          Array.iteri
            (fun j t ->
              if t <> f.Ssvi_calibration.expiries.(j) then
                invalid_arg "pca_latent: expiry grid changed across fits")
            first.Ssvi_calibration.expiries)
        ok;
      let pairs =
        List.map
          (fun (f : Ssvi_calibration.fit) ->
            (f.Ssvi_calibration.date, Array.copy f.Ssvi_calibration.u))
          ok
      in
      let u_hist = Array.of_list (List.map snd pairs)
      and dates = Array.of_list (List.map fst pairs) in
      (* multi-day diffs (failed days, holidays) are not 1-day shocks *)
      let kept = ref [] and dropped = ref 0 in
      let n = Array.length u_hist in
      let p = Array.length u_hist.(0) in
      for i = 1 to n - 1 do
        let d0 = Risk_core.Calendar.of_string dates.(i - 1)
        and d1 = Risk_core.Calendar.of_string dates.(i) in
        if Risk_core.Calendar.business_days_between d0 d1 = 1 then
          kept :=
            ( dates.(i),
              Array.init p (fun a -> u_hist.(i).(a) -. u_hist.(i - 1).(a)) )
            :: !kept
        else incr dropped
      done;
      let kept = List.rev !kept in
      {
        expiries = Array.copy first.Ssvi_calibration.expiries;
        dates;
        u_hist;
        d_hist = Array.of_list (List.map snd kept);
        d_dates = Array.of_list (List.map fst kept);
        n_gap_dropped = !dropped;
      }

type pca = {
  mean : float array;
  loadings : float array array;  (** (p, k): columns are PC directions *)
  explained : float;  (** variance share of retained PCs *)
  residual_share : float;
  k : int;
}

let pca_fit d_hist k =
  let n = Array.length d_hist in
  if n < 2 then invalid_arg "pca_latent: need >= 2 diffs";
  let p = Array.length d_hist.(0) in
  let kk = if k <= 0 then p else Int.min k p in
  let mean = Array.make p 0. in
  Array.iter
    (fun row -> Array.iteri (fun a v -> mean.(a) <- mean.(a) +. v) row)
    d_hist;
  for a = 0 to p - 1 do
    mean.(a) <- mean.(a) /. float_of_int n
  done;
  let centred =
    Array.map (fun row -> Array.mapi (fun a v -> v -. mean.(a)) row) d_hist
  in
  let cov = Stats.sample_covariance centred in
  let vals, vecs = Stats.eigh_sym cov in
  let order =
    Array.init p Fun.id |> Array.to_list
    |> List.sort (fun a b -> Float.compare vals.(b) vals.(a))
  in
  let total = Array.fold_left ( +. ) 0. (Array.map (fun v -> Float.max v 0.) vals) in
  let kept = ref 0. in
  let loadings = Array.make_matrix p kk 0. in
  List.iteri
    (fun c t ->
      if c < kk then (
        kept := !kept +. Float.max vals.(t) 0.;
        (* sign-normalise: largest loading positive *)
        let col = Array.init p (fun i -> vecs.(i).(t)) in
        let amax = ref 0. in
        Array.iter
          (fun v -> if abs_float v > abs_float !amax then amax := v)
          col;
        let s = if !amax >= 0. then 1. else -1. in
        for i = 0 to p - 1 do
          loadings.(i).(c) <- s *. vecs.(i).(t)
        done))
    order;
  let explained = if total > 0. then !kept /. total else 1. in
  {
    mean;
    loadings;
    explained;
    residual_share = 1. -. explained;
    k = kk;
  }

(** Scores of a diff on the retained PCs. *)
let scores pc d =
  let c = Array.make pc.k 0. in
  let p = Array.length pc.mean in
  for a = 0 to p - 1 do
    let centred = d.(a) -. pc.mean.(a) in
    for j = 0 to pc.k - 1 do
      c.(j) <- c.(j) +. (pc.loadings.(a).(j) *. centred)
    done
  done;
  c

(** Project a diff through the retained PCs (PCA-filtered reconstruction). *)
let project pc d =
  let c = scores pc d in
  let p = Array.length pc.mean in
  Array.init p (fun a ->
      let s = ref pc.mean.(a) in
      for j = 0 to pc.k - 1 do
        s := !s +. (pc.loadings.(a).(j) *. c.(j))
      done;
      !s)

(** Most recent latent level at or before [as_of] (ISO date compare). *)
let base_state lat ~as_of =
  let b = ref (-1) in
  Array.iteri (fun j d -> if d <= as_of then b := j) lat.dates;
  if !b < 0 then invalid_arg "pca_latent: no fit on or before as_of";
  Array.copy lat.u_hist.(!b)

(** Diff ending on date d, if a fit pair covers it. *)
let diff_of_date lat d =
  let rec go i =
    if i >= Array.length lat.d_dates then None
    else if lat.d_dates.(i) = d then Some (Array.copy lat.d_hist.(i))
    else go (i + 1)
  in
  go 0

(** Scenario latent state: base + PCA-filtered diff. *)
let state_of_diff lat pc ~as_of d =
  let base = base_state lat ~as_of in
  let rec_ = project pc d in
  Array.init (Array.length base) (fun a -> base.(a) +. rec_.(a))
