(** Daily SSVI calibration over quote grids (plan §7).

    Each day's quotes ([Quotes.to_grids]) are fitted in the unconstrained
    latent space: weighted mean-squared error in IV against [Ssvi.decode],
    minimized by [Optim.nelder_mead]. Initialization is data-driven, never
    a blind zero: the ATM slice gives [θ_j ≈ iv_atm²·T] (cummax-enforced,
    mapped through [softplus_inv] to ladder increments), with neutral
    rho/eta/gamma. Later days warm-start from the previous *successful* fit —
    and a failed day is recorded as [Failed] with a reason, never silently
    replaced by a stale fit, and never leaks into the next day's init beyond
    the last good state (§7 no-silent-reuse, no look-ahead: the chain only
    ever looks backward).

    Fits serialize to JSON (yojson, already a dep) so calibration — the
    slowest stage — runs once and is reloaded thereafter. *)

type fit = {
  date : string;
  expiries : float array;
  u : float array;
  fun_ : float;
  rmse_iv : float;
  nit : int;
  converged : bool;
  warm : bool;
}

type outcome =
  | Ok of fit
  | Failed of { date : string; reason : string }

(** ATM init from one day's grids: nearest-|k| slice per expiry. *)
let atm_init grids =
  let m = List.length grids in
  if m = 0 then invalid_arg "calibration: no expiries";
  let th =
    List.map
      (fun (g : Quotes.expiry_obs) ->
        let n = Array.length g.Quotes.k in
        if n = 0 then invalid_arg "calibration: empty expiry";
        let i0 = ref 0 in
        for i = 1 to n - 1 do
          if abs_float g.Quotes.k.(i) < abs_float g.Quotes.k.(!i0) then i0 := i
        done;
        g.Quotes.t *. g.Quotes.iv.(!i0) *. g.Quotes.iv.(!i0))
      grids
  in
  (* cummax: ATM variance must not decrease *)
  let th =
    let acc = ref neg_infinity in
    List.map
      (fun x ->
        acc := Float.max !acc x;
        !acc)
      th
  in
  let u = Array.make (m + 3) 0. in
  let prev = ref 0. in
  List.iteri
    (fun j tj ->
      let base = if j = 0 then Ssvi.theta_floor else !prev +. Ssvi.incr_floor in
      u.(j) <- Ssvi.softplus_inv (Float.max (tj -. base) 1e-12);
      prev := tj)
    th;
  u

let objective grids u =
  let expiries =
    Array.of_list (List.map (fun (g : Quotes.expiry_obs) -> g.Quotes.t) grids)
  in
  let s = Ssvi.decode expiries u in
  let acc = ref 0. and tot = ref 0. in
  List.iter
    (fun (g : Quotes.expiry_obs) ->
      Array.iteri
        (fun i k ->
          let m =
            try Ssvi.iv s ~k ~t:g.Quotes.t
            with Ssvi.Ssvi_error _ -> Float.nan
          in
          if Float.is_finite m then (
            let d = m -. g.Quotes.iv.(i) in
            acc := !acc +. (g.Quotes.w.(i) *. d *. d);
            tot := !tot +. g.Quotes.w.(i)))
        g.Quotes.k)
    grids;
  if !tot <= 0. then invalid_arg "calibration: no weight";
  !acc /. !tot

let fit_day ?u0 grids ~date =
  if grids = [] then Failed { date; reason = "no expiries" }
  else if List.exists (fun (g : Quotes.expiry_obs) -> Array.length g.Quotes.k = 0) grids
  then Failed { date; reason = "empty expiry" }
  else
    let expiries =
      Array.of_list (List.map (fun (g : Quotes.expiry_obs) -> g.Quotes.t) grids)
    in
    let increasing =
      let n = Array.length expiries in
      let rec go i = i + 1 >= n || (expiries.(i) < expiries.(i + 1) && go (i + 1)) in
      go 0
    in
    if not increasing then Failed { date; reason = "expiries not increasing" }
    else
      let init, warm =
        match u0 with
        | Some u ->
            if Array.length u <> Array.length expiries + 3 then
              (atm_init grids, false)
            else (Array.copy u, true)
        | None -> (atm_init grids, false)
      in
      let obj = objective grids in
      let err = ref "" in
      (match
         (try Some (obj init)
          with Invalid_argument msg ->
            err := "init: " ^ msg;
            None)
       with
      | None -> Failed { date; reason = !err }
      | Some _ -> (
          match
            (try
               Some
                 (Optim.nelder_mead ~maxiter:10000 ~xatol:1e-9 ~fatol:1e-13 obj
                    init)
             with Invalid_argument msg ->
               err := "opt: " ^ msg;
               None)
          with
          | None -> Failed { date; reason = !err }
          | Some r ->
              let rmse = sqrt r.Optim.f in
              Ok
                {
                  date;
                  expiries;
                  u = r.Optim.x;
                  fun_ = r.Optim.f;
                  rmse_iv = rmse;
                  nit = r.Optim.nit;
                  converged = r.Optim.converged;
                  warm;
                }))

(** Resample a fitted surface onto a canonical grid (fixed expiries × k).
    Real listed expiries roll daily, so raw fits have incompatible latent
    dimensions; the canonical refit (warm-started, nearly exact since the
    source is itself SSVI) gives the common grid that latent PCA needs.
    Sampling error is measurable: refit RMSE is reported per day. *)
let canonical_grids (f : fit) ~expiries ~k =
  let s = Ssvi.decode f.expiries f.u in
  List.map
    (fun t ->
      {
        Quotes.t;
        k = Array.copy k;
        iv = Array.map (fun kk -> Ssvi.iv s ~k:kk ~t) k;
        w = Array.make (Array.length k) 1.;
      })
    (Array.to_list expiries)

(** Whole-history canonical refit; each day warm-starts from the previous
    canonical fit. Returns outcomes in date order. *)
let recanon fits ~expiries ~k =
  let last = ref None in
  List.map
    (fun (f : fit) ->
      let grids = canonical_grids f ~expiries ~k in
      let out = fit_day ?u0:!last grids ~date:f.date in
      (match out with
      | Ok g -> last := Some g.u
      | Failed _ -> ());
      out)
    fits

(** Whole history in order; each day warm-starts from the last success.
    A non-converged fit is still recorded (with converged=false) but does
    not seed the next day. *)
let fit_history days =
  let last_ok = ref None in
  List.map
    (fun (date, grids) ->
      let out = fit_day ?u0:!last_ok grids ~date in
      (match out with
      | Ok f -> if f.converged then last_ok := Some f.u
      | Failed _ -> ());
      out)
    days

(* --- JSON cache --- *)

let fit_to_yojson (f : fit) =
  `Assoc
    [
      ("date", `String f.date);
      ("expiries", `List (List.map (fun x -> `Float x) (Array.to_list f.expiries)));
      ("u", `List (List.map (fun x -> `Float x) (Array.to_list f.u)));
      ("fun", `Float f.fun_);
      ("rmse_iv", `Float f.rmse_iv);
      ("nit", `Int f.nit);
      ("converged", `Bool f.converged);
      ("warm", `Bool f.warm);
    ]

let fit_of_yojson j =
  let open Yojson.Basic.Util in
  {
    date = j |> member "date" |> to_string;
    expiries =
      j |> member "expiries" |> to_list |> List.map to_number |> Array.of_list;
    u = j |> member "u" |> to_list |> List.map to_number |> Array.of_list;
    fun_ = j |> member "fun" |> to_number;
    rmse_iv = j |> member "rmse_iv" |> to_number;
    nit = j |> member "nit" |> to_int;
    converged = j |> member "converged" |> to_bool;
    warm = j |> member "warm" |> to_bool;
  }

let to_yojson fits = `List (List.map fit_to_yojson fits)
let of_yojson j = List.map fit_of_yojson (Yojson.Basic.Util.to_list j)
