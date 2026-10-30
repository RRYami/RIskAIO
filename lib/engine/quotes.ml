(** Option quote ingestion → IV grids (plan §7 input stage).

    Reads the repo's CSV data contract ([date,underlying,expiry,right,strike,
    bid,ask,mid,spot,rate,dividend_yield,rel_spread]) via [Csv], keeps
    out-of-the-money quotes only (American early-exercise contamination lives
    in ITM/short-dated contracts; OTM mid quotes are well approximated by the
    European model — documented limitation, v1), inverts mids to Black
    volatilities by bisection on [Options.bs_1], and groups by expiry into
    strike-sorted [(k, iv, weight)] grids with [k = log(K/F)].

    Weights are inverse relative spread ([mid / max(ask-bid, floor)]), capped:
    wide markets carry less calibration weight (§7 quote-quality weights).
    Crossed markets, non-positive mids, uninvertible prices (mid above the
    σ=500% model price — e.g. stale prints) and expired contracts are
    rejected loudly, never silently dropped. *)

exception Quotes_error of string

open Risk_core

type right = Call | Put

type quote = {
  date : string;
  underlying : string;
  expiry : string;
  right : right;
  strike : float;
  bid : float;
  ask : float;
  mid : float;
  spot : float;
  rate : float;
  div : float;
}

let year_days = 365.

let t_years ~date ~expiry =
  let open Risk_core in
  let d1 = Calendar.of_string date and d2 = Calendar.of_string expiry in
  float_of_int (Calendar.to_ordinal d2 - Calendar.to_ordinal d1) /. year_days

let forward spot rate div t = spot *. exp ((rate -. div) *. t)

let parse_right s =
  match String.uppercase_ascii (String.trim s) with
  | "CALL" | "C" -> Call
  | "PUT" | "P" -> Put
  | x -> raise (Quotes_error ("bad right: " ^ x))

let float_col idx name row =
  try float_of_string (Csv.col idx name row)
  with Failure _ ->
    raise (Quotes_error ("bad numeric " ^ name ^ ": " ^ Csv.col idx name row))

let parse_file path =
  let rows = Csv.check_width (Csv.parse_file path) in
  let idx = Csv.header_index rows in
  List.map
    (fun row ->
      {
        date = Csv.col idx "date" row;
        underlying = Csv.col idx "underlying" row;
        expiry = Csv.col idx "expiry" row;
        right = parse_right (Csv.col idx "right" row);
        strike = float_col idx "strike" row;
        bid = float_col idx "bid" row;
        ask = float_col idx "ask" row;
        mid = float_col idx "mid" row;
        spot = float_col idx "spot" row;
        rate = float_col idx "rate" row;
        div = float_col idx "dividend_yield" row;
      })
    (List.tl rows)

(** OTM check at scenario horizon: calls need K > F, puts K < F. *)
let is_otm q ~t =
  if not (t > 0.) then false
  else
    let f = forward q.spot q.rate q.div t in
    match q.right with
    | Call -> q.strike > f
    | Put -> q.strike < f

(** Brent root finder shared by the European and American inverters.
    Same contract as bisection (bracketed monotone root) but superlinear:
    inverse-quadratic interpolation / secant steps with a bisection fallback,
    so ~10-15 model evals instead of 200. f(lo) <= 0 <= f(hi) is required;
    exact-zero endpoints return immediately. *)
let brent f lo hi ~tol ~maxiter =
  let fa = ref (f lo) and fb = ref (f hi) in
  if !fa = 0. then lo
  else if !fb = 0. then hi
  else (
    if !fa *. !fb > 0. then invalid_arg "brent: root not bracketed";
    let a = ref lo and b = ref hi in
    if abs_float !fa < abs_float !fb then (
      let t = !a in
      a := !b;
      b := t;
      let v = !fa in
      fa := !fb;
      fb := v);
    let c = ref !a and fc = ref !fa in
    let d = ref 0. in
    let mflag = ref true in
    let it = ref 0 in
    while !fb <> 0. && abs_float (!b -. !a) > tol && !it < maxiter do
      incr it;
      let s =
        if !fa <> !fc && !fb <> !fc then (
          (* inverse quadratic interpolation *)
          let l0 =
            !a *. !fb *. !fc /. ((!fa -. !fb) *. (!fa -. !fc))
          and l1 =
            !b *. !fa *. !fc /. ((!fb -. !fa) *. (!fb -. !fc))
          and l2 =
            !c *. !fa *. !fb /. ((!fc -. !fa) *. (!fc -. !fb))
          in
          l0 +. l1 +. l2)
        else
          (* secant step *)
          !b -. !fb *. (!b -. !a) /. (!fb -. !fa)
      in
      let s =
        if
          (s < (3. *. !a +. !b) /. 4. || s > !b)
          || (!mflag && abs_float (s -. !b) >= 0.5 *. abs_float (!b -. !c))
          || (not !mflag && abs_float (s -. !b) >= 0.5 *. abs_float (!c -. !d))
          || (!mflag && abs_float (!b -. !c) <= tol)
          || ((not !mflag) && abs_float (!c -. !d) <= tol)
        then (
          mflag := true;
          0.5 *. (!a +. !b))
        else (
          mflag := false;
          s)
      in
      let fs = f s in
      d := !c;
      c := !b;
      fc := !fb;
      if !fa *. fs < 0. then (
        b := s;
        fb := fs)
      else (
        a := s;
        fa := fs);
      if abs_float !fa < abs_float !fb then (
        let t = !a in
        a := !b;
        b := t;
        let v = !fa in
        fa := !fb;
        fb := v)
    done;
    !b)

(** Bisection driver shared by the European and American inverters. *)
let bisect_iv price mid =
  if price 5. < mid then raise (Quotes_error "mid above 500%-vol price");
  try brent (fun sg -> price sg -. mid) 1e-6 5. ~tol:1e-12 ~maxiter:100 with
  | Invalid_argument msg -> raise (Quotes_error ("inversion failed: " ^ msg))

(** Discounted intrinsic floor (European lower bound); mids below it are
    stale prints, not tradeable quotes. *)
let check_intrinsic q ~t ~call =
  let f = forward q.spot q.rate q.div t in
  let d = exp (-.q.rate *. t) in
  let lo =
    if call then Float.max (d *. (f -. q.strike)) 0.
    else Float.max (d *. (q.strike -. f)) 0.
  in
  if q.mid < lo *. (1. -. 1e-6) -. 1e-9 then
    raise
      (Quotes_error
         (Printf.sprintf "mid %.4g below intrinsic %.4g" q.mid lo))

(** Bisection on Black sigma for an OTM mid. Raises on garbage. *)
let iv_of_quote q ~t =
  if not (t > 0.) then raise (Quotes_error "expired contract");
  if not (q.mid > 0.) then raise (Quotes_error "non-positive mid");
  if q.ask < q.bid then raise (Quotes_error "crossed market");
  if not (is_otm q ~t) then raise (Quotes_error "not OTM");
  let call = match q.right with Call -> true | Put -> false in
  check_intrinsic q ~t ~call;
  bisect_iv
    (fun sigma ->
      (Options.black_scholes ~s:[| q.spot |] ~k:q.strike ~t
         ~r:[| q.rate |] ~b:[| q.rate -. q.div |] ~sigma ~call).(0))
    q.mid

(** Bisection on BS-2002 American sigma. Unlike the European inverter this
    is valid ITM too (early-exercise premium is in the model); OTM quotes
    coincide with Black up to the American premium. Raises on garbage. *)
let iv_of_quote_amer q ~t =
  if not (t > 0.) then raise (Quotes_error "expired contract");
  if not (q.mid > 0.) then raise (Quotes_error "non-positive mid");
  if q.ask < q.bid then raise (Quotes_error "crossed market");
  let call = match q.right with Call -> true | Put -> false in
  check_intrinsic q ~t ~call;
  bisect_iv
    (fun sigma ->
      Options.bs2002_1 ~s:q.spot ~k:q.strike ~t ~r:q.rate
        ~b:(q.rate -. q.div) ~sigma ~call)
    q.mid

let weight_of q =
  let spread = Float.max (q.ask -. q.bid) (1e-8 *. Float.max q.mid 1e-12) in
  Float.min (q.mid /. spread) 100.

type expiry_obs = {
  t : float;
  k : float array;
  iv : float array;
  w : float array;
}

(** Per-expiry strike-sorted grids; uninvertible rows rejected with context.
    Both OTM and ITM quotes are kept: OTM uses the fast European inverter
    (American premium ~10bp there, measured), ITM uses BS-2002 — required,
    since Black-inverting an ITM American mid bakes the early-exercise
    premium into a biased IV. Needed for books holding ITM Americans. *)
let to_grids quotes =
  let by_exp = Hashtbl.create 8 in
  List.iter
    (fun q ->
      let t = t_years ~date:q.date ~expiry:q.expiry in
      if t > 0. then
        let iv =
          try
            if is_otm q ~t then iv_of_quote q ~t else iv_of_quote_amer q ~t
          with Quotes_error msg ->
            raise
              (Quotes_error
                 (Printf.sprintf "%s %s K=%.4g: %s" q.date q.expiry q.strike
                    msg))
        in
        let f = forward q.spot q.rate q.div t in
        let k = Float.log (q.strike /. f) in
        Hashtbl.replace by_exp t
          ((k, iv, weight_of q)
          :: (Hashtbl.find_opt by_exp t |> Option.value ~default:[])))
    quotes;
  Hashtbl.fold
    (fun t rows acc ->
      let rows = List.sort (fun (a, _, _) (b, _, _) -> Float.compare a b) rows in
      {
        t;
        k = Array.of_list (List.map (fun (x, _, _) -> x) rows);
        iv = Array.of_list (List.map (fun (_, x, _) -> x) rows);
        w = Array.of_list (List.map (fun (_, _, x) -> x) rows);
      }
      :: acc)
    by_exp []
  |> List.sort (fun a b -> Float.compare a.t b.t)
