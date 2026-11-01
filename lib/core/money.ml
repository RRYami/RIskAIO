(** Decimal-at-the-boundary helpers (invariant §3.7).

    Port of [RiskStartUp/src/riskplatform/core/money.py].

    P7.1: the canonical value is now an exact rational ([Q.t]) parsed from
    decimal text, plus its float64 image for the stochastic core. Engine math
    ([exp], [ndtr], Black/BS-2002, shocks) stays float64; accounting
    (quantities, prices, FX, NAV/PnL summation, rounding) must use [exact_of] or
    [sum_exact] so half-up quantization is exact.

    Python → OCaml error mapping:
    - [TypeError] (float ingress) is unrepresentable: the type system bans it.
    - [ValueError] (bad text, NaN/Inf, [places < 0]) → [Invalid_decimal] /
      [Invalid_argument]. *)

type t = { repr : string; exact : Q.t; value : float }

exception Invalid_decimal of string

let invalid s = raise (Invalid_decimal s)
let is_digit c = c >= '0' && c <= '9'

(** [+-]? (digits [. digits] | . digits | digits [.]) ([eE][+-]?digits)? *)
let check_syntax s =
  let n = String.length s in
  if n = 0 then false
  else
    let i = ref 0 in
    if s.[!i] = '+' || s.[!i] = '-' then incr i;
    let digits = ref 0 in
    while !i < n && is_digit s.[!i] do
      incr digits;
      incr i
    done;
    if !i < n && s.[!i] = '.' then (
      incr i;
      while !i < n && is_digit s.[!i] do
        incr digits;
        incr i
      done);
    if !digits = 0 then false
    else if !i < n && (s.[!i] = 'e' || s.[!i] = 'E') then (
      incr i;
      if !i < n && (s.[!i] = '+' || s.[!i] = '-') then incr i;
      let ed = ref 0 in
      while !i < n && is_digit s.[!i] do
        incr ed;
        incr i
      done;
      !ed > 0 && !i = n)
    else !i = n

let of_validated_text t =
  let exact =
    try Q.of_string t
    with Invalid_argument _ -> invalid ("not a decimal quantity: " ^ t)
  in
  let v = Q.to_float exact in
  if not (Float.is_finite v) then invalid ("not a finite decimal quantity: " ^ t);
  { repr = t; exact; value = v }

let decimal_of_string s =
  let t = String.trim s in
  if not (check_syntax t) then invalid ("not a decimal quantity: " ^ s);
  of_validated_text t

let decimal_of_int i =
  { repr = string_of_int i; exact = Q.of_int i; value = float_of_int i }

(** Explicit decimal → float64 conversion for engine input. Single choke point.
*)
let float_of t = t.value

(** Exact rational value for accounting. *)
let exact_of t = t.exact

(** Exact sum of money values, avoiding float64 accumulation error. *)
let sum_exact ts = List.fold_left (fun acc t -> Q.add acc t.exact) Q.zero ts

let to_string t = t.repr

(** Quantize to fixed places (default cents, ROUND_HALF_UP away from zero).
    Computed on the exact rational so e.g. "2.675" → "2.68" regardless of binary
    float representation. *)
let quantize_amount t places =
  if places < 0 then invalid_arg "places must be >= 0";
  let factor = Z.pow (Z.of_int 10) places in
  let scaled = Q.mul t.exact (Q.of_bigint factor) in
  let num = Q.num scaled and den = Q.den scaled in
  let sign = if Z.sign num < 0 then -1 else 1 in
  let anum = Z.abs num in
  (* round half up on |scaled|: floor((2*|num| + den) / (2*den)) *)
  let rounded_abs =
    Z.div (Z.add (Z.mul (Z.of_int 2) anum) den) (Z.mul (Z.of_int 2) den)
  in
  let rounded = if sign < 0 then Z.neg rounded_abs else rounded_abs in
  let q = Q.make rounded factor in
  let v = Q.to_float q in
  if not (Float.is_finite v) then
    invalid ("not a finite decimal quantity: " ^ t.repr);
  { repr = Printf.sprintf "%.*f" places v; value = v; exact = q }
