(** Decimal-at-the-boundary helpers (invariant §3.7).

    Port of [RiskStartUp/src/riskplatform/core/money.py].

    Engine-first note: full arbitrary precision is deferred to P7 (zarith). This
    module stores the canonical decimal text plus its float64 image and enforces
    the same ingress rules: no float input, finite only.

    Python → OCaml error mapping:
    - [TypeError] (float ingress) is unrepresentable: the type system bans it.
    - [ValueError] (bad text, NaN/Inf, [places < 0]) → [Invalid_decimal] /
      [Invalid_argument]. *)

type t = { repr : string; value : float }

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

let decimal_of_string s =
  let t = String.trim s in
  if not (check_syntax t) then invalid ("not a decimal quantity: " ^ s);
  let v =
    try float_of_string t
    with Failure _ -> invalid ("not a decimal quantity: " ^ s)
  in
  if not (Float.is_finite v) then invalid ("not a finite decimal quantity: " ^ s);
  { repr = t; value = v }

let decimal_of_int i = { repr = string_of_int i; value = float_of_int i }

(** Explicit decimal → float64 conversion for engine input. Single choke point.
*)
let float_of t = t.value

let to_string t = t.repr

(** Quantize to fixed places (default cents, ROUND_HALF_UP away from zero). *)
let quantize_amount t places =
  if places < 0 then invalid_arg "places must be >= 0";
  let factor = 10. ** float_of_int places in
  let scaled = t.value *. factor in
  if not (Float.is_finite scaled) then
    invalid ("not a finite decimal quantity: " ^ t.repr);
  let r =
    if scaled >= 0. then Float.floor (scaled +. 0.5)
    else Float.ceil (scaled -. 0.5)
  in
  let q = r /. factor in
  { repr = Printf.sprintf "%.*f" places q; value = q }
