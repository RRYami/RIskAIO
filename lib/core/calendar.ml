(** TARGET2 business calendar (Q9): weekdays minus 1 Jan, Good Friday, Easter
    Monday, 1 May, 25/26 Dec. Port of [core/calendar.py].

    Dates are proleptic Gregorian records; conversion to/from day counts uses
    Howard Hinnant's civil algorithms (no [Unix] time involved, so results are
    independent of timezones — matching the Python date arithmetic). *)

type date = { y : int; m : int; d : int }

exception Invalid_date of string

let invalid_date s = raise (Invalid_date s)

(** Days since 1970-01-01. *)
let days_from_civil y m d =
  let y' = if m <= 2 then y - 1 else y in
  let era = (if y' >= 0 then y' else y' - 399) / 400 in
  let yoe = y' - (era * 400) in
  let mp = (m + 9) mod 12 in
  let doy = (((153 * mp) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let civil_from_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  ((y + if m <= 2 then 1 else 0), m, d)

let is_leap y = y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0)

let days_in_month y m =
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap y then 29 else 28
  | _ -> invalid_date (Printf.sprintf "month out of range: %d" m)

let make y m d =
  if m < 1 || m > 12 then
    invalid_date (Printf.sprintf "month out of range: %d" m);
  let dim = days_in_month y m in
  if d < 1 || d > dim then
    invalid_date (Printf.sprintf "day out of range: %04d-%02d-%02d" y m d);
  { y; m; d }

let of_string s =
  if String.length s <> 10 || s.[4] <> '-' || s.[7] <> '-' then
    invalid_date ("not a YYYY-MM-DD date: " ^ s);
  let num i j =
    let v = ref 0 in
    for k = i to j do
      let c = s.[k] in
      if c < '0' || c > '9' then invalid_date ("not a YYYY-MM-DD date: " ^ s);
      v := (!v * 10) + (Char.code c - Char.code '0')
    done;
    !v
  in
  make (num 0 3) (num 5 6) (num 8 9)

let to_string dt = Printf.sprintf "%04d-%02d-%02d" dt.y dt.m dt.d
let to_ordinal dt = days_from_civil dt.y dt.m dt.d

let of_ordinal z =
  let y, m, d = civil_from_days z in
  { y; m; d }

let compare a b = Int.compare (to_ordinal a) (to_ordinal b)
let equal a b = compare a b = 0

(** Monday = 0 .. Sunday = 6. 1970-01-01 (ordinal 0) was a Thursday. *)
let weekday dt =
  let w = (to_ordinal dt + 3) mod 7 in
  if w < 0 then w + 7 else w

(** Gregorian Easter (anonymous Gregorian algorithm / Meeus). *)
let easter_sunday year =
  let a = year mod 19 in
  let b = year / 100 in
  let c = year mod 100 in
  let d = b / 4 in
  let e = b mod 4 in
  let f = (b + 8) / 25 in
  let g = (b - f + 1) / 3 in
  let h = ((19 * a) + b - d - g + 15) mod 30 in
  let i = c / 4 in
  let k = c mod 4 in
  let ell = (32 + (2 * e) + (2 * i) - h - k) mod 7 in
  let m = (a + (11 * h) + (22 * ell)) / 451 in
  let x = h + ell - (7 * m) + 114 in
  { y = year; m = x / 31; d = (x mod 31) + 1 }

let target2_holidays year =
  let e = to_ordinal (easter_sunday year) in
  List.map of_ordinal
    [
      days_from_civil year 1 1;
      e - 2;
      e + 1;
      days_from_civil year 5 1;
      days_from_civil year 12 25;
      days_from_civil year 12 26;
    ]

let is_business_day dt =
  weekday dt < 5 && not (List.exists (equal dt) (target2_holidays dt.y))

let previous_business_day dt =
  let d = ref (to_ordinal dt - 1) in
  while not (is_business_day (of_ordinal !d)) do
    decr d
  done;
  of_ordinal !d

(** The [count] business days ending at [as_of] (inclusive), ascending. [as_of]
    must itself be a business day. *)
let business_days_ending as_of count =
  if count < 1 then invalid_arg "count must be >= 1";
  if not (is_business_day as_of) then
    invalid_date (to_string as_of ^ " is not a TARGET2 business day");
  let days = ref [ as_of ] in
  while List.length !days < count do
    days := previous_business_day (List.hd !days) :: !days
  done;
  !days

(** TARGET2 business days in (start, end\] (0 if [end_] <= [start]). *)
let business_days_between start end_ =
  if compare end_ start <= 0 then 0
  else
    let count = ref 0 in
    let d = ref (to_ordinal start + 1) in
    let last = to_ordinal end_ in
    while !d <= last do
      if is_business_day (of_ordinal !d) then incr count;
      incr d
    done;
    !count

(** [d] moved forward by [n] TARGET2 business days ([n = 0] → [d]). Negative [n]
    is a no-op, matching Python's [range(n)] semantics. *)
let add_business_days d n =
  let out = ref (to_ordinal d) in
  for _ = 1 to n do
    incr out;
    while not (is_business_day (of_ordinal !out)) do
      incr out
    done
  done;
  of_ordinal !out
