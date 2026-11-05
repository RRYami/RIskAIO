(** Plain security-master records (ADR-013). Port of [core/refdata.py].

    Kept in core so the engine receives a materialised view without any database
    dependency (ADR-007 engine purity).

    P7b: [inst_type] is a closed 9-type variant; the JSON wire strings are
    unchanged. Missing required terms fail at parse time via [validate_terms]
    instead of surfacing deep inside pricing. *)

(** The 9 supported instrument types (Q19 terms contract). *)
type inst_type =
  | Equity
  | Etf
  | Fund
  | Future
  | Listed_option
  | Gov_bond
  | Corp_bond
  | Cash
  | Fx_forward

let inst_type_of_string = function
  | "EQUITY" -> Equity
  | "ETF" -> Etf
  | "FUND" -> Fund
  | "FUTURE" -> Future
  | "LISTED_OPTION" -> Listed_option
  | "GOV_BOND" -> Gov_bond
  | "CORP_BOND" -> Corp_bond
  | "CASH" -> Cash
  | "FX_FORWARD" -> Fx_forward
  | s -> invalid_arg ("unknown instrument type: " ^ s)

let string_of_inst_type = function
  | Equity -> "EQUITY"
  | Etf -> "ETF"
  | Fund -> "FUND"
  | Future -> "FUTURE"
  | Listed_option -> "LISTED_OPTION"
  | Gov_bond -> "GOV_BOND"
  | Corp_bond -> "CORP_BOND"
  | Cash -> "CASH"
  | Fx_forward -> "FX_FORWARD"

(** Required terms per type (presence-checked; value validation stays in
    pricing, which reports the holding row). Linear/cash need no terms. *)
let required_terms = function
  | Equity | Etf | Fund | Cash -> []
  | Future -> [ "expiry"; "multiplier"; "underlying_instrument_id" ]
  | Listed_option ->
      [
        "expiry";
        "strike";
        "call_put";
        "style";
        "multiplier";
        "underlying_instrument_id";
      ]
  | Gov_bond | Corp_bond ->
      [
        "maturity";
        "coupon_rate";
        "coupon_frequency";
        "country";
        "issuer";
        "issuer_rating";
        "bond_rating";
        "issue_date";
        (* par: [par_value] canonical, [notional] accepted as alias — checked
           in [validate_terms], not here, so exactly one of them is required. *)
      ]
  | Fx_forward ->
      [
        "settlement_date";
        "buy_currency";
        "buy_notional";
        "sell_currency";
        "sell_notional";
      ]

let validate_terms ty terms =
  List.iter
    (fun k ->
      if List.assoc_opt k terms = None then
        invalid_arg
          (Printf.sprintf "%s: missing required term %s"
             (string_of_inst_type ty) k))
    (required_terms ty);
  (match ty with
  | Gov_bond | Corp_bond ->
      let has_par =
        List.assoc_opt "par_value" terms <> None
        || List.assoc_opt "notional" terms <> None
      in
      if not has_par then
        invalid_arg
          (Printf.sprintf "%s: missing required term par_value (or notional)"
             (string_of_inst_type ty))
  | _ -> ())

type instrument_record = {
  instrument_id : string;
  inst_type : inst_type;
  currency : string;
  terms : (string * Yojson.Basic.t) list;
  overlay : bool;
  base_instrument_id : string option;
}

type identifier_record = {
  instrument_id : string;
  scheme : string;
  value : string;
  valid_from : Calendar.date;
  valid_to : Calendar.date option;
  overlay : bool;
}

(** Half-open validity: [valid_from <= d < valid_to]. *)
let valid_on r d =
  Calendar.compare r.valid_from d <= 0
  && match r.valid_to with None -> true | Some e -> Calendar.compare d e < 0

type secmaster_view = {
  tenant_id : string option;
  global_version : int;
  overlay_version : int;
  instruments : (string * instrument_record) list;
  identifiers : identifier_record list;
}

let find_instrument v id = List.assoc_opt id v.instruments

(* --- JSON fixture bridge (instruments.json from export_fixtures.py) --- *)

open Yojson.Basic.Util

let instrument_record_of_yojson j =
  let ty = inst_type_of_string (j |> member "type" |> to_string) in
  let terms = j |> member "terms" |> to_assoc in
  validate_terms ty terms;
  {
    instrument_id = j |> member "instrument_id" |> to_string;
    inst_type = ty;
    currency = j |> member "currency" |> to_string;
    terms;
    overlay =
      (match j |> member "overlay" with `Null -> false | b -> to_bool b);
    base_instrument_id =
      (match j |> member "base_instrument_id" with
      | `Null -> None
      | s -> Some (to_string s));
  }

let instruments_of_yojson j =
  j |> to_list
  |> List.map (fun i ->
      (i |> member "instrument_id" |> to_string, instrument_record_of_yojson i))

let instruments_of_file path =
  instruments_of_yojson (Yojson.Basic.from_file path)
