(** Plain security-master records (ADR-013). Port of [core/refdata.py].

    Kept in core so the engine receives a materialised view without any database
    dependency (ADR-007 engine purity).

    Engine-first note: [inst_type] stays a string until P3 pins the 9-type
    taxonomy against the Q19 terms contract; [terms] is schemaless JSON until
    then (the Python fixture exporter labels this [provisional-v1]). *)

type instrument_record = {
  instrument_id : string;
  inst_type : string;
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
  {
    instrument_id = j |> member "instrument_id" |> to_string;
    inst_type = j |> member "type" |> to_string;
    currency = j |> member "currency" |> to_string;
    terms = j |> member "terms" |> to_assoc;
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
