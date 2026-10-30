(** Holdings contract (data-model §6). Port of [core/holdings.py].

    Divergence (greenfield, agreed): the Python contract is a Parquet payload
    with DECIMAL columns. The OCaml side reads the CSV/JSON fixtures from
    [python/export_fixtures.py], which carry the same columns and decimal text.
    Money columns stay {!Money.t} (never float) at this boundary. *)

let columns =
  [
    "instrument_id";
    "quantity";
    "market_value_local";
    "currency";
    "price";
    "raw_identifier";
    "raw_identifier_type";
  ]

type holding_row = {
  instrument_id : string;
  quantity : Money.t;
  market_value_local : Money.t;
  currency : string;
  price : Money.t option;
  raw_identifier : string;
  raw_identifier_type : string;
}

(** Minimal CSV splitter: handles double-quoted fields and ["""] escapes. Our
    exporter only quotes when a field contains [,",\n]. *)
let split_csv_line line =
  let n = String.length line in
  let buf = Buffer.create 32 in
  let fields = ref [] in
  let i = ref 0 in
  let quoted = ref false in
  Buffer.clear buf;
  while !i < n do
    let c = line.[!i] in
    if !quoted then
      if c = '"' then
        if !i + 1 < n && line.[!i + 1] = '"' then (
          Buffer.add_char buf '"';
          i := !i + 2)
        else (
          quoted := false;
          incr i)
      else (
        Buffer.add_char buf c;
        incr i)
    else if c = '"' && Buffer.length buf = 0 then (
      quoted := true;
      incr i)
    else if c = ',' then (
      fields := Buffer.contents buf :: !fields;
      Buffer.clear buf;
      incr i)
    else (
      Buffer.add_char buf c;
      incr i)
  done;
  fields := Buffer.contents buf :: !fields;
  List.rev !fields

let row_of_fields = function
  | [ iid; qty; mv; ccy; price; raw; raw_ty ] ->
      {
        instrument_id = String.trim iid;
        quantity = Money.decimal_of_string qty;
        market_value_local = Money.decimal_of_string mv;
        currency = String.trim ccy;
        price =
          (if String.trim price = "" then None
           else Some (Money.decimal_of_string price));
        raw_identifier = raw;
        raw_identifier_type = raw_ty;
      }
  | fs ->
      invalid_arg
        (Printf.sprintf "holdings: expected 7 fields, got %d" (List.length fs))

let of_csv text =
  let lines =
    String.split_on_char '\n' text |> List.filter (fun l -> String.trim l <> "")
  in
  match lines with
  | [] -> invalid_arg "holdings: empty CSV"
  | header :: rows ->
      let h = split_csv_line header in
      if h <> columns then
        invalid_arg
          ("holdings columns [" ^ String.concat ";" h ^ "] != contract");
      List.map (fun l -> row_of_fields (split_csv_line l)) rows

open Yojson.Basic.Util

let row_of_yojson j =
  {
    instrument_id = j |> member "instrument_id" |> to_string;
    quantity = j |> member "quantity" |> to_string |> Money.decimal_of_string;
    market_value_local =
      j |> member "market_value_local" |> to_string |> Money.decimal_of_string;
    currency = j |> member "currency" |> to_string;
    price =
      (match j |> member "price" with
      | `Null -> None
      | `String "" -> None
      | `String s -> Some (Money.decimal_of_string s)
      | _ -> raise (Type_error ("expected string or null for price", j)));
    raw_identifier = j |> member "raw_identifier" |> to_string;
    raw_identifier_type = j |> member "raw_identifier_type" |> to_string;
  }

let holdings_of_yojson j = j |> to_list |> List.map row_of_yojson
let holdings_of_file path = holdings_of_yojson (Yojson.Basic.from_file path)
