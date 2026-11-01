(** Run records (P7d). A run is an async [Run.run_engine] execution: the inputs
    and config are held on the live job so the worker is self-contained. Rows
    are stored pre-serialised ([Run.as_json]) so persistence is a plain JSON
    round-trip with no result parser to maintain. *)

open Risk_core
open Risk_engine

type status =
  | Queued
  | Running
  | Done
  | Cancelled
  | Failed of string

let string_of_status = function
  | Queued -> "queued"
  | Running -> "running"
  | Done -> "done"
  | Cancelled -> "cancelled"
  | Failed _ -> "failed"

let status_of_string s err =
  match s with
  | "queued" -> Queued
  | "running" -> Running
  | "done" -> Done
  | "cancelled" -> Cancelled
  | "failed" -> Failed err
  | _ -> invalid_arg ("unknown run status: " ^ s)

type t = {
  id : string;
  created : float;
  mutable finished : float option;
  as_of : Calendar.date;
  base_currency : string;
  inputs : Run.engine_inputs;
  config : Config.run_config;
  mutable status : status;
  mutable rows : Yojson.Basic.t list;
  mutable pnl : (string * float array) list;
  mutable diagnostics : (string * Yojson.Basic.t) list;
}

let is_terminal = function
  | Done | Cancelled | Failed _ -> true
  | Queued | Running -> false

let error_of_status = function Failed s -> Some s | _ -> None

let to_yojson ?(with_rows = true) (j : t) =
  `Assoc
    ([
       ("id", `String j.id);
       ("status", `String (string_of_status j.status));
       ("as_of", `String (Calendar.to_string j.as_of));
       ("base_currency", `String j.base_currency);
       ("created", `Float j.created);
       ("finished", match j.finished with None -> `Null | Some f -> `Float f);
       ( "error",
         match error_of_status j.status with None -> `Null | Some s -> `String s
       );
       ("diagnostics", `Assoc j.diagnostics);
       ("pnl_horizons", `List (List.map (fun (h, _) -> `String h) j.pnl));
     ]
    @ if with_rows then [ ("rows", `List j.rows) ] else [])

(** Empty placeholder inputs for jobs reloaded from persistence. Reloaded jobs
    are results-audit records: resuming them is future work. *)
let placeholder_inputs as_of base_currency =
  Run.
    {
      holdings = [];
      instruments = [];
      bundle =
        Marketdata.
          {
            prices = [];
            fx = [];
            curves = [];
            vols = [];
            spreads = [];
            dividends = [];
          };
      as_of;
      base_currency;
      row_portfolios = None;
      surf_fits = [];
    }

(** Full persistence image (config + result). Stored as one JSONB document by
    the Postgres backend; the memory backend keeps the value. *)
let payload_to_yojson (j : t) =
  `Assoc
    [
      ("id", `String j.id);
      ("created", `Float j.created);
      ("finished", match j.finished with None -> `Null | Some f -> `Float f);
      ("status", `String (string_of_status j.status));
      ( "error",
        match error_of_status j.status with None -> `Null | Some s -> `String s
      );
      ("as_of", `String (Calendar.to_string j.as_of));
      ("base_currency", `String j.base_currency);
      ("config", Config.to_yojson j.config);
      ("diagnostics", `Assoc j.diagnostics);
      ("rows", `List j.rows);
      ( "pnl",
        `Assoc
          (List.map
             (fun (h, a) ->
               (h, `List (List.map (fun x -> `Float x) (Array.to_list a))))
             j.pnl) );
    ]

let payload_of_yojson (j : Yojson.Basic.t) =
  let open Yojson.Basic.Util in
  let to_number = function
    | `Float x -> x
    | `Int i -> float_of_int i
    | _ -> invalid_arg "payload: expected number"
  in
  let str k = j |> member k |> to_string in
  let status_s = str "status" in
  let err =
    match j |> member "error" with `Null -> "" | s -> to_string s
  in
  let finished =
    match j |> member "finished" with `Null -> None | f -> Some (to_number f)
  in
  let as_of = Calendar.of_string (str "as_of") in
  let base_currency = str "base_currency" in
  {
    id = str "id";
    created = j |> member "created" |> to_number;
    finished;
    as_of;
    base_currency;
    inputs = placeholder_inputs as_of base_currency;
    config = j |> member "config" |> Config.of_yojson;
    status = status_of_string status_s err;
    rows = j |> member "rows" |> to_list;
    pnl =
      j |> member "pnl" |> to_assoc
      |> List.map (fun (h, v) ->
          (h, Array.of_list (v |> to_list |> List.map to_number)));
    diagnostics = j |> member "diagnostics" |> to_assoc;
  }
