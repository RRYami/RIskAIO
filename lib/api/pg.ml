(** Postgres run store (P7d). Implements [Store.STORE] on the [risk_runs]
    table ([db/runs.sql]); results are JSONB documents, so reads never touch
    the engine. Connect with a [DATABASE_URL] like
    [postgresql://user:pass@host:5432/db]. *)

open Lwt.Infix

let migrate_sql =
  "CREATE TABLE IF NOT EXISTS risk_runs (id TEXT PRIMARY KEY, status TEXT \
   NOT NULL, updated DOUBLE PRECISION NOT NULL, payload JSONB NOT NULL)"

let migrate_index_sql =
  "CREATE INDEX IF NOT EXISTS risk_runs_updated_idx ON risk_runs \
   (updated DESC)"

let upsert_sql =
  "INSERT INTO risk_runs (id, status, updated, payload) VALUES (?, ?, ?, \
   ?::jsonb) ON CONFLICT (id) DO UPDATE SET status = EXCLUDED.status, \
   updated = EXCLUDED.updated, payload = EXCLUDED.payload"

let find_sql = "SELECT payload FROM risk_runs WHERE id = ?"
let list_sql = "SELECT payload FROM risk_runs ORDER BY updated DESC LIMIT 200"

let fail_wrap f =
  Lwt.bind f (function
    | Ok v -> Lwt.return v
    | Error e -> Lwt.fail_with ("db: " ^ Caqti.Error.show e))

type t = Caqti_lwt.connection

let connect url = fail_wrap (Caqti_lwt_unix.connect (Uri.of_string url))

open Caqti_request.Infix

let migrate_req = Caqti_type.(unit ->. unit) migrate_sql

let migrate_index_req = Caqti_type.(unit ->. unit) migrate_index_sql

let upsert_req =
  Caqti_type.(t4 string string float string ->. unit) upsert_sql

let find_req = Caqti_type.(string ->? string) find_sql
let list_req = Caqti_type.(unit ->* string) list_sql

let migrate (conn : t) =
  let module C = (val conn : Caqti_lwt.CONNECTION) in
  fail_wrap
    (C.exec migrate_req () >>= function
     | Error _ as e -> Lwt.return e
     | Ok () -> C.exec migrate_index_req ())

let save (conn : t) (j : Job.t) =
  let module C = (val conn : Caqti_lwt.CONNECTION) in
  let payload = Yojson.Basic.to_string (Job.payload_to_yojson j) in
  let updated = Unix.gettimeofday () in
  fail_wrap
    (C.exec upsert_req
       (j.Job.id, Job.string_of_status j.Job.status, updated, payload))

let of_payload s =
  try Some (Job.payload_of_yojson (Yojson.Basic.from_string s))
  with _ -> None

let create conn j = save conn j
let update conn j = save conn j

let get (conn : t) id =
  let module C = (val conn : Caqti_lwt.CONNECTION) in
  fail_wrap (C.find_opt find_req id)
  |> Lwt.map (fun opt -> Option.bind opt of_payload)

let list (conn : t) =
  let module C = (val conn : Caqti_lwt.CONNECTION) in
  fail_wrap (C.collect_list list_req ())
  |> Lwt.map (List.filter_map of_payload)
