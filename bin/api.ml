(** risk-ai-api: REST + JSON worker for the risk engine (P7d).

    Memory store by default; pass --db-url postgresql://... to persist runs
    in Postgres (runs db/runs.sql migration first).

    usage: risk-ai-api [--port N] [--db-url URL] *)

open Risk_api

let () =
  let port = ref 8080 and db_url = ref "" in
  Arg.parse
    [
      ("--port", Arg.Set_int port, "N listen port (default 8080)");
      ("--db-url", Arg.Set_string db_url, "URL Postgres connection string");
    ]
    (fun _ -> ())
    "risk-ai-api [--port N] [--db-url URL]";
  let svc =
    if !db_url = "" then Service.make (Service.of_mem (Store.Mem.make ()))
    else
      let conn =
        Lwt_main.run
          (let open Lwt.Infix in
           Pg.connect !db_url >>= fun c ->
           Pg.migrate c >>= fun () -> Lwt.return c)
      in
      Service.make (Service.of_pg conn)
  in
  Printf.printf "risk-ai-api listening on :%d (store=%s)\n%!" !port
    (if !db_url = "" then "memory" else "postgres");
  Lwt_main.run (Server.start svc ~port:!port)
