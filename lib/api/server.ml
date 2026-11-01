(** REST + JSON API (P7d). Thin HTTP skin over [Service]: JSON in, job out.
    All engine-input parsing reuses the existing fixture bridges, so the wire
    contract is exactly the file contract.

    Routes:
    - GET  /health                  liveness
    - POST /api/runs                {as_of, base_currency, holdings,
                                     instruments, marketdata, config,
                                     surf_fits?} -> 202 {id, status, ...}
    - GET  /api/runs                [{id, status, ...}] (no rows)
    - GET  /api/runs/:id            full job (rows included)
    - GET  /api/runs/:id/pnl?h=h1   {"horizon", "pnl": [...]}
    - DELETE /api/runs/:id          cancel *)

open Risk_core
open Risk_engine

let json_response ?(status = `OK) j =
  let headers = Cohttp.Header.init_with "content-type" "application/json" in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status
    ~body:(Yojson.Basic.to_string j) ()

let bad_request msg = json_response ~status:`Bad_request (`Assoc [ ("error", `String msg) ])
let not_found msg = json_response ~status:`Not_found (`Assoc [ ("error", `String msg) ])

let parse_inputs body =
  let open Yojson.Basic.Util in
  let j = Yojson.Basic.from_string body in
  let holdings = j |> member "holdings" |> Holdings.holdings_of_yojson in
  let instruments =
    j |> member "instruments" |> Refdata.instruments_of_yojson
  in
  let bundle = j |> member "marketdata" |> Marketdata.bundle_of_yojson in
  let config = j |> member "config" |> Config.of_yojson in
  Config.validate config;
  let surf_fits =
    match j |> member "surf_fits" with
    | `Null -> []
    | `Assoc l ->
        List.map (fun (u, v) -> (u, Ssvi_calibration.of_yojson v)) l
    | _ -> invalid_arg "surf_fits must be an object"
  in
  let as_of = j |> member "as_of" |> to_string |> Calendar.of_string in
  let base_currency = j |> member "base_currency" |> to_string in
  ( Run.{ holdings; instruments; bundle; as_of; base_currency; row_portfolios = None; surf_fits },
    config )

let callback svc _conn req body =
  let open Lwt.Infix in
  let uri = Cohttp.Request.uri req in
  let path =
    Uri.path uri |> String.split_on_char '/' |> List.filter (fun s -> s <> "")
  in
  match Cohttp.Request.meth req, path with
  | `GET, [ "health" ] ->
      json_response (`Assoc [ ("status", `String "ok"); ("service", `String "risk-ai-api") ])
  | `POST, [ "api"; "runs" ] -> (
      Cohttp_lwt.Body.to_string body >>= fun s ->
      match
        try Ok (parse_inputs s) with
        | Yojson.Basic.Util.Type_error (m, _) -> Error ("bad JSON: " ^ m)
        | Yojson.Json_error m -> Error ("bad JSON: " ^ m)
        | Invalid_argument m -> Error m
        | Failure m -> Error m
        | Calendar.Invalid_date m -> Error ("bad date: " ^ m)
      with
      | Error e -> bad_request e
      | Ok (inputs, config) -> (
          Lwt.catch
            (fun () ->
              Service.submit svc ~inputs ~config >>= fun job ->
              json_response ~status:`Accepted (Job.to_yojson ~with_rows:false job))
            (function
              | Risk_engine.Run.Engine_config_error m -> bad_request m
              | exn -> bad_request (Printexc.to_string exn))))
  | `GET, [ "api"; "runs" ] ->
      Service.list svc >>= fun jobs ->
      json_response (`List (List.map (Job.to_yojson ~with_rows:false) jobs))
  | `GET, [ "api"; "runs"; id ] -> (
      Service.get svc id >>= function
      | None -> not_found ("no such run: " ^ id)
      | Some job -> json_response (Job.to_yojson job))
  | `GET, [ "api"; "runs"; id; "pnl" ] -> (
      Service.get svc id >>= function
      | None -> not_found ("no such run: " ^ id)
      | Some job -> (
          match Uri.get_query_param uri "h" with
          | None -> bad_request "missing query param h (e.g. ?h=h1)"
          | Some h -> (
              match List.assoc_opt h job.Job.pnl with
              | None -> not_found ("no pnl for horizon " ^ h)
              | Some a ->
                  json_response
                    (`Assoc
                      [
                        ("horizon", `String h);
                        ( "pnl",
                          `List (List.map (fun x -> `Float x) (Array.to_list a))
                        );
                      ]))))
  | `DELETE, [ "api"; "runs"; id ] -> (
      Lwt.catch
        (fun () ->
          Service.cancel svc id >>= fun job ->
          json_response (Job.to_yojson ~with_rows:false job))
        (function Not_found -> not_found ("no such run: " ^ id) | exn -> bad_request (Printexc.to_string exn)))
  | _, _ -> not_found "unknown route"

let start svc ~port =
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback:(callback svc) ())
