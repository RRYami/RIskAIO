(** Run service (P7d). Submits [Run.run_engine] executions as background jobs
    on a [Store.STORE], with cooperative cancellation through the engine's
    [should_cancel] thunk. The [runner] is injectable so tests can drive
    cancellation deterministically without running the engine. *)

open Risk_engine

type runner =
  Run.engine_inputs ->
  Config.run_config ->
  should_cancel:(unit -> bool) option ->
  Run.engine_result

let default_runner inputs config ~should_cancel =
  Run.run_engine inputs config ~should_cancel ()

(** First-class store (implemented by [Store.Mem] and [Pg]). *)
type store = {
  create : Job.t -> unit Lwt.t;
  update : Job.t -> unit Lwt.t;
  get : string -> Job.t option Lwt.t;
  list : unit -> Job.t list Lwt.t;
}

let of_mem (m : Store.Mem.t) =
  {
    create = Store.Mem.create m;
    update = Store.Mem.update m;
    get = Store.Mem.get m;
    list = (fun () -> Store.Mem.list m);
  }

let of_pg (conn : Pg.t) =
  {
    create = Pg.create conn;
    update = Pg.update conn;
    get = Pg.get conn;
    list = (fun () -> Pg.list conn);
  }

type t = {
  store : store;
  flags : (string, bool Atomic.t) Hashtbl.t;
  mu : Lwt_mutex.t;
  mutable seq : int;
}

let make store = { store; flags = Hashtbl.create 16; mu = Lwt_mutex.create (); seq = 0 }

let next_id svc =
  Lwt_mutex.with_lock svc.mu (fun () ->
      svc.seq <- svc.seq + 1;
      Lwt.return
        (Printf.sprintf "run-%010.0f-%04d" (Unix.gettimeofday () *. 1000.)
           svc.seq))

let flag_of svc id =
  match Hashtbl.find_opt svc.flags id with
  | Some f -> f
  | None ->
      let f = Atomic.make false in
      Hashtbl.replace svc.flags id f;
      f

let run_job svc (job : Job.t) runner =
  job.Job.status <- Job.Running;
  let open Lwt.Infix in
  svc.store.update job >>= fun () ->
  Lwt.catch
    (fun () ->
      Lwt_preemptive.detach
        (fun () ->
          runner job.Job.inputs job.Job.config
            ~should_cancel:(Some (fun () -> Atomic.get (flag_of svc job.Job.id))))
        ()
      >>= fun (res : Run.engine_result) ->
      job.Job.status <- Job.Done;
      job.Job.rows <- List.map Run.as_json res.Run.rows;
      job.Job.pnl <- res.Run.pnl;
      job.Job.diagnostics <- res.Run.diagnostics;
      job.Job.finished <- Some (Unix.gettimeofday ());
      svc.store.update job)
    (function
      | Run.Engine_cancelled _ ->
          job.Job.status <- Job.Cancelled;
          job.Job.finished <- Some (Unix.gettimeofday ());
          svc.store.update job
      | exn ->
          job.Job.status <- Job.Failed (Printexc.to_string exn);
          job.Job.finished <- Some (Unix.gettimeofday ());
          svc.store.update job)

let submit ?(runner = default_runner) svc ~inputs ~config =
  let open Lwt.Infix in
  next_id svc >>= fun id ->
  let as_of = inputs.Run.as_of and base_currency = inputs.Run.base_currency in
  let job =
    Job.
      {
        id;
        created = Unix.gettimeofday ();
        finished = None;
        as_of;
        base_currency;
        inputs;
        config;
        status = Job.Queued;
        rows = [];
        pnl = [];
        diagnostics = [];
      }
  in
  ignore (flag_of svc id);
  svc.store.create job >>= fun () ->
  Lwt.async (fun () -> run_job svc job runner);
  Lwt.return job

let get svc id = svc.store.get id
let list svc = svc.store.list ()

let cancel svc id =
  let open Lwt.Infix in
  svc.store.get id >>= function
  | None -> Lwt.fail Not_found
  | Some job ->
      if not (Job.is_terminal job.Job.status) then
        Atomic.set (flag_of svc id) true;
      svc.store.get id >>= (function
        | None -> Lwt.fail Not_found
        | Some j -> Lwt.return j)

(** Poll until the job is terminal (or [timeout_s] elapses). *)
let await ?(timeout_s = 120.) ?(poll_s = 0.05) svc id =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec go () =
    let open Lwt.Infix in
    svc.store.get id >>= function
    | None -> Lwt.fail Not_found
    | Some job ->
        if Job.is_terminal job.Job.status then Lwt.return job
        else if Unix.gettimeofday () > deadline then
          Lwt.fail_with ("await timed out for " ^ id)
        else Lwt_unix.sleep poll_s >>= go
  in
  go ()
