(** Run persistence (P7d). [STORE] is implemented in-memory (tests, default)
    and on Postgres (production). Jobs are written on every status transition
    and on Postgres (production). Jobs are written on every status transition
    so a crash loses at most the in-flight run. *)

module type STORE = sig
  type t

  val create : t -> Job.t -> unit Lwt.t
  val update : t -> Job.t -> unit Lwt.t
  val get : t -> string -> Job.t option Lwt.t
  val list : t -> Job.t list Lwt.t
end

(** Process-local store. Thread-safe for the API server's use (one Lwt
    runtime; background engine runs go through [Lwt_preemptive]). *)
module Mem : sig
  type t

  val make : unit -> t
  include STORE with type t := t
end = struct
  type t = {
    mu : Lwt_mutex.t;
    tbl : (string, Job.t) Hashtbl.t;
  }

  let make () = { mu = Lwt_mutex.create (); tbl = Hashtbl.create 32 }

  let create s j =
    Lwt_mutex.with_lock s.mu (fun () ->
        Hashtbl.replace s.tbl j.Job.id j;
        Lwt.return_unit)

  let update = create

  let get s id =
    Lwt_mutex.with_lock s.mu (fun () ->
        Lwt.return (Hashtbl.find_opt s.tbl id))

  let list s =
    Lwt_mutex.with_lock s.mu (fun () ->
        Lwt.return
          (Hashtbl.fold (fun _ j acc -> j :: acc) s.tbl []
          |> List.sort (fun a b -> Float.compare b.Job.created a.Job.created)))
end
