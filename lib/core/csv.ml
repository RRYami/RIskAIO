(** Minimal CSV reader (stdlib only, no new dependency).

    Parses RFC-4180-style files: comma-separated fields, double-quoted fields
    which may contain commas, embedded [""] escapes and newlines. Line breaks
    may be LF or CRLF. A ragged row (wrong field count) is an error — silent
    column shifts are worse than a loud failure.

    The repo's data boundary is this module: market inputs arrive as CSV
    (converted once, outside the repo, from whatever store holds them) and all
    engine code consumes the parsed rows below. *)

exception Csv_error of string

(** Split one buffer into rows of fields. *)
let parse_string s =
  let n = String.length s in
  let rows = ref [] in
  let row = ref [] in
  let buf = Buffer.create 32 in
  let field () =
    let v = Buffer.contents buf in
    Buffer.clear buf;
    row := v :: !row
  in
  let eol () =
    field ();
    rows := List.rev !row :: !rows;
    row := []
  in
  let i = ref 0 in
  let in_quotes = ref false in
  let closed = ref true in
  (* final row pending: any field/row content seen since last eol *)
  let pending = ref false in
  while !i < n do
    let c = s.[!i] in
    pending := true;
    if !in_quotes then (
      if c = '"' then (
        if !i + 1 < n && s.[!i + 1] = '"' then (
          Buffer.add_char buf '"';
          i := !i + 2)
        else (
          in_quotes := false;
          closed := true;
          incr i))
      else (
        Buffer.add_char buf c;
        incr i))
    else if c = '"' && Buffer.length buf = 0 then (
      (* opening quote: only valid at field start *)
      in_quotes := true;
      closed := false;
      incr i)
    else if c = ',' then (
      field ();
      incr i)
    else if c = '\r' then (
      if !i + 1 < n && s.[!i + 1] = '\n' then i := !i + 2 else incr i;
      eol ();
      pending := false)
    else if c = '\n' then (
      incr i;
      eol ();
      pending := false)
    else (
      Buffer.add_char buf c;
      incr i)
  done;
  if !in_quotes then raise (Csv_error "unterminated quoted field");
  if not !closed then raise (Csv_error "unterminated quoted field");
  if !pending then eol ();
  List.rev !rows

let parse_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  try parse_string s
  with Csv_error msg -> raise (Csv_error (path ^ ": " ^ msg))

(** Header row mapped to column positions. Duplicate or empty names rejected. *)
let header_index = function
  | [] -> raise (Csv_error "empty file: no header row")
  | hdr :: _ ->
      let idx = ref [] in
      List.iteri
        (fun i name ->
          let name = String.trim name in
          if name = "" then
            raise (Csv_error ("empty column name at position " ^ string_of_int i));
          if List.mem_assoc name !idx then
            raise (Csv_error ("duplicate column: " ^ name));
          idx := (name, i) :: !idx)
        hdr;
      List.rev !idx

(** All rows (after the header) must have the header's field count. *)
let check_width rows =
  match rows with
  | [] -> raise (Csv_error "empty file: no header row")
  | hdr :: body ->
      let w = List.length hdr in
      List.iteri
        (fun i r ->
          if List.length r <> w then
            raise
              (Csv_error
                 (Printf.sprintf "row %d has %d fields, expected %d" (i + 2)
                    (List.length r) w)))
        body;
      rows

(** Column lookup: [col idx name row] fetches and trims the field. *)
let col idx name row =
  match List.assoc_opt name idx with
  | None -> raise (Csv_error ("missing column: " ^ name))
  | Some i -> String.trim (List.nth row i)
