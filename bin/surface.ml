(** surface.exe: SSVI surface calibration + vol-shocked runs.

    calibrate --quotes QUOTES.CSV --out FITS.JSON [--underlying U]
      CSV (the repo's data contract) → per-day SSVI fits → FITS.JSON
      [{underlying: [fit, ...]}]. Prints days fitted / failed and mean RMSE.
      Non-converged days are kept in the file with converged=false; the
      engine skips them when building diffs (see Pca_latent.of_fits).

    run --fixture DIR --fits FITS.JSON --out RESULTS.JSON
        [--vol bootstrap|frozen] [--vol-pcs K] [--asof YYYY-MM-DD]
      Full engine run with surface shocks; writes the machine-readable
      results the frontend plots later: rows, per-scenario P&L, diagnostics,
      and a summary block (nav, VaR/ES per confidence/horizon).
 *)

open Risk_core
open Risk_engine

let read_json path = Yojson.Basic.from_file path

let calibrate quotes_path out_path underlying =
  let rows = Quotes.parse_file quotes_path in
  let by_date = Hashtbl.create 64 in
  List.iter
    (fun (q : Quotes.quote) ->
      Hashtbl.replace by_date q.Quotes.date
        (q :: (Hashtbl.find_opt by_date q.Quotes.date |> Option.value ~default:[])))
    rows;
  let days =
    Hashtbl.fold (fun d qs acc -> (d, qs) :: acc) by_date []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let outcomes =
    List.map
      (fun (d, qs) ->
        let grids =
          try Quotes.to_grids qs
          with Quotes.Quotes_error msg -> raise (Failure (d ^ ": " ^ msg))
        in
        (d, grids))
      days
  in
  let fits =
    try Ssvi_calibration.fit_history outcomes
    with Failure msg ->
      Printf.eprintf "quote failure: %s\n" msg;
      exit 1
  in
  let ok =
    List.filter_map
      (function Ssvi_calibration.Ok f -> Some f | _ -> None)
      fits
  in
  let failed =
    List.filter_map
      (function
        | Ssvi_calibration.Failed x -> Some (x.date ^ ": " ^ x.reason)
        | _ -> None)
      fits
  in
  let j =
    `Assoc [ (underlying, Ssvi_calibration.to_yojson ok) ]
  in
  Yojson.Basic.to_file out_path j;
  let rmse =
    if ok = [] then Float.nan
    else
      List.fold_left
        (fun a (f : Ssvi_calibration.fit) -> a +. f.Ssvi_calibration.rmse_iv)
        0. ok
      /. float_of_int (List.length ok)
  in
  Printf.printf "days=%d fitted=%d failed=%d mean_rmse_iv=%.6f -> %s\n"
    (List.length days) (List.length ok) (List.length failed) rmse out_path;
  List.iter (fun d -> Printf.printf "  failed: %s\n" d) failed

let load_fits path =
  let j = read_json path in
  let open Yojson.Basic.Util in
  List.map
    (fun (u, v) -> (u, Ssvi_calibration.of_yojson v))
    (to_assoc j)

let run fixture fits_path out_path vol pcs asof_opt =
  let read_json_local name = read_json (Filename.concat fixture name) in
  let manifest = read_json_local "manifest.json" in
  let open Yojson.Basic.Util in
  let as_of_s =
    match asof_opt with
    | Some s -> s
    | None -> manifest |> member "as_of" |> to_string
  in
  let holdings =
    Holdings.holdings_of_file (Filename.concat fixture "holdings.json")
  in
  let bundle =
    Marketdata.bundle_of_file (Filename.concat fixture "marketdata.json")
  in
  let config = Config.of_yojson (read_json_local "config.json") in
  Config.validate config;
  let config =
    {
      config with
      Config.vol_model = vol;
      vol_pcs = pcs;
      vol_universe =
        (match vol with
        | Config.Frozen -> []
        | _ -> List.map fst (load_fits fits_path));
    }
  in
  let surf_fits = match vol with Config.Frozen -> [] | _ -> load_fits fits_path in
  let instruments =
    Refdata.instruments_of_file (Filename.concat fixture "instruments.json")
  in
  let res =
    Run.run_engine
      Run.
        {
          holdings;
          instruments;
          bundle;
          as_of = Calendar.of_string as_of_s;
          base_currency = "USD";
          row_portfolios = None;
          surf_fits;
        }
      config ()
  in
  let summary =
    List.filter_map
      (fun (r : Run.result_row) ->
        if r.Run.view = "summary"
           && (r.measure = "var" || r.measure = "es" || r.measure = "nav")
        then Some (Run.as_json r)
        else None)
      res.Run.rows
  in
  let j =
    `Assoc
      [
        ("rows", `List (List.map Run.as_json res.Run.rows));
        ( "pnl",
          `Assoc
            (List.map
               (fun (h, a) ->
                 (h, `List (List.map (fun x -> `Float x) (Array.to_list a))))
               res.Run.pnl) );
        ("diagnostics", `Assoc res.Run.diagnostics);
        ("summary", `List summary);
      ]
  in
  Yojson.Basic.to_file out_path j;
  Printf.printf "rows=%d diagnostics=%d -> %s\n"
    (List.length res.Run.rows)
    (List.length res.Run.diagnostics)
    out_path

let usage =
  "usage:\n\
  \  surface.exe calibrate --quotes Q.CSV --out FITS.JSON [--underlying U]\n\
  \  surface.exe recanon --fits FITS.JSON --out CANON.JSON [--expiries T,...] [--kgrid K,...]\n\
  \  surface.exe run --fixture DIR --fits FITS.JSON --out RESULTS.JSON \
   [--vol bootstrap|frozen] [--vol-pcs K] [--asof YYYY-MM-DD]"

let get args flag =
  let rec go = function
    | [] -> None
    | a :: b :: rest -> if a = flag then Some b else go (b :: rest)
    | _ :: rest -> go rest
  in
  go args

let () =
  match Array.to_list Sys.argv with
  | _ :: "calibrate" :: rest -> (
      match (get rest "--quotes", get rest "--out") with
      | Some q, Some o ->
          let u =
            match get rest "--underlying" with None -> "EQ1" | Some s -> s
          in
          calibrate q o u
      | _ ->
          prerr_endline usage;
          exit 2)
  | _ :: "run" :: rest -> (
      match (get rest "--fixture", get rest "--fits", get rest "--out") with
      | Some f, Some fi, Some o ->
          let vol =
            match get rest "--vol" with
            | None | Some "frozen" -> Config.Frozen
            | Some "bootstrap" -> Config.Bootstrap
            | Some "gaussian" -> Config.Gaussian
            | Some s ->
                prerr_endline ("unknown vol: " ^ s);
                exit 2
          in
          let pcs =
            match get rest "--vol-pcs" with
            | None -> 0
            | Some s -> int_of_string s
          in
          run f fi o vol pcs (get rest "--asof")
      | _ ->
          prerr_endline usage;
          exit 2)
  | _ :: "recanon" :: rest -> (
      match (get rest "--fits", get rest "--out") with
      | Some fi, Some o ->
          let exps =
            match get rest "--expiries" with
            | None -> [| 0.0822; 0.25; 0.5; 1.0; 2.0 |]
            | Some s ->
                Array.of_list
                  (List.map float_of_string (String.split_on_char ',' s))
          in
          let k =
            match get rest "--kgrid" with
            | None -> [| -1.0; -0.5; -0.25; 0.0; 0.25; 0.5; 1.0 |]
            | Some s ->
                Array.of_list
                  (List.map float_of_string (String.split_on_char ',' s))
          in
          let all = load_fits fi in
          let canon =
            List.map
              (fun (u, fits) ->
                let outs = Ssvi_calibration.recanon fits ~expiries:exps ~k in
                let ok =
                  List.filter_map
                    (function
                      | Ssvi_calibration.Ok f -> Some f | _ -> None)
                    outs
                in
                let nbad = List.length outs - List.length ok in
                let rmse =
                  if ok = [] then Float.nan
                  else
                    List.fold_left
                      (fun a (f : Ssvi_calibration.fit) -> a +. f.rmse_iv)
                      0. ok
                    /. float_of_int (List.length ok)
                in
                Printf.printf "%s: %d/%d refit (%d dropped), mean resample rmse=%.6f\n" u
                  (List.length ok) (List.length outs) nbad rmse;
                (u, ok))
              all
          in
          let j =
            `Assoc
              (List.map
                 (fun (u, fs) -> (u, Ssvi_calibration.to_yojson fs))
                 canon)
          in
          Yojson.Basic.to_file o j;
          Printf.printf "-> %s\n" o
      | _ ->
          prerr_endline usage;
          exit 2)
  | _ ->
      prerr_endline usage;
      exit 2
