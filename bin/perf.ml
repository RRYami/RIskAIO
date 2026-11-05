(** Performance probe: factor build, historical + MC runs, BS2002 microbench.

    usage: perf --fixture DIR [--mc N] [--amer NOPT NSCEN] Prints wall-clock
    seconds per stage plus sanity values (NAV, VaR). The American-option
    workload uses a deterministic grid (no RNG); BS-2002 is the production
    American engine (BAW kept in the library as the tested challenger). *)

open Risk_core
open Risk_engine

let time label f =
  let t0 = Unix.gettimeofday () in
  let v = f () in
  let dt = Unix.gettimeofday () -. t0 in
  Printf.printf "%-28s %8.3f s\n%!" label dt;
  v

(** Deterministic synthetic American-option workload: NOPT options × NSCEN
    scenarios (same grid shape the BAW bench used; BS-2002 is the engine). *)
let amer_workload nopt nscen =
  List.init nopt (fun o ->
      let k = 90. +. (float_of_int (o mod 5) *. 5.) in
      let t = 0.5 +. (float_of_int (o mod 4) *. 0.5) in
      let sigma = 0.15 +. (float_of_int (o mod 6) *. 0.05) in
      let call = o mod 2 = 0 in
      let s =
        Array.init nscen (fun s ->
            80. +. (float_of_int (((o * 131) + (s * 17)) mod 6000) /. 100.))
      in
      let r = Array.make nscen 0.03 in
      let b = Array.make nscen 0.02 in
      (s, k, t, r, b, sigma, call))

let peak_rss_kb () =
  try
    let ic = open_in "/proc/self/status" in
    let rec go () =
      match input_line ic with
      | line -> (
          match String.split_on_char ':' line with
          | [ k; v ] when String.trim k = "VmHWM" -> (
              close_in ic;
              match String.split_on_char ' ' (String.trim v) with
              | n :: _ -> Some (int_of_string (String.trim n))
              | [] -> None)
          | _ -> go ())
      | exception End_of_file ->
          close_in ic;
          None
    in
    go ()
  with _ -> None

let () =
  let fixture = ref ""
  and mc = ref 10_000
  and nopt = ref 20
  and nscen = ref 5_000
  and gemm_size = ref 0
  and syevd_size = ref 0 in
  let args =
    [
      ("--fixture", Arg.Set_string fixture, "DIR fixture directory");
      ("--mc", Arg.Set_int mc, "N MC scenarios (default 10000)");
      ( "--amer",
        Arg.Tuple [ Arg.Set_int nopt; Arg.Set_int nscen ],
        "NOPT NSCEN BS-2002 microbench" );
      ("--gemm", Arg.Set_int gemm_size, "N time N×N dgemm (0 = skip)");
      ("--syevd", Arg.Set_int syevd_size, "N time N×N dsyevd+potrf (0 = skip)");
    ]
  in
  Arg.parse args (fun _ -> ()) "perf --fixture DIR [--mc N] [--amer NOPT NSCEN]";
  if !gemm_size > 0 then (
    let n = !gemm_size in
    let s = ref 1 in
    let rnd () =
      s := ((!s * 1103515245) + 12345) land 0x7fffffff;
      float_of_int !s /. 2147483648.
    in
    let a = Array.init n (fun _ -> Array.init n (fun _ -> rnd ())) in
    let b = Array.init n (fun _ -> Array.init n (fun _ -> rnd ())) in
    let t0 = Unix.gettimeofday () in
    let c = Stats.matmul a b in
    let dt = Unix.gettimeofday () -. t0 in
    Printf.printf "dgemm %d³: %.3f s (%.2f GFLOP/s, checksum %.6f)\n%!" n dt
      (2. *. float_of_int (n * n * n) /. dt /. 1e9)
      (Array.fold_left
         (fun acc row -> acc +. Array.fold_left ( +. ) 0. row)
         0. c);
    exit 0);
  if !syevd_size > 0 then (
    let n = !syevd_size in
    let s = ref 2 in
    let rnd () =
      s := ((!s * 1103515245) + 12345) land 0x7fffffff;
      float_of_int !s /. 2147483648.
    in
    let q = Array.init n (fun _ -> Array.init n (fun _ -> rnd ())) in
    let sym =
      Array.init n (fun i ->
          Array.init n (fun j ->
              let acc = ref 0. in
              for l = 0 to n - 1 do
                acc := !acc +. (q.(i).(l) *. q.(j).(l))
              done;
              !acc +. if i = j then float_of_int n else 0.))
    in
    let t0 = Unix.gettimeofday () in
    let vals, _ = Stats.eigh_sym sym in
    let t1 = Unix.gettimeofday () in
    let _ = Stats.cholesky_lower sym in
    let t2 = Unix.gettimeofday () in
    Printf.printf "syevd %d²: %.3f s; potrf %d²: %.3f s (min eig %.6f)\n%!" n
      (t1 -. t0) n (t2 -. t1) vals.(0);
    exit 0);
  if !fixture = "" then (
    prerr_endline "perf --fixture DIR [--mc N] [--amer NOPT NSCEN]";
    exit 2);
  let dir = !fixture in
  let instruments, holdings, bundle, config, as_of =
    time "load fixtures" (fun () ->
        let instruments =
          Refdata.instruments_of_file (Filename.concat dir "instruments.json")
        in
        let holdings =
          Holdings.holdings_of_file (Filename.concat dir "holdings.json")
        in
        let bundle =
          Marketdata.bundle_of_file (Filename.concat dir "marketdata.json")
        in
        let config =
          Config.of_yojson
            (Yojson.Basic.from_file (Filename.concat dir "config.json"))
        in
        let manifest =
          Yojson.Basic.from_file (Filename.concat dir "manifest.json")
        in
        let open Yojson.Basic.Util in
        let as_of =
          manifest |> member "as_of" |> to_string |> Calendar.of_string
        in
        (instruments, holdings, bundle, config, as_of))
  in
  let state =
    time "market_state" (fun () -> Market.market_state bundle as_of)
  in
  let book =
    time "build_book" (fun () ->
        Pricing.build_book holdings instruments state "EUR" None)
  in
  let factors =
    time "build_factors" (fun () ->
        Market.build_factors bundle ~equity_ids:(Pricing.equity_ids book)
          ~fx_ccys:(Pricing.currencies book)
          ~curve_ccys:(Pricing.curve_ids book)
          ~history_days:(Some config.history_days))
  in
  Printf.printf "%-28s %8d positions, %d factors, %d returns\n%!" "book"
    (List.length book.positions)
    (Market.size factors)
    (Array.length factors.history);
  let inputs : Run.engine_inputs =
    {
      holdings;
      instruments;
      bundle;
      as_of;
      base_currency = "EUR";
      row_portfolios = None;
      surf_fits = [];
    }
  in
  let res = time "run historical" (fun () -> Run.run_engine inputs config ()) in
  let find_var rows =
    List.find_opt
      (fun (r : Run.result_row) ->
        r.measure = "var"
        && List.assoc_opt "confidence" r.slice = Some (`Float 0.99)
        && List.assoc_opt "horizon_days" r.slice = Some (`Int 1))
      rows
  in
  (match find_var res.rows with
  | None -> Printf.printf "hist var99/1d: MISSING\n%!"
  | Some r ->
      Printf.printf "hist var99/1d: %.2f (%d rows)\n%!" r.value
        (List.length res.rows));
  let mc_config : Config.run_config =
    { config with method_ = Config.Montecarlo; n_scenarios = !mc; seed = 0 }
  in
  let mc_res =
    time (Printf.sprintf "run MC n=%d" !mc) (fun () ->
        Run.run_engine inputs mc_config ())
  in
  (match find_var mc_res.rows with
  | None -> Printf.printf "mc var99/1d: MISSING\n%!"
  | Some r -> Printf.printf "mc var99/1d: %.2f\n%!" r.value);
  let workload = amer_workload !nopt !nscen in
  let g0 = Gc.stat () in
  let t0 = Unix.gettimeofday () in
  let checksum = ref 0. in
  List.iter
    (fun (s, k, t, r, b, sigma, call) ->
      let px = Options.bs2002 ~s ~k ~t ~r ~b ~sigma ~call in
      Array.iter (fun v -> checksum := !checksum +. v) px)
    workload;
  let dt = Unix.gettimeofday () -. t0 in
  let g1 = Gc.stat () in
  let evals = !nopt * !nscen in
  let minor_mb = (g1.minor_words -. g0.minor_words) *. 8. /. 1048576.
  and major_mb = (g1.major_words -. g0.major_words) *. 8. /. 1048576. in
  Printf.printf "%-28s %8.3f s  (%d evals, %.2f us/eval, checksum %.6f)\n%!"
    "bs2002 microbench" dt evals
    (dt *. 1e6 /. float_of_int evals)
    !checksum;
  Printf.printf "%-28s minor=%7.1fMB major=%7.1fMB%s\n%!" "bs2002 alloc"
    minor_mb major_mb
    (match peak_rss_kb () with
    | None -> ""
    | Some kb -> Printf.sprintf " peak_rss=%dMB" (kb / 1024));
  (* European Black on the same grid: the cheap path every non-American
     option takes in production. *)
  let g2 = Gc.stat () in
  let t2 = Unix.gettimeofday () in
  let checksum_e = ref 0. in
  List.iter
    (fun (s, k, t, r, b, sigma, call) ->
      let px = Options.black_scholes ~s ~k ~t ~r ~b ~sigma ~call in
      Array.iter (fun v -> checksum_e := !checksum_e +. v) px)
    workload;
  let dt_e = Unix.gettimeofday () -. t2 in
  let g3 = Gc.stat () in
  Printf.printf "%-28s %8.3f s  (%d evals, %.2f us/eval, checksum %.6f)\n%!"
    "black microbench" dt_e evals
    (dt_e *. 1e6 /. float_of_int evals)
    !checksum_e;
  Printf.printf "%-28s minor=%7.1fMB major=%7.1fMB\n%!" "black alloc"
    ((g3.minor_words -. g2.minor_words) *. 8. /. 1048576.)
    ((g3.major_words -. g2.major_words) *. 8. /. 1048576.);
  (* Multi-domain BS-2002 over the same workload: per-eval independence makes
     the split trivially bit-identical (checksums must match the sequential
     run above). Domain count follows RISK_AI_DOMAINS, as in production. *)
  let domains = Run.n_domains () in
  let eval_one (s, k, t, r, b, sigma, call) =
    let px = Options.bs2002 ~s ~k ~t ~r ~b ~sigma ~call in
    Array.fold_left ( +. ) 0. px
  in
  let arr = Array.of_list workload in
  let n = Array.length arr in
  let d = Int.max 1 domains in
  let groups =
    List.init d (fun i ->
        let lo = i * n / d and hi = (i + 1) * n / d in
        Array.sub arr lo (hi - lo) |> Array.to_list)
    |> List.filter (fun g -> g <> [])
  in
  let g4 = Gc.stat () in
  let t4 = Unix.gettimeofday () in
  let workers =
    List.map
      (fun g ->
        Domain.spawn (fun () ->
            List.fold_left (fun acc w -> acc +. eval_one w) 0. g))
      (match groups with [] -> [] | _ :: rest -> rest)
  in
  let mt_sum =
    (match groups with
      | [] -> 0.
      | first :: _ -> List.fold_left (fun acc w -> acc +. eval_one w) 0. first)
    +. List.fold_left (fun acc w -> acc +. Domain.join w) 0. workers
  in
  let dt_mt = Unix.gettimeofday () -. t4 in
  let g5 = Gc.stat () in
  Printf.printf
    "%-28s %8.3f s  (%d evals, %d domains, %.2fx, checksum %.6f)\n%!"
    "bs2002 microbench-mt" dt_mt evals domains (dt /. dt_mt) mt_sum;
  Printf.printf "%-28s minor=%7.1fMB major=%7.1fMB\n%!" "bs2002-mt alloc"
    ((g5.minor_words -. g4.minor_words) *. 8. /. 1048576.)
    ((g5.major_words -. g4.major_words) *. 8. /. 1048576.)
