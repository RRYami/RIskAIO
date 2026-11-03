# Performance — P6 probe (+ P6b BLAS backend)

Question: is the OCaml engine's speed a language limit? Answer: no — it was
(partly) my code, and the rest is which BLAS you link. Method: same fixtures,
same workloads, both engines, wall clock.

## Setup

| | |
|---|---|
| Host | 8-core x86_64 dev machine (this box), OCaml 5.3.0, Python 3.14.7 + numpy 2.5.3 |
| Small fixture | `fixtures/perf-v1`: 190 instruments, 210 factors, 550 closes |
| Big fixture | `fixtures/perf-big`: **1,990 instruments, 2,010 factors, 260 closes** (259 returns), seed 11 |
| Config | historical + Monte Carlo 10k, Gaussian, EWMA λ 0.97, 3 curve PCs; horizons 1/10/20; 95/97.5/99%; components; exposures; starter stress library |
| BLAS | numpy → bundled **OpenBLAS** (multithreaded); OCaml → system **reference** BLAS/LAPACK via LACAML (single-threaded) |
| Reproduce | `make perf` / `make perf-big` (OCaml; the Python column was measured with a since-retired script against the same deterministic grid; the microbench is now BS-2002, the production American engine) |

## Results — big book (1,990 positions, 2,010 factors, MC 10k)

| Stage | Python/OpenBLAS | OCaml/ref-BLAS | |
|---|---|---|---|
| load fixtures | 1.44 s | 0.99 s | OCaml faster (Yojson vs polars JSON) |
| build_factors | (in run) | 0.18 s | after fix 2 below |
| run historical (259 scen) | 0.87 s | 1.13 s | 1.3× |
| run MC 10k | 3.48 s | 57.4 s → **21.6 s** (OpenBLAS) → **18.1 s** (8 domains) | see below |
| black micro (100k evals) | — | 0.137 s (**1.37 µs/eval**, 0.86 KB/eval minor) | European path |
| bs2002 micro (100k evals) | — | 10.8 s (**~108 µs/eval**, 24.8 KB/eval minor) | American path |

Agreement at scale: hist VaR99/1d **235662.57 = 235662.57**; BAW checksums
identical; MC VaR99/1d 329321 vs 325917 (~1% — different RNG streams, n = 10k
sampling noise, expected).

Option-pricing microbench (deterministic 20-option × 5000-scenario grid,
`perf --amer 20 5000`; checksums stable run-to-run: black 1452485.380196,
bs2002 1467663.520710): European Black costs 1.37 µs and 0.86 KB per eval —
the cheap path every non-American option takes. BS-2002 costs ~108 µs and
~25 KB per eval (~80× Black): each eval runs several bivariate-normal
quadratures (20-point DW/Genz central branch, ~40 `exp` each) plus adaptive
Simpson on the quadrature fringe. That cost is why BS-2002 won on accuracy,
not speed — it validates at ≤1.7% vs 1000-step CRR across the book envelope
(q to 25%, r to 0%, T to 3y), where BAW's `1−exp(−rT)` divide breaks down.
Allocation is minor-heap only (~1 MB major per 100k evals), so no GC pauses;
`bs2002 alloc` also reports process peak RSS for context.

Small book for reference: historical 0.074 vs 0.255 s; MC 10k 0.448 vs
2.708 s; BAW 1.06 vs 0.57 µs/eval.

## Measured kernel rates (reference BLAS/LAPACK, `perf --gemm/--syevd`)

| Kernel | Reference BLAS | OpenBLAS 0.3.34 (user-space build, `LD_PRELOAD`) |
|---|---|---|
| dgemm 2000³ | 5.15 s (3.1 GFLOP/s) | **0.156 s (102 GFLOP/s, 33×)** |
| dsyevd 2000² | 9.8 s | **0.61 s** (LAPACK's internal BLAS calls accelerate too) |
| dpotrf 2000² | 0.66 s | **0.09 s** |

Checksums identical across backends. Note: `dune exec` hangs under
`LD_PRELOAD` (its process handling trips on the preloaded threading runtime);
run the built binary directly (`./_build/default/bin/perf.exe`). Also, Arch's
`liblapack.so.3` wants ILP64 (`*_64_`) BLAS symbols, so a blunt
`LD_LIBRARY_PATH` swap breaks LAPACK — `LD_PRELOAD` interposition keeps both
working (BLAS from OpenBLAS, LAPACK entry points from the system lib).

## Where the 57 s went (measured, in order found)

| # | Cause | Fix | Effect on big-book MC |
|---|---|---|---|
| 1 | `Market.wide` scanned all rows per key (1990 × 517k) | single-pass hash pivot | factors 7.3 → 0.18 s |
| 2 | `rows := !rows @ batch` quadratic in 28k rows | cons + one concat | historical 8.3 → 1.1 s |
| 3 | reference BLAS (3.1 GFLOP/s) | LACAML + user-space OpenBLAS (102 GFLOP/s) | kernels 33× |
| 4 | PSD-repair reconstruction ran a **scalar triple loop (8 GFLOP)** — at 259 returns × 2010 factors the covariance is always rank-deficient, so this fired every MC run (~15 s hidden in "fixed cost") | route through dgemm | MC 18.1 → **5.2 s** |
| 5 | per-chunk matrix temps churned the heap | per-domain scratch buffers (`Pricing.ws_out`, `values_into`) | small alone, enables #6 cleanly |

Remaining 5.2 vs 3.5 s: repricing loops (numpy's vectorized transcendentals
vs OCaml scalar `exp`, ~2–3 s) + OCaml 5 domain scaling at 1.7× (9.0 → 5.2 s
on 5 chunks; stop-the-world major slices cap it — per-domain buffers are in,
finer splitting is the next lever). All values bit-identical at every stage
(oracle suite + `par-determinism` test).

## Implementation notes (all oracle-verified, values bit-identical)

* **LACAML backend** (`Stats`: dgemm/dsyrk/dsyevd/dpotrf, threshold-gated at
  50k FLOP; scalar loops below). Plus dgemm-routed PSD reconstruction (#4).
* **Chunk parallelism** (`total_pnl` on OCaml 5 Domains, `RISK_AI_DOMAINS`,
  default = machine count) with pre-warmed roll cache (Hashtbl is not
  domain-safe) and disjoint-slice writes — bit-identical output.
* **Scratch buffers** (`Pricing.ws_out`, `values_into` with row ranges and
  inline scaling — no more scaled-block copies).

## Decision (revised): not a language limit

Verdict on the original question: **no**. Evidence:

* 102 GFLOP/s dgemm called from OCaml; BAW beats numpy outright.
* The pre-BLAS gap decomposed into (a) two O(n²) bugs of mine —
  fixed, 7–40×, oracle-verified; (b) reference-vs-tuned BLAS — fixed in
  user space, 33× on kernels; (c) single-threaded repricing loops.
* (c) got a first treatment: `total_pnl` chunks now run on OCaml 5 Domains
  (`RISK_AI_DOMAINS`, default = machine count), **bit-identical** output
  (proven by the `par-determinism` test: 1 vs 8 domains, byte-equal rows).
  Measured gain is modest — 21.6 → 18.1 s — because per-scenario `Array.init`
  allocation churns the major heap and OCaml 5 stop-the-world slices
  serialize the domains (confirmed insensitive to chunk/domain counts and
  BLAS threading).

Remaining levers, in order: (a) per-domain workspace buffers to kill the
allocation churn (the actual wall; projected ~4–5× on the repricing share);
(b) finer work splitting in `components`/`exposures`; (c) BAW Newton (same
lever as Python's own perf doc). No Owl: LACAML already provides the BLAS
seam with far less weight, and BAW (Python's 65% bottleneck) needs no BLAS
at all.

### OpenBLAS from source (no sudo, no Fortran → BLAS only) — DONE

`gfortran` is absent so a full OpenBLAS (with LAPACK) cannot be built; but
`NOFORTRAN=1` builds BLAS only, which is all LACAML's dgemm/dsyrk need
(syevd/potrf entry points stay in system LAPACK, whose internal BLAS calls
accelerate anyway). Recipe that works here:

```bash
# one-time build (~10 min) into ~/.local/openblas
# source: OpenBLAS-0.3.34.tar.gz in /tmp/opencode/openblas
make NOFORTRAN=1 -j8 && make NOFORTRAN=1 PREFIX=$HOME/.local/openblas install
# run (NOT via dune exec — it hangs under LD_PRELOAD; use the built binary):
LD_PRELOAD=$HOME/.local/openblas/lib/libopenblas.so \
OPENBLAS_NUM_THREADS=8 ./_build/default/bin/perf.exe --fixture fixtures/perf-big --mc 10000
```

Effect on the big book: MC 57.4 → 21.6 s; kernels above. `RISK_AI_DOMAINS`
controls chunk parallelism (default: machine count; 1 = sequential loops).
