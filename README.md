# RiskAI — OCaml port of the RiskStartUp engine

Engine-first port of [`RiskStartUp`](../../PythonProjects/RiskStartUp) (`core/` +
`engine/`) to OCaml 5.3 + dune, extended with PCA-based implied-volatility
surface shocks and a Bjerksund–Stensland 2002 American engine. Ingestion stays
a separate service (agreed); API/worker/DB are later phases. Calibration,
simulation and pricing are pure OCaml; data-prep and cross-language oracle
scripts live outside the repo.

## Status

| Phase | Scope | State |
|---|---|---|
| P0 | dune skeleton, opam 5.3 switch, CI-less `make` flow | done |
| P1 | core: money, calendar, currencies, refdata, marketdata, holdings | done |
| P2 | engine leaves: stats, options, config | done |
| P3 | market factors + pricing book + Python-oracle parity | done |
| P4 | `run_engine` + exposures/stress/components + oracle parity | done |
| P5 | backtest + oracle parity | done |
| P6 | perf probe + BLAS decision (LACAML, no Owl — see PERF.md) | done |
| S1 | constrained SSVI surfaces (admissible latent map, GJ conditions) | done |
| S2 | static-arbitrage validator (grid diagnostics vs analytical guarantee) | done |
| S3 | quote ingest: CSV contract, OTM/ITM inversion, Brent solver | done |
| S4 | daily calibration (Nelder-Mead, warm starts, JSON cache, recanon) | done |
| S5 | latent PCA + joint equity/vol simulation (bootstrap + Gaussian) | done |
| S6 | American engine: CRR-vs-BAW-vs-BS2002 shootout, BS-2002 wins | done |
| — | `python/`: oracle/time scripts removed (out-of-repo); fixtures script kept | done |
| P7a | zarith money (exact rational boundary, float64 core) | done |
| P7b | full taxonomy (closed 9-type variant + terms validation) | done |
| P7c | Postgres read contract (`db/schema.sql`, `lib/db`, USD curves + dividends) | done |
| P7d | API/worker/DB persistence (`lib/api`, `bin/api.exe`, `db/runs.sql`) | done |

Oracle parity: the P3–P5 cross-language goldens (`oracle_*.json`) are consumed
as data and the gated tests skip when they are absent. The scripts that
generated them (RiskStartUp-venv based) no longer live here.

## Quickstart (no sudo needed)

```bash
export PATH="$HOME/.local/bin:$PATH"   # opam lives here
make switch   # one-time: opam switch create 5.3.0 (compiles OCaml, ~10 min)
make deps     # dune, alcotest, yojson, ocamlformat, lacaml
make build
make test     # full suite: 79 tests across 4 binaries (no Python needed)
make parity   # fixtures + full suite with fixture-gated tests + engine check
make perf-big # scaled probe: ~2000 positions (see PERF.md)
```

## Layout

```text
lib/core/    money, calendar, currencies, refdata, marketdata, holdings, csv
lib/engine/  config, stats, options, market, pricing, run, backtest,
             ssvi, surface_constraints, quotes, optim, ssvi_calibration,
             pca_latent
lib/db/      Postgres read contract: v_* views → Marketdata.bundle (no DB dep)
lib/api/     REST worker: jobs, memory/Postgres stores, service, HTTP routes
db/          schema.sql: market_* tables + v_* contract views (Postgres)
db/          runs.sql: risk_runs result archive (Postgres)
bin/         risk-ai (check | parity), perf, surface (calibrate|recanon|run),
             dev (book | price | bench-run — playground, non-production),
             api (REST worker: POST/GET runs — production path)
test/        alcotest suites: unit goldens, scipy cross-check, validator
             injections, CRR-vs-BAW-vs-BS2002 shootout, end-to-end bootstrap
python/      export_fixtures.py only — deterministic universes (stdlib only)
fixtures/    generated (gitignored); soxx-v1 is the real-data demo book
```

## API worker (production path)

`dune exec bin/api.exe -- --port 8080 [--db-url postgresql://...]` — REST +
JSON over the same engine (memory store by default, Postgres via `db/runs.sql`
migration when `--db-url` is given):

```bash
curl -s http://localhost:8080/health
curl -s -X POST http://localhost:8080/api/runs \
  -H 'content-type: application/json' -d @run.json   # 202 {id, status}
curl -s http://localhost:8080/api/runs/RUN_ID        # status + rows + diagnostics
curl -s 'http://localhost:8080/api/runs/RUN_ID/pnl?h=h1'
curl -s -X DELETE http://localhost:8080/api/runs/RUN_ID   # cooperative cancel
```

`run.json` carries `{as_of, base_currency, holdings, instruments, marketdata,
config, surf_fits?}` — the file-fixture shapes, so any parity fixture doubles
as a request body. Results (rows, per-horizon P&L, diagnostics) persist as one
JSONB document per run; market inputs are snapshotted by the caller.

## Dev playground (non-production diagnostics)

`dune exec bin/dev.exe -- ...` — inspect calculations and benchmark the engine:

```bash
dev.exe book --fixture fixtures/soxx-v1 --base USD        # positions, factors, NAV
dev.exe price --fixture fixtures/soxx-v1 --base USD --id OPT1 --spot-bump -0.05
dev.exe bench-run --fixture fixtures/soxx-v1 --base USD --mc 2000   # stage times + Gc deltas + peak RSS
dev.exe option --style european --right call --spot 100 --strike 100 \
  --years 1 --rate 0.05 --div 0.02 --vol 0.25            # price + bump Greeks
dev.exe option --style american --right put --spot 100 --strike 110 \
  --years 0.25 --rate 0.05 --div 0.01 --price 4.32      # invert to implied vol
```

`bench-run` prints per-stage wall time plus minor/major allocation, live heap
and peak RSS, so repricing cost and allocation churn are visible per change.

Option-pricing microbench (100k evals, `perf --amer 20 5000`; see PERF.md):
European Black 1.37 µs/eval, American BS-2002 ~108 µs/eval single-domain
(~34 µs/eval across 8 domains, checksum-identical).

## Deliberate divergences from Python

* **No Parquet** (no mature OCaml lib): fixtures are JSON/CSV with identical
  columns; money stays decimal text at the boundary (§3.7).
* **LACAML, not Owl**: BLAS/LAPACK via Lacaml (see PERF.md); the engine core
  stays `float array array` with fixed-order loops (ADR-007 chunk-independence).
* **RNG is SplitMix64, not PCG64**: MC draws match in distribution, not
  bit-for-bit (tolerance-OK, agreed). Draw order mirrors `mc_shocks`.
* **Money is exact at the boundary, float64 in the core**: `Money.t` holds
  an exact rational plus its float image; quantization is exact half-up.
  Float ingress stays unrepresentable; `erf`/`ndtr` are stdlib-only
  (~1e-15, tested against published CDF goldens).
* **Zero covariance** repairs to ~zero and reports `repaired=true` instead of
  raising like numpy's Cholesky.
* **American engine is BS-2002, not BAW**: BAW's formula divides by
  `1−exp(−rT)`, undefined at r=0, and the degenerate fallback silently returns
  European — up to 87% underpricing measured on deep-ITM calls at r=0/q=25%.
  BS-2002 validates at ≤1.7% vs 1000-step CRR across the whole envelope
  (q to 25%, r to 0%, T to 3y). BAW stays in the codebase as the tested
  challenger; the shootout guards both.
* **IV inversion uses Brent's method** (~30 evals vs 200-step bisection),
  European Black for OTM, BS-2002 for ITM (10bp American premium measured on
  real short-dated OTM puts — second order, kept European for speed).

## SSVI volatility surfaces (PCA-based IV shocks)

Pure-OCaml constrained-SSVI pipeline (`lib/engine/ssvi.ml`,
`surface_constraints.ml`, `quotes.ml`, `optim.ml`, `ssvi_calibration.ml`,
`pca_latent.ml`; CLI: `bin/surface.exe`):

```bash
# 1. daily fits from the CSV data contract (outside-repo conversion first)
dune exec bin/surface.exe -- calibrate --quotes quotes.csv --out fits.json --underlying SOXX
# 2. refit onto the fixed expiry×moneyness grid latent PCA needs
#    (listed expiries roll daily, so raw fits have incompatible latent dims)
dune exec bin/surface.exe -- recanon --fits fits.json --out canon.json
# 3. full run; results.json is the frontend contract (rows, pnl, diagnostics)
dune exec bin/surface.exe -- run --fixture fixtures/soxx-v1 --fits canon.json \
  --out results.json --vol bootstrap --vol-pcs 0
```

`--vol frozen` reproduces the status quo ante exactly (per-option ATM sigma);
`bootstrap` replays same-date latent diffs (exact equity/vol dependence;
requires historical method); `gaussian` draws jointly with market factors
(Monte Carlo only). `vol_pcs 0` keeps full rank; truncation drops residuals
loudly (`residual_share` in diagnostics).

Reference run (SOXX, 491 sessions 2024-08 → 2026-08, 5 options incl. short
calls/puts; 99%/1d VaR): frozen 4165 → bootstrap-full-rank 4788 (+15%) →
bootstrap-2PCs 4635 → gaussian-MC-3PCs 5029.

Assumptions (documented, all revisit-able): flat 4% USD curve (no rate
history in the store), 0.5% dividend, OTM-European/ITM-BS2002 inversion,
forward-filled TARGET2 holidays, mid quotes as observations. Known limits:
γ pins at the 0.5 conservative bound on ~2/3 of days (fit RMSE mean 1.4%,
concentrated short-end wings) — widening needs re-derived no-arbitrage
conditions, deliberately not done here.
