-- P7d run persistence (Postgres).
--
-- One row per run; the result document (config + rows + pnl + diagnostics,
-- see Risk_api.Job.payload_to_yojson) is stored as JSONB so the API can serve
-- results without re-running the engine. Inputs are snapshotted by the caller
-- (bundle_version); the worker holds live inputs in-process.

CREATE TABLE IF NOT EXISTS risk_runs (
  id      TEXT PRIMARY KEY,
  status  TEXT NOT NULL,
  updated DOUBLE PRECISION NOT NULL,
  payload JSONB NOT NULL
);

CREATE INDEX IF NOT EXISTS risk_runs_updated_idx ON risk_runs (updated DESC);
