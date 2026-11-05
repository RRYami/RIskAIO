-- P7.3 Postgres read contract for market data (RiskAI).
--
-- Ingestion owns these tables. The OCaml engine never talks to Postgres
-- directly (ADR-007 purity): the worker SELECTs the v_* views below and
-- passes the rows to Risk_db.Db.bundle_of_tuples, which materialises the
-- same Marketdata.bundle the JSON fixture bridge builds.
--
-- Money columns are NUMERIC and must be selected as TEXT (::text) so the
-- exact decimal reaches Money.decimal_of_string without a float round-trip.
-- Model parameters (zero rates, vols, spreads, dividend yields) are
-- DOUBLE PRECISION, matching the engine's float64 core.

CREATE TABLE IF NOT EXISTS market_prices (
  instrument_id TEXT NOT NULL,
  date          DATE NOT NULL,
  close         NUMERIC NOT NULL,
  currency      TEXT NOT NULL,
  PRIMARY KEY (instrument_id, date)
);

CREATE TABLE IF NOT EXISTS market_fx (
  date         DATE NOT NULL,
  currency     TEXT NOT NULL,
  rate_to_eur  NUMERIC NOT NULL,
  PRIMARY KEY (date, currency)
);

-- One row per (date, curve_id, tenor). The engine expects the full Q14
-- grid per date×curve_id; Risk_db.check_curve_grid enforces it.
-- Curve ids (single table, same logic for gov/corp):
--   SWAP:<CCY>                 risk-free / derivatives discounting
--   GOV:<COUNTRY>:<CCY>        sovereign, e.g. GOV:DE:EUR, GOV:US:USD
--   ISSUER:<issuer>:<CCY>      single-name, ':' sanitised to '-'
--   RATING:<rating>:<CCY>      e.g. RATING:BBB:USD
CREATE TABLE IF NOT EXISTS market_curves (
  date        DATE NOT NULL,
  curve_id    TEXT NOT NULL,
  tenor       TEXT NOT NULL,
  tenor_years DOUBLE PRECISION NOT NULL,
  zero_rate   DOUBLE PRECISION NOT NULL,
  PRIMARY KEY (date, curve_id, tenor)
);

CREATE TABLE IF NOT EXISTS market_vols (
  instrument_id TEXT NOT NULL,
  atm_vol_30d   DOUBLE PRECISION NOT NULL,
  PRIMARY KEY (instrument_id)
);

CREATE TABLE IF NOT EXISTS market_spreads (
  instrument_id TEXT NOT NULL,
  spread        DOUBLE PRECISION NOT NULL,
  PRIMARY KEY (instrument_id)
);

-- As-of snapshot per instrument (no dividend-factor shocks in P7).
CREATE TABLE IF NOT EXISTS market_dividends (
  instrument_id  TEXT NOT NULL,
  dividend_yield DOUBLE PRECISION NOT NULL,
  PRIMARY KEY (instrument_id)
);

-- Contract views: column names/types mirror the JSON bridge
-- (fixtures/mini-v1/marketdata.json).

CREATE OR REPLACE VIEW v_prices AS
  SELECT instrument_id, date::text AS date, close::text AS close, currency
  FROM market_prices;

CREATE OR REPLACE VIEW v_fx AS
  SELECT date::text AS date, currency, rate_to_eur::text AS rate_to_eur
  FROM market_fx;

CREATE OR REPLACE VIEW v_curves AS
  SELECT date::text AS date, curve_id, tenor, tenor_years, zero_rate
  FROM market_curves;

CREATE OR REPLACE VIEW v_vols AS
  SELECT instrument_id, atm_vol_30d FROM market_vols;

CREATE OR REPLACE VIEW v_spreads AS
  SELECT instrument_id, spread FROM market_spreads;

CREATE OR REPLACE VIEW v_dividends AS
  SELECT instrument_id, dividend_yield FROM market_dividends;
