#!/usr/bin/env python3
"""Deterministic mini-universe fixture exporter for RiskAI (OCaml port).

Writes a small synthetic book + market data that mirrors the RiskStartUp
contracts, so the OCaml engine can be parity-tested without Parquet/S3:

* holdings columns == RiskStartUp ``core/holdings.py::COLUMNS``
* market-data tables == ``core/marketdata.py::SCHEMAS`` keys (prices/fx/curves/
  vols/spreads/dividends) and the 12-tenor Q14 grid
* FX start levels == ``marketdata/universe.py::START_FX_TO_EUR``
* TARGET2 calendar == ``core/calendar.py`` (weekdays + fixed + Easter algorithm)

Provisional: instrument ``terms`` are labelled ``terms_schema: provisional-v1``
in the manifest. P3 will pin them against the Q19 taxonomy contract; until
then the OCaml side treats terms as schemaless JSON.

Layout under --out:
  instruments.json  holdings.csv  holdings.json  marketdata.json
  marketdata/*.csv (human-friendly mirrors)  config.json  manifest.json

Stdlib only. Deterministic for a given --seed.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import random
import sys
from datetime import date, timedelta
from pathlib import Path

NAMESPACE = "6f1c0b1e-2d4a-4c7e-9a51-7c1f3f6f2b10"  # same as universe.py
CCYS = ("EUR", "USD", "GBP")
START_FX_TO_EUR = {"EUR": 1.0, "USD": 0.92, "GBP": 1.17}
SECTORS = ("technology", "financials", "health_care", "energy", "industrials", "consumer")
TENORS = (
    ("1M", 1 / 12), ("3M", 0.25), ("6M", 0.5), ("1Y", 1.0), ("2Y", 2.0), ("3Y", 3.0),
    ("5Y", 5.0), ("7Y", 7.0), ("10Y", 10.0), ("15Y", 15.0), ("20Y", 20.0), ("30Y", 30.0),
)
BASE_RATES = {"EUR": 0.02, "USD": 0.035, "GBP": 0.03}
HOLDINGS_COLUMNS = (
    "instrument_id", "quantity", "market_value_local", "currency",
    "price", "raw_identifier", "raw_identifier_type",
)


def easter_sunday(year: int) -> date:
    a = year % 19
    b, c = divmod(year, 100)
    d, e = divmod(b, 4)
    f = (b + 8) // 25
    g = (b - f + 1) // 3
    h = (19 * a + b - d - g + 15) % 30
    i, k = divmod(c, 4)
    ell = (32 + 2 * e + 2 * i - h - k) % 7
    m = (a + 11 * h + 22 * ell) // 451
    month, day = divmod(h + ell - 7 * m + 114, 31)
    return date(year, month, day + 1)


def target2_holidays(year: int) -> set[date]:
    e = easter_sunday(year)
    return {
        date(year, 1, 1), e - timedelta(days=2), e + timedelta(days=1),
        date(year, 5, 1), date(year, 12, 25), date(year, 12, 26),
    }


def is_business_day(d: date) -> bool:
    return d.weekday() < 5 and d not in target2_holidays(d.year)


def business_days_ending(as_of: date, count: int) -> list[date]:
    if not is_business_day(as_of):
        raise ValueError(f"{as_of} is not a TARGET2 business day")
    out = [as_of]
    while len(out) < count:
        d = out[-1] - timedelta(days=1)
        while not is_business_day(d):
            d -= timedelta(days=1)
        out.append(d)
    return out[::-1]


def isin_check_digit(body: str) -> str:
    digits = "".join(str(int(c, 36)) for c in body)
    total = 0
    for i, ch in enumerate(reversed(digits)):
        n = int(ch)
        if i % 2 == 0:
            n *= 2
            if n > 9:
                n -= 9
        total += n
    return str((10 - total % 10) % 10)


def uid(rng: random.Random, key: str) -> str:
    import uuid

    return str(uuid.uuid5(uuid.UUID(NAMESPACE), f"mini-v1:{key}"))


def gbm(rng: random.Random, start: float, vol: float, n: int) -> list[float]:
    px = [start]
    for _ in range(n - 1):
        px.append(px[-1] * math.exp(rng.gauss(0.0, vol) + 0.0002))
    return px


def build(args: argparse.Namespace) -> dict[str, object]:
    rng = random.Random(args.seed)
    as_of = date.fromisoformat(args.as_of)
    bizdays = business_days_ending(as_of, args.days)
    n = len(bizdays)

    instruments: list[dict] = []
    serial = [0]
    COUNTRY = {"EUR": "DE", "USD": "US", "GBP": "GB"}
    EXCHANGE = {"EUR": "XETR", "USD": "XNYS", "GBP": "XLON"}

    def add(key, type_, ccy, terms, ident) -> str:
        iid = uid(rng, key)
        instruments.append({
            "instrument_id": iid, "type": type_, "currency": ccy,
            "terms": terms, "overlay": False, "base_instrument_id": None,
            "identifiers": [{
                "instrument_id": iid, "scheme": ident[0], "value": ident[1],
                "valid_from": "2020-01-01", "valid_to": None, "overlay": False,
            }],
        })
        return iid

    def isin(ccy: str) -> str:
        serial[0] += 1
        body = f"{COUNTRY[ccy]}{serial[0]:09d}"
        return body + isin_check_digit(body)

    # Terms schema mirrors marketdata/universe.py (reference-v1 conventions).
    eq_ids: dict[str, list[str]] = {c: [] for c in CCYS}
    for ccy in CCYS:
        country, exch = COUNTRY[ccy], EXCHANGE[ccy]
        for e in range(args.equities):
            sector = SECTORS[e % len(SECTORS)]
            key = f"EQ-{ccy}-{e}"
            iid = add(key, "EQUITY", ccy, {
                "issuer": f"{country} {sector} co {e}", "country": country,
                "sector": sector, "exchange": exch,
            }, ("ISIN", isin(ccy)))
            eq_ids[ccy].append(iid)
        add(f"ETF-{ccy}", "ETF", ccy, {
            "issuer": f"{country} index fund sponsor", "country": country,
            "sector": "index", "exchange": exch,
        }, ("ISIN", isin(ccy)))
    add("GOV-EUR-1", "GOV_BOND", "EUR", {
        "coupon_rate": 0.025, "coupon_frequency": 1, "maturity": "2035-06-15",
        "day_count": "ACT/ACT", "issue_date": "2020-06-15",
        "country": "DE", "issuer": "DE sovereign",
        "issuer_rating": "AAA", "bond_rating": "AAA",
        "par_value": 100.0,
    }, ("ISIN", isin("EUR")))
    add("CORP-USD-1", "CORP_BOND", "USD", {
        "coupon_rate": 0.045, "coupon_frequency": 2, "maturity": "2030-03-01",
        "day_count": "30/360", "seniority": "senior", "issuer": "US energy co 0",
        "country": "US", "issuer_rating": "BBB", "bond_rating": "BBB",
        "par_value": 100.0, "issue_date": "2020-03-01",
    }, ("ISIN", isin("USD")))
    add("CASH-EUR", "CASH", "EUR", {}, ("BBG_TICKER", "EUR CASH"))
    add("FUND-USD-1", "FUND", "USD", {"issuer": "US asset manager 0"},
        ("ISIN", isin("USD")))
    etf_eur = next(i["instrument_id"] for i in instruments if i["type"] == "ETF" and i["currency"] == "EUR")
    add("FUT-EUR-1", "FUTURE", "EUR", {
        "underlying_instrument_id": etf_eur, "multiplier": 10, "expiry": "2026-12-18",
    }, ("ISIN", isin("EUR")))
    opt_under = eq_ids["EUR"][0]
    opt_iid = add("OPT-EUR-1", "LISTED_OPTION", "EUR", {
        "underlying_instrument_id": opt_under, "strike": 0.0,  # set after paths
        "expiry": "2027-06-18", "call_put": "CALL", "style": "AMERICAN",
        "multiplier": 100,
    }, ("ISIN", isin("EUR")))
    add("FXF-EURUSD-1", "FX_FORWARD", "EUR", {
        "buy_currency": "EUR", "sell_currency": "USD",
        "buy_notional": "1000000.00", "sell_notional": "0.00",  # set after FX paths
        "settlement_date": "2027-03-17",
    }, ("BBG_TICKER", "EURUSD 2027-03-17 FWD"))

    # --- price paths ---
    paths: dict[str, list[float]] = {}
    for inst in instruments:
        ty = inst["type"]
        if ty in ("EQUITY", "ETF", "FUND"):
            paths[inst["instrument_id"]] = gbm(rng, rng.uniform(20.0, 150.0), 0.015, n)
        elif ty in ("GOV_BOND", "CORP_BOND"):
            paths[inst["instrument_id"]] = gbm(rng, rng.uniform(95.0, 105.0), 0.002, n)
        elif ty == "CASH":
            paths[inst["instrument_id"]] = [1.0] * n
        elif ty == "FUTURE":
            paths[inst["instrument_id"]] = list(paths[inst["terms"]["underlying_instrument_id"]])
        elif ty == "LISTED_OPTION":
            paths[inst["instrument_id"]] = list(paths[inst["terms"]["underlying_instrument_id"]])
        elif ty == "FX_FORWARD":
            paths[inst["instrument_id"]] = [1.0] * n

    fx_paths: dict[str, list[float]] = {"EUR": [1.0] * n}
    for ccy in ("USD", "GBP"):
        fx_paths[ccy] = gbm(rng, START_FX_TO_EUR[ccy], 0.005, n)

    # Strike ATM on the as-of underlying close; notionals consistent with FX.
    for inst in instruments:
        if inst["instrument_id"] == opt_iid:
            inst["terms"]["strike"] = round(paths[opt_under][-1], 2)
        if inst["type"] == "FX_FORWARD":
            inst["terms"]["sell_notional"] = f"{1000000 / fx_paths['USD'][-1]:.2f}"

    prices, curves = [], []
    for inst in instruments:
        iid = inst["instrument_id"]
        for d, px in zip(bizdays, paths[iid]):
            prices.append({"instrument_id": iid, "date": d.isoformat(),
                           "close": f"{px:.6f}", "currency": inst["currency"]})
    fx = []
    for ccy in CCYS:
        for d, r in zip(bizdays, fx_paths[ccy]):
            fx.append({"date": d.isoformat(), "currency": ccy, "rate_to_eur": f"{r:.10f}"})
    # Unified curve table: SWAP (risk-free) + GOV + ISSUER + RATING.
    # curve_id encodes issuer/rating/country + currency suffix for
    # Currency-scoped rate shocks (suffix after last ':').
    CURVE_DEFS = [
        ("SWAP:EUR", "EUR", 0.0),
        ("SWAP:USD", "USD", 0.0),
        ("SWAP:GBP", "GBP", 0.0),
        ("GOV:DE:EUR", "EUR", 0.0002),
        ("GOV:US:USD", "USD", 0.0003),
        ("GOV:GB:GBP", "GBP", 0.0002),
        ("ISSUER:US energy co 0:USD", "USD", 0.015),
        ("RATING:BBB:USD", "USD", 0.012),
        ("RATING:AAA:EUR", "EUR", 0.0002),
    ]
    for curve_id, ccy, add_on in CURVE_DEFS:
        for d in bizdays:
            base = BASE_RATES[ccy] + add_on + rng.gauss(0.0, 0.0005)
            for tenor, yrs in TENORS:
                curves.append({"date": d.isoformat(), "curve_id": curve_id, "tenor": tenor,
                               "tenor_years": yrs,
                               "zero_rate": base + 0.0005 * yrs + rng.gauss(0.0, 0.0001)})
    vols = [{"instrument_id": i["instrument_id"],
             "atm_vol_30d": round(rng.uniform(0.18, 0.35), 6)}
            for i in instruments if i["type"] in ("EQUITY", "ETF", "FUND")]
    # Credit lives in ISSUER/RATING curves; residual basis is zero.
    spreads = [{"instrument_id": i["instrument_id"], "spread": 0.0}
               for i in instruments if i["type"] in ("GOV_BOND", "CORP_BOND")]
    dividends = [{"instrument_id": i["instrument_id"],
                  "dividend_yield": round(rng.uniform(0.0, 0.03), 6)}
                 for i in instruments if i["type"] in ("EQUITY", "ETF", "FUND")]

    # --- holdings: one row per instrument, model-NAV quantities ---
    holdings = []
    for inst in instruments:
        iid = inst["instrument_id"]
        last = paths[iid][-1]
        ty = inst["type"]
        if ty in ("GOV_BOND", "CORP_BOND"):
            qty = float(rng.randint(1000, 10000)) 
        elif ty == "CASH":
            qty = 50000.0
        elif ty in ("FUTURE", "LISTED_OPTION"):
            qty = float(rng.randint(1, 10))
        elif ty == "FX_FORWARD":
            qty = 1.0
        else:
            qty = float(rng.randint(10, 500))
        mv = qty * last
        ident = inst["identifiers"][0]
        holdings.append({
            "instrument_id": iid, "quantity": f"{qty:.6f}",
            "market_value_local": f"{mv:.2f}", "currency": inst["currency"],
            "price": f"{last:.6f}", "raw_identifier": ident["value"],
            "raw_identifier_type": ident["scheme"],
        })

    history_days = min(500, n - 1)
    config = {
        "method": "historical", "confidences": [0.95, 0.975, 0.99],
        "horizons": [1, 10, 20], "history_days": history_days,
        "weighting": "equal", "brw_lambda": 0.99, "covariance": "ewma",
        "ewma_lambda": 0.97, "curve_pcs": 3, "distribution": "gaussian",
        "nu": 5.0, "n_scenarios": 10000, "seed": args.seed,
        "component_confidence": 0.99, "ucits_limit": 0.20,
        "stress": [
            {"id": "EQ_DOWN_20", "label": "Equities −20%",
             "shocks": [{"kind": "equity", "size": -0.20, "scope": "all", "target": None}]},
            {"id": "RATES_UP_100", "label": "Rates +100bp parallel",
             "shocks": [{"kind": "rates", "size": 100.0, "scope": "all", "target": None}]},
        ],
        "historical_windows": [], "worst_window_days": 20, "chunk_size": 2000,
    }
    return {"instruments": instruments, "holdings": holdings,
            "marketdata": {"prices": prices, "fx": fx, "curves": curves,
                           "vols": vols, "spreads": spreads, "dividends": dividends},
            "config": config, "as_of": as_of.isoformat(), "bizdays": n}


def sha(b: bytes) -> str:
    return hashlib.sha256(b).hexdigest()


def main() -> int:
    ap = argparse.ArgumentParser(description="Export deterministic RiskAI fixtures")
    ap.add_argument("--out", required=True)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--equities", type=int, default=2, help="equities per currency")
    ap.add_argument("--days", type=int, default=300, help="business days of history")
    ap.add_argument("--as-of", default="2026-09-22")
    args = ap.parse_args()

    if args.days < 251:
        print("need --days >= 251 (history window + test days)", file=sys.stderr)
        return 2
    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)

    fic = build(args)
    files: dict[str, bytes] = {}
    files["instruments.json"] = json.dumps(fic["instruments"], indent=2, sort_keys=True).encode()
    files["holdings.json"] = json.dumps(fic["holdings"], indent=2, sort_keys=True).encode()

    rows = [["instrument_id"] + list(HOLDINGS_COLUMNS[1:])]
    buf = ["instrument_id," + ",".join(HOLDINGS_COLUMNS[1:])]
    for h in fic["holdings"]:
        buf.append(",".join(h[c] for c in HOLDINGS_COLUMNS))
    files["holdings.csv"] = ("\n".join(buf) + "\n").encode()

    files["marketdata.json"] = json.dumps(fic["marketdata"], indent=2, sort_keys=True).encode()
    md_dir = out / "marketdata"
    md_dir.mkdir(exist_ok=True)
    for name, cols in [("prices", ("instrument_id", "date", "close", "currency")),
                       ("fx", ("date", "currency", "rate_to_eur")),
                       ("curves", ("date", "curve_id", "tenor", "tenor_years", "zero_rate")),
                       ("vols", ("instrument_id", "atm_vol_30d")),
                       ("spreads", ("instrument_id", "spread")),
                       ("dividends", ("instrument_id", "dividend_yield"))]:
        lines = [",".join(cols)]
        for r in fic["marketdata"][name]:
            lines.append(",".join(str(r[c]) for c in cols))
        (md_dir / f"{name}.csv").write_text("\n".join(lines) + "\n")
    files["config.json"] = json.dumps(fic["config"], indent=2, sort_keys=True).encode()

    manifest = {
        "generator": "risk-ai/export_fixtures.py",
        "version": "mini-v1",
        "terms_schema": "universe-v2",
        "seed": args.seed,
        "as_of": fic["as_of"],
        "business_days": fic["bizdays"],
        "counts": {
            "instruments": len(fic["instruments"]),
            "holdings": len(fic["holdings"]),
            **{k: len(v) for k, v in fic["marketdata"].items()},
        },
        "files": {name: {"sha256": sha(b), "bytes": len(b)} for name, b in sorted(files.items())},
    }
    files["manifest.json"] = json.dumps(manifest, indent=2, sort_keys=True).encode()

    for name, data in files.items():
        (out / name).write_bytes(data)

    # --- self-verify: re-read everything, check shas + contracts ---
    ok = True
    for name, data in files.items():
        if (out / name).read_bytes() != data:
            print(f"roundtrip mismatch: {name}", file=sys.stderr)
            ok = False
    man = json.loads((out / "manifest.json").read_text())
    for name, meta in man["files"].items():
        if sha((out / name).read_bytes()) != meta["sha256"]:
            print(f"sha mismatch: {name}", file=sys.stderr)
            ok = False
    with (out / "holdings.csv").open() as f:
        if next(csv.reader(f)) != list(HOLDINGS_COLUMNS):
            print("holdings.csv header != contract", file=sys.stderr)
            ok = False
    if not ok:
        return 1
    c = manifest["counts"]
    print(f"mini-v1 seed={args.seed} as_of={fic['as_of']} days={fic['bizdays']}: "
          f"{c['instruments']} instruments, {c['holdings']} holdings, "
          f"{c['prices']} prices, {c['fx']} fx, {c['curves']} curves")
    return 0


if __name__ == "__main__":
    sys.exit(main())
