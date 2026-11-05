# Fixture exporter (Python, stdlib-only)

`export_fixtures.py` generates the deterministic universes the OCaml engine
and its tests consume. It lives here (not in RiskStartUp) so the OCaml repo
is self-contained; the only coupling to RiskStartUp is the *contract* (column
names, tenor grid, FX levels, TARGET2 rules), not its code.

```bash
python3 python/export_fixtures.py --out fixtures/mini-v1 --seed 42
# or: make fixtures
```

The cross-language oracle goldens (`fixtures/mini-v1/oracle_p*.json`) are a
separate matter: they were produced by running RiskStartUp's own engine on
these fixtures, and the scripts that did that live outside this repo now.
The oracle-gated OCaml tests consume the goldens as data and skip when they
are absent — nothing in `dune build` or `dune runtest` needs Python or the
RiskStartUp checkout.

## Contract mirrors

| Fixture | RiskStartUp source |
|---|---|
| `holdings.csv` header | `core/holdings.py::COLUMNS` |
| `marketdata.{prices,fx,curves,vols,spreads,dividends}` | `core/marketdata.py::SCHEMAS` keys + Q14 tenor grid |
| FX start levels | `marketdata/universe.py::START_FX_TO_EUR` |
| Business days | `core/calendar.py` (same Easter algorithm) |
| `config.json` | `engine/config.py::RunConfig` defaults (2-scenario subset) |

## Provisional

Instrument `terms` follow the `marketdata/universe.py` reference-v1 schema
(`terms_schema: universe-v2` in the manifest): `coupon_rate`/`coupon_frequency`/
`maturity`/`country`/`issuer`/`issuer_rating`/`bond_rating`/`par_value`(+`notional`
alias)/`issue_date` bonds, `call_put`/`style`/`strike`/`underlying_instrument_id` options,
`buy_currency`/`sell_currency`/`*_notional`/`settlement_date` FX forwards.

Curves are a unified table keyed by `curve_id` (`SWAP:<ccy>`, `GOV:<country>:<ccy>`,
`ISSUER:<issuer>:<ccy>`, `RATING:<rating>:<ccy>`). Gov bonds price on
`GOV:<country>:<ccy>`; corp bonds on `ISSUER:<issuer>:<ccy>` if present else
`RATING:<bond_rating>:<ccy>`. Face is `quantity × par_value` (bond count).
