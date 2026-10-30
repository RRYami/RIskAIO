(** Market-data snapshot contract (ADR-006). Port of [core/marketdata.py].

    Divergence (greenfield, agreed): the Python bundle is four Parquet files + a
    sha-pinned manifest. There is no mature OCaml Parquet library, so the OCaml
    bundle is an in-memory record loaded from the JSON/CSV fixtures written by
    [python/export_fixtures.py]. Money-like values (closes, FX) stay exact
    {!Money.t} at the boundary (invariant §3.7); model parameters (zero rates,
    vols) are float64. *)

(** Q14 tenor grid; linear interpolation on zero rates inside, flat outside. *)
let tenors =
  [
    ("1M", 1. /. 12.);
    ("3M", 0.25);
    ("6M", 0.5);
    ("1Y", 1.);
    ("2Y", 2.);
    ("3Y", 3.);
    ("5Y", 5.);
    ("7Y", 7.);
    ("10Y", 10.);
    ("15Y", 15.);
    ("20Y", 20.);
    ("30Y", 30.);
  ]

let base_currency = "EUR"

type price_row = {
  instrument_id : string;
  date : Calendar.date;
  close : Money.t;
  currency : string;
}

type fx_row = { date : Calendar.date; currency : string; rate_to_eur : Money.t }

type curve_row = {
  date : Calendar.date;
  currency : string;
  tenor : string;
  tenor_years : float;
  zero_rate : float;
}

type vol_row = { instrument_id : string; atm_vol_30d : float }
type spread_row = { instrument_id : string; spread : float }
type dividend_row = { instrument_id : string; dividend_yield : float }

type bundle = {
  prices : price_row list;
  fx : fx_row list;
  curves : curve_row list;
  vols : vol_row list;
  spreads : spread_row list;
  dividends : dividend_row list;
}

(** [(currency, rate_to_eur)] on one date. EUR itself is not synthesised —
    callers add it, matching the Python [fx_rates] behaviour. *)
let fx_rates (b : bundle) on =
  List.filter_map
    (fun (r : fx_row) ->
      if Calendar.equal r.date on then Some (r.currency, r.rate_to_eur)
      else None)
    b.fx

(* --- JSON fixture bridge (schema: fixtures/mini-v1, see python/README.md) --- *)

open Yojson.Basic.Util

let money_of_json j = Money.decimal_of_string (to_string j)
let date_of_json j = Calendar.of_string (to_string j)

let price_row_of_yojson j =
  {
    instrument_id = j |> member "instrument_id" |> to_string;
    date = j |> member "date" |> date_of_json;
    close = j |> member "close" |> money_of_json;
    currency = j |> member "currency" |> to_string;
  }

let fx_row_of_yojson j =
  {
    date = j |> member "date" |> date_of_json;
    currency = j |> member "currency" |> to_string;
    rate_to_eur = j |> member "rate_to_eur" |> money_of_json;
  }

let curve_row_of_yojson j =
  {
    date = j |> member "date" |> date_of_json;
    currency = j |> member "currency" |> to_string;
    tenor = j |> member "tenor" |> to_string;
    tenor_years = j |> member "tenor_years" |> to_number;
    zero_rate = j |> member "zero_rate" |> to_number;
  }

let vol_row_of_yojson j =
  {
    instrument_id = j |> member "instrument_id" |> to_string;
    atm_vol_30d = j |> member "atm_vol_30d" |> to_number;
  }

let spread_row_of_yojson j =
  {
    instrument_id = j |> member "instrument_id" |> to_string;
    spread = j |> member "spread" |> to_number;
  }

let dividend_row_of_yojson j =
  {
    instrument_id = j |> member "instrument_id" |> to_string;
    dividend_yield = j |> member "dividend_yield" |> to_number;
  }

let bundle_of_yojson j =
  {
    prices = j |> member "prices" |> to_list |> List.map price_row_of_yojson;
    fx = j |> member "fx" |> to_list |> List.map fx_row_of_yojson;
    curves = j |> member "curves" |> to_list |> List.map curve_row_of_yojson;
    vols = j |> member "vols" |> to_list |> List.map vol_row_of_yojson;
    spreads = j |> member "spreads" |> to_list |> List.map spread_row_of_yojson;
    dividends =
      j |> member "dividends" |> to_list |> List.map dividend_row_of_yojson;
  }

let bundle_of_file path = bundle_of_yojson (Yojson.Basic.from_file path)
