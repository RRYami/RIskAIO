(** Postgres read contract (P7.3). The worker SELECTs the [v_*] views in
    [db/schema.sql] and passes the rows here; this module materialises the same
    {!Risk_core.Marketdata.bundle} the JSON fixture bridge builds, so the engine
    stays pure and file fixtures keep working for tests.

    Column contract (mirrors [fixtures/mini-v1/marketdata.json]):
    - prices: instrument_id, date, close (decimal text), currency
    - fx: date, currency, rate_to_eur (decimal text)
    - curves: date, currency, tenor, tenor_years, zero_rate (float64)
    - vols: instrument_id, atm_vol_30d (float64)
    - spreads: instrument_id, spread (float64)
    - dividends: instrument_id, dividend_yield (float64, as-of snapshot)

    Money arrives as decimal text (NUMERIC selected via [::text]) and is parsed
    by {!Risk_core.Money.decimal_of_string}: exact rational plus float64 image,
    float ingress still impossible. *)

open Risk_core

type price_tuple = {
  instrument_id : string;
  date : string;
  close : string;
  currency : string;
}

type fx_tuple = { date : string; currency : string; rate_to_eur : string }

type curve_tuple = {
  date : string;
  currency : string;
  tenor : string;
  tenor_years : float;
  zero_rate : float;
}

type vol_tuple = { instrument_id : string; atm_vol_30d : float }
type spread_tuple = { instrument_id : string; spread : float }
type dividend_tuple = { instrument_id : string; dividend_yield : float }

(** View names in [db/schema.sql]. *)
let views =
  [ "v_prices"; "v_fx"; "v_curves"; "v_vols"; "v_spreads"; "v_dividends" ]

(** Every date×currency in the history must carry the full Q14 tenor grid, in
    order; otherwise factor building would silently misalign. Raises
    [Invalid_argument] naming the first offending group. *)
let check_curve_grid (curves : Marketdata.curve_row list) =
  let expected = List.map fst Marketdata.tenors in
  let groups = Hashtbl.create 16 in
  List.iter
    (fun (r : Marketdata.curve_row) ->
      let k = (Calendar.to_string r.date, r.currency) in
      Hashtbl.replace groups k
        ((r.tenor, r.tenor_years)
        :: (Hashtbl.find_opt groups k |> Option.value ~default:[])))
    curves;
  Hashtbl.iter
    (fun (ds, ccy) tenors ->
      let names = List.map fst (List.sort compare tenors) in
      let want = List.sort compare expected in
      if names <> want then
        invalid_arg
          (Printf.sprintf
             "db: curve grid for %s on %s is [%s], expected Q14 [%s]" ccy ds
             (String.concat "," names) (String.concat "," want)))
    groups

let bundle_of_tuples ~prices ~fx ~curves ~vols ~spreads ~dividends =
  let price_rows =
    List.map
      (fun (p : price_tuple) ->
        {
          Marketdata.instrument_id = p.instrument_id;
          date = Calendar.of_string p.date;
          close = Money.decimal_of_string p.close;
          currency = p.currency;
        })
      prices
  and fx_rows =
    List.map
      (fun (f : fx_tuple) ->
        {
          Marketdata.date = Calendar.of_string f.date;
          currency = f.currency;
          rate_to_eur = Money.decimal_of_string f.rate_to_eur;
        })
      fx
  and curve_rows =
    List.map
      (fun (c : curve_tuple) ->
        {
          Marketdata.date = Calendar.of_string c.date;
          currency = c.currency;
          tenor = c.tenor;
          tenor_years = c.tenor_years;
          zero_rate = c.zero_rate;
        })
      curves
  and vol_rows =
    List.map
      (fun (v : vol_tuple) ->
        {
          Marketdata.instrument_id = v.instrument_id;
          atm_vol_30d = v.atm_vol_30d;
        })
      vols
  and spread_rows =
    List.map
      (fun (s : spread_tuple) ->
        { Marketdata.instrument_id = s.instrument_id; spread = s.spread })
      spreads
  and dividend_rows =
    List.map
      (fun (d : dividend_tuple) ->
        {
          Marketdata.instrument_id = d.instrument_id;
          dividend_yield = d.dividend_yield;
        })
      dividends
  in
  check_curve_grid curve_rows;
  {
    Marketdata.prices = price_rows;
    fx = fx_rows;
    curves = curve_rows;
    vols = vol_rows;
    spreads = spread_rows;
    dividends = dividend_rows;
  }
