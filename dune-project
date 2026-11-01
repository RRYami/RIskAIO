(lang dune 3.16)
(name risk-ai)
(generate_opam_files true)

(package
 (name risk-ai)
 (synopsis "OCaml port of the RiskStartUp risk engine (engine-first)")
 (description
  "Pure OCaml port of RiskStartUp core + engine: TARGET2 calendar, decimal boundary, risk measures, option pricing, run engine.")
 (depends
  (ocaml (>= 5.3))
  (dune (>= 3.16))
  (yojson (>= 2.0))
  (lacaml (>= 11.0))
  (zarith (>= 1.12))
  (alcotest (and (>= 1.8) :with-test))
  (ocamlformat (and (>= 0.27) :with-dev-setup))))
