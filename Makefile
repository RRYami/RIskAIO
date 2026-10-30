SWITCH ?= 5.3.0
DUNE := opam exec --switch=$(SWITCH) -- dune

.PHONY: switch deps build test test-unit fixtures parity fmt clean

switch: # one-time: create the OCaml 5.3.0 opam switch
	opam switch create $(SWITCH) ocaml-base-compiler.5.3.0

deps: # install build + test deps into the switch (no sudo needed)
	opam install --switch=$(SWITCH) dune alcotest yojson ocamlformat lacaml

build:
	$(DUNE) build

test-unit test:
	$(DUNE) runtest

fixtures: # regenerate the deterministic mini universe (stdlib-only python)
	python3 python/export_fixtures.py --out fixtures/mini-v1 --seed 42

# NOTE: the cross-language oracle goldens (fixtures/mini-v1/oracle_p*.json)
# were generated outside this repo by running RiskStartUp's engine on the
# fixtures; the generation scripts no longer live here. Existing goldens
# keep working; the oracle-gated tests skip when they are absent.

parity: build fixtures
	RISK_AI_FIXTURES=$(CURDIR)/fixtures/mini-v1 $(DUNE) runtest --force
	$(DUNE) exec bin/main.exe -- parity --fixture fixtures/mini-v1

perf: build # scaled probe: 190-instrument fixture + timings (see PERF.md)
	python3 python/export_fixtures.py --out fixtures/perf-v1 --seed 7 --equities 60 --days 550
	$(DUNE) exec bin/perf.exe -- --fixture fixtures/perf-v1 --mc 10000 --amer 20 5000

perf-big: build # full-scale probe: ~2000 positions (see PERF.md)
	python3 python/export_fixtures.py --out fixtures/perf-big --seed 11 --equities 660 --days 260
	$(DUNE) exec bin/perf.exe -- --fixture fixtures/perf-big --mc 10000 --amer 20 5000

fmt:
	$(DUNE) fmt

clean:
	$(DUNE) clean
	rm -rf fixtures/mini-v1
