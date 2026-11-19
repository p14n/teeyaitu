# Handoff: teeyaitu momentum signals

Last updated 2026-10-31. The full design is *ML Momentum Trading System Implementation Guide*, saved in the claude.ai **Teeyai** project. This file covers where implementation stands.

## What this is
A Clojure Polylith workspace (top namespace `com.p14n.teeyaitu`) that ranks FTSE 100 shares by momentum each month. It backtests against a 12-minus-1 momentum baseline after spread-bet costs, and will later train an XGBoost model. It produces signals only and never places trades.

## Status

| Phase | State |
| --- | --- |
| 1. Research loop at the REPL | **Code written, not yet verified by a test run** |
| 2. Model (walk-forward XGBoost) | Not started |
| 3. Production path (IG, Lambdas, Step Functions) | Not started |

Nothing from Phase 1 is committed yet: the new `components/`, `development/src/dev/`, and the changes to `deps.edn` and `readme.md` are uncommitted.

The dependencies resolve on the Mac (tablecloth 7.029.2, parquet-hadoop 1.12.0, hadoop-common 3.3.0, hato, jsonista 0.3.8 are on the classpath in `.cpcache`). I don't have any test results yet.

## Pick up here
1. `clojure -M:poly check`, then `clojure -M:poly test :all`. Fix any failures. The likeliest trouble spots:
   - Parquet read/write in `price-store` (column types for dates and instants, string versus keyword column names on read)
   - `tc/rows ... :as-maps` and `tc/select-rows` with a predicate
   - the tolerances in the backtest sanity tests
2. Commit Phase 1.
3. Download SONIA (Bank of England series `IUDSOIA`) as a CSV to `data/sonia/IUDSOIA.csv`.
4. In `development/src/dev/research.clj`, run the `comment` block in order: `backfill!`, `feature-dataset`, `baseline-report`, `save-run!`. Accept the backfill only when the pence/pounds check passes for every ticker.
5. Start Phase 2: a `model` component with walk-forward splits plus a purge gap, a metamorph.ml XGBoost pipeline, metrics, and a save/load round-trip test. Check the scicloj.ml.xgboost option keys in its docs rather than guessing.

## Bricks

| Component | Interface highlights | Notes |
| --- | --- | --- |
| config | `load-config` | `resources/config/defaults.edn`; env overrides `TEEYAITU_DATA_ROOT`, `TEEYAITU_BUCKET`, `IG_SECRET_ARN`, `IG_ENV` |
| yahoo-client | `fetch-daily`, `parse-chart` | Unofficial v8 chart endpoint; GBp is divided by 100; 1 request/s; cache in `.cache/yahoo` |
| price-store | `write-prices!`, `read-prices`, `quality-issues`, `assert-quality!` | Local Parquet at `<root>/prices/source=*/year=*/part.parquet`; flags ~100x or ~1/100x one-day jumps |
| universe | `load-membership`, `member?`, `survivorship-warning` | Bundled FTSE 100 list as of 2025-12-07, `:point-in-time? false`; `ig-epics.edn` is empty |
| panel | `monthly-panel` | Daily vol-60d and 52-week high computed before month-end resampling |
| features | `build`, `model-feature-cols` | Lags by calendar month, per-month percentile ranks, excess return versus the month's median; keeps `:vol-60d-raw` and `:mom-12-1-raw` |
| costs | `trade-cost`, `funding-cost`, `parse-boe-csv`, `sonia-lookup` | Defaults: 10 bps spread, 5 bps slippage, 2.5% funding fee, 0.5% borrow |
| backtest | `run`, `compare-strategies`, `metrics`, `promote?` | Long-only top quintile, inverse-vol weights, 15% target vol, 10% cap, staggered sleeves for horizon > 1 |

There are no bases or projects yet. Those arrive in Phase 3: `signals-lambda`, `training-lambda`, `workflows`.

## Tests written (not yet run)
- **features:** shuffling future prices leaves features unchanged; no target columns in the feature list; ranks are computed per month
- **panel:** shuffling future daily prices leaves daily stats unchanged
- **backtest:** perfect foresight looks implausibly good; random scores match equal weight before costs; doubling the spread lowers returns by about traded notional times the extra spread; the gate; sleeves; caps
- **price-store:** pence/pounds check and Parquet round trip
- **yahoo-client:** parsing
- **costs:** formulas and SONIA CSV parsing
- **universe:** membership windows
- **config:** defaults and overrides

## Assumptions to revisit
- **Target vol:** scaling assumes perfect correlation (Σ w·σ), which is conservative, so realised vol will usually come in below target.
- **Funding nights:** counted from the month-end date to the end of the next month.
- **Missing returns:** a held stock with no forward return counts as 0% and is counted in `:missing-returns`.
- **Sharpe:** computed on net returns with no risk-free deduction, because funding already charges SONIA.
- **Equal-weight portfolio:** rebalances every month and also pays costs.
- **Backfill:** uses `:adj-close` from Yahoo only. Splicing IG closes onto Yahoo history (joining on returns) is Phase 3.
- **Noj:** sits under an optional `:notebook` alias; confirm the version (`2-beta18`) on Clojars.

## Open questions (from the guide)
- Which paid source will provide point-in-time FTSE membership and delisted prices?
- Long-only, or long-short?
- 1-month or 3-month target as the primary model?
- Terraform or CDK?

## Housekeeping
- `.cpcache/teeyaitu-phase1.tgz` and `.cpcache/readme-section.md` were only used to transfer files and can be deleted.
- `.clj-kondo/` is untracked; add it to `.gitignore` or commit its config as you prefer.
- `tref/` is the 2013 Leiningen prototype, kept for reference.
