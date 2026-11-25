<img src="logo.png" width="30%" alt="Polylith" id="logo">

The Polylith documentation can be found here:

- The [high-level documentation](https://polylith.gitbook.io/polylith)
- The [poly tool documentation](https://cljdoc.org/d/polylith/clj-poly/CURRENT)
- The [RealWorld example app documentation](https://github.com/furkan3ayraktar/clojure-polylith-realworld-example-app)

You can also get in touch with the Polylith Team on [Slack](https://clojurians.slack.com/archives/C013B7MQHJQ).

<h1>teeyaitu</h1>

<p>Add your workspace documentation here...</p>


## Momentum signals (Phase 1)

Implementation of the *ML Momentum Trading System* guide (saved in the Teeyai project).
Signals only: nothing here places trades.

| Component | Purpose |
| --- | --- |
| config | Defaults in `resources/config/defaults.edn`, env overrides |
| yahoo-client | Research backfill from Yahoo's daily chart endpoint (pence -> pounds) |
| price-store | Raw daily table as Parquet, `prices/source=*/year=*/part.parquet`; pence/pounds jump check |
| universe | Index membership `[:ticker :from :to]`; bundled FTSE 100 list is **not point-in-time** |
| panel | Daily vol and 52-week high, then month-end resampling |
| features | Momentum features, per-month percentile ranks, forward excess-return targets |
| costs | Spread, slippage, IG overnight funding, SONIA series |
| backtest | Portfolio simulation, metrics, baseline comparison, promotion gate |

```
clojure -M:poly check
clojure -M:poly test :all
clj -M:dev:test          # REPL; see development/src/dev/research.clj
```

`tref/` holds the earlier Leiningen prototype, kept for reference.
