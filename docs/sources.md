# Data Sources

This repository combines public and proprietary datasets into a single `inputs.csv` file. See `data/README.md` for the full schema and source notes.

## Public sources (examples)

- DSIRE policy counts
- RegData regulatory restrictions index
- BLS QCEW employment data
- BEA GDP growth data
- EIG economic dynamism
- NREL renewable potential
- AFDC EV charging and EV registrations
- EIA electricity price and clean electric capacity

## Proprietary or internal sources (examples)

- Good Jobs First incentive compilation
- Internal program database for development policies
- Climate legislation index
- CNBC infrastructure ranking
- Investment monitoring datasets (clean tech, semiconductors, batteries, etc.)

When running in CI or without access to proprietary data, set `Electro-Industrial_USE_SAMPLE_DATA=true` (default) to use `data/examples/sample_inputs.csv`.

Legacy raw files referenced in `Legacy Script/Electro-Industrial_State.R` should be staged under `data/raw/` with matching relative paths. URL-based sources are downloaded into `data/raw/remote/` during `scripts/05_ingest_sources.R` unless `SKIP_DATA_DOWNLOADS=true`.
