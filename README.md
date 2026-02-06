# Electro-Industrial Index

This repository builds the Electro-Industrial Index and its component sub-indices from a standardized `inputs.csv` data contract. The pipeline is organized in an OSI-style scripted structure with explicit configuration, deterministic staging, and reproducible outputs.

## What this repo produces

Running the pipeline generates the following deliverables in `outputs/`:

- `outputs/Electro-Industrial.csv`
- `outputs/Electro-Industrial_state.csv`
- `outputs/Electro-Industrial_pea.csv`
- `outputs/deployment.csv`
- `outputs/reg_friction.csv`
- `outputs/cluster_index.csv`
- `outputs/cluster_index_pea.csv`
- `outputs/Electro-Industrial_Index_Tables.xlsx`
- `outputs/metadata/run_metadata.csv`

See `OUTPUTS_DATA_DICTIONARY.md` for column definitions.

## Inputs and data contract

The pipeline expects a single `data/inputs.csv` file matching the schema documented in `data/README.md`. For CI and non-proprietary runs, the default is to use `data/examples/sample_inputs.csv`.

## Methodology

The canonical methodology is defined by `Legacy Script/Electro-Industrial_State.R`. The refactor preserves the logic while exposing it in modular scripts and configuration files. See `docs/methodology.md` for detailed sub-index definitions and weighting.

## Repository layout

```
.
├── R/
│   ├── categories/
│   ├── indices/
│   └── utils/
├── config/
├── data/
│   ├── examples/
│   ├── processed/
│   └── raw/
├── docs/
├── scripts/
├── tests/
└── run_pipeline.R
```

## Quick start

```bash
# from repo root
Rscript -e "renv::restore()"
Rscript run_pipeline.R
```

To run against real data, set:

```bash
export Electro-Industrial_USE_SAMPLE_DATA=false
export Electro-Industrial_DATA_DIR=/path/to/data
```

Legacy raw files referenced in `Legacy Script/Electro-Industrial_State.R` should be staged under `data/raw/` using the same relative paths (for example, `data/raw/Good Jobs First/gjf_complete.csv`). URL-based sources are downloaded by `scripts/05_ingest_sources.R` into `data/raw/remote/` unless `SKIP_DATA_DOWNLOADS=true`.

## Configuration

Config files live in `config/`:

- `config.yml` (defaults for local runs)
- `config.example.yml` (template)
- `weights.yml` (index weights)
- `index_definition.yml` (variables and polarity)
- `missing_data.yml` (documentation of missing data handling)

Environment variable overrides:

- `Electro-Industrial_CONFIG`
- `Electro-Industrial_WEIGHTS`
- `Electro-Industrial_INDEX_DEFINITION`
- `Electro-Industrial_MISSING_DATA`
- `Electro-Industrial_DATA_DIR`
- `Electro-Industrial_OUTPUT_DIR`
- `Electro-Industrial_USE_SAMPLE_DATA`
- `Electro-Industrial_WRITE_AUDIT`

## Outputs and audits

In addition to the required outputs, you can enable a long-form audit table by setting `Electro-Industrial_WRITE_AUDIT=true` (default). The audit file is written to `outputs/audit/Electro-Industrial_audit_long.csv`.

## Testing

```bash
Rscript -e "renv::restore()"
Rscript -e "testthat::test_dir('tests/testthat')"
```

## Data sources

See `docs/sources.md` for a summary of public and proprietary inputs.

## Known issues / TODOs

- CI runs are configured for sample data only; proprietary data must be supplied locally.
- If you need to validate the refactor against full production data, run the pipeline with `Electro-Industrial_USE_SAMPLE_DATA=false` and compare outputs.

## License

MIT License. See `LICENSE`.
