# Electrotech Index

This repository refactors the legacy `Legacy Script/Electrotech_State.R` into a reproducible R package and `{targets}` pipeline for the Electrotech Index. It emphasizes reproducibility, transparency, and maintainability while keeping the data contracts explicit.

## Quickstart

```bash
# from repo root
Rscript -e "renv::restore()"
Rscript scripts/run_pipeline.R
```

By default the pipeline runs against bundled sample data (`inst/extdata/sample_inputs.csv`). To run against real data, set:

```bash
export ELECTROTECH_USE_SAMPLE_DATA=false
export ELECTROTECH_DATA_DIR=/path/to/data
```

See `data/README.md` for required inputs and schemas.

## Outputs

All outputs are written to `outputs/` (override with `ELECTROTECH_OUTPUT_DIR`).

- `outputs/electrotech.csv`
- `outputs/deployment.csv`
- `outputs/reg_friction.csv`
- `outputs/cluster_index.csv`
- `outputs/Electrotech_Index_Tables.xlsx`
- `outputs/metadata/run_metadata.csv`

See `OUTPUTS_DATA_DICTIONARY.md` for column definitions.

## Reproducibility

- Package dependencies are pinned in `renv.lock`.
- All file paths are configurable via `config.yml` or environment variables.
- Downloaded files should be cached in `data/raw_cache/` via helper utilities.

## Citation

See `CITATION.cff` and `inst/CITATION` for citation instructions.

## License

MIT License. See `LICENSE`.
