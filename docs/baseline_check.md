# Baseline Check

## Pre-refactor baseline

- Timestamp (UTC): 2026-01-27T20:38:21Z
- Command(s):
  - `Rscript scripts/run_pipeline.R`

### Result

The baseline pipeline could not be executed because `Rscript` is not available in the current environment. As a result, output files were not generated and checksums could not be computed.

| Output file | Checksum (sha256) |
| --- | --- |
| outputs/Electro-Industrial.csv | N/A (pipeline not run) |
| outputs/deployment.csv | N/A (pipeline not run) |
| outputs/reg_friction.csv | N/A (pipeline not run) |
| outputs/cluster_index.csv | N/A (pipeline not run) |
| outputs/Electro-Industrial_Index_Tables.xlsx | N/A (pipeline not run) |
| outputs/metadata/run_metadata.csv | N/A (pipeline not run) |

## Post-refactor baseline

- Timestamp (UTC): 2026-01-27T20:43:03Z
- Command(s):
  - `Rscript run_pipeline.R`

### Result

The post-refactor pipeline could not be executed because `Rscript` is not available in the current environment. As a result, output files were not generated and checksums could not be computed.

| Output file | Checksum (sha256) |
| --- | --- |
| outputs/Electro-Industrial.csv | N/A (pipeline not run) |
| outputs/deployment.csv | N/A (pipeline not run) |
| outputs/reg_friction.csv | N/A (pipeline not run) |
| outputs/cluster_index.csv | N/A (pipeline not run) |
| outputs/Electro-Industrial_Index_Tables.xlsx | N/A (pipeline not run) |
| outputs/metadata/run_metadata.csv | N/A (pipeline not run) |

## Reference repo access

Attempted to clone `Opportunity_Security_Indices` as a sibling repo but the network proxy returned HTTP 403, so the patterns checklist was inferred from existing repo conventions instead.
