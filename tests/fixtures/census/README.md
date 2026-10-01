# Census fixtures

`co-est2025-alldata.csv` — recorded from the live Census Bureau file on **2026-09-30**:

    https://www2.census.gov/programs-surveys/popest/datasets/2020-2025/counties/totals/co-est2025-alldata.csv

It exists so `tests/testthat/test-no-network.R` runs with **no network access**, which the project
brief requires of CI. Live-network checks are deliberately not part of this suite.

## How it was trimmed

The live file is 3,195 rows × 99 columns (2.0 MB). The fixture keeps 432 rows × 11 columns
(29 KB):

* **Rows** — county records only (`SUMLEV == "050"`) for five states, chosen so FIPS construction
  is exercised across both low and high state codes and across zero-padding:
  `01` Alabama, `02` Alaska, `06` California, `48` Texas, `56` Wyoming.
* **Columns** — the three the loader reads (`STATE`, `COUNTY`, `POPESTIMATE<vintage>`), the two
  name columns that make the file readable by eye, and `POPESTIMATE2020`–`POPESTIMATE2025`.

Keeping the full run of estimate years is deliberate: it is what lets the tests prove the
population column **tracks the resolved vintage** rather than being hard-coded. `POPESTIMATE2023`
and `POPESTIMATE2025` differ in this fixture, so a loader that silently read the wrong year would
fail rather than coincidentally pass.

Values are unmodified.

## Licensing

Public domain (US Government work), and `census_county_population` is `can_commit_raw: true` in
[`config/sources.yml`](../../../config/sources.yml). Committing this is permitted.

## Refreshing

The fixture pins a vintage on purpose: refreshing it changes what the tests measure. The runtime
cache (`data/raw_cache/census/`, gitignored) is what follows the live service, and the loader
discovers the newest published vintage by itself — see
[`refactor_plan.md` F-11](../../../docs/refactor_plan.md#f-11).
