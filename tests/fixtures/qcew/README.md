# QCEW fixtures

Recorded from the live BLS open-data service on **2026-09-29**:

    https://data.bls.gov/cew/data/api/{year}/a/industry/{industry_code}.csv

These exist so `tests/testthat/test-qcew.R` runs with **no network access**, which the project
brief requires of CI. Live-network checks are deliberately not part of this suite.

## What is here

| file | content |
|---|---|
| `2025_a_<code>.csv` | current period, one file per 4-digit bundle code |
| `2022_a_<code>.csv` | baseline period for `workforce_growth` |
| `*_a_10.csv` | the all-industries denominator |

Nine bundle codes: `2211`, `3342`, `3351`, `3353`, `3359`, `5162`, `5171`, `5174`, `5178` — the
4-digit parents of `electric_man_6d`. See [`../../../docs/methodology.md`](../../../docs/methodology.md)
for why the bundle is read at 4-digit depth.

## How they were trimmed

Rows were filtered to exactly what the connector reads, and nothing else was altered:

* `own_code == "5"` (private ownership)
* `agglvl_code == "56"` for bundle codes (state × 4-digit NAICS × ownership)
* `agglvl_code == "51"` for `10` (state × all industries × ownership)

**Every column is kept**, so the fixtures still document the real 38-column schema — that is what
lets `test-qcew.R` assert the connector fails loudly when a column it depends on disappears.

Values are unmodified. In particular the suppressed rows still carry `disclosure_code == "N"` with
`annual_avg_emplvl` of `0`, which is the behaviour the connector exists to handle and which the
tests assert directly rather than taking on trust.

## Licensing

Public domain (US Government work), and `bls_qcew` is `can_commit_raw: true` in
[`config/sources.yml`](../../../config/sources.yml). Committing these is permitted.

## Refreshing

Fixtures pin a vintage on purpose: refreshing them changes what the tests measure. Re-record only
deliberately, and re-run the diff in [`docs/bls_qcew_options.md` §8](../../../docs/bls_qcew_options.md)
if you do. The runtime cache (`data/raw_cache/qcew/`, gitignored) is what follows the live service.
