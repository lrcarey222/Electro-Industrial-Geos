# Collecting BLS QCEW data: options and a recommendation

**Status:** investigation findings. No code changes accompany this document.
**Date:** 2026-09-17
**Bears on:** `workforce_share`, `workforce_growth` (cluster sub-index), and
`employment_lq` (economic capabilities) if it is restored.
**Related:** [`refactor_plan.md` F-05](refactor_plan.md#f-05) (indicators discarded),
[F-16](refactor_plan.md#f-16) (`blsAPI` archived), [F-22](refactor_plan.md#f-22) (the NAICS bundle),
[`legacy_parity.md` §5](legacy_parity.md) (`blsQCEW` does not exist).

Every figure below was measured against the live BLS open-data service on 2026-09-17, with a
User-Agent identifying the project. Nothing here is inferred from documentation.

---

## 1. Why the current approach cannot work

Three independent problems, any one of which is fatal:

1. **`library(blsQCEW)` names a package that does not exist.** Not on CRAN, not anywhere. The
   function `blsQCEW()` is provided by `blsAPI`. The guard
   `requireNamespace("blsQCEW", quietly = TRUE)` is therefore permanently `FALSE`, so the county
   employment block has never executed — in this repo *or* the upstream it was copied from.
2. **`blsAPI` was archived from CRAN on 2021-07-05.** Even with the name corrected, it cannot be
   installed from CRAN, so it cannot be declared as a dependency (F-16).
3. **The access pattern is ~6,200 HTTP requests per run.** One request per county
   (`tidycensus::fips_codes`, ~3,100 areas) per quarter, for two quarters, with
   `Sys.sleep(0.03)` between each. At 143 KB per county file that is roughly 440 MB and, with
   latency, well over an hour.

So this is not a matter of swapping a client library. The retrieval design needs replacing.

---

## 2. What BLS actually serves

QCEW publishes **open data files** requiring no API key and no registration, at
`https://data.bls.gov/cew/data/`. All verified HTTP 200:

| approach | URL shape | requests needed | size |
|---|---|---|---|
| **Area slices** (current design) | `/api/{year}/{qtr}/area/{area_fips}.csv` | ~3,100 per quarter | 143 KB each, ~440 MB total |
| **Industry slices** | `/api/{year}/{qtr}/industry/{industry_code}.csv` | **one per NAICS code** | 40 KB (6-digit) – 172 KB (4-digit) |
| **Annual single file** | `/files/{year}/csv/{year}_annual_singlefile.zip` | **1** | 75 MB (2024), 63 MB (2025) |
| **Quarterly single file** | `/files/{year}/csv/{year}_qtrly_singlefile.zip` | **1** | 305 MB (2024) |

An industry slice contains **every geography** for that industry — national, state, MSA and county —
so one file serves both a state-level and a county-level need. Fields include everything the
pipeline uses and more: `area_fips`, `own_code`, `industry_code`, `agglvl_code`, `year`, `qtr`,
`annual_avg_emplvl`, the monthly `month{1,2,3}_emplvl` equivalents in quarterly files,
`disclosure_code`, and BLS's own published location quotients (`lq_annual_avg_emplvl`).

**Latest available period: 2026 Q1.** 2026 Q2 returns 404. The 2025 annual file is published. So
QCEW is roughly two quarters behind real time, which is far fresher than anything currently in this
index.

**Not every 6-digit code has a slice.** `335911` and `335912` — storage and primary battery
manufacturing, both in the bundle — return **404** at 6-digit. A connector must tolerate absent
slices rather than treating a 404 as a failure.

There is also the **BLS Public Data API v2** (`api.bls.gov/publicAPI/v2/`), which needs a
registration key and is capped at 500 requests and 50 series per request per day. For 39 codes
across 51 states that is ~2,000 series, or ~40 requests — workable, but it needs a secret and adds
a quota to manage for no benefit over the keyless CSV files. **Not recommended.**

---

## 3. The real problem is suppression, not the client library

QCEW withholds any cell that would disclose an individual employer. Two things make this
consequential here, and neither is fixed by changing how the data is fetched.

**Suppressed cells arrive as `0`, not `NA`.** For NAICS 334210 in 2024, 272 of 313 rows carry
`disclosure_code = "N"`, and **all 272 report `annual_avg_emplvl = 0`**. The pipeline sums with
`na.rm = TRUE`, so every withheld cell is silently counted as "no employment" rather than "unknown".

**Suppression rises sharply as geography narrows and industry detail deepens.** Measured, 2024
annual, private ownership:

| NAICS detail | geography | suppression rate | employment recovered |
|---|---|---|---|
| 6-digit (`334210`) | state | 46% | 11,950 |
| 6-digit (`334210`) | county | **91%** | 6,403 |
| 6-digit (`335311`) | state | 45% | 24,750 |
| 6-digit (`335311`) | county | **93%** | **2,664 — 11% of the state total** |
| 4-digit (`3353`) | state | 10% | 157,949 |
| 4-digit (`3353`) | county | **84%** | **71,302 — 45% of the state total** |
| 3-digit (`334`) | state | 2% | 1,027,528 |
| 3-digit (`334`) | county | 64% | 923,626 — 90% of the state total |

Read the third column against the fourth. **Rolling up county data at 4-digit detail recovers
roughly 45% of the employment that the same industry reports at state level.** At 6-digit it can be
as low as 11%.

**And the loss is not uniform.** Suppression happens where establishments are few, so states with a
small presence in an industry lose proportionally more of it. That biases `workforce_share`
systematically — and any location quotient built from it — in a direction that correlates with the
thing being measured.

This is the finding that matters. The current design pulls county files, aggregates them to state,
and treats withheld as zero, which is close to the worst available combination.

---

## 4. Recommendation

**Pull state-level industry slices, and stop rolling up from counties.**

| change | from | to | why |
|---|---|---|---|
| Geography | county files rolled up | state rows of industry slices | recovers roughly twice the employment at 4-digit; the indicators are state-level anyway |
| Access | one file per county | one file per NAICS code | ~39 requests instead of ~6,200; ~1.5 MB instead of ~440 MB |
| Client | `blsQCEW` / `blsAPI` | plain HTTP + `readr` | removes a nonexistent package and an archived one |
| Suppression | counted as `0` | treated as `NA`, coverage recorded | stops "withheld" reading as "none" |

The county roll-up buys nothing: `workforce_share` and `workforce_growth` are state-level
indicators, and the roll-up is what destroys the data. Dropping it is both simpler and more
accurate.

Concretely: for each code in the bundle, fetch
`/api/{year}/{qtr}/industry/{code}.csv`, keep `own_code == 5` and state-level `area_fips`
(`^[0-9]{2}000$`), set `annual_avg_emplvl` to `NA` where `disclosure_code == "N"`, and record per
source both the number of states with a value and the number suppressed. The manifest fields for
this already exist — `n_geographies` and `schema_fingerprint`.

Denominator: the same slices for `industry_code == "10"` (all industries) give state totals, so the
share and the LQ can both be computed from one mechanism.

---

## 5. What needs a decision before this is built

Three questions, none of which are mine to answer, because each changes what the indicator means.

1. **Is suppressed `NA` or zero?** Recommendation: `NA`, with the state's share computed from what
   is disclosed and the suppressed count published alongside. Treating it as zero is defensible only
   if you say so in the methodology; treating it as zero silently is not.
2. **What NAICS detail?** F-22 settled the bundle's *scope* (broad: manufacturing plus utilities plus
   telecom) but not its *depth*. At 4-digit, state-level suppression is 10–30%. At 3-digit it is
   2–6%, but 3-digit codes are much broader than the bundle intends — `334` is all computer and
   electronic product manufacturing, well beyond the 19 codes chosen. There is a genuine trade-off
   between precision of definition and completeness of data, and it should be recorded.
3. **Does `employment_lq` get restored from this?** The upstream computed it as
   `(state bundle / state total) / (US bundle / US total)` from state-level QCEW — which this
   mechanism supplies directly. BLS also publishes its own `lq_annual_avg_emplvl`, but those are
   per-industry and cannot be averaged across a bundle, so the upstream's manual computation is the
   right approach. See [`legacy_parity.md` §3](legacy_parity.md).

Until (1) and (2) are answered, a connector would be encoding a methodology choice by default —
which is how the current understatement arrived in the first place.
