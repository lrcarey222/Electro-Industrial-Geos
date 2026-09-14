# Refactor Plan — Structural Findings

**Status:** Phase 0 deliverable. Documentation only; no functional changes accompany this file.
**Audit date:** 2026-09-11
**Commit audited:** `d57a0b9` (`main`)
**Companion:** [`data_audit.md`](data_audit.md) — per-indicator provenance.

Every claim here was checked against the code. Where the brief's hypothesis was wrong, this
document says **REFUTED** and explains why, because building on a wrong premise is worse than
having one fewer finding.

---

## Part 1 — Findings log

Severity is about the published index, not about code tidiness.

| id | severity | finding | phase / status |
|---|---|---|---|
| [F-01](#f-01) | **blocker** | The pipeline does not run: `scripts/07_process_data.R` fails to parse | ✅ fixed in step 0a |
| [F-02](#f-02) | **blocker** | The canonical methodology script does not parse and is not self-contained | Phase 5 (blocks parity test) |
| [F-03](#f-03) | **high** | The test suite executes zero assertions; the one parity check compares against `NULL` | pre-Phase 1 |
| [F-04](#f-04) | **high** | EIA electricity price reads the **residential** column, not industrial — a regression against the committed vintage | issue → separate PR |
| [F-05](#f-05) | **high** | `industrial_electricity_price` is joined under the wrong column name, so the indicator keeps sample data | issue → separate PR |
| [F-06](#f-06) | **high** | `SQGDP.zip` is read but never downloaded; BEA filenames are hard-coded with years and fail silently | Phase 2 |
| [F-07](#f-07) | medium | The AFDC station-count parser reads unnamed columns positionally | Phase 2 |
| [F-08](#f-08) | medium | The BNEF snapshot date is hard-coded, so a refreshed file yields zero rows | Phase 3 |
| [F-09](#f-09) | medium | One CIM directory holds two different release vintages, and the facility schema gate fails silently | Phase 1 / 2 |
| [F-10](#f-10) | medium | `FCC_PEA_website.xlsx` is read but untracked and unregistered | Phase 3 |
| [F-11](#f-11) | medium | Live network reads sit at script top level, breaking the no-network CI rule | Phase 2 |
| [F-12](#f-12) | **high** | 19 of 21 staged EIA-860M workbooks are byte-identical HTML error pages; three are committed | Phase 2 |
| [F-13](#f-13) | **governance** | Licensed third-party raw data is committed to a public MIT-licensed repository | needs your decision |
| [F-14](#f-14) | high | No per-indicator vintage metadata exists anywhere | Phase 1 (the core gap) |
| [F-15](#f-15) | medium | DC is excluded from all state outputs; the brief assumes 50 states + DC | needs your decision |
| [F-16](#f-16) | **blocker** | Four packages the pipeline loads are declared nowhere, so CI cannot install them | ✅ fixed in step 0a |
| [F-17](#f-17) | **blocker** | `DESCRIPTION` is not a readable control file, so CI has never installed *any* dependency | ✅ fixed in step 0a |

---

<a id="f-01"></a>
### F-01 — The pipeline does not run *(blocker)*

`scripts/07_process_data.R` has an unbalanced brace at
[line 1229](../scripts/07_process_data.R#L1229):

```
scripts/07_process_data.R:1229:1: unexpected '}'
1228:   }
1229: }
```

`run_Electro_Industrial_pipeline()` invokes each stage with `sys.source()`
([`pipeline.R:33`](../R/utils/pipeline.R#L33)), which parses the whole file before evaluating
anything. So `Rscript run_pipeline.R` aborts at stage three of six and no outputs are produced.

Verified by `parse()` over every `.R` file in the repo: 34 of 35 parse; this one does not.
Corroborated externally — `gh run list` shows **every recorded CI run has failed**, including the
most recent push to `main` (`22186740756`, 2026-02-19). Step-level logs have passed GitHub's
retention window, so I cannot show the exact failing step from CI itself. What I can show is that
both workflow steps which run R code are independently broken, by three separate causes:
`Unit tests` by F-03, and `Smoke test` by this finding *and* by F-16, which fires two stages
earlier. Fixing this brace alone will not turn CI green.

**Bisected.** The file last parsed at `de4a129` (2026-02-06) and broke at `a17d212`
(2026-02-09, *"Harden PEA shapefile loading when sidecar files are missing"*). It has been broken
continuously for 13 commits and roughly six months. Every commit from `a17d212` onward fails to
parse; the reported error migrates from "unexpected end of input" to "unexpected `}`" as later
commits shifted braces around, which is why it reads like a single stray brace and is not.

**It is two defects, not one.**

1. **A missing opener.** `de4a129` had a guard immediately after
   `workforce_growth_update <- NULL`:

   ```r
   if (requireNamespace("blsQCEW", quietly = TRUE) && requireNamespace("tidycensus", quietly = TRUE)) {
   ```

   It was deleted, orphaning its closer at line 1229. Note `"blsQCEW"` — **no such package
   exists**; the client is `blsAPI::blsQCEW()`. So this guard has always been `FALSE` and the
   county-employment block never ran even when the file parsed. That is independent corroboration
   of F-05.

2. **A missing closer.** Restoring the opener alone leaves the file unbalanced the other way. The
   `if (is.null(cluster_pea_inputs) || nrow(cluster_pea_inputs) == 0) {` block never closes.
   `a17d212` introduced the PEA override logic inside that block with a five-deep closing cascade
   where six was required, so this half was **born broken** and has no parsing ancestor to
   restore from.

**Remedy — landed in step 0a.** Opener restored verbatim (behaviour-preserving: the block stays
skipped). Closer added after the PEA override region, and `scripts/check_syntax.R` now parses
every R file in CI ahead of dependency installation, so this class of breakage cannot merge again.

**One judgement call you should confirm.** Because defect 2 has no parsing ancestor, the closer's
placement is a decision, not a restoration. The two candidate positions are behaviourally
**identical** whenever the fallback `if` is TRUE — which is every run against committed data, since
`data/raw/cluster_pea_inputs.csv` does not exist in this repo. They diverge only if someone supplies
that file: the chosen placement then skips the PEA override, the alternative would run it and error
on an undefined `pea_shapefile`. I chose the placement matching the region's indentation. Say so if
you want the other.

---

<a id="f-02"></a>
### F-02 — The canonical methodology script does not parse *(blocker for the parity test)*

[`README.md:31`](../README.md#L31) and [`docs/methodology.md:3`](methodology.md#L3) both state that
`Legacy Script/Electro-Industrial_State.R` defines the canonical methodology. It cannot be run.

**Two syntax errors.** First failure:

```
Legacy Script/Electro-Industrial_State.R:228:58: unexpected '='
227:   left_join(leg_index %>%
228:               rename(Electro-Industrial_legislation_index=
```

A global find-and-replace introduced the token `Electro-Industrial` into approximately **61 bare R
identifiers**. A hyphen is the minus operator in R, so each is a syntax error. Three
`electrostack_*` identifiers survive unreplaced (e.g.
[line 1694](../Legacy%20Script/Electro-Industrial_State.R#L1694)), which is what the original
prefix appears to have been. A second, unrelated error sits at
[line 403](../Legacy%20Script/Electro-Industrial_State.R#L403): `mutate(man_share=`13`/)` —
an incomplete expression.

**It is also not self-contained.** Seven objects are used but never defined in the file:

| object | first use | what it stands in for |
|---|---|---|
| `raw_data` | [`:10`](../Legacy%20Script/Electro-Industrial_State.R#L10) | raw-data root path prefix |
| `state_gdp` | [`:18`](../Legacy%20Script/Electro-Industrial_State.R#L18) | BEA state GDP table |
| `dsire_inc` | [`:85`](../Legacy%20Script/Electro-Industrial_State.R#L85) | DSIRE policy records |
| `spot` | [`:82`](../Legacy%20Script/Electro-Industrial_State.R#L82) | SPOT scores (`spot <- spot`) |
| `feas` | [`:424`](../Legacy%20Script/Electro-Industrial_State.R#L424) | internal feasibility model output |
| `bundle_lq` | [`:482`](../Legacy%20Script/Electro-Industrial_State.R#L482) | employment location quotients |
| `supplycurve_geo` | [`:501`](../Legacy%20Script/Electro-Industrial_State.R#L501) | NREL supply curve, commented "*NB calculated in All Geos*" |

**Consequence for the brief's plan.** Phase 5 step 1 is "golden-vintage parity test: `R/`
reproduces `Legacy Script/` to a documented tolerance. This lands before anything is deleted."
That test **cannot be written against this file as it stands**, and the six indicators those
missing objects would have supplied are exactly the six that
[`data_audit.md §5.3`](data_audit.md#53-indicators-i-could-not-trace-to-any-producer-in-this-repo)
lists as untraceable. The parity target does not exist yet.

**Three options — your call, I am not choosing:**

1. **Repair the legacy script** to a runnable state (mechanical de-hyphenation, fix line 403,
   locate or reconstruct the seven missing objects), then run parity. Highest fidelity, and the
   only option that actually validates the refactor. Cost is real: the seven objects come from at
   least one script that is not in this repo.
2. **Freeze the current committed outputs as the golden vintage** and assert that future `R/`
   changes do not move them. This tests *stability*, not *correctness* — and given F-04 and F-05,
   the current outputs are known to contain at least two wrong indicators, so it would freeze
   those in.
3. **Declare `R/` canonical** and retire the legacy script with a documented statement that
   numerical parity was never established. Honest, cheapest, and loses the audit trail.

I recommend (1) scoped to the *state-level* index only, accepting that the six untraced
indicators stay out of the parity comparison and are documented as such. But this is a
methodology-provenance decision, not an engineering one.

**Until this resolves, `Legacy Script/` must not be deleted** — which is what the brief already
says.

---

<a id="f-03"></a>
### F-03 — The test suite executes zero assertions *(high)*

Two independent defects, either of which alone is sufficient.

**(a) Working-directory mismatch.** All three test files `source()` the implementation with
repo-root-relative paths, e.g. [`test-indices.R:4`](../tests/testthat/test-indices.R#L4):

```r
source("R/utils/utils_helpers.R")
```

`testthat::test_dir('tests/testthat')` — the exact command in
[`README.md:109`](../README.md#L109) and [`ci.yml:28`](../.github/workflows/ci.yml#L28) — sets the
working directory to `tests/testthat/`, so every `source()` fails:

```
[ FAIL 3 | WARN 3 | SKIP 0 | PASS 0 ]
Error ('test-indices.R:4:1'): cannot open file 'R/utils/utils_helpers.R'
```

Three files error out at load. **No test in this repository has ever run in CI.**

**(b) The parity fixture has the wrong column names.** Even with (a) fixed, the only assertion
that checks index values is inert.
[`test-indices.R:123`](../tests/testthat/test-indices.R#L123) reads:

```r
expect_equal(Electro_Industrial$Electro_Industrial_index_w, expected$Electro_Industrial_index_w, tolerance = 1e-8)
```

but [`tests/fixtures/expected_Electro-Industrial.csv`](../tests/fixtures/expected_Electro-Industrial.csv)
has **hyphenated** headers. Verified by executing the builders directly:

```
CODE index cols : ..., Electro_Industrial_index, Electro_Industrial_index_w
FIXTURE cols    : ..., Electro-Industrial_index, Electro-Industrial_index_w
fixture$Electro_Industrial_index_w is NULL: TRUE
```

`expect_equal(<numeric>, NULL)` cannot pass, so once (a) is fixed this test will fail — correctly,
and confusingly, because the *code* is right and the *fixture* is wrong.

**Remedy.** One PR: make the tests locate the repo root independently of wd (a `setup.R` or
`testthat::test_local()`), regenerate the fixture from the code's actual column names, and
re-baseline. The fixture values themselves need re-deriving once F-04 and F-05 are settled.

---

<a id="f-04"></a>
### F-04 — EIA electricity price reads the residential column *(high — affects published values)*

`sales_revenue.xlsx` (EIA-861M) repeats a four-column block —
`Thousand Dollars / Megawatthours / Count / Cents/kWh` — once per customer class. Verified by
reading the sheet's header rows:

```
row 1: . | . | . | . | RESIDENTIAL | . | . | . | COMMERCIAL | . | . | . | INDUSTRIAL | ...
row 2: . | . | . | . | Revenue | Sales | Customers | Price | Revenue | Sales | Customers | Price | ...
row 3: Year | Month | State | Data Status | Thousand Dollars | ... | Cents/kWh | ...
```

The industrial price is the **third** `Cents/kWh` block (spreadsheet column P). Both read paths
select the **first** (column H, residential):

- [`07_process_data.R:492`](../scripts/07_process_data.R#L492) → `electricity_price` (infrastructure)
- [`process_data_helpers.R:49-72`](../R/utils/process_data_helpers.R#L49) →
  `industrial_electricity_price` / `ind_price_m` (cluster)

Both use the same pattern, and `intersect()` preserves the order of its first argument, so `[1]`
is always the leftmost match:

```r
price_col <- intersect(names(eia_raw), names(eia_raw)[stringr::str_detect(names(eia_raw), "cents.*k_wh")])[1]
```

**Verified numerically.** Running each read path against the committed workbook:

| | AL | AK | CA |
|---|---|---|---|
| what the code selects today (`cents_k_wh_8`, Residential) | 16.13 | 26.28 | 32.39 |
| Industrial (`cents_k_wh_16`, spreadsheet col P) | **7.68** | **20.06** | 21.50 |
| committed `inputs_processed.csv` `ind_price_m` | **7.684** | **20.061** | — |

The committed values match the *industrial* column exactly. **So an earlier version of this code
selected correctly and a later refactor regressed it.** Confirmed for both the `readxl` path and
the `openxlsx` path (which dedupes names differently — `cents_k_wh`, `cents_k_wh_2`, … — but still
lands on position 8).

**Remedy.** Select the column by sector, not by position — read the merged sector header row and
resolve `INDUSTRIAL` explicitly. Per the brief this changes published numbers, so it goes in its
own PR with a diff report, not folded into anything else. Note that `electricity_price` is a
*price-trend* index and `industrial_electricity_price` is a *level*; both are affected, and the
infrastructure and cluster sub-indices will both move.

I am **not** treating this as a methodology change — the indicator is named
`industrial_electricity_price` and the documentation calls it "Industrial electricity price"
([`data/README.md:40`](../data/README.md#L40)), so reading the industrial column is a bug fix
against stated intent, not a redefinition. Confirm you agree before I touch it.

---

<a id="f-05"></a>
### F-05 — `industrial_electricity_price` never reaches the indicator column *(high)*

[`process_data_helpers.R:49`](../R/utils/process_data_helpers.R#L49) builds a table whose value
column is named `ind_price_m`, not `industrial_electricity_price`
([`07_process_data.R:1045`](../scripts/07_process_data.R#L1045)):

```r
dplyr::transmute(State = .data$state, ind_price_m)
```

It is then joined into `raw_updates` ([`:1288`](../scripts/07_process_data.R#L1288)) and the merge
step attempts to coalesce it ([`:1321`](../scripts/07_process_data.R#L1321)):

```r
industrial_electricity_price = dplyr::coalesce(industrial_electricity_price),
```

That is a single-argument `coalesce()` — a no-op. There is no `.raw` counterpart to fall back to,
because the joined column arrived under the name `ind_price_m`. Net effect:

- `industrial_electricity_price` retains whatever `base_inputs` supplied — i.e. the three
  sample-data rows. **Verified: 3 of 50 non-NA, and those three are CA / TX / NY.**
- The real EIA value is written out under a stray 36th column, `ind_price_m`, at 50/50 — which no
  sub-index reads.

The same merge block also has the two workforce coalesces **commented out**
([`:1322-1323`](../scripts/07_process_data.R#L1322)):

```r
#workforce_share = dplyr::coalesce(`workforce_share.raw`, workforce_share),
#workforce_growth = dplyr::coalesce(`workforce_growth.raw`, workforce_growth)
```

So the BLS QCEW block at [`:1137-1231`](../scripts/07_process_data.R#L1137) — which issues roughly
3,100 API calls per quarter pulled, twice (latest quarter plus 2022-Q1) — computes
`workforce_share` and `workforce_growth` and then throws both away. Those indicators also sit at
3/50.

Combined with the six untraced indicators in
[`data_audit.md §5.3`](data_audit.md#53-indicators-i-could-not-trace-to-any-producer-in-this-repo),
this is the full explanation for **eight of 33 indicators in the published vintage carrying nothing
but California, Texas and New York sample values** — while the pipeline reports success.

**This is the single strongest argument for the whole Phase 1 accountability spine.** A manifest
with `n_geographies` per source, plus the orphan check that every `index_definition.yml` indicator
is claimed by exactly one source, would have caught all eight on the first run.

**Remedy.** Rename to the contract column, restore the workforce coalesces, and add a
post-merge assertion that no required indicator has fewer than *N* non-NA geographies. Separate PR
from F-04; both move published numbers.

---

<a id="f-06"></a>
### F-06 — BEA files: one is never downloaded, both have years hard-coded *(high)*

`load_quarterly_gdp_growth()` reads `data/raw/remote/SQGDP.zip`
([`07_process_data.R:100`](../scripts/07_process_data.R#L100)), but `SQGDP` appears **nowhere** in
`ingest_legacy_sources()` — the registry has only `SAGDP.zip`
([`ingest_sources.R:59-63`](../R/utils/ingest_sources.R#L59)). The committed copy is the only
source, and no refresh path exists.

Both readers then match a hard-coded filename with the vintage baked in:

```r
gdp_path[stringr::str_detect(basename(gdp_path), "SAGDP9__ALL_AREAS_1997_2024.csv")][1]   # :77
gdp_path[stringr::str_detect(basename(gdp_path), "SQGDP9__ALL_AREAS_2005_2025.csv")][1]   # :108
```

When BEA publishes its next annual release the filename changes, the match returns `NA`, and both
functions **return an empty tibble** ([`:79`](../scripts/07_process_data.R#L79),
[`:110`](../scripts/07_process_data.R#L110)). `safe_left_join()` then silently drops the join and
`gdp_growth_index` plus the `incentives_gdp` denominator go `NA` with no warning. This is precisely
the failure mode the brief's `schema_fingerprint` is meant to catch — detect loudly at ingest, not
three steps later.

**Remedy.** Register SQGDP; glob the member by prefix (`^SAGDP9__ALL_AREAS_`) rather than exact
name; make an empty result an error, not a silent empty tibble.

---

<a id="f-07"></a>
### F-07 — The AFDC station-count parser reads unnamed columns *(medium)*

[`07_process_data.R:436`](../scripts/07_process_data.R#L436) reads sheet 2 at `startRow = 3`. The
resulting header is mostly junk — verified against the committed workbook:

```
x1, x2, x3, x4, stations_charging_outlets, retail_non_retail_total, x7, primary_secondary_total, x9, x10
```

The code looks for `total_chargers` / `total_charging` / `total_evse` / `total_stations`
([`:442`](../scripts/07_process_data.R#L442)); none exist. It then falls back to
**`tail(numeric_cols, 1)`** — the last numeric column, whatever that happens to be — and to
`names(...)[1]` (`x1`) for the state key. `ev_stations_cap` is nonetheless 50/50 populated, so
*something* joined; I could not establish that it is the right something.

**Remedy.** Pin the sheet and header rows explicitly with a column contract, and fail if the
contract does not match. Flagged as `confidence: low` on the value in the audit table.

---

<a id="f-08"></a>
### F-08 — The BNEF snapshot date is hard-coded *(medium)*

Both BNEF aggregations filter `Date == "2025-03-31"`
([`07_process_data.R:768`](../scripts/07_process_data.R#L768),
[`:784`](../scripts/07_process_data.R#L784)) inside a file named `2025-08-08 - …`. When a steward
drops a newer BNEF export, the filter matches **zero rows**, `datacenter_index` and
`datacenter_mw` go empty, and nothing reports it.

**Remedy.** Select the latest available `Date` and record it as the vintage label. Belongs with
the Phase 3 manual lane, since BNEF is a licensed manual drop.

---

<a id="f-09"></a>
### F-09 — Mixed vintages in one CIM directory, and a silent schema gate *(medium)*

**(a) Mixed vintages.** `data/raw/clean_investment_monitor_q2_2025/` contains three files whose
embedded version lines disagree with the directory name:

| file | embedded `version:` | `created:` |
|---|---|---|
| `quarterly_actual_investment.csv` | `2025_Q2.20250811.0` | 2025-08-11 |
| `socioeconomics.csv` | `2025_Q2.20250811.0` | 2025-08-11 |
| `manufacturing_facility_metadata.csv` | **`2025_Q4.20260109.0`** | **2026-01-09** |

One file is two quarters newer than the directory claims. The directory name is currently the only
vintage record, and it is wrong for a third of its contents. Good news: CIM files carry machine-
readable `version:` and `created:` lines, which makes them an ideal first `vintage_label` source
for the Phase 1 manifest.

**(b) Silent schema gate.** [`07_process_data.R:908-913`](../scripts/07_process_data.R#L908)
requires eight columns before processing facility metadata. Verified against the committed file —
two are absent:

| required column | present? |
|---|---|
| `segment`, `technology`, `state`, `announcement_date`, `estimated_total_facility_capex`, `county_2020_geoid` | yes |
| `investment_reported_flag` | **no** |
| `investment_status` | **no** (the file has `current_facility_status`) |

So `all(required_facility %in% names(facility_raw))` is `FALSE`, the entire facility block is
skipped, and `cluster_manufacturing` silently falls back to the coarser
`quarterly_actual_investment.csv` path ([`:977-1010`](../scripts/07_process_data.R#L977)). The PEA
manufacturing rollup, which only the facility path can produce, is lost. There is no message.

A latent consequence: [`:1085`](../scripts/07_process_data.R#L1085) references `cim_facilities`,
which is only ever created *inside* the skipped block, so that line would raise
"object not found". F-01 means execution never reaches it today.

**Remedy.** This is the archetype for the brief's staging/validation/promotion split: a column
contract per source, a schema fingerprint in the manifest, and a loud failure — never a silent
downgrade.

---

<a id="f-10"></a>
### F-10 — A required input is untracked and unregistered *(medium)*

[`07_process_data.R:184-185`](../scripts/07_process_data.R#L184) reads:

```r
pea_county_path <- fs::path(raw_dir, "FCC_PEA_website.xlsx")
pea_counties <- read_excel(pea_county_path, 3)
```

This file is **not tracked in git** and **not listed in `ingest_legacy_sources()`**. It is also
read with no `file.exists()` guard, so a fresh clone errors out at script top level before any
indicator is built. It supplies the PEA↔county crosswalk that produces PEA population, which
`build_pea_facility_rollup()` needs for every per-capita PEA figure
([`process_data_helpers.R:36`](../R/utils/process_data_helpers.R#L36)).

A sibling file, `pea_counties_FCC.xlsx`, is also present and untracked but referenced by no code.

**Remedy.** Register it, track it if FCC terms permit (it is a US government work, so almost
certainly yes), and guard the read. Phase 3 drop-zone work.

---

<a id="f-11"></a>
### F-11 — Live network reads at script top level *(medium)*

Two unguarded network calls execute during `07_process_data.R` regardless of
`SKIP_DATA_DOWNLOADS`:

- [`:182`](../scripts/07_process_data.R#L182) — `readr::read_csv('https://www2.census.gov/…/co-est2023-alldata.csv')`,
  at top level, no cache, no `tryCatch`, not in the ingest registry.
- [`:760`](../scripts/07_process_data.R#L760) — `tigris::states(cb = TRUE, year = 2023)`, inside
  the BNEF block, year hard-coded.

Neither honours the snapshot/caching discipline the rest of the pipeline uses, and the first
breaks the brief's rule that CI must pass with no network access. Both also sit in a stretch of
`07_process_data.R` (lines 160–200) that runs at top level rather than inside a function, so their
failure is unrecoverable.

**Remedy.** Route both through the download registry with caching and fixtures.

---

<a id="f-12"></a>
### F-12 — Most staged EIA-860M workbooks are HTML error pages *(high)*

Of the 21 `*_generator*.xlsx` files in `data/raw/remote/`, **19 are byte-identical 66,904-byte
HTML documents** — verified by file signature and SHA-256:

```
april_generator2024.xlsx        66904 bytes  sig='<!'  sha=F24F4928E22D   <- tracked in git
august_generator2023.xlsx       66904 bytes  sig='<!'  sha=F24F4928E22D   <- tracked in git
august_generator2024.xlsx       66904 bytes  sig='<!'  sha=F24F4928E22D   <- tracked in git
… 16 more, identical hash, all untracked …
december_generator2025.xlsx  13463993 bytes  sig='PK'  sha=1264F1C37CF5   <- real workbook
may_generator2026.xlsx       13797847 bytes  sig='PK'  sha=46FF71D90723   <- real workbook
```

`sig='<!'` is `<!DOCTYPE html>`. These are EIA error pages saved under an `.xlsx` extension. The
two genuine workbooks (`PK`, the ZIP signature) are both **untracked**, so a fresh clone gets three
committed HTML stubs and no usable EIA-860M data.

**Root cause.** `download_with_cache()`
([`utils_download.R:9-17`](../R/utils/utils_download.R#L9)) calls `utils::download.file()` and
checks nothing:

```r
if (!fs::file_exists(dest_path)) {
  utils::download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
}
dest_path
```

No HTTP status check, no content-type check, no magic-byte check, no atomic temp-then-rename. A
404 body is written straight to the destination path, and `fs::file_exists()` then reports it as
present — including on the next run, which short-circuits on existence and never retries.

An `is_valid_xlsx()` helper that checks the `PK` signature *does* exist, but it is defined twice
([`ingest_sources.R:48`](../R/utils/ingest_sources.R#L48),
[`07_process_data.R:557`](../scripts/07_process_data.R#L557)) and applied only in the generator
retry loop and the read guard — never inside `download_with_cache()` itself, which is where the
poisoned files are created.

This is the exact scenario the brief's Phase 2.1 calls for: retry with backoff, content-hash
short-circuit, and **atomic write via temp file + rename so an interrupted download never leaves a
half-file that looks valid**. It is already happening, at scale, in the committed tree.

**Remedy.** Validate in `download_with_cache()` (status, content type, magic bytes), write via
temp + rename, and never trust mere existence. Then purge the 19 stubs from the working tree and
the three from tracking.

---

<a id="f-13"></a>
### F-13 — Licensed raw data in a public MIT-licensed repository *(governance — needs your decision)*

Tracked in git, on `main`:

| path | size | why this is a problem |
|---|---|---|
| `data/raw/BNEF/2025-08-08 - Global Data Center Live IT Capacity Database.xlsx` | **37.7 MB** | BloombergNEF is a commercial subscription product. `data/README.md:32` marks the derived indicator "Proprietary". |
| `data/raw/Good Jobs First/gjf_complete.csv` | 618 KB | Good Jobs First Subsidy Tracker export. `data/README.md:13` marks it "Proprietary GJF download". Note it is **not** a raw GJF download: columns 36–41 (`subs_m`, `Sector`, `investment_m`, …) are RMI-added derived fields, so it is a prepared internal artifact. |
| `data/raw/climate_leg.csv` | 1.1 MB | contains a `quorum_id` column — evidence of a licensed legislative-tracking export (see `data_audit.md` A-3). |
| `data/raw/dbo_Program.csv` | 7.1 MB | SQL Server table export of a programme inventory; provenance unidentified (A-2). |

[`LICENSE`](../LICENSE) is MIT and [`.gitignore`](../.gitignore) excludes only `outputs/` and
`data/raw_cache/` — so `data/raw/` is tracked wholesale. `docs/sources.md:16-22` already
*describes* four of these as proprietary or internal, which means the classification was known
when they were committed.

The repository is public (`https://github.com/lrcarey222/Electro-Industrial-Geos`).

**What I have not done.** I have not rewritten history, run `filter-repo`, or deleted anything —
per your constraint. I have also not asserted what any of these licences actually say, because I
have not read them.

**What needs your decision, in order:**

1. **Is the repository intended to be public?** Everything else follows from this.
2. **Which of the four may be redistributed?** This sets `can_commit_raw` per source in
   `config/sources.yml`. My working assumption for Phase 1 is `false` for all four until told
   otherwise.
3. **History.** These files are in every commit that added them. Options, for you to weigh with
   whoever owns data governance at RMI: leave as-is; remove from `HEAD` and accept that history
   retains them; or rewrite history. I will not run a history rewrite without explicit written
   approval, and I would want RMI legal or data governance to make that call rather than me.

The committed OneDrive tree is a separate, smaller instance of the same class of problem — see
Part 2, item 1.

---

<a id="f-14"></a>
### F-14 — No per-indicator vintage metadata *(high — this is the core gap)*

**CONFIRMED.** [`export_outputs.R:54-59`](../R/utils/export_outputs.R#L54) writes
`outputs/metadata/run_metadata.csv` with exactly three fields:

```r
metadata <- tibble::tibble(
  snapshot_date  = paths$snapshot_date,
  run_timestamp  = as.character(Sys.time()),
  use_sample_data = paths$use_sample_data
)
```

`snapshot_date` is a **config constant**, pinned to `"2025-01-01"` in
[`config/config.yml:7`](../config/config.yml#L7) — it is a cache key, not a data vintage. Nothing
records, per source: when it was retrieved, what the publisher released, what the schema looked
like, or how many geographies it covered.

The nearest thing that exists is
[`data/processed/source_inventory.csv`](../data/processed/source_inventory.csv), written by
[`05_ingest_sources.R:15-16`](../scripts/05_ingest_sources.R#L15). It has four columns —
`source, type, path, exists` — and no dates, hashes, or row counts. It is a presence check, and as
F-12 shows, presence is exactly the wrong thing to check.

There is no scheduled workflow: [`ci.yml:3-5`](../.github/workflows/ci.yml#L3) triggers only on
`push` and `pull_request`. No `schedule:`, no `workflow_dispatch:`, no freshness job.

This is the gap Phase 1 fills, and F-05 is the proof of what it costs: eight indicators degraded
to three-state sample data with the pipeline reporting success.

---

<a id="f-15"></a>
### F-15 — DC is excluded from all state outputs *(medium — needs your decision)*

[`07_process_data.R:9`](../scripts/07_process_data.R#L9) is the spine of every state join:

```r
states <- tibble::tibble(state = state.name, abbr = state.abb)
```

Base R's `state.name` / `state.abb` are the 50 states only. The committed
`inputs_processed.csv` has exactly 50 rows. DC is absent from every state-level output.

The brief's Phase 2.2 asks validation to require "all 50 states + DC where applicable — flag
missing explicitly rather than propagating NA". Adding DC would **change published coverage and
every min-max scaled value** (scaling is relative to the observed range, so one new geography
shifts all 50 existing scores).

I have changed nothing. Flagging it because it is a coverage decision disguised as a validation
rule: `where applicable` needs to be pinned down per source before the validator is written. Note
that several upstream sources do carry DC (CIM socioeconomics, EIA-861M, BEA), so the exclusion is
this pipeline's choice, not a data limitation.

---

<a id="f-16"></a>
### F-16 — Four required packages are declared nowhere *(blocker)*

[`00_setup.R:1-18`](../scripts/00_setup.R#L1) attaches 17 packages before any other stage runs.
Four of them appear in **neither** [`DESCRIPTION`](../DESCRIPTION) `Imports:`/`Suggests:` **nor**
[`renv.lock`](../renv.lock):

| package | where it is needed | how it is called |
|---|---|---|
| `readxl` | [`00_setup.R:16`](../scripts/00_setup.R#L16); [`process_data_helpers.R:52`](../R/utils/process_data_helpers.R#L52); `spot_score` at [`07_process_data.R:1241`](../scripts/07_process_data.R#L1241) | one **bare** `read_excel()` at [`:185`](../scripts/07_process_data.R#L185), so it must be attached |
| `lubridate` | [`07_process_data.R:695`](../scripts/07_process_data.R#L695), [`:922`](../scripts/07_process_data.R#L922) | `lubridate::`, both on **live** code paths |
| `tidycensus` | `fips_codes` at [`07_process_data.R:1149`](../scripts/07_process_data.R#L1149) | `tidycensus::`, only inside the guarded (dead) block, which also `requireNamespace()`s it |
| `blsAPI` | `library()` at `00_setup.R:17`; QCEW pull at [`07_process_data.R:1158`](../scripts/07_process_data.R#L1158) | `blsAPI::`, only inside the guarded (dead) block — the `library()` call was **redundant** |

CI resolves dependencies with `setup-r-dependencies@v2`, which reads `DESCRIPTION`
([`ci.yml:19-21`](../.github/workflows/ci.yml#L19)), and `renv::restore()` cannot help because the
lockfile does not list them either (and is a stub — see 3.5). So on a clean runner
`library(blsAPI)` at `00_setup.R:17` raises "there is no package called 'blsAPI'" and the pipeline
dies at **stage one of six** — earlier than F-01, which sits in stage three.

`sf` and `tigris` are in `DESCRIPTION` but **not** in `renv.lock`, so they survive only because
`DESCRIPTION` is what CI actually reads. That asymmetry is the same defect in a milder form.

**`blsAPI` cannot simply be declared.** It was **archived from CRAN on 2021-07-05**; the copy
installed locally came from GitHub (`mikeasilva/blsAPI`). Adding `Imports: blsAPI` would leave CI
just as unresolvable, and adding a `Remotes:` entry would take on an unmaintained GitHub
dependency in order to satisfy a `library()` call for code that never executes.

**Correction.** The paragraph above is right about a clean *local* install but wrong about CI.
F-17 — a malformed `DESCRIPTION` — means `setup-r-dependencies` has never successfully resolved
anything, so CI never reached `library(blsAPI)` or any other line of this repo's R code. These
undeclared packages were a real blocker waiting behind F-17, not the one CI was dying on. I could
not have distinguished the two from the historical runs: step-level logs for the most recent
failure (`22186740756`, 2026-02-19) have passed GitHub's 90-day retention window, and F-17 only
surfaced when step 0a's fix was actually pushed through CI.

**Remedy — landed in step 0a**, differentiated by how each package is actually called:

- `lubridate`, `readxl` → `DESCRIPTION` `Imports:` (genuine hard dependencies).
- `tidycensus` → `Suggests:`. It is reached only via `::` behind a `requireNamespace()` guard,
  which is precisely the idiom `Suggests` exists for. On CRAN, so it resolves.
- `blsAPI` → **not declared, and the redundant `library(blsAPI)` removed.** The single call site is
  already fully qualified, so dropping the attach costs nothing and un-breaks installation. How to
  source an archived package — vendor the one function, call the BLS API directly, or adopt a
  maintained client — is a real decision that belongs with F-05, the work that actually switches
  the QCEW path on.

`renv.lock` is deliberately **not** touched here; regenerating it needs a real `renv::init()` /
`renv::snapshot()`, which is its own work item (see 3.5).

---

<a id="f-17"></a>
### F-17 — `DESCRIPTION` is not a readable control file *(blocker)*

Found by running step 0a's fix through CI. `DESCRIPTION` is a Debian Control File, in which
**continuation lines must be indented**. The `Authors@R` field closes with a bare `)` at column 0:

```
Authors@R: c(
    person("OpenAI Codex", role = c("aut", "cre"), email = "noreply@example.com")
)
```

DCF reads that `)` as the start of a new field, and `)` is not a valid field name. `read.dcf()`
therefore fails on the file as committed to `main`:

```
ERROR: Line starting ') ...' is malformed!
```

CI resolves dependencies with `setup-r-dependencies@v2`, which reads this file, so the step fails
during resolution with the same message surfaced through `pak`:

```
! error in pak subprocess
Caused by error:
! Could not solve package dependencies:
* deps::.: ! pkgdepends resolution error for deps::..
Caused by error:
! Line starting ') ...' is malformed!
```

**This is the true first CI blocker, and it changes the story in F-16.** CI has never installed a
single declared dependency and has never reached `library(blsAPI)` at all — resolution failed
before any R code in this repo ran. F-16's undeclared packages were real and would have bitten on
the next run; they simply were not what CI was dying on. My original F-16 write-up asserted the
pipeline "dies at stage one of six", which is correct for a clean *local* install but wrong about
CI. Corrected inline there.

It also explains a detail I noted but could not account for in Part 2 item 7: `renv.lock` is a
stub, yet CI appeared to survive dependency installation. It never did.

**Remedy — landed in step 0a.** Indent the closing paren. `scripts/check_syntax.R` now also runs
`read.dcf()` on `DESCRIPTION` and fails with a pointed message, so this cannot recur.

**Not changed here:** the placeholder author value itself. `person("OpenAI Codex", …,
email = "noreply@example.com")` is wrong, but who should be credited is your call, not mine — see
3.7, which also covers `CITATION.cff` and `inst/CITATION`.

---

## Part 2 — The brief's structural hypotheses, confirmed or refuted

### 1. Committed OneDrive tree — **CONFIRMED (portability), and it is dead code**

Tracked path:
`OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/States Data/`

**What is in it:** exactly one file, `eia_sales.xlsx`, 2,173,745 bytes — byte-for-byte the same
size as `data/raw/remote/sales_revenue.xlsx`. It is a duplicate of the EIA-861M workbook.

**Does the pipeline read it?** **No.** `grep -rn "OneDrive" R/ scripts/ config/ _targets.R
run_pipeline.R` returns nothing. The active code reads `data/raw/remote/sales_revenue.xlsx`
([`process_data_helpers.R:50`](../R/utils/process_data_helpers.R#L50)). The OneDrive path survives
only in the legacy script, as three hard-coded absolute reads
([`Legacy:91`](../Legacy%20Script/Electro-Industrial_State.R#L91),
[`:607`](../Legacy%20Script/Electro-Industrial_State.R#L607),
[`:785`](../Legacy%20Script/Electro-Industrial_State.R#L785)) — and those point at
`.../Raw Data/`, a *different* subdirectory that is not committed.

**Assessment.** The portability problem the brief predicted is real but historical: it lives in
`Legacy Script/`, not in the running pipeline, which was correctly migrated to repo-relative
`data/raw/` paths. The committed tree itself is 2.2 MB of unreferenced duplicate.

**Governance:** EIA-861M is a US government work, so this particular file is the *least* sensitive
thing in `data/raw/`. The concern is the precedent and the directory name, which embeds an internal
RMI programme structure into a public repo. Grouped with F-13 for your decision; removal is a
Phase 5 item and is safe from a functional standpoint — nothing reads it.

### 2. Two orchestrators — **CONFIRMED, but not in the way described**

Three entry points exist, not two:

| entry point | what it does |
|---|---|
| [`run_pipeline.R`](../run_pipeline.R) | finds the repo root, sources two helpers, calls `run_Electro_Industrial_pipeline()` |
| [`scripts/run_pipeline.R`](../scripts/run_pipeline.R) | resolves the root from `--file=` and `source()`s the root `run_pipeline.R` — a pure shim |
| [`_targets.R`](../_targets.R) | sources all of `R/`, then declares **one** target: `tar_target(pipeline, run_Electro_Industrial_pipeline())` |

**`_targets.R` is not a DAG.** It is a single opaque target wrapping the same sequential
`sys.source()` loop ([`pipeline.R:19-34`](../R/utils/pipeline.R#L19)), so `targets` provides no
per-stage caching, no invalidation, no parallelism, and no dependency graph — the things you would
adopt `targets` for. It is also the only entry point CI does **not** exercise
([`ci.yml:30`](../.github/workflows/ci.yml#L30) runs `Rscript run_pipeline.R`), and `targets`
is not in `renv.lock`, so `_targets.R` cannot currently run in CI at all.

**Which actually runs:** `run_pipeline.R`, via CI and the README quick start. Today, neither runs
to completion (F-01).

**Recommendation** — unchanged from the brief, with one addition: the real work is decomposing
`07_process_data.R` (1,393 lines, one flat script) into per-source targets. Only then does a DAG
buy anything. Sequence: fix F-01 → split `07_process_data.R` into per-source functions → declare
one target per source → reduce `run_pipeline.R` to `targets::tar_make()`. Phase 5, after the
connectors exist, because the connector interface is what defines the target boundaries.

### 3. Two sources of truth for methodology — **CONFIRMED, and worse than described**

`Legacy Script/` is declared canonical and cannot be parsed (F-02). `R/` is a working
implementation whose parity with the legacy script has never been demonstrated —
[`docs/baseline_check.md`](baseline_check.md) records that **both** the pre- and post-refactor
baseline runs were skipped because "`Rscript` is not available in the current environment", with
every checksum recorded as `N/A (pipeline not run)`. So the refactor was accepted with no
numerical validation whatsoever.

**Parity test first, never delete first** — agreed, and it is the brief's rule. But see F-02: the
parity target does not currently exist. This needs your decision before Phase 5 can be planned.

Two smaller documentation-vs-code divergences found while tracing, both of which should be fixed in
the docs rather than the code:

- **`docs/methodology.md:39-40`** lists "Industrial electricity price (reversed)" and
  "Interconnection queue health (reversed)" under Infrastructure. The code
  ([`build_infrastructure.R:7-8`](../R/categories/build_infrastructure.R#L7)) treats
  `electricity_price` as **positive** and `interconnection_queue` as negative. The code is right:
  the upstream `electricity_price` is already built as `1 - rowmean(...)`
  ([`07_process_data.R:510`](../scripts/07_process_data.R#L510)), so it is a "cheapness" index and
  must not be inverted twice. The prose is misleading; the polarity is correct. **Do not
  "fix" the code here** — that would silently reverse an indicator.
- **`config/weights.yml:1-7`** — the six top-level weights sum to **1.30**, not 1.0. This is not a
  bug: `weighted_index()` divides by the sum of weights present for non-missing indicators
  ([`utils_index.R:15-18`](../R/utils/utils_index.R#L15)), so the result is a proper weighted mean
  regardless. Documenting it so nobody "corrects" it and shifts every published score. Per the
  brief, weights are out of scope for this work entirely.

### 4. Invalid shell env var names — **CONFIRMED as a docs defect, REFUTED as a code defect**

The brief is right that this cannot work as documented, and right about the cause. But the code is
fine — the bug is only in the README.

**What is genuinely broken.** [`README.md:62-65`](../README.md#L62):

```bash
export Electro-Industrial_USE_SAMPLE_DATA=false
export Electro-Industrial_DATA_DIR=/path/to/data
```

A hyphen is not a legal character in a POSIX shell identifier, so `export` rejects this outright.
The documented instruction cannot be followed in `bash`, `zsh`, or `sh`.

**Why the code still works.** The variables are read with `Sys.getenv()`
([`config.R:23`](../R/utils/config.R#L23),
[`utils_paths.R:24-48`](../R/utils/utils_paths.R#L24)), which retrieves by exact string and does
not require a shell-legal name. Any mechanism that sets the variable without going through shell
word-parsing works — and [`ci.yml:10-12`](../.github/workflows/ci.yml#L10) does exactly that:

```yaml
env:
  Electro-Industrial_USE_SAMPLE_DATA: "true"
  SKIP_DATA_DOWNLOADS: "true"
```

GitHub Actions' `env:` block, `env 'Electro-Industrial_X=false' Rscript …`, and `Sys.setenv()` in
R all set it successfully. So the config mechanism is sound; only the README's `export` recipe is
impossible.

**Also found:** two variables are read but undocumented —
`Electro-Industrial_CACHE_DIR` ([`utils_paths.R:25`](../R/utils/utils_paths.R#L25)) and
`Electro-Industrial_SNAPSHOT_DATE` ([`utils_paths.R:46`](../R/utils/utils_paths.R#L46)) — neither
appears in the README's list at lines 81–88. And `SKIP_DATA_DOWNLOADS`
([`05_ingest_sources.R:7`](../scripts/05_ingest_sources.R#L7)) is the one variable with a
shell-legal name and no prefix at all.

**Proposed `EIG_` scheme**, with a one-release deprecation shim:

| current | proposed |
|---|---|
| `Electro-Industrial_CONFIG` | `EIG_CONFIG` |
| `Electro-Industrial_WEIGHTS` | `EIG_WEIGHTS` |
| `Electro-Industrial_INDEX_DEFINITION` | `EIG_INDEX_DEFINITION` |
| `Electro-Industrial_MISSING_DATA` | `EIG_MISSING_DATA` |
| `Electro-Industrial_DATA_DIR` | `EIG_DATA_DIR` |
| `Electro-Industrial_CACHE_DIR` | `EIG_CACHE_DIR` *(currently undocumented)* |
| `Electro-Industrial_OUTPUT_DIR` | `EIG_OUTPUT_DIR` |
| `Electro-Industrial_SNAPSHOT_DATE` | `EIG_SNAPSHOT_DATE` *(currently undocumented)* |
| `Electro-Industrial_USE_SAMPLE_DATA` | `EIG_USE_SAMPLE_DATA` |
| `Electro-Industrial_WRITE_AUDIT` | `EIG_WRITE_AUDIT` |
| `SKIP_DATA_DOWNLOADS` | `EIG_SKIP_DOWNLOADS` |

Shim: a single `eig_env()` accessor that prefers `EIG_*`, falls back to the old name, and emits one
deprecation warning per variable per session. Phase 5, per the brief. Also adds `EIG_MANUAL_DIR`
for the Phase 3 licensed-file path.

### 5. Output column naming inconsistency — **REFUTED**

The brief expected `Electro-Industrial_index` in the state output and `Electro_Industrial_index` in
the PEA output, with alias columns needed for one release. **The code emits the underscore form in
both.** Verified by executing both builders:

```
state builder emits: ..., Electro_Industrial_index, Electro_Industrial_index_w
PEA   builder emits: economic_area, state, cluster_index, Electro_Industrial_index, Electro_Industrial_index_w
```

Source: [`build_Electro-Industrial_index.R:47-48`](../R/indices/build_Electro-Industrial_index.R#L47)
and [`build_Electro-Industrial_pea_index.R:18-19`](../R/indices/build_Electro-Industrial_pea_index.R#L18).
A hyphenated column name is not expressible as a bare R symbol, which is why the code never had
one.

**So there is no code inconsistency and no downstream break risk. No alias columns are needed.**
The hyphen appears in two *non-code* places, and both are simply wrong:

- [`OUTPUTS_DATA_DICTIONARY.md:19-20`](../OUTPUTS_DATA_DICTIONARY.md#L19) documents
  `Electro-Industrial_index` / `_index_w` for the state output, while
  [line 99-100](../OUTPUTS_DATA_DICTIONARY.md#L99) correctly documents the underscore form for the
  PEA output. That internal disagreement is what the brief picked up on — it is a documentation
  error, not a code one.
- [`tests/fixtures/expected_Electro-Industrial.csv`](../tests/fixtures/expected_Electro-Industrial.csv)
  has hyphenated headers, which is the second half of F-03.

**Remedy.** Correct two lines of the data dictionary; regenerate the fixture. Both are trivial and
carry no migration cost. This is strictly better news than the brief assumed — dropping the
alias-column release from the plan.

*Filenames* do legitimately use hyphens throughout (`outputs/Electro-Industrial_state.csv`,
`R/indices/build_Electro-Industrial_index.R`), which is fine and should stay.

### 6. `METHODS.md` is a stub — **CONFIRMED**

[`METHODS.md`](../METHODS.md) is four lines pointing at `docs/methodology.md`.
`docs/methodology.md` is the real document (89 lines) and is referenced from
[`README.md:31`](../README.md#L31). Nothing links to `METHODS.md`.

**Remedy.** Delete `METHODS.md`; `docs/methodology.md` is already canonical and already linked.
Fold in the two prose corrections from Part 2 item 3 at the same time. Phase 5.

### 7. CI runs sample data only — **CONFIRMED, and CI is red**

[`ci.yml`](../.github/workflows/ci.yml) confirms every element of the hypothesis:

- `Electro-Industrial_USE_SAMPLE_DATA: "true"` and `SKIP_DATA_DOWNLOADS: "true"` are pinned in
  `env:` (lines 10–12) — sample data only, no real-data path.
- Triggers are `push` and `pull_request` only (lines 3–5) — **no `schedule:`, no
  `workflow_dispatch:`**.
- No freshness check, no validation step, no diff guard.
- No `renv` snapshot check and no pinned container; `setup-r@v2` floats to whatever R it resolves.
- The secret scan (lines 21–26) greps for `password\s*=` / `secret\s*=` / `token\s*=` across the
  whole tree including `data/raw/` — a 7 MB programme CSV with contact fields is a plausible
  false-positive source, and there is no allowlist.
- `lintr` is installed (line 18) but **never invoked**, so [`.lintr`](../.lintr) is unenforced.

And, as established: every run on record has failed, on `main` and on branches.

**Assessment.** This is the gap the rest of the work fills, as the brief says. One sequencing
consequence: **the freshness workflow must not be the first green thing in a red repo.** F-01 and
F-03 should land first, so that when the Monday 08:00 job opens its first issue, the pipeline it is
reporting on actually runs and the tests actually assert something.

### 8. No per-indicator vintage metadata — **CONFIRMED**

See [F-14](#f-14).

---

## Part 3 — Additional structural findings

Not in the brief's list; found while tracing. All are Phase 5 cleanup unless noted.

**3.1 — Duplicated config loaders.** Two parallel implementations read the same file:
`load_config()` ([`utils_paths.R:7-15`](../R/utils/utils_paths.R#L7)) and
`load_Electro_Industrial_config()` ([`config.R:22-28`](../R/utils/config.R#L22)). Only the latter
is called ([`00_setup.R:32`](../scripts/00_setup.R#L32)). `load_config()` is dead.

**3.2 — Duplicated helpers across the function/script boundary.** `make_generator_source()` and
`is_valid_xlsx()` are each defined twice, once in
[`ingest_sources.R:34-56`](../R/utils/ingest_sources.R#L34) and once in
[`07_process_data.R:543-565`](../scripts/07_process_data.R#L543), with the EIA-860M URL pattern
hard-coded in both. The second definition shadows the first. These belong in the
`R/ingest/utils.R` the brief specifies.

**3.3 — Dead and unreachable code in `07_process_data.R`.** Lines
[1486–1521](../scripts/07_process_data.R#L1486) re-read the PEA shapefile and rebuild
`cluster_pea_clean_electric_capacity_growth` with the `if (!is.na(...))` and `if (nrow(...) > 0)`
conditions stripped out and only their bodies left — the origin of F-01's missing closer. Step 0a
enclosed this region in the fallback block so the file parses, but **the stripped guards still need
reinstating**: as it stands, `sf::st_make_valid(pea_sf)` at
[`:1495`](../scripts/07_process_data.R#L1495) will be reached with `pea_sf = NULL` whenever the
shapefile read fails, since the `tryCatch` above it returns `NULL` and nothing checks. Lines
[160–200](../scripts/07_process_data.R#L160) run
shapefile reads, a live Census download, and a PEA population join at top level, outside any
function, before the input contract is even loaded. This 1,393-line script is the single biggest
obstacle to the brief's per-source connector architecture.

**3.4 — `data/processed/` is tracked and rewritten by every run.** Four tracked CSVs
(`inputs_processed.csv`, `cluster_pea_inputs_processed.csv`, `electrotech_fac.csv`,
`state_electro.csv`, `source_inventory.csv`) are pipeline *outputs* written by
[`07_process_data.R:1550-1553`](../scripts/07_process_data.R#L1550) and
[`:1112-1130`](../scripts/07_process_data.R#L1112). The working tree is currently dirty with four
modified tracked files and seven untracked outputs from a previous run. Committed build artifacts
mean every run produces a spurious diff and there is no way to tell a data refresh from a code
change. `.gitignore` excludes `outputs/` but not `data/processed/`.

This directly shapes Phase 2: the brief's `data/processed/` promotion target is currently a tracked
build-output directory. It needs to become either properly versioned published data
(`docs/data/vintages/<date>/`, per Phase 4.2) or ignored — not both at once.

**3.5 — `renv.lock` is a hand-written stub, not a snapshot.** Every one of the 18 package entries
in [`renv.lock`](../renv.lock) has only `Package` and `Version` — no `Source`, `Repository`, or
`Hash` fields, which is what a genuine `renv::snapshot()` emits and what `renv::restore()` needs to
resolve and verify an install. It also pins `R: 4.3.2` while CI's `setup-r@v2` floats to whatever
it resolves. So CI's `renv::restore()` step ([`ci.yml:20`](../.github/workflows/ci.yml#L20)) is
close to a no-op and real dependency resolution happens via `setup-r-dependencies@v2` reading
[`DESCRIPTION`](../DESCRIPTION) — see F-16 for why that fails too.

Worse, **`renv` is not actually activated.** [`renv/activate.R`](../renv/activate.R) is a six-line
no-op that reads `R_PROFILE_USER` and returns `invisible(NULL)`:

```r
local({
  rprofile <- Sys.getenv("R_PROFILE_USER")
  if (!nzchar(rprofile)) {
    return(invisible(NULL))
  }
})
```

It never sets a project library or hooks the loader, so the `source("renv/activate.R")` in
[`.Rprofile`](../.Rprofile) does nothing. There is no `renv/library/` and no `renv/settings.json`.
The repo has renv's *shape* without its behaviour. Phase 5 item 7 (dated rocker image +
`renv::restore()` + snapshot checking) requires initialising renv for real first —
`renv::init()`, not a hand-edit.

**3.6 — `.lintr` may not even load.** [`.lintr`](../.lintr) uses `with_defaults(...)`, which
`lintr` deprecated at 3.0 in favour of `linters_with_defaults()`. Combined with the fact that CI
installs `lintr` but never invokes it (Part 2 item 7), the config is doubly inert. I could not
execute this check — `lintr` is not installed in the environment I audited — so treat the
deprecation as needing confirmation when Phase 5 item 8 enables linting.

**3.7 — Authorship and citation metadata are scaffolding placeholders.** All three files credit a
code-generation tool rather than RMI or any person:

- [`DESCRIPTION:4-6`](../DESCRIPTION#L4) — `person("OpenAI Codex", role = c("aut", "cre"), email = "noreply@example.com")`
- [`CITATION.cff`](../CITATION.cff) — `authors: - name: "OpenAI Codex"`, and
  `repository-code: "https://example.com/Electro-Industrialindex"` (a placeholder URL), with
  `date-released: "2025-01-01"`
- [`inst/CITATION`](../inst/CITATION) — `author = person("OpenAI Codex")`,
  `textVersion = "OpenAI Codex (2025). Electro-Industrial Index."`

Phase 4.4 of the brief is explicitly about making the index **quotable** — "as of the 2026-09
vintage, X moved from 12th to 7th". A `CITATION.cff` naming OpenAI Codex and pointing at
`example.com` undercuts that entirely, and GitHub surfaces `CITATION.cff` in the repo sidebar. Fix
before the first published vintage, not in Phase 5.

Minor, same files: `inst/CITATION` uses `citEntry()`, which R deprecated at 4.2 in favour of
`bibentry()`.

**3.8 — `missing_data.yml` is documentation, not configuration.** All four keys in
[`config/missing_data.yml`](../config/missing_data.yml) are prose strings describing behaviour
that is hard-coded in `R/`. `load_missing_data()` parses it and stores it in an option
([`config.R:61-67`](../R/utils/config.R#L61)) which is never read by any scaling or index
function. It is a doc comment in YAML clothing — harmless, but it should not be mistaken for a
control surface when `config/validation.yml` is added in Phase 2.

**3.9 — `NA` is silently treated as "no data" where it may mean "zero".** `rowmean_index()`
([`utils_index.R:26-29`](../R/utils/utils_index.R#L26)) drops `NA` columns and averages the rest,
so a state missing one of four `regulatory_ease` indicators is scored on three. For `ordinance`
(32/50 populated) the 18 missing states are plausibly states with *no* recorded solar ordinances —
i.e. genuine zeros, which under reverse scaling would be the most favourable score, not a dropped
column. `build_policy_intent_index()` does coalesce two indicators to 0
([`build_policy_intent.R:9-10`](../R/categories/build_policy_intent.R#L9)) but nothing else does.

Whether each `NA` means "absent" or "zero" is a **per-indicator methodology question**, and the
brief puts methodology out of scope. Recording it because the Phase 2 validator will have to
encode an answer per source, and `config/missing_data.yml` currently documents the mechanism
without answering the question for any specific indicator.

---

## Part 4 — Recommended sequencing

This departs from the brief in one respect, and only one: **two blockers land before Phase 1.**

| step | work | rationale |
|---|---|---|
| **0a** ✅ | Fix F-01 (two brace defects) and F-16 (undeclared packages) + `scripts/check_syntax.R` in CI | the pipeline must parse before anything can report on it; these were two independent blockers |
| **0b** | Fix F-03 (test wd + fixture) | CI must be able to go green before a scheduled job starts filing issues |
| **0c** | Make the smoke test pass: F-10 (untracked `FCC_PEA_website.xlsx`) and F-11 (unguarded live reads), plus whatever the first full run surfaces | step 0a makes the file *parse*; it does not make `Rscript run_pipeline.R` *succeed* |
| **1** | Phase 1 as briefed — `sources.yml`, manifest, freshness engine, notifier | delivers value with zero connectors; the orphan check alone would have caught F-05 |
| **1b** | Issues for F-04, F-05 (each its own PR, each with a diff report) | both move published numbers; needs your sign-off on F-04's bug-vs-methodology framing |
| **2** | Phase 2 connectors, in the §5.2 shortlist order, 2–3 per PR | F-06, F-07, F-11, F-12 are fixed as part of the connectors that own those sources |
| **3** | Phase 3 manual lane | F-08, F-10 land here; this is where BNEF, GJF, CPCN and the untraced sources get owners |
| **4** | Phase 4 diff guard, published layer, dashboard | the diff guard is what makes F-04/F-05-class regressions visible next time |
| **5** | Phase 5 cleanup | blocked on the F-02 decision and the F-13 governance answer |

Cross-cutting, needed before Phase 1 closes: answers on **F-13** (repo visibility and
`can_commit_raw` per source), **F-15** (DC), and the **five `unknown` sources** in
`data_audit.md §5.1` — each needs a publisher and a steward, or it enters `sources.yml` as
`access_class: unknown` with an SLA and no retrieval path, which is the honest but least useful
outcome.
