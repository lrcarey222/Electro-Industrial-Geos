# Data Audit — Electro-Industrial Index

**Status:** Phase 0 deliverable. Documentation only; no functional changes accompany this file.
**Audit date:** 2026-09-11
**Commit audited:** `d57a0b9` (`main`), working tree as checked out locally.

## How to read this

One row per input column of the `data/inputs.csv` data contract, as defined by
[`required_input_columns()`](../R/utils/schema.R#L42) and
[`config/index_definition.yml`](../config/index_definition.yml).

`evidence` cites the file and line where the URL, path, or filename was actually found.
Anything **inferred** rather than read carries `confidence: low` and an `ASSUMPTION:` note in
[§4 Notes and assumptions](#4-notes-and-assumptions). Rows marked `unknown` are treated as
`manual` until a human confirms them — per the brief, twelve honest `unknown`s beat twelve
confident guesses.

`current_vintage` is what the **committed** data actually represents, verified by reading the
files — not what the filename or directory name claims. Where those disagree, the row says so.

### Access class definitions

| class | meaning |
|---|---|
| `api` | programmatic endpoint, versioned response |
| `file_url` | stable(ish) bulk file at an HTTP URL |
| `scrape` | published on a web page, no bulk file |
| `manual` | a human must log in, export, or research it |
| `licensed` | contractual restriction on redistribution |
| `derived` | computed from other indicators or from internal RMI analysis |
| `static` | statutory or structural; changes rarely |
| `unknown` | origin not determinable from this repo |

---

## 1. Indicator inventory

### 1.1 Join keys

| indicator | sub_index | geography | publisher | evidence | access_class | cadence | current_vintage | automatable | confidence |
|---|---|---|---|---|---|---|---|---|---|
| `state` | all (key) | state | — (R `state.name`) | [`07_process_data.R:9`](../scripts/07_process_data.R#L9) | `derived` | n/a | n/a | yes — base R constant | high |
| `abbr` | all (key) | state | — (R `state.abb`) | [`07_process_data.R:9`](../scripts/07_process_data.R#L9) | `derived` | n/a | n/a | yes — base R constant | high |

> **Coverage note.** `states <- tibble(state = state.name, abbr = state.abb)` is 50 rows. **DC is
> excluded** from every state-level output, and the committed `inputs_processed.csv` has exactly
> 50 rows. The brief's Phase 2 validation target of "all 50 states + DC" is therefore a *change* to
> current coverage, not a restoration of it. Flagged for your decision, not changed here.

### 1.2 Policy intent

| indicator | sub_index | geography | publisher | evidence | access_class | cadence | current_vintage | automatable | confidence |
|---|---|---|---|---|---|---|---|---|---|
| `incentives_gdp` | policy_intent | state | Good Jobs First (subsidy values) ÷ U.S. Bureau of Economic Analysis (GDP) | [`07_process_data.R:220-253`](../scripts/07_process_data.R#L220); [`ingest_sources.R:13`](../R/utils/ingest_sources.R#L13); [`ingest_sources.R:61`](../R/utils/ingest_sources.R#L61) | `licensed` + `file_url` | GJF periodic; BEA annual | GJF award years 2021–2024; BEA `SAGDP9__ALL_AREAS_1997_2024` → `gdp_2024` | **needs-decision** — BEA half is automatable; GJF half is a licensed, hand-prepared export | high |
| `spot_score` | policy_intent | state | **unknown** | [`07_process_data.R:1233-1262`](../scripts/07_process_data.R#L1233) | `unknown` → `manual` | unknown | unknown — workbook carries no date | no — publisher unidentified | **low** (see A-1) |
| `dsire_policy_count` | policy_intent | state | **unknown** (docs claim DSIRE; no code produces it) | absent from `scripts/`; [`Legacy:84-86`](../Legacy%20Script/Electro-Industrial_State.R#L84) reads undefined `dsire_inc` | `unknown` → `manual` | unknown | **none — 3/50 values, all sample data** | no — no retrieval path exists | high (that it is untraced) |
| `dev_policy_count` | policy_intent | state | **unknown** (`dbo_Program.csv`) | [`07_process_data.R:256-281`](../scripts/07_process_data.R#L256); [`Legacy:91`](../Legacy%20Script/Electro-Industrial_State.R#L91) | `unknown` → `manual` | unknown — no date column | no — publisher unidentified | **low** (see A-2) |
| `legislation_index` | policy_intent | state | **likely Quorum** (column `quorum_id` present in file) | [`07_process_data.R:283-320`](../scripts/07_process_data.R#L283); [`Legacy:121`](../Legacy%20Script/Electro-Industrial_State.R#L121) | `licensed` → `manual` | 2,545 bills, 49 states; no vintage field | no — licensed legislative tracker | medium (see A-3) |

### 1.3 Regulatory ease

| indicator | sub_index | geography | publisher | evidence | access_class | cadence | current_vintage | automatable | confidence |
|---|---|---|---|---|---|---|---|---|---|
| `cpcn` | regulatory_ease | state | **LLM-generated statutory research** (filename `..._by_State_GPT.csv`) | [`07_process_data.R:322-334`](../scripts/07_process_data.R#L322) | `static` → `manual` | rare (statutory) | 50 states, undated | no — requires legal review, not retrieval | medium (see A-4) |
| `regdata_index` | regulatory_ease | state | Mercatus Center / QuantGov **RegData** (named in docs, no URL in code) | [`07_process_data.R:336-353`](../scripts/07_process_data.R#L336); [`docs/sources.md:8`](sources.md#L8) | `file_url` (URL not in repo) | periodic | period codes 2020–2022; 50 US jurisdictions | **needs-decision** — publisher named but no endpoint recorded anywhere | medium |
| `ordinance` | regulatory_ease | state | **unknown** (`Solar Ordinances.csv`) | [`07_process_data.R:355-366`](../scripts/07_process_data.R#L355) | `unknown` → `manual` | ordinance years 2009–2021; 1,412 rows, **34 states** | no — publisher unidentified | **low** (see A-5) |
| `sepa` | regulatory_ease | state | **unknown** (`state_sepa.csv`, 2 columns) | [`07_process_data.R:368-377`](../scripts/07_process_data.R#L368) | `static` → `manual` | 50 states, undated | no — binary research finding | **low** (see A-6) |

### 1.4 Economic capabilities

| indicator | sub_index | geography | publisher | evidence | access_class | cadence | current_vintage | automatable | confidence |
|---|---|---|---|---|---|---|---|---|---|
| `employment_lq` | economic_capabilities | state | **unknown** (no producer in repo) | absent from `scripts/`; [`Legacy:481-483`](../Legacy%20Script/Electro-Industrial_State.R#L481) reads undefined `bundle_lq` | `unknown` → `manual` | unknown | **none — 3/50 values, all sample data** | no — no retrieval path exists | high (that it is untraced) |
| `gdp_growth_index` | economic_capabilities | state | U.S. Bureau of Economic Analysis, SQGDP | [`07_process_data.R:99-157`](../scripts/07_process_data.R#L99) | `file_url` | quarterly | `SQGDP9__ALL_AREAS_2005_2025`; 48/50 populated | **yes** — but see F-06: `SQGDP.zip` is read yet absent from the download registry, and the filename is hard-coded with years | high |
| `feasibility_index` | economic_capabilities | state | **internal RMI analysis** | absent from `scripts/`; [`Legacy:422`](../Legacy%20Script/Electro-Industrial_State.R#L422) commented path `.../ClimateandEconomicJusticeTool/feasibility_geo.csv` | `derived` → `manual` | unknown | **none — 3/50 values, all sample data** | no — internal model output | medium |
| `economic_dynamism` | economic_capabilities | state | Economic Innovation Group, Index of State Dynamism | [`07_process_data.R:414-428`](../scripts/07_process_data.R#L414); [`ingest_sources.R:65-68`](../R/utils/ingest_sources.R#L65) | `file_url` | periodic (irregular) | **2022** (`max(year)` of 1992–2022 panel); 50/50 populated | **yes** — static asset URL, no key | high |

### 1.5 Infrastructure

| indicator | sub_index | geography | publisher | evidence | access_class | cadence | current_vintage | automatable | confidence |
|---|---|---|---|---|---|---|---|---|---|
| `renewable_potential` | infrastructure | state | **internal RMI analysis** (NREL supply curve per docs) | absent from `scripts/`; [`Legacy:500-535`](../Legacy%20Script/Electro-Industrial_State.R#L500) reads undefined `supplycurve_geo`, commented "*NB calculated in All Geos*" | `derived` → `manual` | unknown | **none — 3/50 values, all sample data** | no — computed in a different, absent script | medium |
| `ev_stations_cap` | infrastructure | state | DOE Alternative Fuels Data Center ÷ Clean Investment Monitor (population) | [`07_process_data.R:435-463`](../scripts/07_process_data.R#L435); [`ingest_sources.R:69-73`](../R/utils/ingest_sources.R#L69) | `file_url` | annual | URL pins `?year=2024`; 50/50 populated — **but see F-07, the column read is positional and lands on unnamed columns** | **yes** — with a rewritten parser | high on source, **low on value** |
| `interconnection_queue` | infrastructure | state | Lawrence Berkeley National Laboratory, *Queued Up* | [`07_process_data.R:465-484`](../scripts/07_process_data.R#L465); [`ingest_sources.R:74-78`](../R/utils/ingest_sources.R#L74) | `file_url` | annual | data **through 2024**; URL path pins `2025-08/`; 48/50 populated | **needs-decision** — URL embeds both release month and file version, so it changes every release | high |
| `electricity_price` | infrastructure | state | U.S. Energy Information Administration, EIA-861M | [`07_process_data.R:486-517`](../scripts/07_process_data.R#L486); [`ingest_sources.R:79-83`](../R/utils/ingest_sources.R#L79) | `file_url` | monthly | file runs to **2025-M11**; code fixes comparison years 2014/2019/2024 | **yes** — stable URL, no key | high on source; **value is wrong, see F-04** |
| `cnbc_rank` | infrastructure | state | CNBC, *America's Top States for Business* (infrastructure sub-rank) | [`07_process_data.R:731-744`](../scripts/07_process_data.R#L731); [`Legacy:607`](../Legacy%20Script/Electro-Industrial_State.R#L607) | `scrape` → `manual` | annual | **undeterminable** — 50 rows, no year column anywhere in the file | no — editorial content, no bulk file | medium on publisher, **low on vintage** |

### 1.6 Deployment

| indicator | sub_index | geography | publisher | evidence | access_class | cadence | current_vintage | automatable | confidence |
|---|---|---|---|---|---|---|---|---|---|
| `clean_tech_investment` | deployment | state | Clean Investment Monitor (Rhodium Group / MIT CEEPR) | [`07_process_data.R:379-412`](../scripts/07_process_data.R#L379) | `file_url` — **terms unverified** | quarterly | `version: 2025_Q2.20250811.0` (embedded in file); 50/50 populated | **needs-decision** — redistribution terms must be confirmed before automating | high |
| `datacenter_index` | deployment | state | BloombergNEF, *Global Data Center Live IT Capacity Database* | [`07_process_data.R:746-812`](../scripts/07_process_data.R#L746); [`ingest_sources.R:22`](../R/utils/ingest_sources.R#L22) | `licensed` → `manual` | file dated `2025-08-08`; code hard-filters `Date == "2025-03-31"` (**see F-08**) | no — commercial licence | high |
| `electric_capacity_growth` | deployment | state | U.S. Energy Information Administration, EIA-860M | [`07_process_data.R:519-728`](../scripts/07_process_data.R#L519); [`ingest_sources.R:89-93`](../R/utils/ingest_sources.R#L89) | `file_url` | monthly | latest valid local workbook is `may_generator2026.xlsx` (**untracked**); 44/50 populated | **yes** — URL is a predictable `<month>_generator<year>.xlsx` pattern | high |
| `semiconductor_investment` | deployment | state | Semiconductor Industry Association (chip supply-chain investment tracker) ÷ CIM real GDP | [`07_process_data.R:839-870`](../scripts/07_process_data.R#L839); URL in comment at [`Legacy:815`](../Legacy%20Script/Electro-Industrial_State.R#L815) | `scrape` → `manual` | 140 projects, 29 states; no date column | no — published as a web page, not a file | medium |
| `evs_per_capita` | deployment | state | DOE Alternative Fuels Data Center ÷ CIM population | [`07_process_data.R:814-837`](../scripts/07_process_data.R#L814); [`ingest_sources.R:84-88`](../R/utils/ingest_sources.R#L84) | `file_url` | annual | filename pins `9-06-24` (2023 registration year); 50/50 populated | **needs-decision** — URL has a dated filename *and* a cache-busting query string `?12518e7893`; both change per release | high |

### 1.7 Cluster

| indicator | sub_index | geography | publisher | evidence | access_class | cadence | current_vintage | automatable | confidence |
|---|---|---|---|---|---|---|---|---|---|
| `workforce_share` | cluster | state (county-rolled-up) | U.S. Bureau of Labor Statistics, QCEW | [`07_process_data.R:1137-1211`](../scripts/07_process_data.R#L1137) | `api` | quarterly | **computed then discarded — 3/50 values, all sample data** (see F-03) | **yes** — QCEW open API, no key; but ~3,100 calls per quarter needs rework | high |
| `workforce_growth` | cluster | state (county-rolled-up) | U.S. Bureau of Labor Statistics, QCEW | [`07_process_data.R:1213-1227`](../scripts/07_process_data.R#L1213) | `api` | quarterly | **computed then discarded — 3/50 values, all sample data** (see F-03); baseline fixed at 2022-Q1 | **yes** — same as above | high |
| `industry_feasibility` | cluster | state / PEA | **internal RMI analysis** (same `industry_feas_perc` as `feasibility_index`) | absent from `scripts/`; [`Legacy:1662`](../Legacy%20Script/Electro-Industrial_State.R#L1662) | `derived` → `manual` | **none — 3/50 values, all sample data** | no — internal model output | medium |
| `clean_electric_capacity_growth` | cluster | state; PEA via point-in-polygon | U.S. Energy Information Administration, EIA-860M | state: [`07_process_data.R:724-725`](../scripts/07_process_data.R#L724); PEA: [`07_process_data.R:1502-1508`](../scripts/07_process_data.R#L1502) | `file_url` | monthly | as `electric_capacity_growth`; 42/50 populated | **yes** | high |
| `industrial_electricity_price` | cluster | state | U.S. Energy Information Administration, EIA-861M | [`process_data_helpers.R:49-72`](../R/utils/process_data_helpers.R#L49); joined at [`07_process_data.R:1288`](../scripts/07_process_data.R#L1288) | `file_url` | monthly | **3/50 values, all sample data** — the real value is emitted under the wrong column name (see F-05) *and* reads the wrong sector (see F-04) | **yes** | high |
| `datacenter_mw` | cluster | state; PEA via point-in-polygon | BloombergNEF | [`07_process_data.R:807-808`](../scripts/07_process_data.R#L807) | `licensed` → `manual` | file dated `2025-08-08`, hard-filtered to `2025-03-31`; 50/50 populated | no — commercial licence | high |
| `semiconductor_manufacturing` | cluster | state; PEA via point-in-polygon | Semiconductor Industry Association | [`07_process_data.R:874-891`](../scripts/07_process_data.R#L874) | `scrape` → `manual` | 140 projects, 29 states; 28/50 populated | no | medium |
| `battery_manufacturing` | cluster | state; PEA via point-in-polygon | Clean Investment Monitor | [`07_process_data.R:898-1019`](../scripts/07_process_data.R#L898) | `file_url` — terms unverified | quarterly | **mixed vintage**: facility metadata is `2025_Q4.20260109.0` inside a directory named `q2_2025` (see F-09); 35/50 populated | **needs-decision** | high |
| `solar_manufacturing` | cluster | state; PEA via point-in-polygon | Clean Investment Monitor | as above | `file_url` — terms unverified | quarterly | as above; 35/50 populated | **needs-decision** | high |
| `ev_manufacturing` | cluster | state; PEA via point-in-polygon | Clean Investment Monitor | as above | `file_url` — terms unverified | quarterly | as above; 35/50 populated | **needs-decision** | high |

---

## 2. Supporting inputs (not index columns, but required to build them)

These are not columns of `inputs.csv`, but the pipeline cannot produce `inputs.csv` without them.
They need registry entries and SLAs exactly like the indicators do.

| input | used for | publisher | evidence | access_class | current_vintage | notes |
|---|---|---|---|---|---|---|
| CIM `socioeconomics.csv` | denominator (population, real GDP) for 4 indicators | Clean Investment Monitor | [`07_process_data.R:380-387`](../scripts/07_process_data.R#L380) | `file_url` | `version: 2025_Q2.20250811.0` | a single missing file silently NA-s `ev_stations_cap`, `evs_per_capita`, `semiconductor_investment`, `clean_tech_investment` |
| BEA `SAGDP.zip` | denominator for `incentives_gdp` | U.S. Bureau of Economic Analysis | [`ingest_sources.R:61`](../R/utils/ingest_sources.R#L61); [`07_process_data.R:69-97`](../scripts/07_process_data.R#L69) | `file_url` | `SAGDP9__ALL_AREAS_1997_2024` | filename hard-coded with years |
| BEA `SQGDP.zip` | `gdp_growth_index` | U.S. Bureau of Economic Analysis | [`07_process_data.R:100-108`](../scripts/07_process_data.R#L100) | `file_url` | `SQGDP9__ALL_AREAS_2005_2025` | **read but never downloaded** — absent from the registry (F-06) |
| FCC PEA shapefile | every PEA-level output | Federal Communications Commission | [`07_process_data.R:163`](../scripts/07_process_data.R#L163), [`:1383`](../scripts/07_process_data.R#L1383) | `file_url` | undated | committed; a second orphan copy exists at `data/raw/FCC_PEAs_website.shp` with no sidecar files |
| `FCC_PEA_website.xlsx` (sheet 3) | PEA↔county crosswalk → PEA population | Federal Communications Commission | [`07_process_data.R:184-185`](../scripts/07_process_data.R#L184) | `file_url` | unknown | **untracked and absent from the ingest registry** — a fresh clone cannot build PEA population (F-10) |
| Census county population | PEA population denominator | U.S. Census Bureau, Population Estimates | [`07_process_data.R:182`](../scripts/07_process_data.R#L182) | `file_url` | `co-est2023-alldata` (2020–2023) | **read from a live URL at script top level**, no cache, no guard — violates the no-network CI rule (F-11) |
| `tigris::states(year = 2023)` | point-in-polygon for BNEF data centres | U.S. Census Bureau (via `tigris`) | [`07_process_data.R:760`](../scripts/07_process_data.R#L760) | `api` | TIGER **2023** | live network call at runtime; year hard-coded |
| `50 State Gap Analysis.xlsx` | `spot_score` | **unknown** | [`07_process_data.R:1233`](../scripts/07_process_data.R#L1233) | `unknown` → `manual` | unknown | one worksheet per state, Yes/No/Partial answers |
| `us_drone_facility_announcements_2022_2025.csv` | facility map / PEA rollups only | **unknown** (`Source` column cites AP News, Site Selection, JobsOhio) | [`07_process_data.R:1061-1079`](../scripts/07_process_data.R#L1061) | `manual` | announcements 2022–2025, 12 rows | feeds `pea_electro.csv`, **not** any index indicator |

---

## 3. Staged but unused

`ingest_legacy_sources()` stages these; nothing in `scripts/` or `R/` reads them. Total ≈ 32 MB
of committed payload with no consumer. Listed for the Phase 5 cleanup decision, not deleted here.

| file | size | evidence it is staged | evidence it is unread |
|---|---|---|---|
| `data/raw/egrid2023_data_metric_rev2.xlsx` | 24.8 MB | [`ingest_sources.R:28`](../R/utils/ingest_sources.R#L28) | no `egrid` reference in `scripts/` or `R/` |
| `data/raw/table_8.xlsx` | 98 KB | [`ingest_sources.R:29`](../R/utils/ingest_sources.R#L29) | no `table_8` reference in `scripts/` or `R/` |
| `data/raw/state_business_cycle_status.csv` | 1.4 KB | [`ingest_sources.R:26`](../R/utils/ingest_sources.R#L26) | no `business_cycle` reference in `scripts/` or `R/` |
| `data/raw/FCC_PEAs_website.shp` | 5.9 MB | — (orphan duplicate) | no code path reads this path; the used copy is under `FCC_PEAs_Website/` |
| `OneDrive - RMI/.../States Data/eia_sales.xlsx` | 2.2 MB | — | byte-identical in size to `data/raw/remote/sales_revenue.xlsx`; **no `OneDrive` reference anywhere in `R/`, `scripts/`, or `config/`** |

Additionally, the `ingest_legacy_sources()` registry points two entries at
`Downloads/…` subpaths that do not exist, so `source_inventory.csv` has recorded
`exists = FALSE` for them since it was last written:
[`ingest_sources.R:26-27`](../R/utils/ingest_sources.R#L26).
The files are present one level up, and `07_process_data.R` falls back to that location for the
drone file only ([`:1061-1064`](../scripts/07_process_data.R#L1061)).

---

## 4. Notes and assumptions

Every `ASSUMPTION` below is something I inferred and could **not** confirm from the repo.
None of them should be acted on until you confirm.

- **A-1 — `spot_score`.** [`data/README.md:14`](../data/README.md#L14) names the source as "SPOT
  Index" and marks it Public. No URL, publisher, or organisation appears anywhere in the code, and
  `50 State Gap Analysis.xlsx` carries no provenance metadata. `Legacy:82` is the tautology
  `spot <- spot`, i.e. the object is supplied by a caller that is not in this repo.
  **ASSUMPTION:** "SPOT" is an external policy scorecard. Cannot verify. Classified `unknown`.
- **A-2 — `dev_policy_count` / `dbo_Program.csv`.** [`data/README.md:16`](../data/README.md#L16)
  says "Internal program database", "Proprietary". The `dbo_` prefix indicates a SQL Server table
  export. Column names (`ProgramCap`, `LegalCitation`, `ReportingRequirement`, `ProgramObjective`,
  plus administrator contact fields) describe a state business-incentive programme inventory.
  **ASSUMPTION:** this is a third-party subscription database rather than RMI-authored. I am
  deliberately **not** naming a vendor. Classified `unknown`; needs an owner to state what it is
  and whether it may be redistributed.
- **A-3 — `legislation_index` / `climate_leg.csv`.** The file contains a `quorum_id` column and
  `source_link` values pointing at state legislature sites. That column name is direct in-file
  evidence of a Quorum export. **ASSUMPTION:** Quorum is the publisher and the data is licensed.
  `confidence: medium` — the column name is evidence, but not a licence.
- **A-4 — `cpcn`.** The filename ends `_GPT.csv`, which is in-repo evidence that the table was
  generated by a language model rather than compiled from statute. The pipeline converts it to a
  regulatory-friction score with no review step
  ([`07_process_data.R:328-332`](../scripts/07_process_data.R#L328)). **This should be verified
  against statute by a human before the next published vintage**, independent of any automation
  work. Not a retrieval problem; a provenance problem.
- **A-5 — `ordinance` / `Solar Ordinances.csv`.** [`data/README.md:20`](../data/README.md#L20)
  says "Internal compilation". The schema (`Feature Type`, `Value Type`, `Value`, `Citation`,
  `Original Captured Date`, `Update Status`) resembles a published local-ordinance database.
  **ASSUMPTION:** derived from an external ordinance database rather than authored at RMI. Cannot
  verify. Note the indicator covers **34 of 50 states**; the remaining 16 are `NA`, not zero, and
  `rowmean_index()` drops them rather than scoring them — so those states' `ease_index` is computed
  from three indicators instead of four. Flagged, not changed.
- **A-6 — `sepa`.** `state_sepa.csv` is two columns and 50 rows of 0/1 with no citations. "SEPA"
  is presumably a State Environmental Policy Act analogue. **ASSUMPTION:** hand-researched.
  Needs a steward and a citation per state.
- **A-7 — CIM redistribution terms.** The Clean Investment Monitor files carry embedded `version:`
  and `created:` lines but **no licence text**. The repo is MIT-licensed. I have **not** assumed
  `can_commit_raw: true` for CIM. Needs confirmation before Phase 2.
- **A-8 — RegData endpoint.** The publisher is named in the docs but no URL exists in the repo, so
  I have recorded `endpoint: null`. Do not let me guess one.

---

## 5. Summary

### 5.1 Count by access class

Counted over the 33 indicator columns (excluding the `state` / `abbr` keys). Where an indicator
draws on two sources, it is counted under the more restrictive class.

| access_class | n | indicators |
|---|---|---|
| `file_url` | 8 | `gdp_growth_index`, `economic_dynamism`, `ev_stations_cap`, `interconnection_queue`, `electricity_price`, `electric_capacity_growth`, `clean_electric_capacity_growth`, `industrial_electricity_price` |
| `file_url` (terms unverified) | 4 | `clean_tech_investment`, `battery_manufacturing`, `solar_manufacturing`, `ev_manufacturing` |
| `api` | 2 | `workforce_share`, `workforce_growth` |
| `licensed` | 3 | `incentives_gdp`, `datacenter_index`, `datacenter_mw` |
| `scrape` | 2 | `semiconductor_investment`, `semiconductor_manufacturing` |
| `scrape` / editorial | 1 | `cnbc_rank` |
| `derived` (internal RMI) | 3 | `feasibility_index`, `renewable_potential`, `industry_feasibility` |
| `static` | 2 | `cpcn`, `sepa` |
| `unknown` | 5 | `spot_score`, `dsire_policy_count`, `dev_policy_count`, `ordinance`, `regdata_index`* |
| `licensed` (inferred) | 1 | `legislation_index` |
| | **33** | |

\* `regdata_index` has a named publisher but no recorded endpoint, so it cannot be automated as
things stand. Counted `unknown` for automation purposes.

### 5.2 Automation shortlist

**Recommended for Phase 2 — verified public, stable, no key required.** Two to three per PR.

| rank | source | feeds | retrieval mechanism | key? |
|---|---|---|---|---|
| 1 | EIA-861M retail sales & revenue | `electricity_price`, `industrial_electricity_price` | static URL `https://www.eia.gov/electricity/data/eia861m/xls/sales_revenue.xlsx` ([`ingest_sources.R:81`](../R/utils/ingest_sources.R#L81)) | no |
| 2 | EIA-860M generator inventory | `electric_capacity_growth`, `clean_electric_capacity_growth` | predictable pattern `https://www.eia.gov/electricity/data/eia860m/xls/<month>_generator<year>.xlsx` ([`ingest_sources.R:42`](../R/utils/ingest_sources.R#L42)) | no |
| 3 | BEA regional GDP (SAGDP + SQGDP) | `gdp_growth_index`, `incentives_gdp` denominator | `https://apps.bea.gov/regional/zip/SAGDP.zip`, and **SQGDP.zip by the same pattern — currently missing from the registry** | no |
| 4 | EIG Index of State Dynamism | `economic_dynamism` | static asset URL ([`ingest_sources.R:66`](../R/utils/ingest_sources.R#L66)) | no |
| 5 | BLS QCEW | `workforce_share`, `workforce_growth` | `blsAPI::blsQCEW()` open data ([`07_process_data.R:1153`](../scripts/07_process_data.R#L1153)) | no |
| 6 | LBNL interconnection queue | `interconnection_queue` | URL embeds release month + version; needs a discovery step | no |
| 7 | DOE AFDC station counts + EV registrations | `ev_stations_cap`, `evs_per_capita` | URLs carry dated filenames and query tokens; needs a discovery step **and** a parser rewrite | no |

**Explicitly out of scope for automation** — licensed, editorial, internal, or untraced:
`incentives_gdp` (GJF half), `datacenter_index`, `datacenter_mw`, `legislation_index`,
`semiconductor_investment`, `semiconductor_manufacturing`, `cnbc_rank`, `cpcn`, `sepa`,
`spot_score`, `dsire_policy_count`, `dev_policy_count`, `ordinance`, `feasibility_index`,
`renewable_potential`, `industry_feasibility`.

**Blocked on your decision:** the four Clean Investment Monitor indicators, and `regdata_index`.

### 5.3 Indicators I could not trace to any producer in this repo

Six indicators are consumed by the index but produced by nothing in `scripts/` or `R/`. They can
only arrive via `data/inputs.csv`, **which does not exist in this repo or working tree**.

`dsire_policy_count` · `employment_lq` · `feasibility_index` · `renewable_potential` ·
`industry_feasibility` · (`spot_score` is produced, but from an unidentified workbook)

Together with the two discarded QCEW indicators and the misnamed EIA price column, this is why
eight indicators in the published vintage carry only sample data. See F-03 and F-05 in
[`refactor_plan.md`](refactor_plan.md).

### 5.4 Verified coverage of the committed vintage

Non-`NA` counts per indicator in [`data/processed/inputs_processed.csv`](../data/processed/inputs_processed.csv)
(50 rows). Indicators at exactly **3/50** hold only the California / Texas / New York rows from
[`data/examples/sample_inputs.csv`](../data/examples/sample_inputs.csv).

| n non-NA | indicators |
|---|---|
| 50/50 | `spot_score`, `dev_policy_count`, `cpcn`, `sepa`, `economic_dynamism`, `ev_stations_cap`, `electricity_price`, `cnbc_rank`, `clean_tech_investment`, `datacenter_index`, `evs_per_capita`, `datacenter_mw` |
| 49/50 | `regdata_index` |
| 48/50 | `gdp_growth_index`, `interconnection_queue` |
| 44/50 | `electric_capacity_growth` |
| 42/50 | `clean_electric_capacity_growth` |
| 40/50 | `incentives_gdp` |
| 38/50 | `legislation_index` |
| 35/50 | `battery_manufacturing`, `solar_manufacturing`, `ev_manufacturing` |
| 32/50 | `ordinance` |
| 28/50 | `semiconductor_investment`, `semiconductor_manufacturing` |
| **3/50** | **`dsire_policy_count`, `employment_lq`, `feasibility_index`, `renewable_potential`, `workforce_share`, `workforce_growth`, `industry_feasibility`, `industrial_electricity_price`** |

The file also carries a 36th column, `ind_price_m`, at 50/50 — an unintended output that holds the
value `industrial_electricity_price` was supposed to receive. See F-05.
