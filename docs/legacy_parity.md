# Legacy parity: what the upstream scripts recover

**Status:** investigation findings. No code changes accompany this document.
**Date:** 2026-09-16
**Prompted by:** a pointer to
[`Clean_Econ_Dev/Econ Dev Scripts/Electrotech_State.R`](https://github.com/lrcarey222/Clean_Econ_Dev/blob/main/Econ%20Dev%20Scripts/Electrotech_State.R)
**Bears on:** [`refactor_plan.md` F-02](refactor_plan.md#f-02) (the parity target),
[F-05](refactor_plan.md#f-05), and the six indicators
[`data_audit.md` §5.3](data_audit.md#53-indicators-i-could-not-trace-to-any-producer-in-this-repo)
recorded as having no producer.

---

## 1. `Legacy Script/` is a corrupted copy of an upstream that still works

`Clean_Econ_Dev/Econ Dev Scripts/Electrotech_State.R` is the pre-rename original of this repo's
`Legacy Script/Electro-Industrial_State.R`. Compared directly:

| | upstream `Electrotech_State.R` | this repo's `Legacy Script/` |
|---|---|---|
| lines | 2,698 | 2,354 |
| hyphenated bare identifiers | **0** | ~61 |
| parses? | **yes — 445 top-level expressions** | **no — structural error at line 228** |
| only parse blocker | one stray `0xF7` byte inside a string literal at line 2513 | the identifiers, plus an incomplete expression at line 403 |

Verified by transliterating each file's non-ASCII bytes to ASCII and re-parsing. The upstream then
parses cleanly; this repo's copy still fails structurally.

So the rename that produced `Electro-Industrial` from `Electrotech` did two things: it corrupted ~61
identifiers into syntax errors, and it **lost roughly 344 lines** — including the definition of
`bundle_lq`, which exists in the upstream and is one of the objects F-02 recorded as undefined.

**This changes the basis of the F-02 decision.** Option 3 (declare `R/` canonical, accept that
parity was never established) was chosen when the only available reference could not be executed.
A reference that runs now exists.

## 2. Parity is more feasible than the F-02 write-up assumed

Checked on this machine:

| prerequisite | status |
|---|---|
| Packages for `CRED R Setup Script.R` | 33 referenced, **33 installed** |
| Packages for `Electrotech_State.R` | 21 referenced, 20 installed — only `blsQCEW` missing |
| Packages for `All_Geos_Combined.R` | 16 referenced, **16 installed** |
| The `raw_data` tree the scripts read | **exists**, 424 files / 136 directories |
| All 11 legacy input files the script needs | **all present** |
| `feasibility_complete_dataset.csv` | **exists**, 72 MB |

`raw_data` resolves via `CRED R Setup Script.R`, which does
`setwd("C:/Users/LCarey/")` and sets
`raw_data <- "OneDrive - RMI/.../ACRE/Data/Raw Data/"`.

**Remaining blockers, in order of difficulty:**

1. **`dsire_inc` is unrecoverable.** It is undefined in *both* repositories — no assignment
   anywhere in `Clean_Econ_Dev` either. `dsire_policy_count` therefore has no recoverable producer
   at all, upstream or downstream, and cannot be included in a parity comparison.
2. **`supplycurve_geo` needs `All_Geos_Combined.R` to run.** It is built at line 1572 from
   `bind_rows(results_list)`, i.e. a computation loop, and no prepared output file exists (unlike
   feasibility). That script makes network calls and may need API keys.
3. **`blsQCEW` is not a real package** — see §4.
4. `CRED R Setup Script.R` has an ordering bug of its own: line 4 uses `state_abbreviation` in
   `output_folder` before line 11 defines it, so it errors on a clean session until reordered.

**Assessment:** a *scoped* numerical parity run over the state-level index is achievable — most of
the way there, not a research project. A complete one is not, because of `dsire_inc`.

## 3. Four of the six "no producer" indicators are recovered

| indicator | recovered definition | where |
|---|---|---|
| `employment_lq` | `LQ_bundle = (state_bundle_empl / state_total_empl) / (us_bundle_empl / us_total_empl)`, from **state-level** QCEW annual rows (`own_code == 5`, `qtr == "A"`), summing the 6-digit `electric_man` bundle | `Electrotech_State.R:333-367` |
| `feasibility_index` | `industry_feas_perc`, weighted mean by `pci`, filtered to `geo == "State"` and `aggregation_level == 4` | `All_Geos_Combined.R:2466`, `:2794`; data at `ACRE/Data/feasibility_complete_dataset.csv` (72 MB, present) |
| `industry_feasibility` | same source as `feasibility_index`, different aggregation | as above |
| `renewable_potential` | `ren_index`, a row-mean over scaled supply-curve metrics with the first three numeric columns reversed | `All_Geos_Combined.R:1572-1584` — **computed, no stored output** |
| `spot_score` | `mean(Policy_Index)` per state over the 50 State Gap Analysis workbook | `Policy.R:1118-1124` — identical to this repo's implementation |
| `dsire_policy_count` | **not recoverable** | `dsire_inc` undefined in both repositories |

Two things worth drawing out.

**`spot_score` is reproduced faithfully.** This repo's implementation matches the upstream exactly:
`mean(Policy_Index)` per state. That matters for the SPOT use-restriction conflict recorded in
`config/sources.yml` — the additive aggregation SPOT's terms disclaim **predates this repo** and was
inherited, not introduced here. It still needs resolving, but it is not a refactor defect.

**`feasibility_index` is directly recoverable today.** The 72 MB dataset exists locally. This is the
cheapest of the four to restore.

---

## 4. ⚠️ The refactor doubled the employment NAICS bundle, and nobody decided to

This is the most consequential finding, and it is exactly the class of drift a parity test exists to
catch.

The upstream `electric_man` bundle is **19 six-digit NAICS codes**, all in 334xxx and 335xxx —
electrical equipment and electronic component manufacturing.

This repo's `electric_man_6d` ([`07_process_data.R:1137-1141`](../scripts/07_process_data.R#L1137))
is **39 codes**. It keeps all 19 and adds 20:

| added block | codes | what it is |
|---|---|---|
| Utilities | `221111`–`221121` (10 codes) | electric power generation — hydro, fossil, nuclear, solar, wind, geothermal, biomass |
| Telecom / broadcasting | `513322`, `513340`, `513390`, `515210`, `517210`, `517211`, `517212`, `517410`, `517910`, `517919` | wireless carriers, broadcasting, satellite, other telecommunications |

Nothing was dropped; the bundle was doubled. So the upstream measured *electrical equipment
manufacturing* employment, and this repo measures *manufacturing + utilities + telecom*.

**It gets wider still.** Line 1142 truncates to four digits:

```r
electric_man_4d <- unique(stringr::str_sub(electric_man_6d, 1, 4))
```

A 4-digit filter admits every 6-digit child, so `3342` pulls in codes beyond the three listed, and
`2211` pulls in all of utilities regardless of which generation types were enumerated. The effective
bundle is therefore broader than even the 39 codes suggest.

**Why this has not yet shown up in published numbers.** In this repo `electric_man_6d` feeds only
`workforce_share` and `workforce_growth`, and both are currently discarded before reaching the index
(F-05) — they sit at 3/50, holding sample data. `employment_lq`, which is what the upstream computed
from this bundle, has no producer here at all.

**Why that is a problem rather than a relief.** The moment F-05 is fixed — restoring the two
commented-out coalesces — this bundle becomes live, and `cluster` starts measuring a materially
different concept from the one the methodology describes, with no decision having been taken and no
note in `docs/methodology.md`. A bug fix would silently ship a methodology change.

**This needs a decision before F-05 is fixed**, and it is a methodology question, not an
engineering one: is the electro-industrial workforce (a) electrical equipment manufacturing, as
upstream, or (b) manufacturing plus utilities plus telecom, as the current constant says? Either is
defensible. Only one is documented, and it is (a).

## 5. `blsQCEW` is not a real package, in either repository

The upstream does `library(blsQCEW)` at `Electrotech_State.R:1282`, with the comment
`# provides blsQCEW()`. No such package exists on CRAN — the function `blsQCEW()` is provided by
`blsAPI`.

So the county-level QCEW pull could not run in the upstream either. The
`requireNamespace("blsQCEW")` guard restored in step 0a is inherited, not a defect introduced by the
refactor, and `workforce_share` / `workforce_growth` have very likely never been computed from real
data in any version of this analysis.

---

## 6. Recommendation

**Revisit F-02.** Specifically:

1. **Do not retire `Legacy Script/`.** Replace it with a note pointing at the upstream, which is the
   real methodology record. Keeping a corrupted copy in-tree as "canonical" is worse than having
   none.
2. **Resolve §4 before fixing F-05**, as a documented methodology decision either way.
3. **Restore `feasibility_index` and `industry_feasibility` first** of the recovered four — the
   dataset exists and the aggregation is written down, so it is mostly transcription.
4. **Attempt scoped parity** on the state-level index, excluding `dsire_policy_count` and noting the
   exclusion. Fixing one byte plus the setup-script ordering bug is the whole cost of entry.
5. **Accept `dsire_policy_count` as unrecoverable.** It has never had a producer. Either define what
   it should count against the DSIRE API now registered in `config/sources.yml`, or remove it from
   `policy_intent` — which is a composition change and needs its own decision.

**What is genuinely lost regardless:** the upstream is itself a working script, not a specification,
so "parity" means agreeing with one person's analysis code at one point in time. That is worth
having as a regression baseline. It is not the same as the methodology being independently
documented, and `docs/methodology.md` should say which of the two it is describing.
