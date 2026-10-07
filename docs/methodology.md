# Electro-Industrial Index Methodology

This repository implements the Electro-Industrial Index as specified in `Legacy Script/Electro-Industrial_State.R`. The refactor keeps the index logic identical while organizing the pipeline and configs in an OSI-style structure.

> **Note on the legacy script.** `Legacy Script/Electro-Industrial_State.R` is kept as a
> **historical record and is not runnable**. It does not parse, and it is not self-contained — it
> reads objects, such as `bundle_lq`, that are defined nowhere in this repository. Do not treat it
> as the executable specification; the pipeline under `scripts/` is. A working earlier version of
> the same analysis was found in a separate repository and is compared against this one in
> [`legacy_parity.md`](legacy_parity.md). Decision 2026-10-05: keep it, labelled. See
> `refactor_plan.md` F-02.

## Geographic scope

**The index covers the 50 states. Washington DC is excluded, deliberately, and so are the
territories.**

This has always been the behaviour — `states <- tibble(state = state.name, abbr = state.abb)` is
the spine of every state-level join, and base R's constants are the 50 states — but until
2026-10-05 it was a side effect of a constant rather than a stated scope. It is recorded here so it
is not later "fixed" by someone who assumes it was an oversight.

Two consequences worth knowing:

* Several upstream sources **do** carry DC — CIM socioeconomics, EIA-861M, BEA — so those rows are
  dropped on the join rather than never fetched. A DC row in an input file is not a sign of
  anything wrong.
* Adding DC later would change **every** published score, not just add a row, because min-max
  scaling is relative to the observed range. It is a re-baselining, not an extension.

PEA-level outputs follow the same rule: the FCC crosswalk's non-state entries (Puerto Rico, the
Virgin Islands) are dropped, so every PEA resolves to one of the 50 states.

## Sub-index definitions

### Policy Intent
Inputs (higher is better):
- Incentives as % of GDP
- SPOT policy score
- DSIRE policy count
- Economic development policy count
- Electro-Industrial legislation index

Each indicator is min-max scaled to [0, 1] and averaged to compute `intent_index`.

### Regulatory Ease
Inputs (higher is worse, reversed to [0, 1]):
- CPCN requirements
- Regulatory restrictions index
- Solar ordinance count
- SEPA presence

Each indicator is reverse min-max scaled and averaged to compute `ease_index`.

### Economic Capabilities
Inputs (higher is better):
- Electro-Industrial employment specialization (LQ)
- Manufacturing GDP growth index
- Feasibility index
- Economic dynamism score

Each indicator is min-max scaled and averaged to compute `econ_index`.

### Infrastructure
Inputs (higher is better unless noted):
- Renewable potential index
- EV charging per capita
- Industrial electricity price (reversed)
- Interconnection queue health (reversed)
- CNBC infrastructure rank (reversed)

Indicators are scaled by polarity, averaged for `infra_index`, and also combined with equal weights for `infra_index_w`.

### Deployment
Inputs (higher is better):
- Clean tech investment per GDP
- Datacenter index
- Electric capacity growth index
- Semiconductor investment per GDP
- EVs per capita

Indicators are min-max scaled and averaged to compute `deployment_index`.

### The electro-industrial NAICS bundle

`workforce_share` and `workforce_growth` measure employment in an explicitly
defined bundle of six-digit NAICS codes (`electric_man_6d` in
`scripts/07_process_data.R`). **The bundle is the broad definition**, decided
2026-09-17:

| block | codes | what it covers |
|---|---|---|
| Computer and electronic product manufacturing | `334210`, `334220`, `334290` | communications and broadcast equipment — 3 codes |
| Electrical equipment, appliance and component manufacturing | `335131`, `335132`, `335139`, `335311`, `335312`, `335313`, `335314`, `335910`, `335921`, `335929`, `335931`, `335932`, `335991`, `335999` | lighting, transformers, batteries, wiring devices, carbon and graphite — 14 codes |
| Utilities | `221111`–`221121` | electric power generation: hydro, fossil, nuclear, solar, wind, geothermal, biomass — 10 codes |
| Telecommunications and broadcasting | `516210`, `517112`, `517410`, `517810` | media streaming and social networks, wireless carriers, satellite, other telecommunications — 4 codes |

33 codes, all **NAICS 2022** except `221119`, which QCEW tags `NAICS07` and whose
successor cannot be established from the 2017-to-2022 concordance. Its parent
`2211` is current, and the parent is what the filter uses.

So the electro-industrial workforce is **manufacturing plus utilities plus
telecommunications**, not manufacturing alone.

The telecommunications block was remapped to NAICS 2022 on 2026-09-18. It
previously held codes from three superseded vintages, so nine of ten matched no
series BLS publishes and contributed nothing at all. Three codes — `513322`,
`513340`, `513390` — were **dropped rather than remapped**: they are not NAICS in
any vintage QCEW recognises, so what they were meant to denote cannot be
recovered. If broadcasting was intended, the current codes are `516110` (radio)
and `516120` (television), and adding them is a further decision. See
[`bls_qcew_options.md` §7](bls_qcew_options.md).

The four-digit parents actually used for retrieval are `2211`, `3342`, `3351`,
`3353`, `3359`, `5162`, `5171`, `5174`, `5178` — all nine current and all nine
retrievable from QCEW, against six of ten before the remap.

One consequence of reading at four digits: `5171` is "wired and wireless
telecommunications (except satellite)", so **wired carriers are included** even
though the six-digit list names only wireless. At this depth the two cannot be
separated.

This is recorded because it is a deliberate choice and it differs from the
upstream analysis this pipeline descends from, which used only the 19
manufacturing codes. See [`legacy_parity.md` §4](legacy_parity.md) for the
comparison and `refactor_plan.md` F-22 for how the divergence was found. Anyone
reading a workforce number should know which of the two definitions produced it.

Note that the retrieval truncates these to four digits (`electric_man_4d`), and
a four-digit filter admits every six-digit child — so the effective bundle is
broader still than the list above.

Since 2026-09-29 that truncation is a **deliberate decision rather than an
implementation accident**. QCEW withholds any cell that would disclose an
individual employer, and suppression rises steeply as industry detail deepens:
across the bundle, 16% of state cells are withheld at four-digit against 45–46%
at six-digit. Four-digit trades precision of definition for completeness of
data. The nine codes actually read are `2211`, `3342`, `3351`, `3353`, `3359`,
`5162`, `5171`, `5174`, `5178`.

### How the two workforce indicators are computed

Both come from BLS QCEW state industry slices, annual averages, private
ownership (`own_code 5`), via `R/connectors/qcew.R`:

    workforce_share  = bundle_employment / total_private_employment * 100
    workforce_growth = (matched_employment - matched_employment_3yr_prior)
                         / matched_employment_3yr_prior

`workforce_growth` is a **proportional growth rate over the three-year span**:
`0.12` means the electro-industrial bundle grew 12%. It is computed on a
**matched basket** — only the NAICS codes disclosed in *both* periods — and
`data/processed/qcew_coverage.csv` records how many codes each state's basket
contains.

The matched basket is not a refinement; without it the indicator is wrong.
QCEW decides suppression per period, so comparing two periods' bundle totals
compares two different baskets of industries. Nevada is the worked example:
NAICS 3359 reported 12,513 in 2022 and was withheld in 2025, so a naive
comparison shows employment collapsing 62% while every other Nevada code is
flat or rising. On the matched basket Nevada is −0.3%. Across the 2022–2025
pair, 17 of 50 states change their disclosure pattern, and those states showed
2.6× the spread of the 33 that did not.

This fixes comparability, not completeness: a matched basket still omits
whatever was withheld in either period, and a state whose basket is small —
the smallest is 2 of 9 codes — rests on correspondingly thin evidence. That is
why the basket size is published alongside the rate.

**Changed 2026-09-29.** The upstream divided the same numerator by *current
total private employment*, making the value a percentage-point change in
`workforce_share`'s numerator rather than a growth rate — so a state's reported
"growth" depended on the size of its whole private economy. It also compared
unmatched baskets, so the old values carried the same contamination, merely
compressed into a range (−0.009 to 0.001) too narrow for it to be visible. See
`refactor_plan.md` F-23.

**Suppressed cells are `NA`, not zero.** QCEW reports a withheld cell's value as
literal `0`, so treating it naively counts "withheld" as "none". Converting to
`NA` does not recover the employment and does not change the bundle total — it
makes the gap countable. 15% of state-industry cells were withheld in the 2025
vintage, affecting 27 of 50 states; per-state counts are published alongside the
numbers in `data/processed/qcew_coverage.csv`. **The bundle total understates
electro-industrial employment by an unknown amount.**

PEA-level values for both indicators are their state's value, broadcast by
join. QCEW cannot support genuine PEA-level workforce figures: 181 of 410 PEAs
would have no disclosed bundle employment at all. See
[`bls_qcew_options.md` §6](bls_qcew_options.md).

### Cluster Index
Inputs (higher is better unless noted):
- Workforce share
- Workforce growth
- Industry feasibility
- Clean electric capacity growth
- Industrial electricity price (reversed)
- Anchor metrics: datacenter MW, semiconductor manufacturing, battery manufacturing, solar manufacturing, EV manufacturing

Indicators are scaled by polarity. The cluster index is computed as the mean of non-anchor inputs plus the maximum anchor metric, then rescaled to [0, 1] for comparability. The output also records `dominant_anchor` (which anchor won), scaled `positive`/`negative` summaries, and a `cluster_top` label for areas with `cluster_index > 0.5`. The pipeline now computes both a PEA-level cluster index and a state-level cluster index where each state inherits its top-scoring PEA cluster (legacy behavior).

### What a PEA row means

A Partial Economic Area is **a geography in its own right, not a slice of a
state**. `outputs/Electro-Industrial_pea.csv` carries exactly one row per PEA,
and the facility anchors in it are summed over the whole PEA.

106 of the 416 PEAs cross a state line, so attaching state-level context —
workforce, electricity price, capacity growth — requires choosing one state per
PEA. That is **the state holding the largest share of the PEA's population**.
Population rather than land area or county count: the index is an economic
measure, so the state where most of a PEA's people live is the state whose
economic context applies to it.

Two consequences worth knowing when reading the outputs:

* A PEA's state is the *dominant* state, not the only one. `Baltimore,
  MD-Washington, DC` is attributed to Maryland although it also covers Virginia.
* The reverse mapping is **not** dominance. The state-level cluster index is
  the best-scoring PEA that **overlaps** that state, which is a different
  question — Connecticut, New Jersey and Rhode Island dominate no PEA at all,
  and inherit from the larger metros they sit inside.

Before 2026-10-06 neither choice was made: a PEA straddling a border emitted one
row per state, so the same PEA appeared two or three times with different
values. See `refactor_plan.md` F-21.

## Electro-Industrial Index

The combined Electro-Industrial Index uses the following weights (from the legacy script):

- Deployment: 0.4
- Infrastructure: 0.15
- Economic Capabilities: 0.15
- Policy Intent: 0.2
- Cluster Index: 0.2
- Regulatory Ease: 0.2

Weighted scores are computed with NA-aware normalization so that missing indicators do not zero-out the score.

## Missing data and outliers

- Missing and infinite values are converted to `NA` before scaling.
- Min-max scaling clamps values to the observed range.
- Weighted indices normalize by available weights to avoid penalizing missing data.

## Limitations

- Results depend on source data updates; use `snapshot_date` with cached downloads for determinism.
- Several indicators require proprietary or internal data. See `data/README.md` for access details.
- Sensitivity checks: consider alternate weightings, trimming extreme values, and alternate scaling methods.
