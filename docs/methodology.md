# Electro-Industrial Index Methodology

This repository implements the Electro-Industrial Index as specified in `Legacy Script/Electro-Industrial_State.R`. The refactor keeps the index logic identical while organizing the pipeline and configs in an OSI-style structure.

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

Note that the county-level pull truncates these to four digits
(`electric_man_4d`), and a four-digit filter admits every six-digit child — so
the effective bundle is broader still than the list above. That is a
consequence of the implementation rather than a separate decision.

### Cluster Index
Inputs (higher is better unless noted):
- Workforce share
- Workforce growth
- Industry feasibility
- Clean electric capacity growth
- Industrial electricity price (reversed)
- Anchor metrics: datacenter MW, semiconductor manufacturing, battery manufacturing, solar manufacturing, EV manufacturing

Indicators are scaled by polarity. The cluster index is computed as the mean of non-anchor inputs plus the maximum anchor metric, then rescaled to [0, 1] for comparability. The output also records `dominant_anchor` (which anchor won), scaled `positive`/`negative` summaries, and a `cluster_top` label for areas with `cluster_index > 0.5`. The pipeline now computes both a PEA-level cluster index and a state-level cluster index where each state inherits its top-scoring PEA cluster (legacy behavior).

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
