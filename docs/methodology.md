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
| Electrical equipment and electronic components | `334210`, `334220`, `334290`, `335110`, `335121`, `335122`, `335129`, `335311`, `335312`, `335313`, `335314`, `335911`, `335912`, `335921`, `335929`, `335931`, `335932`, `335991`, `335999` | manufacturing — 19 codes |
| Utilities | `221111`–`221121` | electric power generation: hydro, fossil, nuclear, solar, wind, geothermal, biomass — 10 codes |
| Telecommunications and broadcasting | `513322`, `513340`, `513390`, `515210`, `517210`, `517211`, `517212`, `517410`, `517910`, `517919` | carriers, broadcasting, satellite — 10 codes |

So the electro-industrial workforce is **manufacturing plus utilities plus
telecommunications**, not manufacturing alone.

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
