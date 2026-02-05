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

### Cluster Index
Inputs (higher is better unless noted):
- Workforce share
- Workforce growth
- Industry feasibility
- Clean electric capacity growth
- Industrial electricity price (reversed)
- Anchor metrics: datacenter MW, semiconductor manufacturing, battery manufacturing, solar manufacturing, EV manufacturing

Indicators are scaled by polarity. The cluster index is computed as the mean of non-anchor inputs plus the maximum anchor metric, then rescaled to [0, 1] for comparability.

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
