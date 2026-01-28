# Data Inventory

This repository separates **public** data sources (downloadable) from **proprietary** or internal datasets. The pipeline expects a single `inputs.csv` in `data/` unless `ELECTROTECH_USE_SAMPLE_DATA=true`.

## Required inputs (`data/inputs.csv`)

The full pipeline expects the following columns:

| Column | Description | Source | Notes |
| --- | --- | --- | --- |
| state | State full name | Derived | Join key |
| abbr | State abbreviation | Derived | Join key |
| incentives_gdp | Incentives as % of GDP | Good Jobs First + BEA GDP | Proprietary GJF download |
| spot_score | SPOT policy score | SPOT Index | Public |
| dsire_policy_count | DSIRE policy count | DSIRE | Public |
| dev_policy_count | Economic development policy count | Internal program database | Proprietary |
| legislation_index | Energy legislation index | Climate legislation data | Proprietary |
| cpcn | CPCN requirements | Internal compilation | Proprietary |
| regdata_index | Regulatory restrictions index | RegData | Public |
| ordinance | Solar ordinance count | Internal compilation | Proprietary |
| sepa | SEPA presence | Internal compilation | Proprietary |
| employment_lq | Electrotech employment LQ | BLS QCEW | Public (API) |
| gdp_growth_index | Manufacturing GDP growth index | BEA GDP | Public |
| feasibility_index | Feasibility index | Internal model | Proprietary |
| economic_dynamism | Economic dynamism score | EIG Dynamism | Public |
| renewable_potential | Renewable potential index | NREL supply curve | Public |
| ev_stations_cap | EV charging per capita | AFDC | Public |
| interconnection_queue | Interconnection queue health | LBNL queue | Public |
| electricity_price | Industrial electricity price index | EIA | Public |
| cnbc_rank | CNBC infrastructure rank | CNBC | Proprietary |
| clean_tech_investment | Clean tech investment per GDP | Internal monitor | Proprietary |
| datacenter_index | Datacenter index | BNEF | Proprietary |
| electric_capacity_growth | Clean electric capacity growth | EIA | Public |
| semiconductor_investment | Semiconductor investment per GDP | SIA | Proprietary |
| evs_per_capita | EV registrations per capita | AFDC | Public |
| workforce_share | Electrotech workforce share | BLS QCEW | Public |
| workforce_growth | Electrotech workforce growth | BLS QCEW | Public |
| industry_feasibility | Industry feasibility | Internal model | Proprietary |
| clean_electric_capacity_growth | Clean electric capacity growth | EIA | Public |
| industrial_electricity_price | Industrial electricity price | EIA | Public |
| datacenter_mw | Datacenter MW | BNEF | Proprietary |
| semiconductor_manufacturing | Semiconductor manufacturing investment | SIA | Proprietary |
| battery_manufacturing | Battery manufacturing investment | Internal | Proprietary |
| solar_manufacturing | Solar manufacturing investment | Internal | Proprietary |
| ev_manufacturing | EV manufacturing investment | Internal | Proprietary |

## Sample data

A minimal synthetic dataset is included at `data/examples/sample_inputs.csv` (and mirrored in `inst/extdata/` for package usage). It enables CI smoke tests and unit tests without proprietary data.

## Caching

Downloaded public data should be cached under `data/raw_cache/` using `download_with_cache()`.
