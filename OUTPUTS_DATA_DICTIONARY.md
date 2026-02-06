# Output Data Dictionary

## outputs/Electro-Industrial.csv

| Column | Description |
| --- | --- |
| state | State name |
| abbr | State abbreviation |
| deployment_index | Deployment sub-index (0-1) |
| infra_index | Infrastructure sub-index (0-1) |
| econ_index | Economic capabilities sub-index (0-1) |
| intent_index | Policy intent sub-index (0-1) |
| ease_index | Regulatory ease sub-index (0-1) |
| cluster_index | Cluster sub-index (0-1) |
| Electro-Industrial_index | Unweighted sum of scaled sub-indexes |
| Electro-Industrial_index_w | Weighted Electro-Industrial Index |

## outputs/deployment.csv

| Column | Description |
| --- | --- |
| state | State name |
| abbr | State abbreviation |
| clean_tech_investment | Clean tech investment index |
| datacenter_index | Datacenter index |
| electric_capacity_growth | Electric capacity growth index |
| semiconductor_investment | Semiconductor investment index |
| evs_per_capita | EV registrations per capita |
| deployment_index | Deployment sub-index (0-1) |

## outputs/reg_friction.csv

| Column | Description |
| --- | --- |
| state | State name |
| abbr | State abbreviation |
| cpcn | CPCN requirements (scaled) |
| regdata_index | Regulatory restrictions index (scaled) |
| ordinance | Solar ordinance count (scaled) |
| sepa | SEPA presence (scaled) |
| ease_index | Regulatory ease sub-index (0-1) |

## outputs/cluster_index.csv

| Column | Description |
| --- | --- |
| state | State name |
| abbr | State abbreviation |
| workforce_share | Workforce share |
| workforce_growth | Workforce growth |
| industry_feasibility | Industry feasibility |
| clean_electric_capacity_growth | Clean electric capacity growth |
| industrial_electricity_price | Industrial electricity price |
| datacenter_mw | Datacenter MW |
| semiconductor_manufacturing | Semiconductor manufacturing |
| battery_manufacturing | Battery manufacturing |
| solar_manufacturing | Solar manufacturing |
| ev_manufacturing | EV manufacturing |
| dominant_anchor | Anchor metric with the maximum scaled contribution |
| positive | Mean of scaled positive cluster indicators |
| negative | Mean of scaled reversed negative indicators |
| cluster_top | State label for high-performing clusters (cluster_index > 0.5) |
| cluster_index | Cluster sub-index (0-1) |

## outputs/Electro-Industrial_Index_Tables.xlsx

Contains one sheet per sub-index and a combined Electro-Industrial sheet with the same columns as the CSV outputs above.
