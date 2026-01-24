library(targets)
source("R/utils_helpers.R")

options(tidyverse.quiet = TRUE)

# Function dependencies live in R/; load via package-style
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

tar_option_set(
  packages = c(
    "config", "dplyr", "fs", "glue", "here", "janitor", "jsonlite",
    "purrr", "readr", "rlang", "stringr", "targets", "tidyr", "tibble",
    "yaml", "openxlsx"
  )
)

list(
  tar_target(config, load_config()),
  tar_target(paths, resolve_paths(config)),
  tar_target(inputs, if (paths$use_sample_data) load_sample_inputs() else load_inputs(paths)),
  tar_target(policy_intent, build_policy_intent_index(dplyr::select(inputs, state, abbr, incentives_gdp, spot_score, dsire_policy_count, dev_policy_count, legislation_index))),
  tar_target(regulatory_ease, build_regulatory_ease_index(dplyr::select(inputs, state, abbr, cpcn, regdata_index, ordinance, sepa))),
  tar_target(economic_caps, build_economic_capabilities_index(dplyr::select(inputs, state, abbr, employment_lq, gdp_growth_index, feasibility_index, economic_dynamism))),
  tar_target(infrastructure, build_infrastructure_index(dplyr::select(inputs, state, abbr, renewable_potential, ev_stations_cap, interconnection_queue, electricity_price, cnbc_rank))),
  tar_target(deployment, build_deployment_index(dplyr::select(inputs, state, abbr, clean_tech_investment, datacenter_index, electric_capacity_growth, semiconductor_investment, evs_per_capita))),
  tar_target(cluster, build_cluster_index(dplyr::select(inputs, state, abbr, workforce_share, workforce_growth, industry_feasibility, clean_electric_capacity_growth, industrial_electricity_price, datacenter_mw, semiconductor_manufacturing, battery_manufacturing, solar_manufacturing, ev_manufacturing)) %>% dplyr::mutate(state = state)),
  tar_target(electrotech, build_electrotech_index(deployment, infrastructure, economic_caps, policy_intent, regulatory_ease, cluster)),
  tar_target(outputs, {
    export_outputs(
      outputs = list(
        policy = policy_intent,
        regulatory = regulatory_ease,
        economic = economic_caps,
        infrastructure = infrastructure,
        deployment = deployment,
        cluster = cluster,
        electrotech = electrotech
      ),
      paths = paths
    )
  })
)
