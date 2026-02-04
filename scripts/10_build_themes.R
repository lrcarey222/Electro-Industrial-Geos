if (exists("validated_inputs", inherits = TRUE)) {
  processed_inputs <- validated_inputs
} else if (!exists("processed_inputs", inherits = TRUE)) {
  paths <- getOption("Electro-Industrial.paths")
  if (is.null(paths)) {
    rlang::abort("processed_inputs not found and paths are not configured. Run scripts/00_setup.R first.")
  }
  processed_path <- fs::path(paths$processed_dir, "inputs_processed.csv")
  if (!fs::file_exists(processed_path)) {
    rlang::abort("processed_inputs not found. Ensure scripts/07_process_data.R ran successfully.")
  }
  processed_inputs <- readr::read_csv(processed_path, show_col_types = FALSE)
}

policy_intent <- build_policy_intent_index(
  dplyr::select(
    processed_inputs,
    state, abbr, incentives_gdp, spot_score, dsire_policy_count, dev_policy_count, legislation_index
  )
)

regulatory_ease <- build_regulatory_ease_index(
  dplyr::select(processed_inputs, state, abbr, cpcn, regdata_index, ordinance, sepa)
)

economic_caps <- build_economic_capabilities_index(
  dplyr::select(processed_inputs, state, abbr, employment_lq, gdp_growth_index, feasibility_index, economic_dynamism)
)

infrastructure <- build_infrastructure_index(
  dplyr::select(processed_inputs, state, abbr, renewable_potential, ev_stations_cap, interconnection_queue, electricity_price, cnbc_rank)
)

deployment <- build_deployment_index(
  dplyr::select(processed_inputs, state, abbr, clean_tech_investment, datacenter_index, electric_capacity_growth, semiconductor_investment, evs_per_capita)
)

cluster <- build_cluster_index(
  dplyr::select(
    processed_inputs,
    state, abbr, workforce_share, workforce_growth, industry_feasibility,
    clean_electric_capacity_growth, industrial_electricity_price, datacenter_mw,
    semiconductor_manufacturing, battery_manufacturing, solar_manufacturing, ev_manufacturing
  )
) %>%
  dplyr::mutate(state = state)
