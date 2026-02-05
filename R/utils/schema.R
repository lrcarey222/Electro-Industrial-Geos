#' Validate input schema
#'
#' @param inputs Input data frame.
#' @param required_cols Character vector of required columns.
#' @return Input data frame.
#' @export
validate_inputs_schema <- function(inputs, required_cols) {
  missing_cols <- setdiff(required_cols, names(inputs))
  if (length(missing_cols) > 0) {
    rlang::abort(
      glue::glue(
        "Inputs are missing required columns: {paste(missing_cols, collapse = ', ')}. ",
        "See data/README.md for schema."
      )
    )
  }

  if (!is.character(inputs$state)) {
    rlang::abort("Column 'state' must be character. See data/README.md.")
  }
  if (!is.character(inputs$abbr)) {
    rlang::abort("Column 'abbr' must be character. See data/README.md.")
  }

  numeric_cols <- setdiff(required_cols, c("state", "abbr"))
  non_numeric <- numeric_cols[!vapply(inputs[numeric_cols], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    rlang::abort(
      glue::glue(
        "Inputs have non-numeric columns where numeric is expected: {paste(non_numeric, collapse = ', ')}."
      )
    )
  }

  inputs
}

#' Required input columns for Electro-Industrial index
#'
#' @return Character vector of required columns.
#' @export
required_input_columns <- function() {
  c(
    "state", "abbr",
    "incentives_gdp", "spot_score", "dsire_policy_count", "dev_policy_count", "legislation_index",
    "cpcn", "regdata_index", "ordinance", "sepa",
    "employment_lq", "gdp_growth_index", "feasibility_index", "economic_dynamism",
    "renewable_potential", "ev_stations_cap", "interconnection_queue", "electricity_price", "cnbc_rank",
    "clean_tech_investment", "datacenter_index", "electric_capacity_growth", "semiconductor_investment", "evs_per_capita",
    "workforce_share", "workforce_growth", "industry_feasibility", "clean_electric_capacity_growth",
    "industrial_electricity_price", "datacenter_mw", "semiconductor_manufacturing",
    "battery_manufacturing", "solar_manufacturing", "ev_manufacturing"
  )
}
