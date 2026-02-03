paths <- getOption("electrotech.paths")
config <- getOption("electrotech.config")

if (is.null(paths) || is.null(config)) {
  rlang::abort("Configuration not loaded. Run scripts/00_setup.R first.")
}

raw_dir <- paths$raw_dir
states <- tibble::tibble(state = state.name, abbr = state.abb)

read_optional_csv <- function(path) {
  if (fs::file_exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    NULL
  }
}

load_state_gdp <- function(raw_dir) {
  zip_path <- fs::path(raw_dir, "remote", "SAGDP.zip")
  if (!fs::file_exists(zip_path)) {
    return(tibble::tibble())
  }
  temp_dir <- tempfile()
  fs::dir_create(temp_dir)
  utils::unzip(zip_path, exdir = temp_dir)
  gdp_path <- list.files(temp_dir, full.names = TRUE)
  gdp_path <- gdp_path[stringr::str_detect(basename(gdp_path), "SAGDP9__ALL_AREAS_1997_2024.csv")][1]
  if (is.na(gdp_path)) {
    return(tibble::tibble())
  }
  gdp <- readr::read_csv(
    gdp_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  ) %>%
    janitor::clean_names()
  gdp %>%
    dplyr::filter(.data$description == "All industry total ") %>%
    dplyr::select(
      GeoFIPS = "geo_fips",
      GeoName = "geo_name",
      Unit = "unit",
      Description = "description",
      gdp_2024 = "x2024"
    ) %>%
    dplyr::mutate(gdp_2024 = readr::parse_number(gdp_2024))
}

base_inputs <- if (isTRUE(paths$use_sample_data)) {
  load_sample_inputs(paths)
} else {
  input_path <- fs::path(paths$data_dir, "inputs.csv")
  if (fs::file_exists(input_path)) {
    load_inputs(paths)
  } else {
    states
  }
}

processed_inputs <- states %>%
  dplyr::left_join(base_inputs, by = c("state", "abbr"))

state_gdp <- load_state_gdp(raw_dir)

gjf_path <- fs::path(raw_dir, "Good Jobs First", "gjf_complete.csv")
gjf <- read_optional_csv(gjf_path)

incentives <- NULL
if (!is.null(gjf) && nrow(state_gdp) > 0) {
  gjf <- gjf %>% janitor::clean_names()
  gjf_electrotech <- gjf %>%
    dplyr::filter(
      major_industry_of_parent %in% c(
        "miscellaneous energy products and systems",
        "industrial equipment",
        "electrical and electronic equipment",
        "motor vehicles",
        "information technology",
        "utilities and power generation",
        "automotive parts",
        "telecommunications"
      ),
      year > 2020,
      !stringr::str_detect(specific_industry_of_parent, "media"),
      specific_industry_of_parent != "computers"
    )

  electrotech_state_tot <- gjf_electrotech %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(subs_m = sum(subs_m, na.rm = TRUE), .groups = "drop")

  incentives <- electrotech_state_tot %>%
    dplyr::left_join(state_gdp, by = c("location" = "GeoName")) %>%
    dplyr::mutate(incentives_gdp = ifelse(is.na(gdp_2024), NA_real_, subs_m / gdp_2024 * 100)) %>%
    dplyr::transmute(state = location, incentives_gdp)
}

dev_pol_path <- fs::path(raw_dir, "dbo_Program.csv")
dev_pol <- read_optional_csv(dev_pol_path)

dev_policy <- NULL
if (!is.null(dev_pol)) {
  dev_pol <- dev_pol %>% janitor::clean_names()
  keywords <- c(
    "renewable", "solar", "wind",
    "electric", "vehicle", "battery", "batteries", "storage",
    "ev", "transportation", "manufacturing", "job",
    "jobs", "research", "r&d", "innovation",
    "datacenter", "data center", "drone", "semiconductor",
    "motor"
  )
  pattern <- paste0("(?i)\\b(", paste(keywords, collapse = "|"), ")\\b")
  climate_dev_pol <- dev_pol %>%
    dplyr::mutate(keywords = stringr::str_extract_all(program_description, pattern)) %>%
    dplyr::filter(lengths(.data$keywords) > 0)

  dev_policy <- climate_dev_pol %>%
    dplyr::filter(program_status == "Active") %>%
    dplyr::group_by(state) %>%
    dplyr::summarize(dev_policy_count = dplyr::n_distinct(program_name), .groups = "drop")
}

leg_path <- fs::path(raw_dir, "climate_leg.csv")
climate_leg <- read_optional_csv(leg_path)

legislation <- NULL
if (!is.null(climate_leg)) {
  climate_leg <- climate_leg %>% janitor::clean_names()
  leg_types <- c(
    "Renewables &amp; Energy Efficiency",
    "Green Banks",
    "Utilities and the Grid",
    "Electric Vehicles",
    "100% Clean Energy and Zero Emissions Targets",
    "Emerging Energy Technologies"
  )
  climate_leg <- climate_leg %>%
    dplyr::mutate(state = dplyr::coalesce(.data$statename, .data$state))

  legislation <- climate_leg %>%
    dplyr::filter(
      .data$bill_type_1 %in% leg_types | .data$bill_type_2 %in% leg_types,
      .data$issue_type_1 == "Energy" | .data$issue_type_2 == "Energy"
    ) %>%
    dplyr::mutate(
      status_index = dplyr::case_when(
        status_type == "679" ~ 1,
        status_type == "680" ~ 2,
        status_type == "681" ~ 3,
        status_type == "682" ~ 15,
        status_type == "683" ~ 5,
        status_type == "723" ~ 4,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::group_by(state) %>%
    dplyr::summarize(legislation_index = sum(status_index, na.rm = TRUE), .groups = "drop")
}

cpcn_path <- fs::path(raw_dir, "CPCN_Requirements_and_Enactment_Years_by_State_GPT.csv")
cpcn_raw <- read_optional_csv(cpcn_path)
cpcn <- NULL
if (!is.null(cpcn_raw)) {
  cpcn_raw <- cpcn_raw %>% janitor::clean_names()
  cpcn <- cpcn_raw %>%
    dplyr::transmute(
      state = .data$state,
      cpcn = dplyr::if_else(cpcn_required_for_utility_scale_projects == "No", 0.75, 0.25)
    )
}

regdata_path <- fs::path(raw_dir, "Regdata_subnational.csv")
regdata_raw <- read_optional_csv(regdata_path)
regdata <- NULL
if (!is.null(regdata_raw)) {
  regdata_raw <- regdata_raw %>% janitor::clean_names()
  regdata <- regdata_raw %>%
    dplyr::filter(country == "United States") %>%
    dplyr::mutate(
      restrictions_num = readr::parse_number(restrictions),
      words_num = readr::parse_number(words),
      restrictions_scaled = scale_minmax(restrictions_num),
      words_scaled = scale_minmax(words_num),
      regdata_index = rowMeans(dplyr::select(., restrictions_scaled, words_scaled), na.rm = TRUE)
    ) %>%
    dplyr::transmute(state = jurisdiction, regdata_index)
}

ordinance_path <- fs::path(raw_dir, "Solar Ordinances.csv")
ordinance_raw <- read_optional_csv(ordinance_path)
ordinance <- NULL
if (!is.null(ordinance_raw)) {
  ordinance_raw <- ordinance_raw %>% janitor::clean_names()
  ordinance <- ordinance_raw %>%
    dplyr::filter(state != "") %>%
    dplyr::group_by(state) %>%
    dplyr::summarize(ordinance = dplyr::n(), .groups = "drop")
}

sepa_path <- fs::path(raw_dir, "state_sepa.csv")
sepa_raw <- read_optional_csv(sepa_path)
sepa <- NULL
if (!is.null(sepa_raw)) {
  sepa_raw <- sepa_raw %>% janitor::clean_names()
  sepa <- sepa_raw %>%
    dplyr::transmute(state = .data$state, sepa = .data$has_sepa)
}

raw_updates <- states %>%
  dplyr::left_join(incentives, by = "state") %>%
  dplyr::left_join(dev_policy, by = "state") %>%
  dplyr::left_join(legislation, by = "state") %>%
  dplyr::left_join(cpcn, by = "state") %>%
  dplyr::left_join(regdata, by = "state") %>%
  dplyr::left_join(ordinance, by = "state") %>%
  dplyr::left_join(sepa, by = "state")

processed_inputs <- processed_inputs %>%
  dplyr::left_join(raw_updates, by = "state", suffix = c("", ".raw")) %>%
  dplyr::mutate(
    incentives_gdp = dplyr::coalesce(`incentives_gdp.raw`, incentives_gdp),
    dev_policy_count = dplyr::coalesce(`dev_policy_count.raw`, dev_policy_count),
    legislation_index = dplyr::coalesce(`legislation_index.raw`, legislation_index),
    cpcn = dplyr::coalesce(`cpcn.raw`, cpcn),
    regdata_index = dplyr::coalesce(`regdata_index.raw`, regdata_index),
    ordinance = dplyr::coalesce(`ordinance.raw`, ordinance),
    sepa = dplyr::coalesce(`sepa.raw`, sepa)
  ) %>%
  dplyr::select(-dplyr::ends_with(".raw"))

required_cols <- required_input_columns()
missing_cols <- setdiff(required_cols, names(processed_inputs))
if (length(missing_cols) > 0) {
  for (col in missing_cols) {
    processed_inputs[[col]] <- if (col %in% c("state", "abbr")) NA_character_ else NA_real_
  }
}

validated_inputs <- validate_inputs_schema(processed_inputs, required_cols)

processed_path <- fs::path(paths$processed_dir, "inputs_processed.csv")
readr::write_csv(validated_inputs, processed_path)
