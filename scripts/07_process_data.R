paths <- getOption("Electro_Industrial.paths")
config <- getOption("Electro_Industrial.config")

if (is.null(paths) || is.null(config)) {
  rlang::abort("Configuration not loaded. Run scripts/00_setup.R first.")
}

raw_dir <- paths$raw_dir
states <- tibble::tibble(state = state.name, abbr = state.abb)

# ---- Helpers -------------------------------------------------------------
read_optional_csv <- function(path) {
  if (fs::file_exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    NULL
  }
}

read_optional_csv_skip <- function(path, skip = 0) {
  if (fs::file_exists(path)) {
    readr::read_csv(path, skip = skip, show_col_types = FALSE)
  } else {
    NULL
  }
}

read_optional_xlsx <- function(path, sheet = 1, start_row = 1) {
  if (!fs::file_exists(path)) {
    return(NULL)
  }
  tryCatch(
    openxlsx::read.xlsx(path, sheet = sheet, startRow = start_row),
    error = function(e) NULL
  )
}

latest_quarter_filter <- function(data) {
  if (!"quarter" %in% names(data)) {
    return(data)
  }
  quarter_info <- stringr::str_match(data$quarter, "^(\\d{4})-Q(\\d)$")
  quarter_rank <- as.integer(quarter_info[, 2]) * 10 + as.integer(quarter_info[, 3])
  data %>%
    dplyr::mutate(quarter_rank = quarter_rank) %>%
    dplyr::filter(.data$quarter_rank == max(.data$quarter_rank, na.rm = TRUE)) %>%
    dplyr::select(-.data$quarter_rank)
}

ensure_optional_numeric <- function(data, column) {
  if (!is.null(data)) {
    return(data)
  }
  tibble::tibble(
    state = character(),
    !!rlang::sym(column) := numeric()
  )
}

safe_left_join <- function(x, y, by) {
  if (is.null(y)) {
    return(x)
  }
  dplyr::left_join(x, y, by = by)
}

# ---- GDP + Growth Index --------------------------------------------------
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
  gdp <- utils::read.csv(
    gdp_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
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

load_quarterly_gdp_growth <- function(raw_dir, states) {
  zip_path <- fs::path(raw_dir, "remote", "SQGDP.zip")
  if (!fs::file_exists(zip_path)) {
    return(tibble::tibble())
  }
  temp_dir <- tempfile()
  fs::dir_create(temp_dir)
  utils::unzip(zip_path, exdir = temp_dir)
  gdp_path <- list.files(temp_dir, full.names = TRUE)
  gdp_path <- gdp_path[stringr::str_detect(basename(gdp_path), "SQGDP9__ALL_AREAS_2005_2025.csv")][1]
  if (is.na(gdp_path)) {
    return(tibble::tibble())
  }

  gdp_raw <- utils::read.csv(gdp_path, stringsAsFactors = FALSE, check.names = FALSE) %>%
    janitor::clean_names()

  quarter_cols <- names(gdp_raw)[stringr::str_detect(names(gdp_raw), "^x\\d{4}_q[1-4]$")]
  if (length(quarter_cols) == 0) {
    return(tibble::tibble())
  }

  gdp_raw <- gdp_raw %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(quarter_cols), ~ readr::parse_number(as.character(.x))))

  quarter_info <- stringr::str_match(quarter_cols, "^x(\\d{4})_q([1-4])$")
  years <- as.integer(quarter_info[, 2])
  quarters <- as.integer(quarter_info[, 3])
  latest_idx <- order(years, quarters, decreasing = TRUE)[1]
  latest_col <- quarter_cols[latest_idx]
  latest_year <- years[latest_idx]
  latest_quarter <- quarters[latest_idx]

  prior_col <- function(offset) {
    target <- paste0("x", latest_year - offset, "_q", latest_quarter)
    if (target %in% quarter_cols) target else NA_character_
  }

  col_1yr <- prior_col(1)
  col_5yr <- prior_col(5)
  col_10yr <- prior_col(10)

  gdp_ind <- gdp_raw %>%
    dplyr::filter(.data$industry_classification == "321,327-339") %>%
    dplyr::mutate(
      gdp_growth_1yr = if (is.na(col_1yr)) NA_real_ else .data[[latest_col]] / .data[[col_1yr]] - 1,
      gdp_growth_5yr = if (is.na(col_5yr)) NA_real_ else .data[[latest_col]] / .data[[col_5yr]] - 1,
      gdp_growth_10yr = if (is.na(col_10yr)) NA_real_ else .data[[latest_col]] / .data[[col_10yr]] - 1
    ) %>%
    dplyr::select(geo_name, gdp_growth_1yr, gdp_growth_5yr, gdp_growth_10yr) %>%
    dplyr::inner_join(states, by = c("geo_name" = "state"))

  growth_cols <- c("gdp_growth_1yr", "gdp_growth_5yr", "gdp_growth_10yr")

  gdp_ind %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(growth_cols), scale_minmax)) %>%
    dplyr::mutate(gdp_growth_index = rowmean_index(dplyr::select(., dplyr::all_of(growth_cols)))) %>%
    dplyr::transmute(state = .data$geo_name, gdp_growth_index)
}

# ---- Base Inputs ---------------------------------------------------------
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

# ---- Incentives (Good Jobs First) ----------------------------------------
gjf_path <- fs::path(raw_dir, "Good Jobs First", "gjf_complete.csv")
gjf <- read_optional_csv(gjf_path)

incentives <- NULL
if (!is.null(gjf) && nrow(state_gdp) > 0) {
  gjf <- gjf %>% janitor::clean_names()
  gjf_Electro_Industrial <- gjf %>%
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

  Electro_Industrial_state_tot <- gjf_Electro_Industrial %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(subs_m = sum(subs_m, na.rm = TRUE), .groups = "drop")

  incentives <- Electro_Industrial_state_tot %>%
    dplyr::left_join(state_gdp, by = c("location" = "GeoName")) %>%
    dplyr::mutate(incentives_gdp = ifelse(is.na(gdp_2024), NA_real_, subs_m / gdp_2024 * 100)) %>%
    dplyr::transmute(state = location, incentives_gdp)
}
incentives <- ensure_optional_numeric(incentives, "incentives_gdp")

# ---- Development Policy Programs ----------------------------------------
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
dev_policy <- ensure_optional_numeric(dev_policy, "dev_policy_count")

# ---- Climate Legislation -------------------------------------------------
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
    dplyr::mutate(state = dplyr::coalesce(.data$statename, .data$statename))

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
legislation <- ensure_optional_numeric(legislation, "legislation_index")

# ---- CPCN Requirements ---------------------------------------------------
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
cpcn <- ensure_optional_numeric(cpcn, "cpcn")

# ---- RegData Index -------------------------------------------------------
regdata_path <- fs::path(raw_dir, "Regdata_subnational.csv")
regdata_raw <- read_optional_csv(regdata_path)
regdata <- NULL
if (!is.null(regdata_raw)) {
  regdata_raw <- regdata_raw %>% janitor::clean_names()
  regdata <- regdata_raw %>%
    dplyr::filter(country == "United States") %>%
    dplyr::mutate(
      restrictions_num = readr::parse_number(as.character(restrictions)),
      words_num = readr::parse_number(as.character(words)),
      restrictions_scaled = scale_minmax(restrictions_num),
      words_scaled = scale_minmax(words_num),
      regdata_index = rowMeans(dplyr::pick(restrictions_scaled, words_scaled), na.rm = TRUE)
    ) %>%
    dplyr::transmute(state = jurisdiction, regdata_index)
}
regdata <- ensure_optional_numeric(regdata, "regdata_index")

# ---- Solar Ordinances ----------------------------------------------------
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
ordinance <- ensure_optional_numeric(ordinance, "ordinance")

# ---- SEPA Presence -------------------------------------------------------
sepa_path <- fs::path(raw_dir, "state_sepa.csv")
sepa_raw <- read_optional_csv(sepa_path)
sepa <- NULL
if (!is.null(sepa_raw)) {
  sepa_raw <- sepa_raw %>% janitor::clean_names()
  sepa <- sepa_raw %>%
    dplyr::transmute(state = .data$state, sepa = .data$has_sepa)
}
sepa <- ensure_optional_numeric(sepa, "sepa")

# ---- Clean Investment Monitor -------------------------------------------
socioecon_path <- fs::path(raw_dir, "clean_investment_monitor_q2_2025", "socioeconomics.csv")
socioecon_raw <- read_optional_csv_skip(socioecon_path, skip = 5)
socioecon <- NULL
if (!is.null(socioecon_raw)) {
  socioecon <- socioecon_raw %>%
    janitor::clean_names() %>%
    latest_quarter_filter()
}

investment_path <- fs::path(raw_dir, "clean_investment_monitor_q2_2025", "quarterly_actual_investment.csv")
investment_raw <- read_optional_csv_skip(investment_path, skip = 5)
clean_investment <- NULL
if (!is.null(investment_raw) && !is.null(socioecon)) {
  investment_raw <- investment_raw %>% janitor::clean_names()
  investment_state <- investment_raw %>%
    dplyr::filter(.data$technology %in% c("Batteries", "Solar", "Nuclear", "Zero Emission Vehicles")) %>%
    dplyr::mutate(
      tech = dplyr::if_else(.data$segment == "Manufacturing", paste(.data$technology, .data$segment), .data$technology)
    ) %>%
    dplyr::group_by(.data$state, .data$tech) %>%
    dplyr::summarize(inv = sum(.data$estimated_actual_quarterly_expenditure, na.rm = TRUE), .groups = "drop") %>%
    dplyr::left_join(socioecon %>% dplyr::select(state, state_name, real_gdp), by = c("state" = "state")) %>%
    dplyr::mutate(inv_gdp = .data$inv / .data$real_gdp) %>%
    dplyr::select(state, tech, inv_gdp) %>%
    tidyr::pivot_wider(names_from = .data$tech, values_from = .data$inv_gdp)

  clean_investment <- investment_state %>%
    dplyr::mutate(dplyr::across(where(is.numeric), scale_minmax)) %>%
    dplyr::mutate(clean_tech_investment = rowmean_index(dplyr::select(., where(is.numeric)))) %>%
    dplyr::left_join(states, by = c("state" = "abbr")) %>%
    dplyr::transmute(state = .data$state.y, clean_tech_investment)
}
clean_investment <- ensure_optional_numeric(clean_investment, "clean_tech_investment")

# ---- Economic Dynamism ---------------------------------------------------
eig_path <- fs::path(raw_dir, "remote", "Downloadable-Data-EIG-Index-of-State-Dynamism-2022.xlsx")
eig_raw <- read_optional_xlsx(eig_path, sheet = 1, start_row = 1)
economic_dynamism <- NULL
if (!is.null(eig_raw)) {
  eig_raw <- eig_raw %>% janitor::clean_names()
  if ("year" %in% names(eig_raw)) {
    eig_raw <- eig_raw %>% dplyr::filter(.data$year == max(.data$year, na.rm = TRUE))
  }
  economic_dynamism <- eig_raw %>%
    dplyr::select(state_abbreviation, combined_score) %>%
    dplyr::left_join(states, by = c("state_abbreviation" = "abbr")) %>%
    dplyr::transmute(state = .data$state, economic_dynamism = .data$combined_score)
}
economic_dynamism <- ensure_optional_numeric(economic_dynamism, "economic_dynamism")

# ---- GDP Growth Index ----------------------------------------------------
gdp_growth_index <- load_quarterly_gdp_growth(raw_dir, states)
gdp_growth_index <- ensure_optional_numeric(gdp_growth_index, "gdp_growth_index")

# ---- EV Stations ---------------------------------------------------------
ev_station_path <- fs::path(raw_dir, "remote", "historical-station-counts.xlsx")
ev_station_raw <- read_optional_xlsx(ev_station_path, sheet = 2, start_row = 3)
ev_station <- NULL
if (!is.null(ev_station_raw) && !is.null(socioecon)) {
  ev_station_raw <- ev_station_raw %>% janitor::clean_names()
  state_col <- if ("state" %in% names(ev_station_raw)) "state" else names(ev_station_raw)[1]
  numeric_cols <- names(dplyr::select(ev_station_raw, where(is.numeric)))
  count_candidates <- intersect(c("total_chargers", "total_charging", "total_evse", "total_stations"), names(ev_station_raw))
  count_col <- if (length(count_candidates) > 0) count_candidates[1] else if (length(numeric_cols) > 0) tail(numeric_cols, 1) else NA_character_

  if (!is.na(count_col)) {
    ev_station <- ev_station_raw %>%
      dplyr::transmute(
        state_raw = .data[[state_col]],
        total_chargers = .data[[count_col]]
      ) %>%
      dplyr::mutate(
        state_raw = stringr::str_trim(as.character(.data$state_raw)),
        state = dplyr::case_when(
          nchar(.data$state_raw) == 2 ~ states$state[match(.data$state_raw, states$abbr)],
          TRUE ~ .data$state_raw
        )
      ) %>%
      dplyr::left_join(socioecon %>% dplyr::select(state_name, population), by = c("state" = "state_name")) %>%
      dplyr::mutate(ev_stations_cap = .data$total_chargers / .data$population) %>%
      dplyr::transmute(state = .data$state, ev_stations_cap)
  }
}
ev_station <- ensure_optional_numeric(ev_station, "ev_stations_cap")

# ---- Interconnection Queue ----------------------------------------------
queue_path <- fs::path(raw_dir, "remote", "LBNL_Ix_Queue_Data_File_thru2024_v2.xlsx")
queue_raw <- read_optional_xlsx(queue_path, sheet = 6, start_row = 2)
interconnection_queue <- NULL
if (!is.null(queue_raw)) {
  queue_raw <- queue_raw %>% janitor::clean_names()
  mw_col <- intersect(c("mw1", "mw"), names(queue_raw))[1]
  if (!is.na(mw_col)) {
    queue <- queue_raw %>%
      dplyr::filter(.data$q_status != "withdrawn") %>%
      dplyr::group_by(.data$state, .data$q_status) %>%
      dplyr::summarize(mw = sum(.data[[mw_col]], na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = .data$q_status, values_from = mw) %>%
      dplyr::mutate(interconnection_queue = .data$active / .data$operational) %>%
      dplyr::left_join(states, by = c("state" = "abbr")) %>%
      dplyr::transmute(state = .data$state.y, interconnection_queue)
    interconnection_queue <- queue
  }
}
interconnection_queue <- ensure_optional_numeric(interconnection_queue, "interconnection_queue")

# ---- Electricity Price ---------------------------------------------------
eia_path <- fs::path(raw_dir, "remote", "sales_revenue.xlsx")
eia_raw <- read_optional_xlsx(eia_path, sheet = 1, start_row = 3)
electricity_price <- NULL
if (!is.null(eia_raw)) {
  eia_raw <- eia_raw %>% janitor::clean_names()
  price_col <- intersect(names(eia_raw), names(eia_raw)[stringr::str_detect(names(eia_raw), "cents.*k_wh")])[1]
  if (!is.na(price_col)) {
    ind_price_m <- eia_raw %>%
      dplyr::mutate(ind_price_m = .data[[price_col]]) %>%
      dplyr::select(state, year, month, ind_price_m)

    ind_price <- ind_price_m %>%
      dplyr::group_by(.data$state, .data$year) %>%
      dplyr::summarize(ind_price = mean(.data$ind_price_m, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(.data$year %in% c(2014, 2019, 2024)) %>%
      tidyr::pivot_wider(names_from = .data$year, values_from = .data$ind_price) %>%
      dplyr::mutate(
        ind_price_10yr = (`2024` / `2014` - 1) * 100,
        ind_price_5yr = (`2024` / `2019` - 1) * 100,
        ind_price_cents_kwh = .data$`2024`
      ) %>%
      dplyr::select(.data$state, ind_price_10yr, ind_price_5yr, ind_price_cents_kwh) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), scale_minmax)) %>%
      dplyr::mutate(price_index = 1 - rowmean_index(dplyr::select(., where(is.numeric)))) %>%
      dplyr::left_join(states, by = c("state" = "abbr")) %>%
      dplyr::transmute(state = .data$state.y, electricity_price = .data$price_index)

    electricity_price <- ind_price
  }
}
electricity_price <- ensure_optional_numeric(electricity_price, "electricity_price")

# ---- Electricity Capacity -----------------------------------------------
generator_dir <- fs::path(raw_dir, "remote")
fs::dir_create(generator_dir, recurse = TRUE)
generator_files <- fs::dir_ls(
  generator_dir,
  regexp = "^[A-Za-z]+_generator\\d{4}\\.xlsx$",
  type = "file",
  fail = FALSE
)
generator_months <- setNames(tolower(month.name), tolower(month.name))
parse_generator_date <- function(path) {
  filename <- basename(path)
  match_info <- stringr::str_match(filename, "^([A-Za-z]+)_generator(\\d{4})\\.xlsx$")
  if (is.na(match_info[1, 1])) {
    return(NA_real_)
  }
  month_key <- tolower(match_info[1, 2])
  if (!month_key %in% names(generator_months)) {
    return(NA_real_)
  }
  month_num <- base::match(month_key, generator_months)
  as.numeric(sprintf("%04d%02d", as.integer(match_info[1, 3]), month_num))
}

make_generator_source <- function(date) {
  filename <- paste0(
    tolower(format(date, "%B")),
    "_generator",
    format(date, "%Y"),
    ".xlsx"
  )
  list(
    filename = filename,
    url = paste0("https://www.eia.gov/electricity/data/eia860m/xls/", filename),
    path = fs::path(generator_dir, filename)
  )
}

is_valid_xlsx <- function(path) {
  if (!fs::file_exists(path) || fs::file_size(path) <= 0) {
    return(FALSE)
  }
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = "raw", n = 2)
  identical(sig, charToRaw("PK"))
}

generator_reference_date <- Sys.Date()
candidate_start <- seq(generator_reference_date, length.out = 2, by = "-1 month")[2]

op_gen_raw <- NULL
elec_fac <- NULL
datacenter_fac <- NULL
semi_fac <- NULL
drones_fac <- NULL
generator_dates <- vapply(generator_files, parse_generator_date, numeric(1))
latest_generator <- if (length(generator_dates) > 0 && any(!is.na(generator_dates))) {
  generator_files[which.max(generator_dates)]
} else {
  NA_character_
}

if (!is.na(latest_generator)) {
  if (is_valid_xlsx(latest_generator)) {
    op_gen_raw <- suppressWarnings(read_optional_xlsx(latest_generator, sheet = 1, start_row = 3))
  }
}

if (is.null(op_gen_raw)) {
  for (months_back in 0:1) {
    candidate_date <- seq(candidate_start, length.out = months_back + 1, by = "-1 month")[months_back + 1]
    candidate_src <- make_generator_source(candidate_date)

    if (!fs::file_exists(candidate_src$path)) {
      candidate_cached <- tryCatch(
        download_with_cache(
          url = candidate_src$url,
          dest_dir = paths$cache_dir,
          snapshot_date = paths$snapshot_date,
          filename = candidate_src$filename
        ),
        error = function(e) NULL
      )
      if (!is.null(candidate_cached) && is_valid_xlsx(candidate_cached)) {
        fs::file_copy(candidate_cached, candidate_src$path, overwrite = TRUE)
      }
    }

    if (is_valid_xlsx(candidate_src$path)) {
      candidate_raw <- suppressWarnings(read_optional_xlsx(candidate_src$path, sheet = 1, start_row = 3))
      if (!is.null(candidate_raw)) {
        op_gen_raw <- candidate_raw
        break
      }
    }
  }
}

states_gen <- tibble::tibble(State = character(), nameplate_capacity_mw = numeric())
electric_capacity_growth <- NULL
clean_electric_capacity_growth <- NULL
if (!is.null(op_gen_raw)) {
  op_gen_raw <- op_gen_raw %>% janitor::clean_names()
  required_cols <- c("plant_state", "status", "operating_year", "technology", "nameplate_capacity_mw")
  if (all(required_cols %in% names(op_gen_raw))) {
    census_divisions <- tibble::tibble(
      State = state.name,
      State.Code = state.abb,
      Division = as.character(state.division)
    )

    op_gen <- op_gen_raw %>%
      dplyr::mutate(
        tech = dplyr::case_when(
          .data$technology == "Natural Gas Steam Turbine" ~ "Natural Gas",
          .data$technology == "Natural Gas Fired Combined Cycle" ~ "Natural Gas",
          .data$technology == "Natural Gas Internal Combustion Engine" ~ "Natural Gas",
          .data$technology == "Natural Gas Fired Combustion Turbine" ~ "Natural Gas",
          .data$technology == "Conventional Steam Coal" ~ "Coal",
          .data$technology == "Conventional Hydroelectric" ~ "Hydro",
          .data$technology == "Onshore Wind Turbine" ~ "Wind",
          .data$technology == "Offshore Wind Turbine" ~ "Wind",
          .data$technology == "Batteries" ~ "Storage",
          .data$technology == "Solar Photovoltaic" ~ "Solar",
          .data$technology == "Solar Thermal with Energy Storage" ~ "Solar",
          .data$technology == "Hydroelectric Pumped Storage" ~ "Hydro",
          .data$technology == "Geothermal" ~ "Geothermal",
          .data$technology == "Wood/Wood Waste Biomass" ~ "Biomass",
          TRUE ~ NA_character_
        )
      )

    elec_fac <- op_gen %>%
      dplyr::filter(
        .data$technology %in% c("Batteries", "Solar Photovoltaic"),
        .data$operating_year > 2021,
        .data$status == "(OP) Operating"
      ) %>%
      dplyr::transmute(
        name = .data$entity_name,
        size = .data$nameplate_capacity_mw,
        cat = dplyr::if_else(.data$technology == "Batteries", "Electricity Storage", "Solar Generation"),
        tech = .data$technology,
        Latitude = .data$latitude,
        Longitude = .data$longitude,
        state_abbr = .data$plant_state,
        unit = "MW"
      )

    states_gen <- op_gen %>%
      dplyr::filter(.data$status == "(OP) Operating") %>%
      dplyr::group_by(.data$plant_state) %>%
      dplyr::summarize(nameplate_capacity_mw = sum(.data$nameplate_capacity_mw, na.rm = TRUE), .groups = "drop") %>%
      dplyr::left_join(census_divisions, by = c("plant_state" = "State.Code"))

    states_rengen <- op_gen %>%
      dplyr::filter(.data$status == "(OP) Operating") %>%
      dplyr::group_by(.data$plant_state, .data$operating_year, .data$technology) %>%
      dplyr::summarize(nameplate_capacity_mw = sum(.data$nameplate_capacity_mw, na.rm = TRUE), .groups = "drop") %>%
      dplyr::left_join(census_divisions, by = c("plant_state" = "State.Code")) %>%
      dplyr::filter(.data$technology %in% c(
        "Conventional Hydroelectric",
        "Onshore Wind Turbine",
        "Batteries",
        "Nuclear",
        "Solar Photovoltaic",
        "Solar Thermal with Energy Storage",
        "Hydroelectric Pumped Storage",
        "Geothermal",
        "Solar Thermal without Energy Storage",
        "Offshore Wind Turbine"
      )) %>%
      dplyr::group_by(.data$Division, .data$State, .data$operating_year) %>%
      dplyr::summarize(nameplate_capacity_mw = sum(.data$nameplate_capacity_mw, na.rm = TRUE), .groups = "drop") %>%
      tidyr::complete(operating_year = 2013:2025, fill = list(nameplate_capacity_mw = 0)) %>%
      dplyr::mutate(Year = lubridate::make_date(.data$operating_year)) %>%
      dplyr::group_by(.data$Division, .data$State) %>%
      dplyr::mutate(
        cum_cap = cumsum(.data$nameplate_capacity_mw),
        base_cap = dplyr::first(.data$cum_cap[.data$operating_year == 2022], default = NA_real_),
        cap_index_22 = dplyr::if_else(.data$base_cap > 0, 100 * .data$cum_cap / .data$base_cap, NA_real_),
        rengrowth_22_25 = .data$cum_cap - .data$base_cap
      ) %>%
      dplyr::select(-.data$base_cap)

    states_rengen_2025 <- states_rengen %>%
      dplyr::filter(.data$operating_year == 2025) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$State, .data$cum_cap, .data$cap_index_22, .data$rengrowth_22_25) %>%
      dplyr::left_join(
        states_gen %>% dplyr::select(.data$State, .data$nameplate_capacity_mw),
        by = "State"
      ) %>%
      dplyr::mutate(ren_share = .data$cum_cap / .data$nameplate_capacity_mw * 100)

    states_rengen_index <- states_rengen_2025 %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$State, .data$cum_cap, .data$cap_index_22, .data$rengrowth_22_25, .data$ren_share) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), scale_minmax)) %>%
      dplyr::mutate(capacity_index = rowmean_index(dplyr::select(., where(is.numeric))))

    electric_capacity_growth <- states_rengen_index %>%
      dplyr::transmute(state = .data$State, electric_capacity_growth = .data$capacity_index)

    clean_electric_capacity_growth <- states_rengen_2025 %>%
      dplyr::transmute(state = .data$State, clean_electric_capacity_growth = .data$rengrowth_22_25)
  }
}
electric_capacity_growth <- ensure_optional_numeric(electric_capacity_growth, "electric_capacity_growth")
clean_electric_capacity_growth <- ensure_optional_numeric(clean_electric_capacity_growth, "clean_electric_capacity_growth")

# ---- CNBC Rankings -------------------------------------------------------
cnbc_path <- fs::path(raw_dir, "cnbc_bus_rankings.csv")
cnbc_raw <- read_optional_csv(cnbc_path)
cnbc_rank <- NULL
if (!is.null(cnbc_raw)) {
  cnbc_raw <- cnbc_raw %>% janitor::clean_names()
  infra_col <- intersect(c("infra_structure", "infrastructure_rank", "INFRA"), names(cnbc_raw))[1]
  state_col <- intersect(c("state", "state_name"), names(cnbc_raw))[1]
  if (!is.na(infra_col) && !is.na(state_col)) {
    cnbc_rank <- cnbc_raw %>%
      dplyr::transmute(state = .data[[state_col]], cnbc_rank = .data[[infra_col]])
  }
}
cnbc_rank <- ensure_optional_numeric(cnbc_rank, "cnbc_rank")

# ---- Data Centers --------------------------------------------------------
datacenter_path <- fs::path(raw_dir, "BNEF", "2025-08-08 - Global Data Center Live IT Capacity Database.xlsx")
datacenter_raw <- read_optional_xlsx(datacenter_path, sheet = "Data Centers", start_row = 8)
datacenter_index <- NULL
datacenter_mw <- NULL
if (!is.null(datacenter_raw)) {
  # Keep only rows with coordinates
  datacenter_points <- datacenter_raw %>%
    dplyr::filter(!is.na(Latitude), !is.na(Longitude)) %>%
    # keep original lon/lat columns for convenience; add geometry
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

  options(tigris_use_cache = TRUE)

  states_sf <- tigris::states(cb = TRUE, year = 2023, class = "sf") %>%
    dplyr::filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) %>%
    sf::st_transform(4326) %>% # match your points CRS
    dplyr::select(STATEFP, STUSPS, STATE = NAME) # keep only useful cols

  datacenter_fac <- datacenter_points %>%
    sf::st_join(states_sf, join = sf::st_within, left = TRUE) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(Date == "2025-03-31") %>%
    dplyr::transmute(
      name = .data$Company,
      tech = paste(.data$`Facility Category`, "Datacenter"),
      cat = "Datacenter",
      size = .data$`Headline Capacity (MW)`,
      Latitude = .data$Latitude,
      Longitude = .data$Longitude,
      state_abbr = .data$STUSPS,
      unit = "MW"
    ) %>%
    dplyr::distinct()

  datacenter_states <- datacenter_points %>%
    sf::st_join(states_sf, join = sf::st_within, left = TRUE) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(Date == "2025-03-31") %>%
    dplyr::group_by(STATE) %>%
    dplyr::summarize(
      headline_mw = sum(`Headline Capacity (MW)`, na.rm = TRUE),
      construction_mw = sum(`Under Construction Capacity (MW)`, na.rm = TRUE),
      committed_mw = sum(`Committed Capacity (MW)`, na.rm = TRUE)
    ) %>%
    dplyr::left_join(states_gen, by = c("STATE" = "State")) %>%
    dplyr::mutate(datacenter_share = headline_mw / .data$nameplate_capacity_mw) %>%
    dplyr::select(STATE, headline_mw, construction_mw, committed_mw, datacenter_share)

  datacenter_index <- datacenter_states %>%
    dplyr::ungroup() %>%
    dplyr::select(STATE, committed_mw, datacenter_share) %>%
    dplyr::mutate(dplyr::across(
      where(is.numeric),
      ~ (. - min(.[!is.infinite(.)], na.rm = TRUE)) /
        (max(.[!is.infinite(.)] - min(.[!is.infinite(.)], na.rm = TRUE), na.rm = TRUE))
    )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(datacenter_index = rowMeans(dplyr::across(where(is.numeric)), na.rm = TRUE)) %>%
    dplyr::transmute(state = STATE, datacenter_index)

  datacenter_mw <- datacenter_states %>%
    dplyr::transmute(state = STATE, datacenter_mw = headline_mw)
}
datacenter_index <- ensure_optional_numeric(datacenter_index, "datacenter_index")
datacenter_mw <- ensure_optional_numeric(datacenter_mw, "datacenter_mw")

# ---- EV Registrations ----------------------------------------------------
evs_path <- fs::path(raw_dir, "remote", "10962-ev-registration-counts-by-state_9-06-24.xlsx")
evs_raw <- read_optional_xlsx(evs_path, sheet = 1, start_row = 3)
evs_per_capita <- NULL
if (!is.null(evs_raw) && !is.null(socioecon)) {
  evs_raw <- evs_raw %>% janitor::clean_names()
  state_col <- intersect(c("state", "state_name"), names(evs_raw))[1]
  reg_col <- intersect(c("registration_count", "registration_counts", "registrations"), names(evs_raw))[1]
  if (!is.na(state_col) && !is.na(reg_col)) {
    evs_per_capita <- evs_raw %>%
      dplyr::transmute(state_raw = .data[[state_col]], ev_reg = .data[[reg_col]]) %>%
      dplyr::mutate(
        state_raw = stringr::str_trim(as.character(.data$state_raw)),
        state = dplyr::case_when(
          nchar(.data$state_raw) == 2 ~ states$state[match(.data$state_raw, states$abbr)],
          TRUE ~ .data$state_raw
        )
      ) %>%
      dplyr::left_join(socioecon %>% dplyr::select(state_name, population), by = c("state" = "state_name")) %>%
      dplyr::mutate(evs_per_capita = .data$ev_reg / .data$population) %>%
      dplyr::transmute(state = .data$state, evs_per_capita)
  }
}
evs_per_capita <- ensure_optional_numeric(evs_per_capita, "evs_per_capita")

# ---- Semiconductor Investments ------------------------------------------
semiconductor_path <- fs::path(raw_dir, "semiconductor_man.csv")
semiconductor_raw <- read_optional_csv(semiconductor_path)
semiconductor_investment <- NULL
if (!is.null(semiconductor_raw) && !is.null(socioecon)) {
  semiconductor_raw <- semiconductor_raw %>% janitor::clean_names()
  project_col <- intersect(c("project_size", "project_size_text", "project_size_usd"), names(semiconductor_raw))[1]
  if (!is.na(project_col)) {
    semi_fac <- semiconductor_raw %>%
      dplyr::transmute(
        name = .data$company,
        cat = "Semiconductor Manufacturing",
        tech = paste("Semiconductor", .data$category),
        size = readr::parse_number(as.character(.data[[project_col]])) / 1000000,
        Latitude = suppressWarnings(as.numeric(.data$lat)),
        Longitude = suppressWarnings(as.numeric(.data$lon)),
        state_abbr = .data$state,
        unit = "USD"
      )
  }
  if (!is.na(project_col)) {
    semiconductor_investment <- semiconductor_raw %>%
      dplyr::mutate(project_size_usd = readr::parse_number(.data[[project_col]])) %>%
      dplyr::group_by(.data$state) %>%
      dplyr::summarize(project_size_usd = sum(.data$project_size_usd, na.rm = TRUE), .groups = "drop") %>%
      dplyr::left_join(socioecon %>% dplyr::select(state, real_gdp), by = c("state" = "state")) %>%
      dplyr::mutate(semiconductor_investment = (.data$project_size_usd / 1000000) / .data$real_gdp) %>%
      dplyr::left_join(states, by = c("state" = "abbr")) %>%
      dplyr::transmute(state = .data$state.y, semiconductor_investment)
  }
}
semiconductor_investment <- ensure_optional_numeric(semiconductor_investment, "semiconductor_investment")


# ---- Cluster Inputs (legacy cluster section wiring) ---------------------
semiconductor_manufacturing <- NULL
if (!is.null(semiconductor_raw)) {
  semiconductor_raw <- semiconductor_raw %>% janitor::clean_names()
  state_col <- intersect(c("state", "state_code"), names(semiconductor_raw))[1]
  project_col <- intersect(c("project_size", "project_size_text", "project_size_usd"), names(semiconductor_raw))[1]
  if (!is.na(state_col) && !is.na(project_col)) {
    semiconductor_manufacturing <- semiconductor_raw %>%
      dplyr::mutate(
        state_abbr = stringr::str_trim(as.character(.data[[state_col]])),
        semiconductor_manufacturing = readr::parse_number(as.character(.data[[project_col]])) / 1000000
      ) %>%
      dplyr::group_by(.data$state_abbr) %>%
      dplyr::summarize(semiconductor_manufacturing = sum(.data$semiconductor_manufacturing, na.rm = TRUE), .groups = "drop") %>%
      dplyr::left_join(states, by = c("state_abbr" = "abbr")) %>%
      dplyr::transmute(state = .data$state, semiconductor_manufacturing)
  }
}
semiconductor_manufacturing <- ensure_optional_numeric(semiconductor_manufacturing, "semiconductor_manufacturing")

cluster_manufacturing <- NULL
quarterly_path <- fs::path(raw_dir, "clean_investment_monitor_q2_2025", "quarterly_actual_investment.csv")
quarterly_actual <- read_optional_csv_skip(quarterly_path, skip = 5)
if (!is.null(quarterly_actual)) {
  quarterly_actual <- quarterly_actual %>% janitor::clean_names()
  required_quarterly <- c("segment", "state", "technology", "estimated_actual_quarterly_expenditure")
  if (all(required_quarterly %in% names(quarterly_actual))) {
    cluster_manufacturing <- quarterly_actual %>%
      dplyr::filter(
        .data$segment == "Manufacturing",
        .data$technology %in% c("Batteries", "Solar", "Zero Emission Vehicles")
      ) %>%
      dplyr::mutate(
        technology = dplyr::case_when(
          .data$technology == "Batteries" ~ "battery_manufacturing",
          .data$technology == "Solar" ~ "solar_manufacturing",
          .data$technology == "Zero Emission Vehicles" ~ "ev_manufacturing",
          TRUE ~ NA_character_
        ),
        value = readr::parse_number(as.character(.data$estimated_actual_quarterly_expenditure))
      ) %>%
      dplyr::group_by(.data$state, .data$technology) %>%
      dplyr::summarize(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = .data$technology, values_from = .data$value, values_fill = 0) %>%
      dplyr::left_join(states, by = c("state" = "abbr")) %>%
      dplyr::transmute(
        state = dplyr::coalesce(.data$state.y, .data$state.x),
        battery_manufacturing = .data$battery_manufacturing,
        solar_manufacturing = .data$solar_manufacturing,
        ev_manufacturing = .data$ev_manufacturing
      )
  }
}
if (is.null(cluster_manufacturing)) {
  cluster_manufacturing <- tibble::tibble(
    state = character(),
    battery_manufacturing = numeric(),
    solar_manufacturing = numeric(),
    ev_manufacturing = numeric()
  )
}

industrial_electricity_price <- NULL
industrial_price_path <- fs::path(raw_dir, "table_8.xlsx")
industrial_price_raw <- read_optional_xlsx(industrial_price_path, sheet = 1, start_row = 3)
if (!is.null(industrial_price_raw)) {
  industrial_price_raw <- industrial_price_raw %>% janitor::clean_names()
  state_col <- intersect(c("state", "state_name"), names(industrial_price_raw))[1]
  price_col <- intersect(c("average_price_cents_kwh", "average_price_cents_per_kwh", "average_price"), names(industrial_price_raw))[1]
  if (!is.na(state_col) && !is.na(price_col)) {
    industrial_electricity_price <- industrial_price_raw %>%
      dplyr::transmute(
        state_raw = stringr::str_trim(as.character(.data[[state_col]])),
        industrial_electricity_price = readr::parse_number(as.character(.data[[price_col]]))
      ) %>%
      dplyr::mutate(
        state = dplyr::case_when(
          nchar(.data$state_raw) == 2 ~ states$state[match(.data$state_raw, states$abbr)],
          TRUE ~ .data$state_raw
        )
      ) %>%
      dplyr::filter(!is.na(.data$state)) %>%
      dplyr::group_by(.data$state) %>%
      dplyr::summarize(industrial_electricity_price = mean(.data$industrial_electricity_price, na.rm = TRUE), .groups = "drop")
  }
}
industrial_electricity_price <- ensure_optional_numeric(industrial_electricity_price, "industrial_electricity_price")


# ---- Electrotech facilities chart wiring ---------------------------------
median_scurve <- function(x, gamma = 0.5) {
  r <- dplyr::percent_rank(x)
  (r^gamma) / (r^gamma + (1 - r)^gamma)
}

if (is.null(elec_fac)) {
  elec_fac <- tibble::tibble(name = character(), tech = character(), cat = character(), size = numeric(), Latitude = numeric(), Longitude = numeric(), state_abbr = character(), unit = character())
}
if (is.null(datacenter_fac)) {
  datacenter_fac <- tibble::tibble(name = character(), tech = character(), cat = character(), size = numeric(), Latitude = numeric(), Longitude = numeric(), state_abbr = character(), unit = character())
}
if (is.null(semi_fac)) {
  semi_fac <- tibble::tibble(name = character(), tech = character(), cat = character(), size = numeric(), Latitude = numeric(), Longitude = numeric(), state_abbr = character(), unit = character())
}

drones_path <- fs::path(raw_dir, "us_drone_facility_announcements_2022_2025.csv")
if (!fs::file_exists(drones_path)) {
  drones_path <- fs::path(raw_dir, "Downloads", "us_drone_facility_announcements_2022_2025.csv")
}
drones_raw <- read_optional_csv(drones_path)
if (!is.null(drones_raw)) {
  drones_raw <- drones_raw %>% janitor::clean_names()
  drones_fac <- drones_raw %>%
    dplyr::transmute(
      name = .data$company,
      cat = "Drone Manufacturing",
      tech = .data$facility_project,
      size = readr::parse_number(as.character(.data$investment_usd_millions)),
      Latitude = readr::parse_number(as.character(.data$latitude)),
      Longitude = readr::parse_number(as.character(.data$longitude)),
      state_abbr = .data$state,
      unit = "USD"
    )
}
if (is.null(drones_fac)) {
  drones_fac <- tibble::tibble(name = character(), tech = character(), cat = character(), size = numeric(), Latitude = numeric(), Longitude = numeric(), state_abbr = character(), unit = character())
}

electrotech_fac <- dplyr::bind_rows(datacenter_fac, semi_fac, elec_fac, drones_fac) %>%
  dplyr::filter(!is.na(.data$cat), !is.na(.data$size)) %>%
  dplyr::group_by(.data$cat) %>%
  dplyr::mutate(
    size_perc = scale_minmax(.data$size),
    size_perc_scurve = median_scurve(.data$size)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(size = dplyr::if_else(.data$unit == "MW", .data$size * 0.66, .data$size))

if (nrow(electrotech_fac) > 0) {
  readr::write_csv(electrotech_fac, fs::path(paths$processed_dir, "electrotech_fac.csv"))

  state_electro <- electrotech_fac %>%
    dplyr::filter(!is.na(.data$state_abbr)) %>%
    dplyr::group_by(.data$state_abbr, .data$cat, .data$unit) %>%
    dplyr::summarize(size = sum(.data$size, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$cat, values_from = .data$size)

  readr::write_csv(state_electro, fs::path(paths$processed_dir, "state_electro.csv"))
}

# ---- County employment (legacy wiring, state-level rollup) --------------
workforce_share_update <- NULL
workforce_growth_update <- NULL
if (requireNamespace("blsQCEW", quietly = TRUE) && requireNamespace("tidycensus", quietly = TRUE)) {
  electric_man_6d <- c(
    "513322", "513340", "513390", "515210", "517210", "517211", "517212", "517410", "517910", "517919",
    "334210", "334220", "334290", "335912", "221112", "221111", "221113", "221114", "221115", "221116", "221117", "221118", "221119", "221121",
    "335110", "335121", "335122", "335129", "335311", "335312", "335313", "335314", "335921", "335929", "335931", "335932", "335991", "335999", "335911"
  )
  electric_man_4d <- unique(stringr::str_sub(electric_man_6d, 1, 4))

  county_codes <- tidycensus::fips_codes %>%
    dplyr::transmute(
      area_code = sprintf("%02d%03d", as.integer(.data$state_code), as.integer(.data$county_code)),
      state_abbr = .data$state
    ) %>%
    dplyr::distinct()

  fetch_county_qtr <- function(area_code, y, q) {
    y_try <- c(as.character(y), as.integer(y))
    q_try <- unique(c(as.character(q), paste0("q", as.character(q))))

    for (yy in y_try) {
      for (qq in q_try) {
        out <- tryCatch(
          blsQCEW::blsQCEW("Area", year = yy, quarter = qq, area = area_code),
          error = function(e) NULL
        )
        if (!is.null(out) && nrow(out) > 0) {
          return(out)
        }
      }
    }
    NULL
  }

  detect_latest_yq <- function(sample_area = "01001") {
    candidates <- tidyr::expand_grid(year = 2026:2020, quarter = c("4", "3", "2", "1")) %>%
      dplyr::arrange(dplyr::desc(.data$year), dplyr::desc(.data$quarter))

    for (i in seq_len(nrow(candidates))) {
      probe <- fetch_county_qtr(sample_area, candidates$year[i], candidates$quarter[i])
      if (!is.null(probe) && nrow(probe) > 0) {
        return(list(year = as.character(candidates$year[i]), quarter = as.character(candidates$quarter[i])))
      }
    }

    list(year = "2025", quarter = "2")
  }

  pull_qtr <- function(y, q) {
    purrr::map_dfr(county_codes$area_code, function(ac) {
      df <- fetch_county_qtr(ac, y, q)
      if (is.null(df) || nrow(df) == 0) {
        return(tibble::tibble())
      }
      Sys.sleep(0.03)
      df %>% dplyr::mutate(area_code = ac)
    })
  }

  latest_yq <- detect_latest_yq("01001")
  all_latest <- tryCatch(pull_qtr(latest_yq$year, latest_yq$quarter), error = function(e) tibble::tibble())
  all_2022q1 <- tryCatch(pull_qtr("2022", "1"), error = function(e) tibble::tibble())

  if (nrow(all_latest) > 0) {
    county_elec_latest <- all_latest %>%
      dplyr::filter(.data$own_code == 5, nchar(.data$industry_code) == 4, .data$industry_code %in% electric_man_4d) %>%
      dplyr::mutate(latest_month_emplvl = dplyr::coalesce(.data$month3_emplvl, .data$month2_emplvl, .data$month1_emplvl)) %>%
      dplyr::left_join(county_codes, by = "area_code") %>%
      dplyr::group_by(.data$state_abbr) %>%
      dplyr::summarize(elec_emp = sum(.data$latest_month_emplvl, na.rm = TRUE), .groups = "drop")

    state_all_latest <- all_latest %>%
      dplyr::filter(.data$own_code == 5, .data$industry_code == "10") %>%
      dplyr::mutate(latest_month_emplvl = dplyr::coalesce(.data$month3_emplvl, .data$month2_emplvl, .data$month1_emplvl)) %>%
      dplyr::left_join(county_codes, by = "area_code") %>%
      dplyr::group_by(.data$state_abbr) %>%
      dplyr::summarize(total_emp = sum(.data$latest_month_emplvl, na.rm = TRUE), .groups = "drop")

    workforce_share_update <- county_elec_latest %>%
      dplyr::left_join(state_all_latest, by = "state_abbr") %>%
      dplyr::mutate(workforce_share = dplyr::if_else(.data$total_emp > 0, .data$elec_emp / .data$total_emp * 100, NA_real_)) %>%
      dplyr::left_join(states, by = c("state_abbr" = "abbr")) %>%
      dplyr::transmute(state = .data$state, workforce_share)

    if (nrow(all_2022q1) > 0) {
      county_elec_2022 <- all_2022q1 %>%
        dplyr::filter(.data$own_code == 5, nchar(.data$industry_code) == 4, .data$industry_code %in% electric_man_4d) %>%
        dplyr::mutate(latest_month_emplvl = dplyr::coalesce(.data$month3_emplvl, .data$month2_emplvl, .data$month1_emplvl)) %>%
        dplyr::left_join(county_codes, by = "area_code") %>%
        dplyr::group_by(.data$state_abbr) %>%
        dplyr::summarize(elec_emp_2022 = sum(.data$latest_month_emplvl, na.rm = TRUE), .groups = "drop")

      workforce_growth_update <- county_elec_latest %>%
        dplyr::left_join(county_elec_2022, by = "state_abbr") %>%
        dplyr::left_join(state_all_latest, by = "state_abbr") %>%
        dplyr::mutate(workforce_growth = dplyr::if_else(.data$total_emp > 0, (.data$elec_emp - .data$elec_emp_2022) / .data$total_emp, NA_real_)) %>%
        dplyr::left_join(states, by = c("state_abbr" = "abbr")) %>%
        dplyr::transmute(state = .data$state, workforce_growth)
    }
  }
}
workforce_share_update <- ensure_optional_numeric(workforce_share_update, "workforce_share")
workforce_growth_update <- ensure_optional_numeric(workforce_growth_update, "workforce_growth")

spot_path <- fs::path(raw_dir, "50 State Gap Analysis.xlsx")
spot <- tibble::tibble(state = character(), spot_score = numeric())
if (fs::file_exists(spot_path)) {
  sheet_names <- readxl::excel_sheets(spot_path)
  policy_index_df <- purrr::map_dfr(sheet_names, function(sheet_name) {
    sheet_data <- readxl::read_excel(spot_path, sheet = sheet_name, skip = 1) %>%
      dplyr::rename(Question = 1, Answer = 2) %>%
      dplyr::filter(!is.na(.data$Answer)) %>%
      dplyr::mutate(
        answer_lower = stringr::str_to_lower(as.character(.data$Answer)),
        Policy_Index = dplyr::case_when(
          answer_lower == "yes" ~ 1,
          answer_lower == "no" ~ 0,
          stringr::str_detect(answer_lower, "partial|some") ~ 0.5,
          TRUE ~ NA_real_
        ),
        State = sheet_name
      )
    sheet_data
  })

  spot <- policy_index_df %>%
    dplyr::filter(!is.na(.data$Question)) %>%
    dplyr::mutate(
      state = stringr::str_replace_all(.data$State, "_", " "),
      state = stringr::str_to_title(.data$state)
    ) %>%
    dplyr::group_by(.data$state) %>%
    dplyr::summarize(spot_score = mean(.data$Policy_Index, na.rm = TRUE), .groups = "drop")
}

raw_updates <- states %>%
  safe_left_join(incentives, by = "state") %>%
  safe_left_join(dev_policy, by = "state") %>%
  safe_left_join(legislation, by = "state") %>%
  safe_left_join(cpcn, by = "state") %>%
  safe_left_join(regdata, by = "state") %>%
  safe_left_join(ordinance, by = "state") %>%
  safe_left_join(sepa, by = "state") %>%
  safe_left_join(sepa, by = "state") %>%
  safe_left_join(gdp_growth_index, by = "state") %>%
  safe_left_join(economic_dynamism, by = "state") %>%
  safe_left_join(ev_station, by = "state") %>%
  safe_left_join(interconnection_queue, by = "state") %>%
  safe_left_join(electricity_price, by = "state") %>%
  safe_left_join(cnbc_rank, by = "state") %>%
  safe_left_join(clean_investment, by = "state") %>%
  safe_left_join(datacenter_index, by = "state") %>%
  safe_left_join(electric_capacity_growth, by = "state") %>%
  safe_left_join(semiconductor_investment, by = "state") %>%
  safe_left_join(evs_per_capita, by = "state") %>%
  safe_left_join(clean_electric_capacity_growth, by = "state") %>%
  safe_left_join(datacenter_mw, by = "state") %>%
  safe_left_join(semiconductor_manufacturing, by = "state") %>%
  safe_left_join(cluster_manufacturing, by = "state") %>%
  safe_left_join(industrial_electricity_price, by = "state") %>%
  safe_left_join(workforce_share_update, by = "state") %>%
  safe_left_join(workforce_growth_update, by = "state")

# ---- Merge + Validate ----------------------------------------------------
processed_inputs <- processed_inputs %>%
  dplyr::left_join(raw_updates, by = "state", suffix = c("", ".raw")) %>%
  dplyr::mutate(
    incentives_gdp = dplyr::coalesce(`incentives_gdp.raw`, incentives_gdp),
    spot_score = dplyr::coalesce(`spot_score.raw`, spot_score),
    dev_policy_count = dplyr::coalesce(`dev_policy_count.raw`, dev_policy_count),
    legislation_index = dplyr::coalesce(`legislation_index.raw`, legislation_index),
    cpcn = dplyr::coalesce(`cpcn.raw`, cpcn),
    regdata_index = dplyr::coalesce(`regdata_index.raw`, regdata_index),
    ordinance = dplyr::coalesce(`ordinance.raw`, ordinance),
    sepa = dplyr::coalesce(`sepa.raw`, sepa),
    gdp_growth_index = dplyr::coalesce(`gdp_growth_index.raw`, gdp_growth_index),
    economic_dynamism = dplyr::coalesce(`economic_dynamism.raw`, economic_dynamism),
    ev_stations_cap = dplyr::coalesce(`ev_stations_cap.raw`, ev_stations_cap),
    interconnection_queue = dplyr::coalesce(`interconnection_queue.raw`, interconnection_queue),
    electricity_price = dplyr::coalesce(`electricity_price.raw`, electricity_price),
    cnbc_rank = dplyr::coalesce(`cnbc_rank.raw`, cnbc_rank),
    clean_tech_investment = dplyr::coalesce(`clean_tech_investment.raw`, clean_tech_investment),
    datacenter_index = dplyr::coalesce(`datacenter_index.raw`, datacenter_index),
    electric_capacity_growth = dplyr::coalesce(`electric_capacity_growth.raw`, electric_capacity_growth),
    semiconductor_investment = dplyr::coalesce(`semiconductor_investment.raw`, semiconductor_investment),
    evs_per_capita = dplyr::coalesce(`evs_per_capita.raw`, evs_per_capita),
    clean_electric_capacity_growth = dplyr::coalesce(`clean_electric_capacity_growth.raw`, clean_electric_capacity_growth),
    datacenter_mw = dplyr::coalesce(`datacenter_mw.raw`, datacenter_mw),
    semiconductor_manufacturing = dplyr::coalesce(`semiconductor_manufacturing.raw`, semiconductor_manufacturing),
    battery_manufacturing = dplyr::coalesce(`battery_manufacturing.raw`, battery_manufacturing),
    solar_manufacturing = dplyr::coalesce(`solar_manufacturing.raw`, solar_manufacturing),
    ev_manufacturing = dplyr::coalesce(`ev_manufacturing.raw`, ev_manufacturing),
    industrial_electricity_price = dplyr::coalesce(`industrial_electricity_price.raw`, industrial_electricity_price),
    workforce_share = dplyr::coalesce(`workforce_share.raw`, workforce_share),
    workforce_growth = dplyr::coalesce(`workforce_growth.raw`, workforce_growth)
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

# ---- Output --------------------------------------------------------------
processed_path <- fs::path(paths$processed_dir, "inputs_processed.csv")
readr::write_csv(validated_inputs, processed_path)
