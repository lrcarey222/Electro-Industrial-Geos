#!/usr/bin/env Rscript
#
# Seed data/_state/manifest.csv from the data currently committed to the repo.
#
# This is a one-off migration aid, not part of the refresh cycle. From Phase 2
# onwards each connector writes its own manifest row at ingest; this script
# exists so the freshness board starts from measured facts rather than an empty
# table, and so the first run tells the truth about how stale the index is.
#
# Everything it records is COMPUTED from the committed file -- sha256, row
# count, geography count, schema fingerprint. The only hand-entered values are
# `publisher_release_date` and `vintage_label`, and only where a vintage is
# actually evidenced: a date inside the file, an embedded version line, or a
# dated filename. Where no vintage can be evidenced the row is left `pending`
# with the reason in `notes`, because inventing a date would defeat the point.
#
# Usage:
#   Rscript scripts/03_seed_manifest.R [--dry-run]

suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(glue)
  library(readr)
  library(rlang)
  library(tibble)
  library(yaml)
})

repo_root <- normalizePath(
  file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."),
  winslash = "/",
  mustWork = FALSE
)
if (is.na(repo_root) || !dir.exists(repo_root)) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
dry_run <- "--dry-run" %in% commandArgs(trailingOnly = TRUE)

`%||%` <- function(x, y) if (!is.null(x)) x else y
for (f in c("sources_registry.R", "manifest.R")) source(file.path(repo_root, "R", f))

raw <- function(...) file.path(repo_root, "data", "raw", ...)

# ---------------------------------------------------------------------------
# Legacy path map: source_id -> the file currently in the repo.
#
# Needed only for seeding. Phase 2 lands remote payloads under
# data/raw/remote/<source_id>/<vintage_label>/ and Phase 3 puts manual drops in
# data/manual/<source_id>/, both derived from source_id by convention, so this
# table does not survive into the refresh cycle.
#
# `release` and `vintage` are populated ONLY where the vintage is evidenced.
# `reader` says how to count rows; NULL means "hash the file but do not parse".
# ---------------------------------------------------------------------------
read_csv_q <- function(p, skip = 0) {
  suppressWarnings(readr::read_csv(p, skip = skip, show_col_types = FALSE, progress = FALSE))
}
read_xlsx_q <- function(p, sheet = 1, skip = 0) {
  suppressWarnings(readxl::read_excel(p, sheet = sheet, skip = skip))
}

entries <- list(
  list(id = "eia_861m_sales_revenue", path = raw("remote", "sales_revenue.xlsx"),
       reader = function(p) read_xlsx_q(p, 1, 2),
       release = "2025-11-30", vintage = "2025-M11",
       notes = "Latest Year/Month present in the committed workbook is 2025-11 (Preliminary)."),

  list(id = "eia_860m_generators", path = raw("remote", "may_generator2026.xlsx"),
       reader = function(p) read_xlsx_q(p, 1, 2),
       release = "2026-05-31", vintage = "2026-M05",
       notes = "Filename encodes the release month. NOTE: 3 committed generator workbooks are HTML error pages (F-12)."),

  list(id = "bea_sqgdp", path = raw("remote", "SQGDP.zip"), reader = NULL,
       release = NA, vintage = "SQGDP9__ALL_AREAS_2005_2025",
       notes = "Member filename encodes the vintage. No release date available without unzipping; read but never downloaded (F-06)."),

  list(id = "bea_sagdp", path = raw("remote", "SAGDP.zip"), reader = NULL,
       release = NA, vintage = "SAGDP9__ALL_AREAS_1997_2024",
       notes = "Member filename encodes the vintage; annual series ends 2024."),

  list(id = "eig_dynamism", path = raw("remote", "Downloadable-Data-EIG-Index-of-State-Dynamism-2022.xlsx"),
       reader = function(p) read_xlsx_q(p, 1, 0),
       release = "2022-12-31", vintage = "2022",
       notes = "Panel ends 2022 and the code filters to max(year), so the indicator is a 2022 vintage."),

  list(id = "lbnl_interconnection_queue", path = raw("remote", "LBNL_Ix_Queue_Data_File_thru2024_v2.xlsx"),
       reader = NULL,
       release = "2024-12-31", vintage = "thru2024_v2",
       notes = "Filename states coverage through 2024; URL path indicates a 2025-08 release."),

  list(id = "afdc_station_counts", path = raw("remote", "historical-station-counts.xlsx"),
       reader = NULL,
       release = "2024-12-31", vintage = "2024",
       notes = "URL pins ?year=2024. Header row is unnamed, so the column contract is unverified (F-07)."),

  list(id = "afdc_ev_registrations", path = raw("remote", "10962-ev-registration-counts-by-state_9-06-24.xlsx"),
       reader = function(p) read_xlsx_q(p, 1, 2),
       release = "2024-09-06", vintage = "2023-registrations",
       notes = "Filename encodes a 2024-09-06 publication of 2023 registration counts."),

  list(id = "census_county_population", path = NA, reader = NULL,
       release = "2024-03-14", vintage = "co-est2023",
       notes = "Read from a live URL at runtime, not staged locally (F-11). Vintage taken from the dataset name."),

  list(id = "census_tiger_states", path = NA, reader = NULL,
       release = NA, vintage = "TIGER2023",
       notes = "Fetched at runtime via tigris::states(year = 2023); nothing is staged locally."),

  list(id = "fcc_pea_shapefile", path = raw("FCC_PEAs_Website", "FCC_PEAs_website.shp"),
       reader = NULL, release = NA, vintage = "FCC-PEA",
       notes = "416 PEAs. Undated by the publisher; static geography."),

  list(id = "fcc_pea_county_crosswalk", path = raw("FCC_PEA_website.xlsx"),
       reader = function(p) read_xlsx_q(p, 3, 0),
       release = NA, vintage = "FCC-PEA",
       notes = "Sheet 3 t_FCC_PEA_Counties, 3,236 county rows across 416 PEAs. Verified identical to the FCC original."),

  list(id = "clean_investment_monitor",
       path = raw("clean_investment_monitor_q2_2025", "quarterly_actual_investment.csv"),
       reader = function(p) read_csv_q(p, skip = 5),
       release = "2025-08-11", vintage = "2025_Q2.20250811.0",
       notes = "Embedded version line. Directory says q2_2025 but manufacturing_facility_metadata.csv is 2025_Q4.20260109.0 (F-09)."),

  list(id = "bnef_datacenter_capacity",
       path = raw("BNEF", "2025-08-08 - Global Data Center Live IT Capacity Database.xlsx"),
       reader = NULL,
       release = "2025-08-08", vintage = "2025-08-08",
       notes = "Filename encodes the export date. Code hard-filters Date == 2025-03-31 (F-08). Licensed; should not be committed (F-13)."),

  list(id = "gjf_subsidy_tracker", path = raw("Good Jobs First", "gjf_complete.csv"),
       reader = read_csv_q,
       release = NA, vintage = "awards-through-2024",
       notes = "Max award Year in the committed extract is 2024; the file itself is undated. Licensed; should not be committed (F-13)."),

  list(id = "climate_legislation", path = raw("climate_leg.csv"), reader = read_csv_q,
       release = NA, vintage = NA,
       notes = "No vintage field and no release date; 2,545 bills across 49 states. Publisher unconfirmed."),

  list(id = "sia_semiconductor_investment", path = raw("semiconductor_man.csv"), reader = read_csv_q,
       release = NA, vintage = NA,
       notes = "Hand-compiled, undated. 140 projects across 29 states; 43 rows have 'Not Available' project size."),

  list(id = "cnbc_state_rankings", path = raw("cnbc_bus_rankings.csv"), reader = read_csv_q,
       release = NA, vintage = NA,
       notes = "No year column anywhere in the file, so the ranking year cannot be determined from the data."),

  list(id = "cpcn_requirements",
       path = raw("CPCN_Requirements_and_Enactment_Years_by_State_GPT.csv"), reader = read_csv_q,
       release = NA, vintage = NA,
       notes = "Undated. _GPT filename indicates LLM-generated statutory research; needs human review (audit A-4)."),

  list(id = "state_sepa", path = raw("state_sepa.csv"), reader = read_csv_q,
       release = NA, vintage = NA, notes = "Undated, no citations."),

  list(id = "solar_ordinances", path = raw("Solar Ordinances.csv"), reader = read_csv_q,
       release = NA, vintage = NA,
       notes = "Ordinance years 2009-2021; file itself undated. Covers 34 of 50 states."),

  list(id = "regdata_subnational", path = raw("Regdata_subnational.csv"), reader = read_csv_q,
       release = NA, vintage = "2020-2022",
       notes = "Period codes 2020-2022. No release date and no recorded endpoint."),

  list(id = "dev_program_db", path = raw("dbo_Program.csv"), reader = read_csv_q,
       release = NA, vintage = NA, notes = "SQL Server export, undated. Publisher unidentified (audit A-2)."),

  list(id = "spot_gap_analysis", path = raw("50 State Gap Analysis.xlsx"), reader = NULL,
       release = NA, vintage = NA, notes = "One worksheet per state. Undated; publisher unidentified (audit A-1)."),

  list(id = "drone_facility_announcements",
       path = raw("us_drone_facility_announcements_2022_2025.csv"), reader = read_csv_q,
       release = NA, vintage = "2022-2025",
       notes = "Announcement dates span 2022-2025; hand-compiled from press sources."),

  # No artefact exists for these: no producer in the repo at all.
  list(id = "bls_qcew", path = NA, reader = NULL, release = NA, vintage = NA,
       notes = "Pulled live from the BLS API, nothing staged. Results are currently discarded before reaching the index (F-05)."),
  list(id = "rmi_feasibility", path = NA, reader = NULL, release = NA, vintage = NA,
       notes = "No producer in this repository; internal analysis output. Indicators hold sample data only."),
  list(id = "nrel_supply_curve", path = NA, reader = NULL, release = NA, vintage = NA,
       notes = "No producer in this repository; computed in a script that is not committed here."),
  list(id = "rmi_employment_lq", path = NA, reader = NULL, release = NA, vintage = NA,
       notes = "No producer in this repository; legacy script reads an undefined bundle_lq object."),
  list(id = "dsire_policy_count", path = NA, reader = NULL, release = NA, vintage = NA,
       notes = "No producer in this repository and no recorded endpoint. Indicator holds sample data only.")
)

registry <- load_sources_registry(repo_root)
registry_ids <- vapply(registry$sources, function(s) s$id, character(1))
seeded_ids <- vapply(entries, function(e) e$id, character(1))

if (length(setdiff(registry_ids, seeded_ids)) > 0) {
  stop(
    "Sources in the registry with no seed entry: ",
    paste(setdiff(registry_ids, seeded_ids), collapse = ", "),
    call. = FALSE
  )
}
if (length(setdiff(seeded_ids, registry_ids)) > 0) {
  stop(
    "Seed entries with no registry source: ",
    paste(setdiff(seeded_ids, registry_ids), collapse = ", "),
    call. = FALSE
  )
}

manifest <- manifest_empty()
now <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

for (e in entries) {
  present <- !is.na(e$path) && fs::file_exists(e$path)
  data <- NULL
  if (present && !is.null(e$reader)) {
    data <- tryCatch(e$reader(e$path), error = function(err) NULL)
  }

  has_vintage <- !is.na(e$vintage %||% NA) || !is.na(e$release %||% NA)
  notes <- e$notes %||% ""
  if (!present && !is.na(e$path)) {
    notes <- paste0("File not present at seed time. ", notes)
  }
  if (!has_vintage) {
    notes <- paste0(notes, " No vintage could be evidenced, so this stays pending rather than being given a made-up date.")
  }

  manifest <- manifest_upsert(manifest, list(
    source_id = e$id,
    # Seeding is not a retrieval: leave retrieved_at_utc empty for anything we
    # did not actually fetch, so the freshness engine never mistakes the seed
    # date for a real ingest.
    retrieved_at_utc = NA_character_,
    publisher_release_date = e$release %||% NA_character_,
    vintage_label = e$vintage %||% NA_character_,
    sha256 = if (present) file_sha256(e$path) else NA_character_,
    n_rows = if (!is.null(data)) nrow(data) else NA_integer_,
    n_geographies = count_geographies(data),
    schema_fingerprint = schema_fingerprint(data),
    ingest_method = "manual",
    ingested_by = "scripts/03_seed_manifest.R",
    status = if (has_vintage) "ok" else "pending",
    notes = trimws(notes)
  ))
}

path <- manifest_path(repo_root)
if (dry_run) {
  cat("seed_manifest: --dry-run, nothing written\n")
} else {
  manifest_write(manifest, path)
  cat(sprintf("seed_manifest: wrote %d rows to %s\n", nrow(manifest), path))
}

cat(sprintf(
  "  %d with an evidenced vintage, %d pending, %d with a computed sha256, %d with a row count\n",
  sum(manifest$status == "ok"),
  sum(manifest$status == "pending"),
  sum(!is.na(manifest$sha256)),
  sum(!is.na(manifest$n_rows))
))
