library(testthat)

# Implementation comes from tests/testthat/setup.R.
#
# Fixtures in tests/fixtures/qcew/ are real slices recorded from
# https://data.bls.gov/cew/data/api/ on 2026-09-29, trimmed to the state rows
# the connector reads (own_code 5, agglvl 56 for the bundle and 51 for the
# all-industries denominator). Every column is kept, so the fixture still
# documents the real schema. Nothing here touches the network.

qcew_fixture <- function(year, code) {
  fixture_path("qcew", sprintf("%s_a_%s.csv", year, code))
}

bundle_codes <- c("2211", "3342", "3351", "3353", "3359", "5162", "5171", "5174", "5178")

load_bundle <- function(year) {
  slices <- lapply(bundle_codes, function(cc) qcew_read_slice(qcew_fixture(year, cc)))
  names(slices) <- bundle_codes
  slices
}

state_lookup <- function() {
  qcew_state_lookup(qcew_read_area_titles(
    file.path(repo_root, "data", "reference", "qcew_area_titles.csv")
  ))
}

test_that("the URL matches the endpoint recorded in the source registry", {
  # sources.yml is the registry of record; a connector that drifts from it
  # would make the registry a lie.
  registry <- load_sources_registry(repo_root)
  entry <- Filter(function(s) identical(s$id, "bls_qcew"), registry$sources)[[1]]
  expect_equal(
    entry$endpoint,
    "https://data.bls.gov/cew/data/api/{year}/{quarter}/industry/{industry_code}.csv"
  )
  expect_equal(
    qcew_industry_url("2025", "a", "3342"),
    "https://data.bls.gov/cew/data/api/2025/a/industry/3342.csv"
  )
})

test_that("4-digit parents are derived, deduplicated and sorted", {
  expect_equal(qcew_naics4(c("335131", "335132", "335139")), "3351")
  expect_equal(
    qcew_naics4(c("517410", "221111", "221121", "334210")),
    c("2211", "3342", "5174")
  )
  # Deterministic order, so the fetch sequence and manifest are stable.
  expect_equal(qcew_naics4(c("999900", "111100")), c("1111", "9999"))
  expect_error(qcew_naics4(c("33", "3342")), "at least 4 characters")
})

test_that("the state lookup resolves exactly the 50 states", {
  lookup <- state_lookup()
  expect_equal(nrow(lookup), 50L)
  expect_setequal(lookup$state, state.name)

  # The four non-states sharing the statewide FIPS shape must be excluded:
  # DC (F-15), Puerto Rico, Virgin Islands, and the FBI's undesignated area.
  expect_false(any(c("11000", "57000", "72000", "78000") %in% lookup$area_fips))

  expect_equal(lookup$state[lookup$area_fips == "06000"], "California")
  expect_equal(lookup$abbr[lookup$area_fips == "48000"], "TX")
})

test_that("a lookup that does not resolve 50 states is refused, not guessed", {
  truncated <- tibble::tibble(
    area_fips = c("01000", "06000"),
    area_title = c("Alabama -- Statewide", "California -- Statewide")
  )
  expect_error(qcew_state_lookup(truncated), "expected 50 states, resolved 2")
})

# --- The decision that matters: suppressed is NA, not zero -----------------

test_that("suppressed cells become NA rather than the zero QCEW reports", {
  slice <- qcew_read_slice(qcew_fixture("2025", "3351"))
  rows <- qcew_state_rows(slice, QCEW_AGGLVL_STATE_NAICS4)

  suppressed <- rows[rows$suppressed, ]
  expect_gt(nrow(suppressed), 0)
  expect_true(all(is.na(suppressed$employment)))

  # And confirm the premise: every suppressed row really does arrive as 0 in
  # the raw file, so reading it naively would count withheld as none.
  raw <- slice[slice$own_code == "5" & slice$agglvl_code == "56", ]
  raw_suppressed <- raw[!is.na(raw$disclosure_code) & raw$disclosure_code == "N", ]
  expect_true(all(as.numeric(raw_suppressed$annual_avg_emplvl) == 0))
  expect_equal(nrow(raw_suppressed), nrow(suppressed))
})

test_that("disclosed cells keep their value and are not flagged", {
  rows <- qcew_state_rows(qcew_read_slice(qcew_fixture("2025", "2211")), QCEW_AGGLVL_STATE_NAICS4)
  disclosed <- rows[!rows$suppressed, ]
  expect_true(all(!is.na(disclosed$employment)))
  expect_true(all(disclosed$employment > 0))
})

test_that("a schema change is refused rather than guessed at", {
  slice <- qcew_read_slice(qcew_fixture("2025", "3342"))
  expect_error(
    qcew_state_rows(slice[, setdiff(names(slice), "annual_avg_emplvl")], "56"),
    "missing annual_avg_emplvl"
  )
  expect_error(
    qcew_state_rows(slice[, setdiff(names(slice), "disclosure_code")], "56"),
    "missing disclosure_code"
  )
})

test_that("only the requested aggregation level and ownership survive", {
  slice <- qcew_read_slice(qcew_fixture("2025", "3353"))
  rows <- qcew_state_rows(slice, QCEW_AGGLVL_STATE_NAICS4)
  expect_gt(nrow(rows), 0)
  # Asking for the all-industries level against a bundle slice yields nothing,
  # which is what makes picking the wrong constant loud rather than silent.
  expect_equal(nrow(qcew_state_rows(slice, QCEW_AGGLVL_STATE_TOTAL)), 0L)
  expect_equal(nrow(qcew_state_rows(slice, QCEW_AGGLVL_STATE_NAICS4, own_code = "1")), 0L)
})

# --- Absent vs suppressed are different things -----------------------------

test_that("absence and suppression are counted separately", {
  lookup <- state_lookup()
  bundle <- qcew_bundle_state_employment(load_bundle("2025"), lookup)

  expect_equal(nrow(bundle), 50L)
  expect_setequal(bundle$state, state.name)

  # Every state is accounted for across all nine codes, however it is missing.
  expect_true(all(
    bundle$n_disclosed + bundle$n_suppressed + bundle$n_absent == length(bundle_codes)
  ))

  # Both kinds of gap genuinely occur in the recorded vintage, so neither
  # branch is untested in practice.
  expect_gt(sum(bundle$n_suppressed), 0)
  expect_gt(sum(bundle$n_absent), 0)
})

test_that("a state with no disclosed cell reports NA, not zero", {
  lookup <- state_lookup()
  slices <- load_bundle("2025")

  # Force every Wyoming row to be withheld, across the whole bundle.
  blinded <- lapply(slices, function(s) {
    s$disclosure_code[s$area_fips == "56000"] <- "N"
    s
  })
  bundle <- qcew_bundle_state_employment(blinded, lookup)
  wy <- bundle[bundle$state == "Wyoming", ]

  expect_equal(wy$n_disclosed, 0L)
  expect_true(is.na(wy$employment))
  # The rest of the country is unaffected.
  expect_false(any(is.na(bundle$employment[bundle$state != "Wyoming"])))
})

test_that("bundle employment sums only disclosed cells", {
  lookup <- state_lookup()
  bundle <- qcew_bundle_state_employment(load_bundle("2025"), lookup)

  # Recomputed independently from the fixtures rather than restating the
  # function's own arithmetic.
  manual <- 0
  for (cc in bundle_codes) {
    s <- qcew_read_slice(qcew_fixture("2025", cc))
    s <- s[s$own_code == "5" & s$agglvl_code == "56", ]
    s <- s[s$area_fips %in% lookup$area_fips, ]
    keep <- is.na(s$disclosure_code) | s$disclosure_code != "N"
    manual <- manual + sum(as.numeric(s$annual_avg_emplvl[keep]))
  }
  expect_equal(sum(bundle$employment, na.rm = TRUE), manual)
})

test_that("slices must be named by NAICS code", {
  lookup <- state_lookup()
  unnamed <- unname(load_bundle("2025"))
  expect_error(qcew_bundle_state_employment(unnamed, lookup), "must be named")
})

# --- Denominator and indicators --------------------------------------------

test_that("state totals come from the all-industries level", {
  lookup <- state_lookup()
  totals <- qcew_state_totals(qcew_read_slice(qcew_fixture("2025", "10")), lookup)

  expect_equal(nrow(totals), 50L)
  expect_true(all(!is.na(totals$total_employment)))
  # Private employment nationally is in the low hundred-millions; this catches
  # a wrong agglvl or ownership filter, which would be off by orders of magnitude.
  expect_gt(sum(totals$total_employment), 1e8)
  expect_lt(sum(totals$total_employment), 2e8)
})

test_that("workforce_share is a plausible percentage and tracks the bundle", {
  lookup <- state_lookup()
  bundle <- qcew_bundle_state_employment(load_bundle("2025"), lookup)
  totals <- qcew_state_totals(qcew_read_slice(qcew_fixture("2025", "10")), lookup)
  ind <- qcew_workforce_indicators(bundle, totals)

  expect_equal(nrow(ind), 50L)
  expect_true(all(ind$workforce_share > 0, na.rm = TRUE))
  expect_true(all(ind$workforce_share < 10, na.rm = TRUE))

  # The share must rise with the bundle and fall with the denominator.
  expect_gt(cor(ind$workforce_share, ind$employment / ind$total_employment), 0.99)
})

test_that("workforce_growth uses the baseline period and the legacy arithmetic", {
  lookup <- state_lookup()
  bundle <- qcew_bundle_state_employment(load_bundle("2025"), lookup)
  totals <- qcew_state_totals(qcew_read_slice(qcew_fixture("2025", "10")), lookup)
  baseline <- qcew_bundle_state_employment(load_bundle("2022"), lookup)

  ind <- qcew_workforce_indicators(bundle, totals, baseline)
  expect_true(any(!is.na(ind$workforce_growth)))

  # Verbatim legacy formula: change in bundle employment over *current total*
  # employment -- a percentage-point change, not a growth rate.
  tx <- ind[ind$state == "Texas", ]
  tx_base <- baseline$employment[baseline$state == "Texas"]
  expect_equal(
    tx$workforce_growth,
    (tx$employment - tx_base) / tx$total_employment
  )

  # Without a baseline the indicator is NA rather than silently zero.
  expect_true(all(is.na(qcew_workforce_indicators(bundle, totals)$workforce_growth)))
})

test_that("a zero or missing denominator yields NA, never a division blow-up", {
  lookup <- state_lookup()
  bundle <- qcew_bundle_state_employment(load_bundle("2025"), lookup)
  totals <- qcew_state_totals(qcew_read_slice(qcew_fixture("2025", "10")), lookup)

  totals$total_employment[totals$state == "Nevada"] <- 0
  totals$total_employment[totals$state == "Oregon"] <- NA_real_
  ind <- qcew_workforce_indicators(bundle, totals)

  expect_true(is.na(ind$workforce_share[ind$state == "Nevada"]))
  expect_true(is.na(ind$workforce_share[ind$state == "Oregon"]))
  expect_false(is.na(ind$workforce_share[ind$state == "Utah"]))
})

# --- Offline behaviour, which is what keeps CI hermetic ---------------------

test_that("offline never touches the network and returns cached slices only", {
  cache <- withr::local_tempdir()

  # Nothing cached: NA rather than an attempted download.
  expect_true(is.na(qcew_fetch_slice("2025", "a", "3342", cache, offline = TRUE)))
  # And it must not have created the directory as a side effect of trying.
  expect_length(list.files(cache), 0L)

  # Cached: returned without any network access.
  file.copy(qcew_fixture("2025", "3342"), file.path(cache, "2025_a_3342.csv"))
  got <- qcew_fetch_slice("2025", "a", "3342", cache, offline = TRUE)
  expect_false(is.na(got))
  expect_equal(nrow(qcew_read_slice(got)), nrow(qcew_read_slice(qcew_fixture("2025", "3342"))))
})

test_that("a period with nothing cached aborts offline rather than half-building", {
  cache <- withr::local_tempdir()
  expect_error(
    qcew_fetch_period(bundle_codes, "2025", "a", cache, state_lookup(), offline = TRUE),
    "no bundle slice could be retrieved"
  )
})

test_that("a missing denominator is an error, not a silent NA share", {
  cache <- withr::local_tempdir()
  for (cc in bundle_codes) {
    file.copy(qcew_fixture("2025", cc), file.path(cache, sprintf("2025_a_%s.csv", cc)))
  }
  # Every bundle slice present, denominator absent.
  expect_error(
    qcew_fetch_period(bundle_codes, "2025", "a", cache, state_lookup(), offline = TRUE),
    "denominator slice is missing"
  )
})

test_that("fetch_period assembles from cache and records absent codes", {
  cache <- withr::local_tempdir()
  # Stage eight of nine bundle codes plus the denominator; 5174 is left out to
  # stand in for a slice QCEW does not publish.
  staged <- setdiff(bundle_codes, "5174")
  for (cc in c(staged, "10")) {
    file.copy(qcew_fixture("2025", cc), file.path(cache, sprintf("2025_a_%s.csv", cc)))
  }
  res <- qcew_fetch_period(bundle_codes, "2025", "a", cache, state_lookup(), offline = TRUE)

  expect_equal(res$absent_codes, "5174")
  expect_equal(nrow(res$bundle), 50L)
  expect_equal(nrow(res$totals), 50L)

  # Two different gaps, reported in two different places. A code QCEW does not
  # publish at all is a property of the period, so it lands in `absent_codes`;
  # the per-state counts cover only the codes that were published, and must
  # still account for every one of them.
  expect_true(all(
    res$bundle$n_disclosed + res$bundle$n_suppressed + res$bundle$n_absent ==
      length(staged)
  ))

  # Dropping a code must lower the total rather than quietly leave it unchanged.
  full <- qcew_fetch_period(
    staged, "2025", "a", cache, state_lookup(), offline = TRUE
  )
  expect_equal(sum(res$bundle$employment, na.rm = TRUE),
               sum(full$bundle$employment, na.rm = TRUE))
  expect_lt(
    sum(res$bundle$employment, na.rm = TRUE),
    sum(qcew_bundle_state_employment(load_bundle("2025"), state_lookup())$employment,
        na.rm = TRUE)
  )
})

test_that("the latest annual year is discovered, not pinned", {
  cache <- withr::local_tempdir()
  expect_true(is.na(qcew_latest_annual_year(cache, from = 2025, offline = TRUE)))

  file.copy(qcew_fixture("2022", "10"), file.path(cache, "2022_a_10.csv"))
  file.copy(qcew_fixture("2025", "10"), file.path(cache, "2025_a_10.csv"))

  # Newest first, so 2025 wins over the equally-present 2022.
  expect_equal(qcew_latest_annual_year(cache, from = 2026, offline = TRUE), "2025")
  # And it walks back when the newest years are not published yet.
  expect_equal(qcew_latest_annual_year(cache, from = 2024, offline = TRUE), "2022")
  # Giving up rather than reaching for an implausibly old vintage.
  expect_true(is.na(qcew_latest_annual_year(cache, from = 2030, max_back = 2L, offline = TRUE)))
})

# --- Suppression is recorded, because it is the known weakness --------------

test_that("state-level suppression stays in the range the design assumed", {
  lookup <- state_lookup()
  bundle <- qcew_bundle_state_employment(load_bundle("2025"), lookup)

  rate <- sum(bundle$n_suppressed) /
    (sum(bundle$n_disclosed) + sum(bundle$n_suppressed))
  # docs/bls_qcew_options.md chose 4-digit state level on the basis that
  # suppression sits far below the 84% seen at county level. If a future
  # vintage broke that, the premise of the design would be gone.
  expect_lt(rate, 0.35)
  expect_gt(rate, 0)
})
