library(testthat)

# Implementation comes from tests/testthat/setup.R.
#
# Guards the F-04 regression: the EIA-861M sheet has five identically named
# Cents/kWh columns, one per customer class, and the sector is named only in a
# merged cell on row 1. Selecting the first match silently yields RESIDENTIAL,
# which is what shipped.

write_eia_fixture <- function(path, sectors = c("RESIDENTIAL", "COMMERCIAL", "INDUSTRIAL", "TRANSPORTATION", "TOTAL")) {
  skip_if_not_installed("openxlsx")

  n_lead <- 4L
  width <- n_lead + 4L * length(sectors)

  row1 <- rep(NA_character_, width)
  for (i in seq_along(sectors)) {
    row1[n_lead + 4L * (i - 1L) + 1L] <- sectors[i]
  }
  row2 <- c(rep(NA_character_, n_lead), rep(c("Revenue", "Sales", "Customers", "Price"), length(sectors)))
  row3 <- c(
    c("Year", "Month", "State", "Data Status"),
    rep(c("Thousand Dollars", "Megawatthours", "Count", "Cents/kWh"), length(sectors))
  )
  # One data row, with each sector's price set to its own index so a wrong
  # column selection is unambiguous rather than merely different.
  data_row <- c("2025", "11", "AL", "Preliminary")
  for (i in seq_along(sectors)) {
    data_row <- c(data_row, "1", "1", "1", as.character(i * 100))
  }

  sheet <- as.data.frame(rbind(row1, row2, row3, data_row), stringsAsFactors = FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Monthly-States")
  openxlsx::writeData(wb, "Monthly-States", sheet, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

test_that("the industrial price column is resolved, not guessed", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  write_eia_fixture(f)

  # RESIDENTIAL block starts at column 5, so its price is column 8;
  # INDUSTRIAL starts at 13, so its price is column 16.
  expect_equal(eia_price_column_index(f, sheet = 1, sector = "RESIDENTIAL"), 8L)
  expect_equal(eia_price_column_index(f, sheet = 1, sector = "INDUSTRIAL"), 16L)
  expect_equal(eia_price_column_index(f, sheet = 1, sector = "TOTAL"), 24L)

  # The industrial column must NOT be the first Cents/kWh column.
  expect_false(
    eia_price_column_index(f, sheet = 1, sector = "INDUSTRIAL") ==
      eia_price_column_index(f, sheet = 1, sector = "RESIDENTIAL")
  )
})

test_that("the resolved index points at the industrial values", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  write_eia_fixture(f)

  idx <- eia_price_column_index(f, sheet = 1, sector = "INDUSTRIAL")
  # skip = 2 makes row 3 the header, matching how the pipeline reads it.
  d <- suppressWarnings(readxl::read_excel(f, sheet = 1, skip = 2, .name_repair = "minimal"))
  # Third sector, so the fixture value is 300.
  expect_equal(as.numeric(d[[idx]][1]), 300)
})

test_that("it is insensitive to sector ordering", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  # A publisher reordering the blocks must not silently change which column is
  # read -- that is the whole point of resolving by name.
  write_eia_fixture(f, sectors = c("INDUSTRIAL", "RESIDENTIAL", "COMMERCIAL"))

  idx <- eia_price_column_index(f, sheet = 1, sector = "INDUSTRIAL")
  expect_equal(idx, 8L)
  d <- suppressWarnings(readxl::read_excel(f, sheet = 1, skip = 2, .name_repair = "minimal"))
  expect_equal(as.numeric(d[[idx]][1]), 100)
})

test_that("a missing or ambiguous sector fails loudly rather than guessing", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  write_eia_fixture(f)
  expect_error(
    eia_price_column_index(f, sheet = 1, sector = "NONEXISTENT"),
    "Could not locate a unique"
  )

  dup <- withr::local_tempfile(fileext = ".xlsx")
  write_eia_fixture(dup, sectors = c("INDUSTRIAL", "INDUSTRIAL"))
  expect_error(
    eia_price_column_index(dup, sheet = 1, sector = "INDUSTRIAL"),
    "Could not locate a unique"
  )
})

test_that("sector matching is case insensitive and whitespace tolerant", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  write_eia_fixture(f, sectors = c("Residential", "  industrial  "))
  expect_equal(eia_price_column_index(f, sheet = 1, sector = "INDUSTRIAL"), 12L)
})

test_that("against the committed workbook, industrial is not the first block", {
  path <- file.path(repo_root, "data", "raw", "remote", "sales_revenue.xlsx")
  skip_if_not(file.exists(path), "EIA-861M workbook not staged")

  industrial <- eia_price_column_index(path, sheet = 1, sector = "INDUSTRIAL")
  residential <- eia_price_column_index(path, sheet = 1, sector = "RESIDENTIAL")

  expect_equal(residential, 8L)
  expect_equal(industrial, 16L)

  # And the values really are different, so the regression was material.
  d <- suppressWarnings(readxl::read_excel(path, sheet = 1, skip = 2, .name_repair = "minimal"))
  al <- d[d[[3]] == "AL" & d[[1]] == 2025, ]
  skip_if(nrow(al) == 0, "no 2025 Alabama rows in the staged workbook")
  expect_false(isTRUE(all.equal(
    mean(as.numeric(al[[industrial]]), na.rm = TRUE),
    mean(as.numeric(al[[residential]]), na.rm = TRUE)
  )))
})
