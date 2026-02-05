library(testthat)
library(dplyr)

source("R/utils/utils_helpers.R")
source("R/utils/utils_scale.R")
source("R/utils/utils_index.R")
source("R/utils/ingest_sample.R")
source("R/categories/build_policy_intent.R")
source("R/categories/build_regulatory_ease.R")
source("R/categories/build_economic_capabilities.R")
source("R/categories/build_infrastructure.R")
source("R/categories/build_deployment.R")
source("R/categories/build_cluster_index.R")
source("R/indices/build_Electro-Industrial_index.R")

options(
  Electro_Industrial.paths = list(examples_dir = "tests/fixtures"),
  Electro_Industrial.weights = list(
    `Electro-Industrial` = list(
      deployment_index = 0.4,
      infra_index = 0.15,
      econ_index = 0.15,
      intent_index = 0.2,
      cluster_index = 0.2,
      ease_index = 0.2
    ),
    infrastructure = list(
      renewable_potential = 0.2,
      ev_stations_cap = 0.2,
      interconnection_queue = 0.2,
      electricity_price = 0.2,
      cnbc_rank = 0.2
    )
  )
)


test_that("sub-index builders return expected columns", {
  inputs <- load_sample_inputs()

  policy <- build_policy_intent_index(inputs)
  expect_true(all(c("state", "abbr", "intent_index") %in% names(policy)))

  regulatory <- build_regulatory_ease_index(inputs)
  expect_true(all(c("state", "abbr", "ease_index") %in% names(regulatory)))

  economic <- build_economic_capabilities_index(inputs)
  expect_true(all(c("state", "abbr", "econ_index") %in% names(economic)))

  infra <- build_infrastructure_index(inputs)
  expect_true(all(c("state", "abbr", "infra_index", "infra_index_w") %in% names(infra)))

  deployment <- build_deployment_index(inputs)
  expect_true(all(c("state", "abbr", "deployment_index") %in% names(deployment)))

  cluster <- build_cluster_index(inputs)
  expect_true(all(c("state", "abbr", "dominant_anchor", "positive", "negative", "cluster_top", "cluster_index") %in% names(cluster)))
})

test_that("cluster index uses max anchor", {
  df <- data.frame(
    state = c("A", "B"),
    abbr = c("AA", "BB"),
    workforce_share = c(1, 2),
    workforce_growth = c(1, 2),
    industry_feasibility = c(1, 2),
    clean_electric_capacity_growth = c(1, 2),
    industrial_electricity_price = c(1, 2),
    datacenter_mw = c(1, 2),
    semiconductor_manufacturing = c(5, 1),
    battery_manufacturing = c(3, 1),
    solar_manufacturing = c(2, 1),
    ev_manufacturing = c(4, 1)
  )

  out <- build_cluster_index(df)

  expect_true(out$cluster_index[1] >= out$cluster_index[2])
  expect_true(out$dominant_anchor[1] == "semiconductor_manufacturing")
  expect_true(out$cluster_top[1] != "")
})

test_that("Electro-Industrial weighted index matches fixture", {
  inputs <- load_sample_inputs()

  policy <- build_policy_intent_index(inputs)
  regulatory <- build_regulatory_ease_index(inputs)
  economic <- build_economic_capabilities_index(inputs)
  infra <- build_infrastructure_index(inputs)
  deployment <- build_deployment_index(inputs)
  cluster <- build_cluster_index(inputs)

  Electro_Industrial <- build_Electro_Industrial_index(deployment, infra, economic, policy, regulatory, cluster)

  expected <- readr::read_csv("tests/fixtures/expected_Electro-Industrial.csv", show_col_types = FALSE)

  expect_equal(Electro_Industrial$Electro_Industrial_index_w, expected$Electro_Industrial_index_w, tolerance = 1e-8)
})
