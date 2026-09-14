library(testthat)
library(dplyr)

# Implementation, fixture paths and index weights all come from
# tests/testthat/setup.R.

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

  # build_cluster_index() ends with arrange(desc(cluster_index)), so output row
  # order is by score and NOT by input order. This test previously asserted
  # against out[1, ] as though it were state A; with this data state B scores
  # higher (it leads on every non-anchor positive), so the assertion was reading
  # B's row and failing. It never surfaced because the suite errored at load and
  # ran no assertions at all. Select by state instead of by position.
  a <- out[out$state == "A", ]
  b <- out[out$state == "B", ]

  # A's anchors are 1/5/3/2/4, so after per-column scaling its winning anchor is
  # semiconductor_manufacturing; B leads only on datacenter_mw.
  expect_equal(a$dominant_anchor, "semiconductor_manufacturing")
  expect_equal(b$dominant_anchor, "datacenter_mw")

  # The sort invariant itself, asserted as a property rather than assumed.
  expect_equal(out$cluster_index, sort(out$cluster_index, decreasing = TRUE))

  # cluster_top is populated only above the 0.5 threshold, which B clears here.
  expect_true(b$cluster_index > 0.5)
  expect_equal(b$cluster_top, "B")
})

test_that("state cluster rolls up top PEA cluster", {
  cluster_pea <- tibble::tibble(
    economic_area = c("Metro A", "Metro B", "Metro C"),
    state = c("Texas", "Texas", "California"),
    abbr = c("TX", "TX", "CA"),
    cluster_index = c(0.6, 0.9, 0.5),
    cluster_top = c("Metro A", "Metro B", "")
  )

  state_cluster <- build_state_cluster_from_pea(cluster_pea)
  expect_equal(nrow(state_cluster), 2)
  expect_equal(state_cluster$economic_area[state_cluster$state == "Texas"], "Metro B")
})

test_that("PEA Electro-Industrial index is cluster-driven", {
  cluster_pea <- tibble::tibble(
    economic_area = c("Metro A", "Metro B"),
    state = c("Texas", "California"),
    cluster_index = c(0.8, 0.4)
  )

  pea_index <- build_Electro_Industrial_pea_index(cluster_pea)
  expect_equal(pea_index$Electro_Industrial_index_w, pea_index$cluster_index)
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

  expected <- readr::read_csv(
    fixture_path("expected_Electro-Industrial.csv"),
    show_col_types = FALSE
  )

  # Guard against the failure mode this fixture used to have: its headers were
  # hyphenated (`Electro-Industrial_index_w`) while the code emits the
  # underscore form, so `expected$Electro_Industrial_index_w` was NULL and the
  # comparison below was vacuous. Assert the column exists before using it.
  expect_true("Electro_Industrial_index_w" %in% names(expected))
  expect_true("Electro_Industrial_index" %in% names(expected))

  # Match on state rather than relying on row order.
  actual <- Electro_Industrial[match(expected$state, Electro_Industrial$state), ]
  expect_equal(actual$state, expected$state)

  expect_equal(actual$Electro_Industrial_index_w, expected$Electro_Industrial_index_w, tolerance = 1e-8)
  expect_equal(actual$Electro_Industrial_index, expected$Electro_Industrial_index, tolerance = 1e-8)

  for (sub_index in c(
    "deployment_index", "infra_index", "econ_index",
    "intent_index", "cluster_index", "ease_index"
  )) {
    expect_equal(
      actual[[sub_index]],
      expected[[sub_index]],
      tolerance = 1e-8,
      info = sub_index
    )
  }
})

test_that("test weights still agree with config/weights.yml", {
  # setup.R pins the weights so this fixture stays hermetic. That is only safe
  # while the pinned values match the shipped config -- otherwise a weights
  # change would pass CI while silently moving every published score. Methodology
  # is meant to stay frozen while data moves, so make drift fail loudly here.
  shipped <- yaml::read_yaml(file.path(repo_root, "config", "weights.yml"))

  expect_equal(
    shipped[["Electro-Industrial"]][order(names(shipped[["Electro-Industrial"]]))],
    test_weights[["Electro-Industrial"]][order(names(test_weights[["Electro-Industrial"]]))]
  )
  expect_equal(
    shipped$infrastructure[order(names(shipped$infrastructure))],
    test_weights$infrastructure[order(names(test_weights$infrastructure))]
  )
})
