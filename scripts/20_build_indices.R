if (!exists("deployment", inherits = TRUE)) {
  rlang::abort("Theme indices not found. Ensure scripts/10_build_themes.R ran successfully.")
}

Electro-Industrial <- build_Electro-Industrial_index(
  deployment,
  infrastructure,
  economic_caps,
  policy_intent,
  regulatory_ease,
  cluster
)
