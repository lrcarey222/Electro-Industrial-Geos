paths <- getOption("Electro_Industrial.paths")
index_definition <- getOption("Electro_Industrial.index_definition")

outputs <- list(
  policy = policy_intent,
  regulatory = regulatory_ease,
  economic = economic_caps,
  infrastructure = infrastructure,
  deployment = deployment,
  cluster_pea = cluster_pea,
  cluster = cluster,
  `Electro-Industrial` = Electro_Industrial,
  `Electro-Industrial-PEA` = Electro_Industrial_pea
)

export_outputs(
  outputs = outputs,
  paths = paths,
  raw_inputs = processed_inputs,
  index_definition = index_definition
)
