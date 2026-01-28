paths <- getOption("electrotech.paths")
index_definition <- getOption("electrotech.index_definition")

outputs <- list(
  policy = policy_intent,
  regulatory = regulatory_ease,
  economic = economic_caps,
  infrastructure = infrastructure,
  deployment = deployment,
  cluster = cluster,
  electrotech = electrotech
)

export_outputs(
  outputs = outputs,
  paths = paths,
  raw_inputs = processed_inputs,
  index_definition = index_definition
)
