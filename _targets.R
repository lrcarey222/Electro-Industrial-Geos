library(targets)

for (f in list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)) {
  source(f)
}

tar_option_set(
  packages = c(
    "dplyr", "fs", "glue", "janitor",
    "purrr", "readr", "rlang", "stringr", "targets", "tidyr", "tibble",
    "yaml", "openxlsx"
  )
)

list(
  tar_target(pipeline, run_Electro-Industrial_pipeline())
)
