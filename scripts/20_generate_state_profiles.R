source("scripts/00_setup.R")

paths <- getOption("Electro_Industrial.paths")
root <- getOption("Electro_Industrial.root")
state_index_path <- fs::path(paths$output_dir, "Electro-Industrial_state.csv")

if (!fs::file_exists(state_index_path)) {
  message("Missing outputs; running full pipeline first.")
  run_Electro_Industrial_pipeline(root)
}

state_index <- readr::read_csv(state_index_path, show_col_types = FALSE)
out_dir <- fs::path(paths$output_dir, "state_profiles")
fs::dir_create(out_dir, recurse = TRUE)

render_one <- function(st) {
  slug <- gsub("[^a-z0-9]+", "-", tolower(st))
  out_file <- fs::path(out_dir, paste0(slug, ".html"))

  if (nzchar(Sys.which("quarto"))) {
    params_yaml <- tempfile(fileext = ".yml")
    writeLines(c(
      paste0("state_name: \"", st, "\""),
      "top_n: 10",
      paste0("output_dir: \"", paths$output_dir, "\"")
    ), params_yaml)
    system2("quarto", c("render", fs::path(root, "reports", "state_profile.qmd"), "--to", "html", "--execute-params", params_yaml, "--output", basename(out_file), "--output-dir", out_dir), stdout = FALSE, stderr = FALSE)
  } else {
    message("Quarto not available; writing fallback CSV summaries for ", st)
    source(fs::path(root, "R", "visualization", "state_profile_helpers.R"))
    outs <- load_outputs(paths)
    defs <- get_category_definition(root)
    categories <- list(
      policy_intent = list(df = outs$policy, idx = "intent_index"),
      regulatory_ease = list(df = outs$regulatory, idx = "ease_index"),
      economic_capabilities = list(df = outs$economic, idx = "econ_index"),
      infrastructure = list(df = outs$infrastructure, idx = "infra_index"),
      deployment = list(df = outs$deployment, idx = "deployment_index"),
      cluster = list(df = outs$cluster, idx = "cluster_index")
    )
    for (nm in names(categories)) {
      vars <- defs %>% dplyr::filter(.data$category == nm) %>% dplyr::pull(.data$variable)
      tbl <- build_category_component_table(st, categories[[nm]]$df, vars, categories[[nm]]$idx)
      readr::write_csv(tbl, fs::path(out_dir, paste0(slug, "_", nm, "_components.csv")))
    }
  }
}

purrr::walk(unique(state_index$state), render_one)
message("State profile generation complete: ", out_dir)
