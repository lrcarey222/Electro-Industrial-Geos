#' Find repository root
#'
#' @param start Path to start searching.
#' @return Repository root path.
#' @export
find_repo_root <- function(start = getwd()) {
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    return(rprojroot::find_root(rprojroot::is_git_root, path = start))
  }

  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, ".git")) ||
      length(list.files(current, pattern = "\\.Rproj$")) > 0) {
      return(current)
    }
    parent <- dirname(current)
    if (parent == current) {
      break
    }
    current <- parent
  }

  rlang::abort("Unable to locate repository root. Run from within the repo.")
}

#' Normalize path relative to repo root
#'
#' @param root Repo root.
#' @param path Path to normalize.
#' @return Normalized path.
#' @export
root_path <- function(root, path) {
  if (fs::path_has_parent(path, root) || grepl("^/", path)) {
    return(path)
  }
  fs::path(root, path)
}
