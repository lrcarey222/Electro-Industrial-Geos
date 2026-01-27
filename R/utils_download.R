#' Download a file with caching and metadata
#'
#' @param url URL to download.
#' @param dest_dir Directory to store cached files.
#' @param snapshot_date Snapshot date to include in cache key.
#' @param filename Optional filename override.
#' @return Path to the downloaded file.
#' @export
download_with_cache <- function(url, dest_dir, snapshot_date, filename = NULL) {
  fs::dir_create(dest_dir, recurse = TRUE)
  safe_name <- filename %||% paste0(gsub("[^A-Za-z0-9]+", "_", basename(url)), "_", snapshot_date)
  dest_path <- fs::path(dest_dir, safe_name)
  if (!fs::file_exists(dest_path)) {
    utils::download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
  }
  dest_path
}
