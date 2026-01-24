local({
  rprofile <- Sys.getenv("R_PROFILE_USER")
  if (!nzchar(rprofile)) {
    return(invisible(NULL))
  }
})
