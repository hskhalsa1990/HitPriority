#' Set file permissions (Unix only; silently skipped on Windows)
chmod_all <- function(path, mode = "777") {
  if (.Platform$OS.type == "unix" && file.exists(path)) {
    try(system(paste("chmod -R", mode, shQuote(path))), silent = TRUE)
  }
  invisible(path)
}
