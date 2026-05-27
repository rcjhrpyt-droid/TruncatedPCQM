#' @rdname print
#' @method print pcqm_moments
#' @export
print.pcqm_moments <- function(x, digits = 6, ...) {

  cat("Adjusted Moment Estimators for PCQM Density\n")
  cat("===========================================\n")

  cat("Number of focal points:", x$n_points, "\n")
  cat("Total sectors:", x$n_sectors, "\n")
  cat("Censored sectors:", x$n_censored,
      sprintf("(%.1f%%)", 100 * x$censored_rate), "\n")
  cat("\n")

  cat("Density Estimators:\n")
  cat("-------------------\n")

  if (!is.na(x$DK_censored)) {
    cat("DK_censored:       ", formatC(x$DK_censored, digits = digits, format = "f"), "\n")
  }

  cat("Cottam_censored:   ", formatC(x$Cottam_censored, digits = digits, format = "f"), "\n")
  cat("Pollard_censored:  ", formatC(x$Pollard_censored, digits = digits, format = "f"), "\n")

  if (!is.na(x$Shen_censored)) {
    cat("Shen_censored:     ", formatC(x$Shen_censored, digits = digits, format = "f"), "\n")
  }

  if (!is.na(x$Morisita_censored)) {
    cat("Morisita_censored: ", formatC(x$Morisita_censored, digits = digits, format = "f"), "\n")
  }

  cat("\n")
  cat("Aggregation Parameter:\n")
  cat("----------------------\n")
  cat("k_hat: ", formatC(x$k_hat, digits = digits, format = "f"), "\n")

  invisible(x)
}
