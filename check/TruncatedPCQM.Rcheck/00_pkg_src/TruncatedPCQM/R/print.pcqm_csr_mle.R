#' @rdname print
#' @method print pcqm_csr_mle
#' @export
print.pcqm_csr_mle <- function(x, digits = 6, ...) {

  cat("CSR-based PCQM density estimation (MLE)\n")
  cat("--------------------------------------------------\n")

  if (is.na(x$lambda)) {
    cat("Estimation failed (lambda = NA)\n")
    return(invisible(x))
  }

  cat("Estimated density (lambda):",
      formatC(x$lambda, digits = digits, format = "f"), "\n")

  cat("Number of focal points:", formatC(x$n_points, format = "f"), "\n")
  cat("Total sectors:", x$n_sectors, "\n")
  cat("Censored sectors:", x$n_censored,
      sprintf("(%.1f%%)", 100 * x$censored_rate), "\n")

  invisible(x)
}
