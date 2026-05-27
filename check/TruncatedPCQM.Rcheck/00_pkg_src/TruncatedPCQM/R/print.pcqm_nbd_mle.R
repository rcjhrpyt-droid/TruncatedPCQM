#' @rdname print
#' @method print pcqm_nbd_mle
#' @export
print.pcqm_nbd_mle <- function(x, digits = 6, ...) {

  cat("NBD-based PCQM density estimation (MLE)\n")
  cat("--------------------------------------------------\n")

  if (is.na(x$lambda) || is.na(x$k)) {
    cat("Estimation failed (lambda or k = NA)\n")
    return(invisible(x))
  }

  cat("Estimated density (lambda):",
      formatC(x$lambda, digits = digits, format = "f"), "\n")

  cat("Aggregation parameter (k):",
      formatC(x$k, digits = digits, format = "f"), "\n")

  cat("Number of focal points:", formatC(x$n_points, format = "f"), "\n")
  cat("Censored sectors:", x$n_censored,
      sprintf("(%.1f%%)", 100 * x$censored_rate), "\n")

  invisible(x)
}
