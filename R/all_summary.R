#' @name summary
#'
#' @title Summary Methods for PCQM Density Estimation Objects
#'
#' @description
#' Generate structured summary objects and formatted console output for
#' PCQM (Point-Centered Quarter Method) density estimation results under
#' different model assumptions.
#'
#' Supported S3 methods:
#' \itemize{
#'   \item \code{summary.pcqm_csr_mle}
#'   \item \code{summary.pcqm_moments}
#'   \item \code{summary.pcqm_nbd_mle}
#'   \item \code{print.summary.pcqm_*}
#' }
#'
#' @param object Model object returned by \code{csr_mle},
#'   \code{adjusted_moments}, or \code{nbd_mle}.
#' @param x Summary object returned by corresponding \code{summary()} method.
#' @param digits Number of digits to display.
#' @param ... Additional arguments (unused).
#'
#' @keywords pcqm density estimation CSR NBD moments
NULL

# ================================
# CSR MLE SUMMARY
# ================================

#' @rdname summary
#' @method summary pcqm_csr_mle
#' @export
#'
#' @return An object of class \code{"summary.pcqm_csr_mle"}, a named list with components:
#' \describe{
#'   \item{model}{Character string: \code{"csr_mle"}.}
#'   \item{lambda}{Estimated population density (intensity parameter \eqn{\lambda}).
#'     \code{NA} if estimation failed.}
#'   \item{logLik}{Maximized log-likelihood evaluated at \eqn{\hat{\lambda}}.}
#'   \item{n_points}{Number of focal sampling points.}
#'   \item{n_sectors}{Total number of sectors.}
#'   \item{n_censored}{Number of censored sectors.}
#'   \item{censored_rate}{Proportion of censored sectors.}
#' }
summary.pcqm_csr_mle <- function(object, ...) {

  out <- list(
    model = "csr_mle",
    lambda = object$lambda,
    logLik = object$logLik,
    n_points = object$n_points,
    n_sectors = object$n_sectors,
    n_censored = object$n_censored,
    censored_rate = object$censored_rate
  )

  class(out) <- "summary.pcqm_csr_mle"
  out
}

#' @rdname summary
#' @method print summary.pcqm_csr_mle
#' @export
print.summary.pcqm_csr_mle <- function(x, digits = 6, ...) {

  cat("Summary of CSR-based PCQM MLE\n")
  cat("--------------------------------------------------\n")

  if (is.na(x$lambda)) {
    cat("Estimation failed.\n")
    return(invisible(x))
  }

  cat("Estimated density (lambda):",
      formatC(x$lambda, digits = digits, format = "f"), "\n")

  cat("Log-likelihood:",
      formatC(x$logLik, digits = digits, format = "f"), "\n")

  cat("Number of focal points:", x$n_points, "\n")
  cat("Total sectors:", x$n_sectors, "\n")
  cat("Censored sectors:", x$n_censored,
      sprintf("(%.1f%%)", 100 * x$censored_rate), "\n")

  invisible(x)
}

# ================================
# MOMENT ESTIMATORS SUMMARY
# ================================

#' @rdname summary
#' @method summary pcqm_moments
#' @export
#'
#' @return An object of class \code{"summary.pcqm_moments"}, a named list with components:
#' \describe{
#'   \item{model}{Character string: \code{"moments"}.}
#'   \item{estimators}{Named numeric vector of valid adjusted moment estimators (NA removed).}
#'   \item{k_hat}{Estimated aggregation (dispersion) parameter \eqn{\hat{k}}.}
#'   \item{n_points}{Number of focal sampling points.}
#'   \item{n_sectors}{Total number of sectors.}
#'   \item{n_censored}{Number of censored sectors.}
#'   \item{censored_rate}{Proportion of censored sectors.}
#' }
summary.pcqm_moments <- function(object, ...) {

  estimators <- c(
    DK = object$DK_censored,
    Cottam = object$Cottam_censored,
    Pollard = object$Pollard_censored,
    Shen = object$Shen_censored,
    Morisita = object$Morisita_censored
  )

  estimators <- estimators[!is.na(estimators)]

  out <- list(
    model = "moments",
    estimators = estimators,
    k_hat = object$k_hat,
    n_points = object$n_points,
    n_sectors = object$n_sectors,
    n_censored = object$n_censored,
    censored_rate = object$censored_rate
  )

  class(out) <- "summary.pcqm_moments"
  out
}

#' @rdname summary
#' @method print summary.pcqm_moments
#' @export
print.summary.pcqm_moments <- function(x, digits = 6, ...) {

  cat("Summary of Adjusted Moment Estimators\n")
  cat("=====================================\n\n")

  cat("Sample Information:\n")
  cat("------------------\n")
  cat("Number of focal points:", x$n_points, "\n")
  cat("Total sectors:", x$n_sectors, "\n")
  cat("Censored sectors:", x$n_censored, "\n")
  cat("Censored rate:",
      sprintf("%.1f%%", 100 * x$censored_rate), "\n")

  cat("\nDensity Estimators:\n")
  cat("-------------------\n")

  if (length(x$estimators) == 0) {
    cat("No valid estimators available.\n")
  } else {
    for (i in seq_along(x$estimators)) {
      cat(sprintf("%-15s ",
                  paste0(names(x$estimators)[i], ":")))
      cat(formatC(x$estimators[i], digits = digits, format = "f"), "\n")
    }
  }

  cat("\nAggregation Parameter:\n")
  cat("----------------------\n")
  cat("k_hat:",
      formatC(x$k_hat, digits = digits, format = "f"), "\n")

  invisible(x)
}

# ================================
# NBD MLE SUMMARY
# ================================

#' @rdname summary
#' @method summary pcqm_nbd_mle
#' @export
#'
#' @return An object of class \code{"summary.pcqm_nbd_mle"}, a named list with components:
#' \describe{
#'   \item{model}{Character string: \code{"nbd_mle"}.}
#'   \item{lambda}{Estimated population density (intensity parameter \eqn{\lambda}).
#'     \code{NA} if estimation failed.}
#'   \item{k}{Estimated aggregation (dispersion) parameter of the Negative Binomial distribution.
#'     \code{NA} if estimation failed.}
#'   \item{logLik}{Maximized log-likelihood evaluated at \eqn{(\hat{\lambda}, \hat{k})}.}
#'   \item{n_points}{Number of focal sampling points.}
#'   \item{n_sectors}{Total number of sectors.}
#'   \item{n_censored}{Number of censored sectors.}
#'   \item{censored_rate}{Proportion of censored sectors.}
#' }
summary.pcqm_nbd_mle <- function(object, ...) {

  out <- list(
    model = "nbd_mle",
    lambda = object$lambda,
    k = object$k,
    logLik = object$logLik,
    n_points = object$n_points,
    n_sectors = object$n_sectors,
    n_censored = object$n_censored,
    censored_rate = object$censored_rate
  )

  class(out) <- "summary.pcqm_nbd_mle"
  out
}

#' @rdname summary
#' @method print summary.pcqm_nbd_mle
#' @export
print.summary.pcqm_nbd_mle <- function(x, digits = 6, ...) {

  cat("Summary of NBD-based PCQM MLE\n")
  cat("--------------------------------------------------\n")

  if (is.na(x$lambda) || is.na(x$k)) {
    cat("Estimation failed.\n")
    return(invisible(x))
  }

  cat("Estimated density (lambda):",
      formatC(x$lambda, digits = digits, format = "f"), "\n")

  cat("Aggregation parameter (k):",
      formatC(x$k, digits = digits, format = "f"), "\n")

  cat("Log-likelihood:",
      formatC(x$logLik, digits = digits, format = "f"), "\n")

  cat("Number of focal points:", x$n_points, "\n")
  cat("Total sectors:", x$n_sectors, "\n")
  cat("Censored sectors:", x$n_censored,
      sprintf("(%.1f%%)", 100 * x$censored_rate), "\n")

  invisible(x)
}
