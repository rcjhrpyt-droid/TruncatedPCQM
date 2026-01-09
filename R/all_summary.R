#' @name summary
#'
#' @title Summary Methods for PCQM Density Estimation Objects
#'
#' @description Generates structured summary objects and formatted console output
#'   for PCQM (Point-Centered Quarter Method) density estimation results under different model assumptions,
#'   with detailed statistical information beyond basic print output. The following specialized 
#'   S3 methods are supported:
#'   \itemize{
#'     \item \code{summary.pcqm_csr_mle} – create summary object for CSR-based PCQM MLE results
#'     \item \code{summary.pcqm_moments} – create summary object for adjusted moment-based PCQM estimators
#'     \item \code{summary.pcqm_nbd_mle} – create summary object for NBD-based PCQM MLE results
#'     \item \code{print.summary.pcqm_csr_mle} – formatted print for CSR-based PCQM MLE summary
#'     \item \code{print.summary.pcqm_moments} – formatted print for adjusted moment estimator summary
#'     \item \code{print.summary.pcqm_nbd_mle} – formatted print for NBD-based PCQM MLE summary
#'   }
#'
#' @param object An object of class \code{"pcqm_csr_mle"}, \code{"pcqm_moments"}, or \code{"pcqm_nbd_mle"},
#'   returned by \code{\link{csr_mle}}, \code{\link{adjusted_moments}}, or \code{\link{nbd_mle}}, respectively.
#'   Used for the \code{summary.*} S3 methods only.
#' @param x An object of class \code{"summary.pcqm_csr_mle"}, \code{"summary.pcqm_moments"}, or \code{"summary.pcqm_nbd_mle"},
#'   returned by the corresponding \code{summary.*} method. Used for the \code{print.summary.*} S3 methods only.
#' @param digits Number of decimal places to display for numeric estimates. Default is 6.
#' @param ... Additional arguments passed to generic \code{summary} or \code{print} methods
#'   (currently unused; included for S3 consistency).
#'
#'
#' @seealso \code{\link{csr_mle}} for CSR-based MLE estimation;
#'   \code{\link{adjusted_moments}} for adjusted moment estimators;
#'   \code{\link{nbd_mle}} for NBD-based MLE estimation;
#'   \code{\link{print}} for basic PCQM estimation result display.
#'
#' @keywords pcqm density estimation CSR NBD moments
#'
#' @examples
#' distances_matrix <- matrix(c(
#'   9999.000000, 9999.000000, 9999.000000, 9999.000000,
#'   9999.000000,    7.655136,   13.815876,   10.423496,
#'      6.094721,    4.135461,    7.732912,    5.454545,
#'   9999.000000, 9999.000000,   14.787289,   15.670821,
#'   9999.000000,    9.825537,   11.611850,   15.757861,
#'   9999.000000,    9.670381,   14.055394,   17.075678,
#'     11.529219,    4.464136,    7.793114,   11.309553,
#'     13.307828,    5.864490,   13.309636,    5.897720,
#'   9999.000000, 9999.000000, 9999.000000, 9999.000000,
#'   9999.000000, 9999.000000,   18.201084, 9999.000000,
#'   9999.000000,    7.809056,   12.612496,    5.601366,
#'      9.201294, 9999.000000,    8.353524,    9.683701,
#'      6.592604,   19.117869,   19.758384,   12.923507,
#'     15.574824,   10.643719,    9.494539,    7.382031,
#'      9.143077, 9999.000000,   15.551414,    5.266916,
#'   9999.000000, 9999.000000, 9999.000000, 9999.000000,
#'     18.604278,    7.279454,    9.385355,    5.573127,
#'   9999.000000, 9999.000000, 9999.000000, 9999.000000,
#'   9999.000000, 9999.000000, 9999.000000, 9999.000000,
#'     11.980811, 9999.000000,   11.853695,   14.405252
#' ), nrow = 20, ncol = 4, byrow = TRUE)
#'
#' # CSR-based MLE Summary
#' csr_result <- csr_mle(distances = distances_matrix, C = 20, q = 4, l = 2,
#'                       lambda_lower = 1e-4, lambda_upper = 1)
#' summary(csr_result)
#'
#' # Adjusted Moment Estimators Summary
#' moment_result <- adjusted_moments(distances = distances_matrix, C = 20, q = 4, l = 2,
#'                                   init_method = "Pollard_censored")
#' summary(moment_result)
#'
#' # NBD-based MLE Summary
#' nbd_result <- nbd_mle(distances = distances_matrix, C = 20, q = 4, l = 2,
#'                       init_method = "Pollard_censored", lambda_lower = 1e-4, lambda_upper = 10)
#' summary(nbd_result)
#' 
NULL

#' @rdname summary
#' @method summary pcqm_csr_mle
#' @export
summary.pcqm_csr_mle <- function(object, ...) {
  
  out <- list(
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

#' @rdname summary
#' @method summary pcqm_moments
#' @export
summary.pcqm_moments <- function(object, ...) {
  
  out <- list(
    estimators = c(
      DK = object$DK_censored,
      Cottam = object$Cottam_censored,
      Pollard = object$Pollard_censored,
      Shen = object$Shen_censored,
      Morisita = object$Morisita_censored
    ),
    k_hat = object$k_hat,
    sample_info = c(
      n_points = object$n_points,
      n_sectors = object$n_sectors,
      n_censored = object$n_censored,
      censored_rate = object$censored_rate
    )
  )
  
  # Remove NA values from estimators
  out$estimators <- out$estimators[!is.na(out$estimators)]
  
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
  cat("Number of focal points: ", x$sample_info["n_points"], "\n")
  cat("Total sectors:          ", x$sample_info["n_sectors"], "\n")
  cat("Censored sectors:       ", x$sample_info["n_censored"], "\n")
  cat("Censored rate:          ",
      sprintf("%.1f%%", 100 * x$sample_info["censored_rate"]), "\n")
  
  cat("\nDensity Estimators:\n")
  cat("-------------------\n")
  
  for (i in seq_along(x$estimators)) {
    name <- names(x$estimators)[i]
    value <- x$estimators[i]
    cat(sprintf("%-15s ", paste0(name, ":")))
    cat(formatC(value, digits = digits, format = "f"), "\n")
  }
  
  cat("\nAggregation Parameter:\n")
  cat("----------------------\n")
  cat("k_hat: ", formatC(x$k_hat, digits = digits, format = "f"), "\n")
  
  invisible(x)
}

#' @rdname summary
#' @method summary pcqm_nbd_mle
#' @export
summary.pcqm_nbd_mle <- function(object, ...) {
  
  out <- list(
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
  cat("Censored sectors:", x$n_censored,
      sprintf("(%.1f%%)", 100 * x$censored_rate), "\n")
  
  invisible(x)
}