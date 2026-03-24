#' @name print
#'
#' @title Print Methods for PCQM Density Estimation Objects
#'
#' @description Provides formatted console output for fitted PCQM (Point-Centered Quarter Method)
#'   density estimation results under different model assumptions. The following specialized
#'   print methods for S3 classes are supported:
#'   \itemize{
#'     \item \code{print.pcqm_csr_mle} – display CSR-based PCQM maximum likelihood estimation results.
#'     \item \code{print.pcqm_moments} – display adjusted moment-based PCQM density estimators.
#'     \item \code{print.pcqm_nbd_mle} – display NBD-based PCQM maximum likelihood estimation results.
#'   }
#'
#' @param x An object of class \code{"pcqm_csr_mle"}, \code{"pcqm_moments"}, or \code{"pcqm_nbd_mle"},
#'   returned by \code{\link{csr_mle}}, \code{\link{adjusted_moments}}, or \code{\link{nbd_mle}}, respectively.
#' @param digits Number of decimal places to display for numeric estimates. Default is 6.
#' @param ... Additional arguments passed to the generic \code{print} method
#'   (currently unused; included for S3 consistency).
#'
#' @return The input model object \code{x}, invisibly. These functions are called primarily for
#'   their side effect of printing a formatted summary of the PCQM density estimation results
#'   to the R console.
#'
#' @seealso \code{\link{csr_mle}} for CSR-based MLE estimation;
#'   \code{\link{adjusted_moments}} for adjusted moment estimators;
#'   \code{\link{nbd_mle}} for NBD-based MLE estimation.
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
#' # CSR-based MLE
#' csr_result <- csr_mle(
#'   distances = distances_matrix,
#'   C = 20,
#'   q = 4,
#'   l = 2,
#'   lambda_lower = 1e-4,
#'   lambda_upper = 1
#' )
#' print(csr_result)
#'
#' # Adjusted Moment Estimators
#' moment_result <- adjusted_moments(
#'   distances = distances_matrix,
#'   C = 20,
#'   q = 4,
#'   l = 2,
#'   init_method = "Pollard_censored"
#' )
#' print(moment_result)
#'
#' # NBD-based MLE
#' nbd_result <- nbd_mle(
#'   distances = distances_matrix,
#'   C = 20,
#'   q = 4,
#'   l = 2,
#'   init_method = "Pollard_censored",
#'   lambda_lower = 1e-4,
#'   lambda_upper = 10
#' )
#' print(nbd_result)
#'


NULL
