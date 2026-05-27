#' NBD-based Maximum Likelihood Estimator for Right-censored PCQM Data
#'
#' Computes the maximum likelihood estimate (MLE) of population density
#' under Negative Binomial model, accounting for right-censored
#' point-centered quarter method (PCQM) distances.
#'
#' @param distances A numeric vector, matrix, data.frame, or list of point-to-tree distances.
#'   For matrix/data.frame inputs, each row represents a sampling point and each column a sector.
#' @param C Maximum search radius (censoring threshold). Distances exceeding C are treated
#'   as right-censored observations. Default is 20.
#' @param q Number of sectors per sampling point. Default is 4.
#' @param l Neighbor order. Default is 1.
#' @param init_method Method for obtaining initial parameter values. One of:
#'   "Shen_censored" (default), "Pollard_censored", "Cottam_censored", or "DK_censored" (l=1 only).
#' @param lambda_lower Lower bound for \eqn{\lambda} in numerical optimization. Must be positive. Default is 1e-4.
#' @param lambda_upper Upper bound for \eqn{\lambda} in numerical optimization. Must be positive. Default is 1.
#'
#' @importFrom stats optim pbeta pgamma qgamma
#'
#' @return An object of class \code{"pcqm_nbd_mle"} containing:
#' \itemize{
#'   \item \code{lambda}: MLE of population density (individuals per unit area)
#'   \item \code{k}: MLE of aggregation parameter (smaller values indicate stronger aggregation)
#'   \item \code{logLik}: Maximized log-likelihood value
#'   \item \code{n_obs}: Number of fully observed (non-censored) distances
#'   \item \code{n_censored}: Number of censored observations (R > C)
#'   \item \code{n_sectors}: Total number of sectors analyzed
#'   \item \code{n_points}: Inferred number of sampling points
#'   \item \code{censored_rate}: Proportion of censored sectors
#'   \item \code{call}: Original function call
#' }
#'
#' @keywords pcqm density estimation NBD
#'
#' @examples
#'
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
#' nbd_result <- nbd_mle(
#'   distances = distances_matrix,
#'   C = 20,
#'   q = 4,
#'   l = 2,
#'   init_method = "Pollard_censored",
#'   lambda_lower = 1e-4,
#'   lambda_upper = 10
#' )
#'
#'nbd_result
#'
#'
#'
#'
#'
#'
#' @export
nbd_mle <- function(distances,
                    C = 20,
                    q = 4,
                    l = 1,
                    init_method = "Shen_censored",
                    lambda_lower = 1e-4,
                    lambda_upper = 1) {

  # ==================== 1. Standardize Input ====================
  if (is.matrix(distances) || is.data.frame(distances)) {
    r_vec <- as.vector(t(as.matrix(distances)))
  } else if (is.vector(distances)) {
    r_vec <- distances
  } else {
    stop("`distances` must be a vector, matrix, data.frame, or list.")
  }

  r_vec <- r_vec[!is.na(r_vec) & is.finite(r_vec)]

  if (length(r_vec) == 0) {
    warning("No valid distance data provided.")
    return(structure(
      list(lambda = NA_real_, k = NA_real_),
      class = "pcqm_nbd_mle"
    ))
  }

  # ==================== 2. Censoring Structure ====================
  r_obs <- r_vec[r_vec <= C]
  n_obs <- length(r_obs)

  n_cens <- sum(r_vec > C)
  n_tot <- n_obs + n_cens

  if (n_tot %% q != 0) {
    warning("Total number of sectors not divisible by q; results may be unreliable.")
  }

  n_points <- n_tot / q

  if (n_obs == 0) {
    warning("All observations are censored; NBD MLE not identifiable.")
    return(structure(
      list(lambda = NA_real_, k = NA_real_),
      class = "pcqm_nbd_mle"
    ))
  }

  # ==================== 3. Initial Value Calculation ====================
  init_pde <- simplified_PDEs_nbd(r_vec, C, q, l, init_method)
  k_init <- init_pde$k_init
  lambda_init <- max(init_pde$lambda_init, lambda_lower)

  # ==================== 4. Define negative log-likelihood (internal closure) ====================
  neg_loglik <- function(par) {
    k <- exp(par[1])
    lambda <- exp(par[2])
    a <- pi * lambda / q

    if (!is.finite(k) || !is.finite(lambda)) return(Inf)

    # Non-censored terms
    ll_obs <- sum(
      log(2) + l * log(a) + (2 * l - 1) * log(r_obs) +
        lgamma(l + k) - lgamma(k) - lgamma(l) -
        l * log(k) -
        (l + k) * log(1 + (a * r_obs^2) / k)
    )

    # Censored terms
    ll_cens <- 0
    if (n_cens > 0) {
      uC <- (a * C^2) / k
      wC <- uC / (1 + uC)
      S_C <- 1 - pbeta(wC, shape1 = l, shape2 = k)
      if (!is.finite(S_C) || S_C <= 0) return(Inf)
      ll_cens <- n_cens * log(S_C)
    }

    -(ll_obs + ll_cens)
  }

  # ==================== 5. Optimization ====================
  opt <- tryCatch(
    optim(
      par = c(log(k_init), log(lambda_init)),
      fn = neg_loglik,
      method = "L-BFGS-B",
      lower = c(log(1e-2), log(lambda_lower)),
      upper = c(log(1e3), log(lambda_upper))
    ),
    error = function(e) NULL
  )

  if (is.null(opt) || opt$convergence != 0) {
    warning("NBD MLE optimization failed.")
    return(structure(
      list(lambda = NA_real_, k = NA_real_),
      class = "pcqm_nbd_mle"
    ))
  }

  # ==================== 6. Assemble Results ====================
  result <- list(
    lambda = exp(opt$par[2]),
    k = exp(opt$par[1]),
    logLik = -opt$value,
    n_obs = n_obs,
    n_censored = n_cens,
    n_sectors = n_tot,
    n_points = n_points,
    censored_rate = n_cens / n_tot,
    call = match.call()
  )

  structure(result, class = "pcqm_nbd_mle")
}

