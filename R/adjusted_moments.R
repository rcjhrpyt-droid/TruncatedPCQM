#' Adjusted Moment-based Estimators for Right-censored PCQM Data
#'
#' Computes various density estimators (DK, Cottam, Pollard, Shen, Morisita)
#' using adjusted moments to account for right-censoring in PCQM data.
#'
#' @param distances A numeric vector, matrix, data.frame, or list of point-to-tree distances.
#'   For matrix/data.frame inputs, each row represents a sampling point and each column a sector.
#' @param C Maximum search radius (censoring threshold). Distances exceeding C are treated
#'   as right-censored observations. Default is 20.
#' @param q Number of sectors per sampling point. Default is 4.
#' @param l Neighbor order. Default is 1.
#' @param init_method Method for obtaining initial \eqn{\lambda} estimate used in NBD-based estimators.
#'   One of: "Pollard_censored" (default), "Cottam_censored", or "DK_censored" (only for l = 1).
#'
#' @importFrom stats pgamma qgamma
#'
#' @return An object of class \code{"pcqm_moments"} containing:
#' \itemize{
#'   \item \code{DK_censored}: Diggle-Koedam estimator value (NA if l is not 1)
#'   \item \code{Cottam_censored}: Cottam-type estimator value
#'   \item \code{Pollard_censored}: Pollard-type estimator value
#'   \item \code{Shen_censored}: Shen's NBD-based estimator value (NA if estimated k < 1)
#'   \item \code{Morisita_censored}: Morisita's first estimator value (NA if l = 1)
#'   \item \code{k_hat}: Estimated aggregation parameter k from Shen's estimator
#'   \item \code{censored_rate}: Proportion of censored sectors
#'   \item \code{n_points}: Number of sampling points
#'   \item \code{n_sectors}: Total number of sectors analyzed
#'   \item \code{n_censored}: Number of censored observations (R > C)
#'   \item \code{call}: Original function call
#' }
#'
#'
#' @keywords pcqm density estimation moments
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
#'
#' moment_result <- adjusted_moments(
#'   distances = distances_matrix,
#'   C = 20,
#'   q = 4,
#'   l = 2,
#'   init_method = "Pollard_censored"
#' )
#'
#'moment_result
#'
#'
#' @export

adjusted_moments <- function(distances,
                             C = 20,
                             q = 4,
                             l = 1,
                             init_method = "Pollard_censored") {

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
      list(
        DK_censored = NA_real_,
        Cottam_censored = NA_real_,
        Pollard_censored = NA_real_,
        Shen_censored = NA_real_,
        Morisita_censored = NA_real_,
        k_hat = NA_real_,
        censored_rate = NA_real_,
        n_points = 0,
        n_sectors = 0,
        n_censored = 0,
        call = match.call()
      ),
      class = "pcqm_moments"
    ))
  }

  # ==================== 2. Vector-based Processing ====================
  if (length(r_vec) %% q != 0) {
    stop("Vector length must be divisible by q")
  }

  n_points <- length(r_vec) / q
  total_sectors <- length(r_vec)

  # Check validity of init_method for given l
  if (l > 1 && init_method == "DK_censored") {
    stop("When l > 1, init_method cannot be 'DK_censored'. ",
         "DK estimator is only defined for l = 1. ",
         "Please use 'Cottam_censored' or 'Pollard_censored' instead.")
  }

  # ==================== 3. Censoring Structure (Vector-based) ====================
  censored <- (r_vec > C) | is.na(r_vec) | is.infinite(r_vec)
  censored_sectors <- sum(censored)
  n_censored <- censored_sectors
  censored_rate <- censored_sectors / total_sectors

  R_non <- r_vec[!censored]
  n_obs <- length(R_non)

  # If all distances are censored
  if (n_obs == 0) {
    warning("All observations are censored.")

    result <- list(
      DK_censored = NA_real_,
      Cottam_censored = NA_real_,
      Pollard_censored = NA_real_,
      Shen_censored = NA_real_,
      Morisita_censored = NA_real_,
      k_hat = NA_real_,
      censored_rate = censored_rate,
      n_points = n_points,
      n_sectors = total_sectors,
      n_censored = n_censored,
      call = match.call()
    )

    return(structure(result, class = "pcqm_moments"))
  }

  # ==================== 4. Basic Statistics ====================
  hat_P_base <- 1 - censored_rate
  hat_M1 <- mean(R_non)
  hat_M2 <- mean(R_non^2)

  # Solve hat_theta_base
  if (hat_P_base == 1) {
    hat_theta_base <- Inf
  } else if (hat_P_base == 0) {
    hat_theta_base <- 0
  } else {
    hat_theta_base <- qgamma(hat_P_base, shape = l, lower.tail = TRUE)
  }

  # ==================== 5. Gamma Ratios ====================
  if (is.infinite(hat_theta_base)) {
    gamma_ratio1 <- 1
    gamma_ratio2 <- 1
  } else {
    lower_gamma1 <- pgamma(hat_theta_base, shape = l + 0.5, lower.tail = TRUE) * gamma(l + 0.5)
    gamma_ratio1 <- gamma(l + 0.5) / lower_gamma1

    lower_gamma2 <- pgamma(hat_theta_base, shape = l + 1, lower.tail = TRUE) * gamma(l + 1)
    gamma_ratio2 <- gamma(l + 1) / lower_gamma2
  }

  # ==================== 6. Adjusted Moments ====================
  tilde_M1 <- hat_P_base * hat_M1 * gamma_ratio1
  tilde_M2 <- hat_P_base * hat_M2 * gamma_ratio2

  # ==================== 7. Basic Estimators ====================
  # DK_censored (only if l = 1)
  DK_censored <- if (l == 1) {
    q * hat_P_base / (4 * hat_M1^2)
  } else {
    NA_real_
  }

  # Cottam_censored
  Cottam_censored <- q * l / (4 * tilde_M1^2)

  # Pollard_censored
  Pollard_censored <- (n_points * q * l - 1) / (pi * n_points * tilde_M2)

  # ==================== 8. Advanced Estimators ====================
  # Select initial lambda
  if (init_method == "DK_censored" && l == 1) {
    lambda_init <- DK_censored
  } else if (init_method == "Cottam_censored") {
    lambda_init <- Cottam_censored
  } else {
    lambda_init <- Pollard_censored
  }

  # Initialize advanced estimators
  Shen_censored <- NA_real_
  Morisita_censored <- NA_real_
  k_hat <- NA_real_

  if (!is.na(lambda_init) && lambda_init > 0) {
    # Compute required expectations - ADD q AS PARAMETER
    result_neg1 <- compute_hat_E(R_non, censored_sectors, total_sectors, C, lambda_init, l, -1, q)
    hat_E_neg1 <- result_neg1$hat_E

    result_1 <- compute_hat_E(R_non, censored_sectors, total_sectors, C, lambda_init, l, 1, q)
    hat_E_1 <- result_1$hat_E

    result_2 <- compute_hat_E(R_non, censored_sectors, total_sectors, C, lambda_init, l, 2, q)
    hat_E_2 <- result_2$hat_E

    # Morisita_censored (only if l != 1)
    if (l != 1) {
      result_neg2 <- compute_hat_E(R_non, censored_sectors, total_sectors, C, lambda_init, l, -2, q)
      hat_E_neg2 <- result_neg2$hat_E
      Morisita_censored <- q * (l - 1) / pi * hat_E_neg2
    }

    # Shen_censored
    Shen_censored <- q * (2 * l - 1) * hat_E_neg1 / (pi * hat_E_1) - q * l / (pi * hat_E_2)

    # Compute k_hat
    denom <- hat_E_neg1 * hat_E_2 * (1 - 2 * l) + 2 * hat_E_1 * l
    if (!is.na(denom) && abs(denom) >= 1e-12) {
      k_hat <- 1 - hat_E_1 * l / denom
    }

    # If k_hat invalid, set Shen to NA
    if (is.na(k_hat) || k_hat < 1) {
      Shen_censored <- NA_real_
    }
  }

  # ==================== 9. Assemble Results ====================
  result <- list(
    DK_censored = DK_censored,
    Cottam_censored = Cottam_censored,
    Pollard_censored = Pollard_censored,
    Shen_censored = Shen_censored,
    Morisita_censored = Morisita_censored,
    k_hat = k_hat,
    censored_rate = censored_rate,
    n_points = n_points,
    n_sectors = total_sectors,
    n_censored = n_censored,
    call = match.call()
  )

  structure(result, class = "pcqm_moments")
}












