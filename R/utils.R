#' @noRd
# ==================== Internal Helper Function 1: Compute Expectations ====================
compute_hat_E <- function(R_non, censored_sectors, total_sectors, C, lambda_init, l, v,q) {
  sum_v <- sum(R_non^v)
  if (censored_sectors == 0) {
    return(list(hat_E = mean(R_non^v), cond_E = NA))
  }

  theta_init <- pi * lambda_init * C^2 / q

  if (theta_init > 1e8) {
    cond_E <- C^v
  } else {
    shape_v <- l + v/2
    upper_gamma_lv <- pgamma(theta_init, shape = shape_v, lower.tail = FALSE) * gamma(shape_v)
    upper_gamma_l <- pgamma(theta_init, shape = l, lower.tail = FALSE) * gamma(l)

    if (upper_gamma_l <= 0 || is.nan(upper_gamma_l)) {
      cond_E <- C^v
    } else {
      ratio <- upper_gamma_lv / upper_gamma_l
      cond_E <- (q / (pi * lambda_init))^(v/2) * ifelse(is.finite(ratio), ratio, 0)
    }
  }

  hat_E <- (sum_v + censored_sectors * cond_E) / total_sectors
  return(list(hat_E = hat_E, cond_E = cond_E))
}


#' @noRd
# ==================== Internal Helper Function 2: Initial Value Calculation ====================
simplified_PDEs_nbd <- function(all_r, C_lim = 20, q = 4, l = 2, init_function = "Shen_censored") {
  # 1. Standardize input format
  if (is.vector(all_r)) {
    if (length(all_r) %% q != 0) stop("Vector length must be divisible by number of sectors q")
    all_r <- matrix(all_r, ncol = q, byrow = TRUE)
  }
  n <- nrow(all_r)
  total_sectors <- n * q

  # 2. Censoring identification and basic statistics
  censored <- (all_r > C_lim) | is.na(all_r) | is.infinite(all_r)
  censored_sectors <- sum(censored)
  non_censored_num <- total_sectors - censored_sectors
  R_non <- all_r[!censored]

  # Return default values if no valid data
  if (non_censored_num == 0) {
    return(list(k_init = 2, lambda_init = 0.1, lambda_shen = 0.1))
  }

  # 3. Basic moment estimation
  hat_M1 <- mean(R_non)
  hat_M2 <- mean(R_non^2)
  hat_P_base <- 1 - censored_sectors / total_sectors

  # 4. Compute gamma_ratio1/gamma_ratio2
  if (hat_P_base %in% c(0, 1)) {
    hat_theta_base <- 1
  } else {
    hat_theta_base <- qgamma(hat_P_base, shape = l, lower.tail = TRUE)
  }

  # gamma_ratio1 (for Cottam)
  gamma_ratio1 <- if (hat_theta_base > 1e8) {
    1
  } else {
    lower_gamma1 <- pgamma(hat_theta_base, shape = l + 0.5) * gamma(l + 0.5)
    gamma(l + 0.5) / lower_gamma1
  }

  # gamma_ratio2 (for Pollard's tilde_M2)
  gamma_ratio2 <- if (hat_theta_base > 1e8) {
    1
  } else {
    lower_gamma2 <- pgamma(hat_theta_base, shape = l + 1) * gamma(l + 1)
    gamma(l + 1) / lower_gamma2
  }

  # Adjusted moments
  tilde_M1 <- hat_P_base * hat_M1 * gamma_ratio1
  tilde_M2 <- hat_P_base * hat_M2 * gamma_ratio2

  # 5. Compute initial values
  # 5.1 DK_censored (only for l=1)
  lambda_dk <- if (l == 1) max(q * hat_P_base / (4 * hat_M1^2), 0.001) else NA

  # 5.2 Cottam_censored
  lambda_cottam <- max(q * l / (4 * tilde_M1^2), 0.001)

  # 5.3 Pollard_censored
  lambda_pollard <- (n * q * l - 1) / (pi * n * tilde_M2)
  lambda_pollard <- max(lambda_pollard, 0.001)

  # 5.4 Shen_censored
  temp_lambda <- lambda_pollard
  res_neg1 <- compute_hat_E(R_non, censored_sectors, total_sectors, C_lim, temp_lambda, l, v = -1,q=q)
  hat_E_neg1 <- res_neg1$hat_E
  res_1 <- compute_hat_E(R_non, censored_sectors, total_sectors, C_lim, temp_lambda, l, v = 1,q=q)
  hat_E_1 <- res_1$hat_E
  res_2 <- compute_hat_E(R_non, censored_sectors, total_sectors, C_lim, temp_lambda, l, v = 2,q=q)
  hat_E_2 <- res_2$hat_E

  # Compute k_hat
  denom <- hat_E_neg1 * hat_E_2 * (1 - 2 * l) + 2 * hat_E_1 * l
  k_hat <- if (abs(denom) < 1e-12) NA else 1 - hat_E_1 * l / denom

  # Compute Shen_censored
  lambda_shen <- if (is.na(k_hat) || k_hat < 1) {
    NA
  } else {
    max(q * (2 * l - 1) * hat_E_neg1 / (pi * hat_E_1) - q * l / (pi * hat_E_2), 0.001)
  }

  # 6. Determine final initial values
  if (init_function == "DK_censored" && l == 1) {
    lambda_init <- lambda_dk
    k_init <- if (is.na(k_hat) || is.infinite(k_hat)) 1 else max(k_hat, 0.5)
  } else if (init_function == "Cottam_censored") {
    lambda_init <- lambda_cottam
    k_init <- if (is.na(k_hat) || is.infinite(k_hat)) 1 else max(k_hat, 0.5)
  } else if (init_function == "Pollard_censored") {
    lambda_init <- lambda_pollard
    k_init <- if (is.na(k_hat) || is.infinite(k_hat)) 1 else max(k_hat, 0.5)
  } else {  # Default Shen_censored
    lambda_init <- if (!is.na(lambda_shen)) lambda_shen else lambda_pollard
    k_init <- if (is.na(k_hat) || is.infinite(k_hat)) {
      1
    } else {
      max(k_hat, 0.5)
    }
  }

  return(list(k_init = k_init, lambda_init = lambda_init))
}

