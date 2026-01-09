# Test adjusted_moments function with real PCQM sampling data
test_that("adjusted_moments handles real 20-row PCQM sampling data", {
  # 1. Replicate real data structure
  real_pcqm_data <- matrix(9999, nrow = 20, ncol = 4,
                           dimnames = list(NULL, c("NE", "NW", "SW", "SE")))

  # Fill with actual observed distances
  real_pcqm_data[2, c("NE", "NW")] <- c(19.01865, 9.999772)
  real_pcqm_data[5, "NW"] <- 18.776533
  real_pcqm_data[6, c("NE", "NW")] <- c(15.56287, 16.566480)
  real_pcqm_data[10, "SW"] <- 12.07625
  real_pcqm_data[14, c("SW", "SE")] <- c(17.21624, 19.25134)
  real_pcqm_data[19, "NE"] <- 12.10763

  # 2. Execute function call
  res <- adjusted_moments(
    distances = real_pcqm_data,
    C = 20,
    q = 4,
    l = 2,
    init_method = "Pollard_censored"
  )

  # 3. Assertions based on actual results
  # 3.1 Verify return object type
  expect_s3_class(res, "pcqm_moments")

  # 3.2 Verify specific values based on actual output
  # DK_censored should be NA when l > 1
  expect_true(is.na(res$DK_censored))

  # Cottam_censored should be around 0.001600619
  expect_true(is.finite(res$Cottam_censored))
  expect_true(abs(res$Cottam_censored - 0.001600619) < 1e-6)

  # Pollard_censored should be around 0.001799838
  expect_true(is.finite(res$Pollard_censored))
  expect_true(abs(res$Pollard_censored - 0.001799838) < 1e-6)

  # Shen_censored should be NA because k_hat < 1
  expect_true(is.na(res$Shen_censored))

  # Morisita_censored should be around 0.00170674
  expect_true(is.finite(res$Morisita_censored))
  expect_true(abs(res$Morisita_censored - 0.00170674) < 1e-6)

  # k_hat should be negative (around -92.20438)
  expect_true(is.finite(res$k_hat))
  expect_true(res$k_hat < 0)
  expect_true(abs(res$k_hat - (-92.20438)) < 1e-4)

  # 3.3 Verify data dimensions and censoring statistics
  expect_equal(res$n_sectors, 20 * 4)
  expect_equal(res$n_censored, 71)
  expect_equal(round(res$censored_rate, 4), 0.8875)

  # 3.4 Verify all estimators are non-negative when not NA (except k_hat can be negative)
  if (!is.na(res$Cottam_censored)) {
    expect_true(res$Cottam_censored >= 0)
  }
  if (!is.na(res$Pollard_censored)) {
    expect_true(res$Pollard_censored >= 0)
  }
  if (!is.na(res$Morisita_censored)) {
    expect_true(res$Morisita_censored >= 0)
  }

  # 3.5 Verify call object
  expect_equal(class(res$call), "call")
})

# Additional tests: Edge cases (all censored / no censored)
test_that("adjusted_moments handles edge cases of real PCQM data", {
  # Scenario 1: All censored data
  all_censored <- matrix(9999, nrow = 20, ncol = 4)

  expect_warning(
    res_all_cens <- adjusted_moments(all_censored, C = 20, q = 4, l = 2),
    regexp = "All observations are censored."
  )

  # Verify all estimators are NA
  expect_true(is.na(res_all_cens$DK_censored))
  expect_true(is.na(res_all_cens$Cottam_censored))
  expect_true(is.na(res_all_cens$Pollard_censored))
  expect_true(is.na(res_all_cens$Shen_censored))
  expect_true(is.na(res_all_cens$Morisita_censored))
  expect_true(is.na(res_all_cens$k_hat))

  # Verify censoring rate is 1
  expect_equal(res_all_cens$censored_rate, 1)

  # Scenario 2: No censored data
  no_censored <- matrix(15, nrow = 20, ncol = 4)
  res_no_cens <- adjusted_moments(no_censored, C = 20, q = 4, l = 2)

  expect_equal(res_no_cens$censored_rate, 0)
  expect_equal(res_no_cens$n_censored, 0)

  # Verify estimators are finite (but Shen might be NA depending on k_hat)
  expect_true(is.finite(res_no_cens$Cottam_censored))
  expect_true(is.finite(res_no_cens$Pollard_censored))
  expect_true(is.finite(res_no_cens$Morisita_censored))

  # DK_censored should be NA when l > 1
  expect_true(is.na(res_no_cens$DK_censored))

  # k_hat may be negative, causing Shen_censored to be NA
  if (!is.na(res_no_cens$Shen_censored)) {
    expect_true(is.finite(res_no_cens$Shen_censored))
    expect_true(res_no_cens$Shen_censored >= 0)
  }
})



