# Test nbd_mle function with real PCQM sampling data (compatible with older testthat versions)
test_that("nbd_mle handles real 20-row PCQM sampling data", {
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
  res <- nbd_mle(
    distances = real_pcqm_data,
    C = 20,
    q = 4,
    l = 2,
    init_method = "Shen_censored",
    lambda_lower = 1e-4,
    lambda_upper = 1
  )

  # 3. Assertions compatible with older testthat versions
  # 3.1 Verify return object type (replaces deprecated expect_is)
  expect_s3_class(res, "pcqm_nbd_mle")
  # 3.2 Verify lambda is a finite numeric value
  expect_type(res$lambda, "double")
  expect_true(is.finite(res$lambda))
  # 3.3 Verify k is a finite numeric value
  expect_type(res$k, "double")
  expect_true(is.finite(res$k))
  # 3.4 Verify data dimensions and censoring statistics
  expect_equal(res$n_sectors, 20 * 4)
  expect_equal(res$n_obs, 9)
  expect_equal(res$n_censored, 71)
  expect_equal(round(res$censored_rate, 4), 0.8875)
})

# Additional tests: Edge cases (all censored / no censored)
test_that("nbd_mle handles edge cases of real PCQM data", {
  # Scenario 1: All censored data (specify warning pattern to ensure capture)
  all_censored <- matrix(9999, nrow = 20, ncol = 4)
  expect_warning(
    res_all_cens <- nbd_mle(all_censored, C = 20, q = 4, l = 2),
    regexp = "All observations are censored; NBD MLE not identifiable."
  )
  # Verify lambda is NA
  expect_true(is.na(res_all_cens$lambda))

  # Scenario 2: No censored data
  no_censored <- matrix(15, nrow = 20, ncol = 4)
  res_no_cens <- nbd_mle(no_censored, C = 20, q = 4, l = 2)
  expect_equal(res_no_cens$censored_rate, 0)
})
