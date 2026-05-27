# Test csr_mle function with real PCQM sampling data
test_that("csr_mle handles real 20-row PCQM sampling data", {
  # 1. Replicate real data structure (使用与nbd_mle测试相同的数据)
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
  res <- csr_mle(
    distances = real_pcqm_data,
    C = 20,
    q = 4,
    l = 2,
    lambda_lower = 1e-4,
    lambda_upper = 1
  )

  # 3. Assertions
  # 3.1 Verify return object type
  expect_s3_class(res, "pcqm_csr_mle")

  # 3.2 Verify lambda is a finite numeric value
  expect_type(res$lambda, "double")
  expect_true(is.finite(res$lambda))

  # 3.3 Verify log-likelihood is a finite numeric value
  expect_type(res$logLik, "double")
  expect_true(is.finite(res$logLik))

  # 3.4 Verify data dimensions and censoring statistics
  expect_equal(res$n_sectors, 20 * 4)
  expect_equal(res$n_obs, 9)
  expect_equal(res$n_censored, 71)
  expect_equal(round(res$censored_rate, 4), 0.8875)

  # 3.5 Verify lambda is within bounds
  expect_true(res$lambda >= 1e-4 && res$lambda <= 1)

  # 3.6 Verify call object
  expect_equal(class(res$call), "call")
})

# Additional tests: Edge cases (all censored / no censored)
test_that("csr_mle handles edge cases of real PCQM data", {
  # Scenario 1: All censored data
  all_censored <- matrix(9999, nrow = 20, ncol = 4)

  expect_warning(
    res_all_cens <- csr_mle(all_censored, C = 20, q = 4, l = 2),
    regexp = "All observations are censored; CSR MLE not identifiable."
  )

  # Verify lambda is NA for all censored
  expect_true(is.na(res_all_cens$lambda))

  # Scenario 2: No censored data (all distances within C)
  no_censored <- matrix(15, nrow = 20, ncol = 4)
  res_no_cens <- csr_mle(no_censored, C = 20, q = 4, l = 2)

  expect_equal(res_no_cens$censored_rate, 0)
  expect_equal(res_no_cens$n_censored, 0)
  expect_equal(res_no_cens$n_obs, 80)
  expect_true(is.finite(res_no_cens$lambda))
  expect_true(is.finite(res_no_cens$logLik))
})
