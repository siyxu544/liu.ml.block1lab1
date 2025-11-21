test_that("RidgeOpt works correctly with basic input", {
  # 1. Create dummy data
  X_dummy <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2) # 2x2 matrix
  y_dummy <- c(10, 20)
  lambda_val <- 1

  # 2. Run the function
  # We wrap this in expect_error(..., NA) to assert "No Error occurs"
  expect_error(result <- RidgeOpt(lambda_val, X_dummy, y_dummy), NA)

  # 3. Check return structure
  expect_type(result, "list")
  expect_true("theta" %in% names(result))
  expect_true("sigma" %in% names(result))

  # 4. Check dimensions and types
  expect_true(is.numeric(result$theta))
  expect_true(is.numeric(result$sigma))
  expect_equal(length(result$theta), ncol(X_dummy))
  expect_equal(length(result$sigma), 1)

  # 5. Check logic: sigma should be positive
  expect_gt(result$sigma, 0)
})

test_that("RidgeOpt handles lambda = 0 (should be close to OLS logic)", {
  # We add slight noise to prevent sigma from converging to 0.
  # If sigma -> 0, the log-likelihood goes to infinity, causing optim to crash.
  X_dummy <- matrix(c(1, 1, 2, 2), nrow = 4, ncol = 1)
  # Perfect fit was: c(2, 2, 4, 4).
  # Add tiny noise:
  y_dummy <- c(2.01, 1.99, 4.01, 3.99)

  # With lambda = 0, Ridge is essentially Maximum Likelihood
  # We expect no error
  expect_error(result <- RidgeOpt(lambda = 0, X_dummy, y_dummy), NA)

  # Theta should be close to 2 (Slope is approx 2)
  expect_equal(as.numeric(result$theta), 2, tolerance = 0.1)

  # Sigma should be calculable and positive (approx sd of the noise 0.01)
  expect_gt(result$sigma, 0)
})
