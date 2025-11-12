# This script tests the LogLikelihood function

# Load the testthat library
library(testthat)
# Load the function from your package (assuming it's loaded)
# If not loaded, use: source(here::here("R", "LogLikelihood.R"))

# --- 1. Create a small, reproducible dataset for testing ---
# We will use simple data where we can calculate the answer by hand
X_test_data <- as.matrix(c(1, 2, 3)) # n=3, p=1
y_test_data <- c(1.1, 1.9, 3.1)
theta_test <- c(1)  # A single beta
sigma_test <- 0.1

# --- 2. Test 1: Check if calculation is correct ---
test_that("LogLikelihood calculates the correct value", {

  # Calculate our predictions
  # y_hat = [1*1, 2*1, 3*1] = [1, 2, 3]
  y_hat_truth <- X_test_data %*% theta_test

  # Calculate the log-likelihoods "by hand" using dnorm
  # y = [1.1, 1.9, 3.1]
  # y_hat = [1, 2, 3]
  # sd = 0.1
  # We expect dnorm(1.1, 1, 0.1, log=T) + dnorm(1.9, 2, 0.1, log=T) + dnorm(3.1, 3, 0.1, log=T)

  expected_val <- sum(dnorm(y_test_data, mean = y_hat_truth, sd = sigma_test, log = TRUE))

  # Test our function
  calculated_val <- LogLikelihood(theta_test, sigma_test, X_test_data, y_test_data)

  # We use expect_equal() for numeric comparisons
  expect_equal(calculated_val, expected_val)
})

# --- 3. Test 2: Check guard clauses ---
test_that("LogLikelihood errors on bad input", {

  # Test for bad sigma (must be > 0)
  expect_error(LogLikelihood(theta_test, -1, X_test_data, y_test_data))
  expect_error(LogLikelihood(theta_test, "a", X_test_data, y_test_data))

  # Test for dimension mismatch
  expect_error(LogLikelihood(theta_test, sigma_test, X_test_data, y_test_data[1:2])) # y too short
  expect_error(LogLikelihood(c(1,2), sigma_test, X_test_data, y_test_data)) # theta too long
})
