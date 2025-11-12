# This script tests the Ridge function

# Load the testthat library
library(testthat)
# Load the functions from your package
# If not loaded, use:
# source(here::here("R", "LogLikelihood.R"))
# source(here::here("R", "Ridge.R"))

# --- 1. Create a small, reproducible dataset for testing ---
X_test_data <- as.matrix(c(1, 2, 3)) # n=3, p=1
y_test_data <- c(1.1, 1.9, 3.1)
theta_test <- c(1)  # A single beta
sigma_test <- 0.1

# --- 2. Test 1: Check that Ridge(lambda=0) is just -LogLikelihood ---
test_that("Ridge cost with lambda = 0 equals -LogLikelihood", {

  lambda_zero <- 0

  # Get the raw minus-log-likelihood
  expected_cost <- -LogLikelihood(theta_test, sigma_test, X_test_data, y_test_data)

  # Get the Ridge cost
  ridge_cost <- Ridge(theta_test, sigma_test, X_test_data, y_test_data, lambda_zero)

  expect_equal(ridge_cost, expected_cost)
})

# --- 3. Test 2: Check that penalty is added correctly ---
test_that("Ridge cost correctly adds penalty", {

  lambda_one <- 1

  # Calculate penalty: lambda * sum(theta^2) = 1 * (1^2) = 1
  penalty <- lambda_one * sum(theta_test^2)

  # Calculate expected cost
  expected_cost <- -LogLikelihood(theta_test, sigma_test, X_test_data, y_test_data) + penalty

  # Get the Ridge cost
  ridge_cost <- Ridge(theta_test, sigma_test, X_test_data, y_test_data, lambda_one)

  expect_equal(ridge_cost, expected_cost)
})

# --- 4. Test 3: Check guard clauses ---
test_that("Ridge errors on bad lambda", {

  # Test for bad lambda (must be >= 0)
  expect_error(Ridge(theta_test, sigma_test, X_test_data, y_test_data, -1))
  expect_error(Ridge(theta_test, sigma_test, X_test_data, y_test_data, "a"))
})
