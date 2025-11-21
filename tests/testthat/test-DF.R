test_that("DF function works correctly", {
  # 1. Create a simple matrix with orthogonal columns for easy calculation
  # If columns are orthogonal, singular values are just column norms.
  # Column 1: norm sqrt(1+1) = 1.414
  # Column 2: norm sqrt(1+1) = 1.414
  X_dummy <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)

  # 2. Test Case A: lambda = 0
  # When lambda = 0, Ridge is OLS. DF should equal number of columns (p=2).
  df_ols <- DF(lambda = 0, X = X_dummy)
  expect_equal(df_ols, 2)

  # 3. Test Case B: Large lambda
  # When lambda is huge, coefficients are shrunk to near 0. DF should approach 0.
  df_shrink <- DF(lambda = 10000, X = X_dummy)
  expect_lt(df_shrink, 0.01)

  # 4. Test Case C: Specific manual calculation
  # Singular values of identity matrix are all 1.
  # Formula: sum( d^2 / (d^2 + lambda) )
  # Let lambda = 1. d = 1.
  # Each term: 1 / (1 + 1) = 0.5. Sum of 2 terms = 1.0.
  X_identity <- diag(2)
  expect_equal(DF(lambda = 1, X = X_identity), 1)

  # 5. Input validation
  expect_error(DF(lambda = "a", X = X_dummy))
  expect_error(DF(lambda = 1, X = "b"))
})
