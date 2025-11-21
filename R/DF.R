#' Calculate Degrees of Freedom for Ridge Regression
#'
#' This function computes the effective degrees of freedom (DF) for a
#' Ridge regression model given a penalty parameter lambda.
#'
#' @param lambda A numeric scalar for the penalty parameter.
#' @param X A numeric matrix of predictors (training data).
#'
#' @return A single numeric value: the degrees of freedom.
#'
#' @export
DF <- function(lambda, X) {
  # Input validation
  if (missing(lambda)) {
    stop("Please input lambda, the 1st parameter.")
  }
  if (!(is.numeric(lambda) && length(lambda) == 1 && lambda >= 0)) {
    stop(
      "Invalid input. The parameter lambda should be a numeric scalar equal to or greater than zero"
    )
  }
  if (missing(X)) {
    stop("Please input X, the 2nd parameter.")
  }
  if (!((is.matrix(X) || is.data.frame(X)) && is.numeric(X))) {
    stop("Invalid input. The parameter X should be a numeric matrix")
  }

  X <- as.matrix(X)

  # Compute Singular Value Decomposition (SVD) to get singular values (d)
  # This is numerically more stable than calculating matrix inverses
  svd_result <- svd(X)
  d <- svd_result$d

  # Calculate Degrees of Freedom using the formula:
  # sum( d_i^2 / (d_i^2 + lambda) )
  df_value <- sum((d^2) / (d^2 + lambda))

  return(df_value)
}
