#' Calculate the Ridge Regression Cost Function
#'
#' This function computes the penalized minus log-likelihood for a
#' Ridge regression model. This is the cost function to be *minimized*.
#'
#' @param theta A numeric vector of model parameters (β).
#' @param sigma A single numeric value for the error standard deviation.
#' @param X A numeric matrix of predictors (training data).
#' @param y A numeric vector of the response (training data).
#' @param lambda A single numeric value for the penalty parameter.
#'
#' @return A single numeric value: the total cost (Minus Log-Likelihood + Ridge penalty).
#'
#' @export
Ridge <- function(theta, sigma, X, y, lambda) {
  if (missing(theta)) {
    stop("Please input theta, the 1st parameter.")
  }
  if (missing(sigma)) {
    stop("Please input sigma, the 2nd parameter.")
  }
  if (missing(X)) {
    stop("Please input X, the 3rd parameter.")
  }
  if (missing(y)) {
    stop("Please input y, the 4th parameter.")
  }
  if (missing(lambda)) {
    stop("Please input lambda, the 5th parameter.")
  }
  if (!(is.numeric(theta) && length(theta) >= 1)) {
    stop("Invalid input. The parameter theta should be a numeric vector")
  }
  if (!(is.numeric(sigma) && length(sigma) == 1 && sigma > 0)) {
    stop("Invalid input. The parameter sigma should be a numeric scalar")
  }
  if (!((is.matrix(X) || is.data.frame(X)) && is.numeric(X))) {
    stop("Invalid input. The parameter X should be a numeric matrix")
  }
  if (!(is.numeric(y) && length(y) >= 1)) {
    stop("Invalid input. The parameter y should be a numeric vector")
  }
  if (!(is.numeric(lambda) && length(lambda) == 1 && lambda >= 0)) {
    stop(
      "Invalid input. The parameter lambda should be a numeric scalar equal to or greater than zero"
    )
  }
  X <- as.matrix(X)
  if (nrow(X) != length(y)) {
    stop(paste0(
      "Dimension mismatch: nrow(X) is ",
      nrow(X),
      " but length(y) is ",
      length(y),
      "."
    ))
  }
  if (ncol(X) != length(theta)) {
    stop(
      paste0(
        "Dimension mismatch: ncol(X) is ",
        ncol(X),
        " but length(theta) is ",
        length(theta),
        "."
      )
    )
  }
  # Call the log likelihood function created for assignment 2‘s 3a
  # and flip the sign of its output to get the minus log likelihood function
  minus_log_likelihood <- -LogLikelihood(theta, sigma, X, y)
  # Add the Ridge penalty to the minus_log_likelihood to get total cost
  total_cost <- minus_log_likelihood + (lambda * sum(theta^2))
  return(total_cost)
}
