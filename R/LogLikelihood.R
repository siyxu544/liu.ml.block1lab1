#' Calculate the Log-Likelihood for a Linear Model with Intercept of Zero
#'
#' This function computes the total log-likelihood for a linear model
#' y = theta*X + epsilon, assuming epsilon are i.i.d. Normal(0, sigma^2).
#'
#' @param theta A numeric vector of model parameters (β).
#' @param sigma A single numeric value for the error standard deviation.
#' @param X A numeric matrix of predictors (training data).
#' @param y A numeric vector of the response (training data).
#'
#' @return A single numeric value: the total log-likelihood.
#' @export
LogLikelihood <- function (theta, sigma, X, y) {
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
  if (!(is.numeric(theta) && length(theta) > 1)) {
    stop("Invalid input. The parameter theta should be a numeric vector")
  }
  if (!(is.numeric(sigma) && length(sigma) == 1)) {
    stop("Invalid input. The parameter sigma should be a numeric scalar")
  }
  if (!((is.matrix(X) || is.data.frame(X)) && is.numeric(X))) {
    stop("Invalid input. The parameter X should be a numeric matrix")
  }
  if (!(is.numeric(y) && length(y) > 1)) {
    stop("Invalid input. The parameter y should be a numeric vector")
  }
  X <- as.matrix(X)
  # Calculate the y^hat with predictors(X) and their coefficient theta (or β)
  y_hat <- X %*% theta

  # Calculate the log-likelihood for each observation by PDF function(dnorm()) of normal distribution
  log_likelihood <- dnorm(y, mean = y_hat, sd = sigma, log = True)

  # The sum of log-likelihood
  sum_log_likelihood <- sum(log_likelihood)
  return (sum_log_likelihood)
}
