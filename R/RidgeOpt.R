#' Optimize Ridge Regression Parameters
#'
#' This function uses numerical optimization (BFGS) to find the optimal
#' coefficients (theta) and standard deviation (sigma) for a given penalty parameter lambda.
#'
#' @param lambda A numeric scalar for the penalty parameter.
#' @param X A numeric matrix of predictors (training data).
#' @param y A numeric vector of the response (training data).
#'
#' @return A list containing:
#' \item{theta}{The estimated optimal coefficient vector.}
#' \item{sigma}{The estimated optimal standard deviation.}
#'
#' @export
RidgeOpt <- function(lambda, X, y) {
  # Input validation
  if (missing(lambda)) {
    stop("Please input lambda, the 1st parameter.")
  }
  if (missing(X)) {
    stop("Please input X, the 2nd parameter.")
  }
  if (missing(y)) {
    stop("Please input y, the 3rd parameter.")
  }
  if (!(is.numeric(lambda) && length(lambda) == 1 && lambda >= 0)) {
    stop(
      "Invalid input. The parameter lambda should be a numeric scalar equal to or greater than zero"
    )
  }
  if (!((is.matrix(X) || is.data.frame(X)) && is.numeric(X))) {
    stop("Invalid input. The parameter X should be a numeric matrix")
  }
  if (!(is.numeric(y) && length(y) >= 1)) {
    stop("Invalid input. The parameter y should be a numeric vector")
  }
  # Ensure X is matrix and y is vector
  X <- as.matrix(X)
  y <- as.vector(y)

  # Initialize parameters for optim()
  # params vector = [theta_1, theta_2, ..., theta_p, sigma]
  p <- ncol(X)
  initial_theta <- rep(0, p)    # Start theta at 0
  initial_sigma <- sd(y)        # Start sigma at standard deviation of y
  initial_params <- c(initial_theta, initial_sigma)

  # Run optimization
  opt_results <- optim(
    par = initial_params,
    fn = function(params) {
      len <- length(params)
      current_sigma <- params[len]        # Last element is sigma
      current_theta <- params[1:(len-1)]  # All previous elements are theta
      # optim() might try negative sigma. We should return Inf instead of crashing.
      if (current_sigma <= 0) {
        return(Inf)
      }
      return(Ridge(current_theta, current_sigma, X, y, lambda))
    },
    method = "BFGS"
  )

  # Extract best parameters
  final_params <- opt_results$par
  len <- length(final_params)
  best_sigma <- final_params[len]
  best_theta <- final_params[1:(len-1)]

  return(list(theta = best_theta, sigma = best_sigma))
}
