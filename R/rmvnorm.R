#' Random Multivariate Normal Draw
#'
#' This is a stripped down version of a multivariate random number
#' generator.  All draws have mean 0 and standard deviation 1.
#' @param n The number of draws
#' @param rho Correlation between draws
#' @param mu Mean of random variables (scalar)
#' @param sd Standard deviation of random variables
#' @return
rmvnorm <- function(n, rho, mu = 0, sd = 1) {
  if (!is.numeric(n) | !is.numeric(rho) | length(n) != 1L | length(rho) != 1L)
    stop('Must supply numeric scalars for n and rho to rmvnorm.')
  if (rho < -1 | rho > 1)
    stop('rho must be betwee -1 and +1.')

  ret_val <- matrix(rnorm(n * 2), n, 2)
  ret_val[, 2] <- rho * ret_val[, 1] + sqrt(1 - (rho * rho)) * ret_val[, 2]

  (ret_val * sd) + mu
}

