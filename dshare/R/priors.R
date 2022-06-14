#' Calculate Beta parameters for given mean and standard deviation
#'
#' @param mu Target mean
#' @param var Target variance
#' @return A list with parameters alpha and beta of a Beta distributin
#'   with the input mean and standard deviation
beta_params <- function(mu, var) {
  alpha <- ((1 - mu)/var - (1 / mu))*mu^2
  beta <- alpha * (1/mu - 1)
  return(list(alpha=alpha, beta=beta))
}
