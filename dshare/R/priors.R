#' Calculate beta parameters for given mean and standard deviation
#' 
#' @param mu Target mean
#' @param var Target variance
#' @return A list 
beta_params <- function(mu, var) {
  alpha <- ((1 - mu)/var - (1 / mu))*mu^2
  beta <- alpha * (1/mu - 1)
  return(list(alpha=alpha, beta=beta))
}
