#' Estimation of the proportion of cases in a group choosing an item
#'
#' Estimates a Bayesian multivariate (several outcomes) regression
#' (using Stan) to calculate the proportion of cases in a given group
#' choosing a given item
#'
#' @param formula An object of class formula with the choice on the
#'   LHS and the grouping variable on the RHS. Both variables are
#'   expected to be factors.
#' @param weights The variable in \code{data} containing the case weights
#' @param data An optional data frame containing the variables and
#'   weights in the model.
#' @param priors A table or a matrix with the vote share for each
#'   choice in each group for the model. The names of the first
#'   dimension (rows) should match the levels of the LHS of the
#'   formula; and the names of the second dimension (columns), the
#'   RHS.
#' @param sd A standard deviation for each the prior
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter,
#'   chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @importFrom stats aggregate model.matrix model.response model.weights
#'
#' @details The function passes data to a Stan model that calculates
#'   the multivariate (i.e., with several outcomes) Bayesian linear
#'   regression in the formula. The coefficients of the regressors are
#'   constrained to sum to one. The priors passed by the user (the
#'   proportion choosing an item in a given group) are transformed
#'   into a Beta with means \code{priors} and variance \code{sd}.
#' 
#' @export
dshare <- function(formula, data, weights, priors, sd, ...) {
  
  mf <- match.call(expand.dots=FALSE)
  m <- match(c("formula", "data", "subset", "weights"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  variables <- all.vars(mf[[2L]])

  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  yres <- model.response(mf)

  if (!inherits(yres, "factor")) {
    stop("Response variable must be a factor")
  }

  y <- model.matrix(~ yres - 1)
  w <- as.vector(model.weights(mf))
  if (!is.null(w) && !is.numeric(w)) {
    stop(sprintf("%s must be a numeric vector"),
         sQuote("weights"))
  }
  mt <- attr(mf, "terms")
  attr(mt, "intercept") <- 0
  tvar <- attr(mt, "term.labels")
  if (length(tvar) != 1) {
    stop("More than one regressor found")
  }

  if (attr(mt, "dataClasses")[tvar] != "factor") {
    stop("Regressor variable must be a factor")
  }

  mt <- model.matrix(mt, mf)

  ## Priors
  if (!inherits(priors, "matrix") || !inherits(priors, "array")) {
    stop(sprintf("%s must be a matrix or a table",
                 sQuote("priors")))
  }

  datanames <- dimnames(table(mf[[1]], mf[[2]]))
  nchoice <- dimnames(priors)[[1]]
  ndistrict <- dimnames(priors)[[2]]

  if (!isTRUE(all.equal(nchoice, datanames[[1]]))) {
    stop(sprintf("The levels of %s and first dimension of %s do not match",
                 sQuote(variables[1]),
                 sQuote("priors")))
  }
  if (!isTRUE(all.equal(ndistrict, datanames[[2]]))) {
    stop(sprintf("The levels of %s and second dimension of %s do not match",
                 sQuote(variables[2]),
                 sQuote("priors")))
  }

  totals <- colSums(priors)
  totals <- as.vector(totals)

  if (!all.equal(totals, rep(1, length(totals)))) {
    stop("Priors for each district must add up to 1")
  }
  
  data <- cbind(weight=w, y, mt) 
  res <- aggregate(weight ~ ., data, sum)
  
  priors <- dshare:::beta_params(priors, sd^2)
  alpha <- priors$alpha
  beta <- priors$beta
  
  ## Data object to pass to Stan
  data <- list(results=res[, grepl(paste("yres", collapse="|"), names(res))],
               dummies=res[, grepl(paste(variables[2], collapse="|"), names(res))],
               weights=res[, "weight"],
               a=t(alpha), 
               b=t(beta),  
               D=ncol(mt),  
               P=ncol(y), 
               N=nrow(res))

  fit <- rstan::sampling(stanmodels$dshare,
                         data=data,
                         ...)
  
  return(fit)
}
