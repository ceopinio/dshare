#' Estimation of vote share at the district level
#'
#' @param formula An object of class formula with the choice on the
#'   LHS and the grouping variable on the RHS
#' @param weights The variable in \code{data} containing the weights
#' @param data An optional data frame containing the varaibles in the
#'   model
#' @param P A table or a matrix with the priors
#' @param var A variance
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter,
#'   chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @importFrom stats aggregate model.matrix model.response model.weights
#' 
#' @export
dshare <- function(formula, data, weights, P, var, ...) {

  if (!inherits(P, "table") & !inherits(P, "matrix")) {
    stop(sprintf("%s must be a table or a matrix",
                 sQuote("P")))
  }
  
  totals <- as.vector(colSums(P))
  
  if (!all.equal(totals, rep(1, length(totals)))) {
    stop("Priors for each district must add up to 1")
  }
  
  nchoice <- dimnames(P)[[1]]
  ndistrict <- dimnames(P)[[2]]

  mf <- match.call(expand.dots=FALSE)
  m <- match(c("formula", "data", "subset", "weights"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  yres <- model.response(mf)

  if (!inherits(yres, "factor")) {
    stop("Response variable must be a factor")
  }
  if (!setequal(levels(yres), nchoice) &
        any(levels(yres) != nchoice)) {
    stop("Levels of the response variable do not match the priors")
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
    stop("Response variable must be a factor")
  }
  if (!setequal(levels(mf[, tvar]), ndistrict) |
        any(levels(mf[, tvar]) != ndistrict)) {
    stop("Levels of the response variable do not match the priors")
  }
  mt <- model.matrix(mt, mf)

  data <- cbind(weight=w, y, mt) 
  res <- aggregate(weight ~ ., data, sum)
  
  priors <- dshare:::beta_params(P, var^2)
  alpha <- priors$alpha
  beta <- priors$beta
  
  ## Data object to pass to Stan
  data <- list(results=res[, grepl(paste(nchoice, collapse="|"), names(res))],
               dummies=res[, grepl(paste(ndistrict, collapse="|"), names(res))],
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
