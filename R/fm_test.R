#' Function to do Fama-MacBeth cross-sectional tests using monthly
#' rolling approach and single regression approach
#'
#' @param port.exc.ret.month one-month ahead monthly return for each portfolio
#' @param fact.load.lst a list of factor loadings for each portfolio for each type of models
#' @param subsample indicators of which out-of-sample period should be used; default is to use all periods
#'
#' @return a data frame in the form of each panel in Table 2 in the paper
#' @export
#'
#' @examples
#' y <- matrix(rnorm(50, 0, 1), 10, 5)
#' fact.load.1 <- list(factor.loading = matrix(rnorm(100, 0, 1), 20, 5),
#'                     names.variable = c("p1", "p2"))
#'
#' fact.load.lst <- list(Model1 = fact.load.1)
#' test_res <- fm_test(y, fact.load.lst)
#'
fm_test <- function(port.exc.ret.month, fact.load.lst, subsample = NULL){
  # get number of estimated models
  if(is.null(dim(port.exc.ret.month))){
    port.exc.ret.month = array(port.exc.ret.month, c(length(port.exc.ret.month), 1))
  }
  num.mod <- nrow(port.exc.ret.month)

  # get the average monthly portfolio excess return
  port.exc.ret.avg = colMeans(port.exc.ret.month)

  # Monthly Rolling Approach ##
  # indices of months in subsample
  if(!is.null(subsample)){
    idx.month <- c(1:num.mod)[subsample]
  }else{
    idx.month <- c(1:(num.mod))
  }

  n_model <- length(fact.load.lst)
  model_names <- names(fact.load.lst)

  variable_names <- vector("list", n_model)
  coef.monroll.m <- vector("list", n_model)
  tval.monroll.m <- vector("list", n_model)
  # Summary.models <- vector("list", n_model)
  Summary.tab <- vector("list", n_model)
  r2.m <- rep(0.0, n_model)

  for (i in 1:n_model) {
    # get model name
    name <- model_names[i]

    # load the factor loadings
    fact.load.m <- fact.load.lst[[name]]$factor.loading
    VarNames.m <- fact.load.lst[[name]]$names.variable
    variable_names[[i]] <- VarNames.m

    # Monthly rolling
    res.monroll.m <- t(sapply(idx.month, FUN = MonthRoll, simplify = 'array', port.exc.ret.month, fact.load.m))
    colnames(res.monroll.m)[-1] <- VarNames.m

    # get mean
    coef.monroll.m[[i]] <- t(colMeans(res.monroll.m))

    # get t statistics
    tval.monroll.m[[i]] <- t(apply(res.monroll.m, 2, FUN = function(x){t.test(x)$statistic}))

    Summary.tab[[i]] <- cbind(t(coef.monroll.m[[i]]), t(tval.monroll.m[[i]]))
    colnames(Summary.tab[[i]]) <- c("mean.coef", "t.value")
    rownames(Summary.tab[[i]])[1] <- "Intercept"

    # Single regression approach for ZCAPM_ini with alpha.ini = mean(y-signal)
    r2.m[i] <- SingleReg(port.exc.ret.avg, fact.load.m, num.mod, idx.month)

    # Summary.models[[i]] <- list(coeffs = coef.monroll.m[[i]], tval = tval.monroll.m[[i]], r2 = r2.m[[i]])
  }

  # names(Summary.models) <- model_names

  # output
  res <- list(Summary.models = Summary.tab,
              r2 = r2.m,
              model.names = model_names,
              variables = variable_names,
              num.mod = num.mod)
              # Summary.models = Summary.models)
  return(res)
}

