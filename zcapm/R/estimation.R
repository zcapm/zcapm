#' EM regression: the EM algorithm for fitting
#' Estimate the coefficients for ZCAPM for given response and covariates
#'
#' @param month.idx A numeric number indicating current month
#' @param port.exc.ret A vector of portfolio excess return
#' @param mkt.exc.ret A vector of market excess return
#' @param mkt.sigma A vector of market sigma
#' @param tol A numeric number specifying tolerance for relative difference
#' @param MaxIter A numeric number specifying the largest iteration number
#' @param criterion Stopping criterion with 1 for absolute difference and 2 for relative difference
#' @param start_timestamp A numeric number indicating the start time point
#' @param end_timestamp A numeric number indicating the end time point
#'
#' @return A vector of coefficients' estimates
#' @export
#'
#' @examples
#' test.yyyymm <- c(rep(202001, 31), rep(202002, 29), rep(202003, 31))
#' month.list.test <- unique(test.yyyymm)
#' month.list.test <- month.list.test[-length(month.list.test)]
#' y <- runif(length(test.yyyymm), 0, 1)
#' test.width <- 2
#' data.factors <- matrix(runif(length(test.yyyymm) * 2, 0, 1), length(test.yyyymm), 2)
#' tol <- 1e-6
#' MaxIter <- 100
#' criterion <- 1
#' idx <- test.yyyymm %in% month.list.test[1]
#' EMRegression(month.list.test[1], y[idx], data.factors[idx, 1], data.factor[idx, 2], tol, MaxIter, criterion)
EMRegression <- function(port.exc.ret, mkt.exc.ret, mkt.sigma, tol, MaxIter, criterion, start_timestamp, end_timestamp){
  # initialize D and p
  resid.d <- resid(lm.fit(matrix(mkt.exc.ret, ncol = 1), port.exc.ret))
  D <- numeric(length(resid.d))
  D[resid.d >= 0] <- 1
  D[resid.d < 0] <- -1
  p.hat <- mean(D == 1)

  # initialize beta, Z, sigma_sq
  init.lm <- lm.fit(cbind(mkt.exc.ret, I(D * mkt.sigma)), port.exc.ret)
  beta.hat <- coef(init.lm)[1]
  Z.hat <- coef(init.lm)[2]
  sigma.sq.hat <- mean(resid(init.lm)^2)

  output <- EM_loop(beta.hat, Z.hat, sigma.sq.hat, p.hat, port.exc.ret, mkt.exc.ret, mkt.sigma, tol, MaxIter, criterion, start_timestamp, end_timestamp)
  return(output)
}

#' Estimation for ZCAPM
#'
#' This function select a period and then get the corresponding response and covariates to
#' get the ZCAPM estimates using EM.
#'
#' @param month.list A vector of complete month for the observations
#' @param yyyymm The year and month of the data to be selected
#' @param port.exc.ret A vector of portfolio excess return
#' @param factors, the 1st column for mkt.exc.ret, a vector of market excess return,
#'                 and the 2nd column for mkt.sigma A vector of market sigma
#' @param params A vector (tol, MaxIter, criterion) where
#'               * tol: A numeric number specifying the tolerance of the relative difference of estimates,
#'               * MaxIter: A numeric number specifying the maximum iteration number
#'               * criterion: criterion chosen for the stopping. 1 for absolute difference and 2 for relative difference
#'
#' @return A vector of estimates for coefficients
#' @export
#'
#' @examples
#' test.yyyymm <- c(rep(202001, 31), rep(202002, 29), rep(202003, 31))
#' month.list.test <- unique(test.yyyymm)
#' month.list.test <- month.list.test[-length(month.list.test)]
#' y <- runif(length(test.yyyymm), 0, 1)
#' test.width <- 2
#' data.factors <- matrix(runif(length(test.yyyymm) * 2, 0, 1), length(test.yyyymm), 2)
#' tol <- 1e-6
#' MaxIter <- 100
#' criterion <- 1
#' params <- c(tol, MaxIter, criterion)
#' estZCAPM(month.list.test[1], test.yyyymm, y, data.factors, params)
estZCAPM <- function(month.list, yyyymm, port.exc.ret, factors, params){
  idx <- (yyyymm %in% month.list)
  port.exc.ret <- port.exc.ret[idx]
  mkt.exc.ret <- factors[idx, 1]
  mkt.sigma <- factors[idx, 2]
  tol <- params[1]
  MaxIter <- params[2]
  criterion <- params[3]
  if (length(month.list) == 1){
    start_timestamp <- month.list
    end_timestamp <- month.list
  }else{
    start_timestamp <- month.list[1]
    end_timestamp <- month.list[length(month.list)]
  }
  return(EMRegression(port.exc.ret, mkt.exc.ret, mkt.sigma, tol, MaxIter, criterion, start_timestamp, end_timestamp))
}

#' Estimation for linear model
#'
#' @param month.list A vector of complete month for the observations
#' @param yyyymm The year and month of the data to be selected
#' @param port.exc.ret A vector of portfolio excess return
#' @param factors A vector of market excess return
#' @param intercept_include bool, return the intercept or not
#'
#' @return A vector of linear regression w/w.o. intercepts
#' @export
#'
#' @examples
#' test.yyyymm <- c(rep(202001, 31), rep(202002, 29), rep(202003, 31))
#' month.list.test <- unique(test.yyyymm)
#' month.list.test <- month.list.test[-length(month.list.test)]
#' y <- runif(length(test.yyyymm), 0, 1)
#' test.width <- 2
#' data.factors <- matrix(runif(length(test.yyyymm) * 2, 0, 1), length(test.yyyymm), 2)
#' include_intercept <- FALSE
#' estLM(month.list.test[1], test.yyyymm, y, data.factors)
estLM <- function(month.list, yyyymm, port.exc.ret, factors, intercept_include = FALSE){
  idx <- (yyyymm %in% month.list)
  port.exc.ret <- port.exc.ret[idx]
  if(is.vector(factors)){
    factors <- cbind(1, matrix(unlist(factors[idx]), ncol = 1))
  }else{
    factors <- cbind(1, matrix(unlist(factors[idx, ]), ncol = dim(factors)[2]))
  }
  mod.fit <- lm.fit(factors, port.exc.ret)
  if(intercept_include){
    res <- mod.fit$coefficients
  }else{
    res <- mod.fit$coefficients[-1]
  }
  return(res)
}

#' Month rolling estimation for ZCAPM
#'
#' @param port.exc.ret A matrix of portfolios excess return, n_time x n_portfolio
#' @param month.list A vector of complete month for the observations
#' @param roll.width The width of rolling
#' @param yyyymm The year and month of the data to be selected
#' @param factors A vector of market excess return
#' @param params A vector of tolerance, maximum number of iterations, and stopping criterion for EM
#'               1 for relative difference, `abs(diff(new - old)/old)` and 2 for `abs(diff(new - old)/(abs(old) + 1))`
#'
#' @return A list of factor loading and variable names.
#'         The factor loading matrix is of size (dim(factors)[2] * n_models) x n_portfolios.
#'         The variable names contains the names included in the model.
#' @export
#'
#' @examples
#' test.yyyymm <- c(rep(202001, 31), rep(202002, 29), rep(202003, 31))
#' month.list.test <- unique(test.yyyymm)
#' month.list.test <- month.list.test[-length(month.list.test)]
#' n_portfolios <- 5
#' y <- matrix(runif(length(test.yyyymm) * n_portfolios, 0, 1), length(test.yyyymm), n_portfolios)
#' test.width <- 2
#' data.factors <- matrix(runif(length(test.yyyymm) * 2, 0, 1), length(test.yyyymm), 2)
#' colnames(data.factors) <- c("p1", "p2")
#' tol <- 1e-6
#' MaxIter <- 100
#' criterion <- 1
#' params <- c(tol, MaxIter, criterion)
#' ZCAPM(y, month.list.test, test.width, test.yyyymm, data.factors, params)
ZCAPM <- function(port.exc.ret, month.list, roll.width, yyyymm, factors, params){
  if (is.vector(port.exc.ret)){
    port.exc.ret <- matrix(port.exc.ret, length(port.exc.ret), 1)
  }
  fact.load <- apply(port.exc.ret, 2, FUN = RollForward, month.list, roll.width, yyyymm, estZCAPM, factors, params)
  varNames <- colnames(factors)
  res <- list(factor.loading = fact.load,
              names.variable = varNames)
  return(res)
}


#' Month rolling estimation for linear model
#'
#' @param port.exc.ret A matrix of portfolios excess return, n_time x n_portfolio
#' @param month.list A vector of complete month for the observations
#' @param roll.width The width of rolling
#' @param yyyymm The year and month of the data to be selected
#' @param factors A vector of market excess return
#' @param intercept_include bool, whether to return intercept or not
#'
#' @return A list of factor loading and variable names
#' @export
#'
#' @examples
#' test.yyyymm <- c(rep(202001, 31), rep(202002, 29), rep(202003, 31))
#' month.list.test <- unique(test.yyyymm)
#' month.list.test <- month.list.test[-length(month.list.test)]
#' n_portfolios <- 5
#' y <- matrix(runif(length(test.yyyymm) * n_portfolios, 0, 1), length(test.yyyymm), n_portfolios)
#' test.width <- 2
#' data.factors <- matrix(runif(length(test.yyyymm) * 2, 0, 1), length(test.yyyymm), 2)
#' colnames(data.factors) <- c("p1", "p2")
#' intercept_include <- FALSE
#' FactorModel(y, month.list.test, test.width, test.yyyymm, data.factors, intercept_include)
FactorModel <- function(port.exc.ret, month.list, roll.width, yyyymm, factors, intercept_include = FALSE){
  if(is.vector(port.exc.ret)){
    port.exc.ret <- matrix(port.exc.ret, length(port.exc.ret), 1)
  }
  fact.load <- apply(port.exc.ret, 2, FUN = RollForward, month.list, roll.width, yyyymm, estLM, factors, intercept_include)
  varNames <- colnames(factors)
  res <- list(factor.loading = fact.load,
              names.variable = varNames)
  return(res)
}
