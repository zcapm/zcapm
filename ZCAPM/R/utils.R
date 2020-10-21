#' Check if the packaegs required installed, if not install them
#' Load the packages required
#'
#' @param ReqPackage A sequence of names of required packages
#' @export
#'
#' @examples
#' req_packages <- c("MASS")
#' CheckPackage(req_packages)
CheckPackage <- function(ReqPackage){
  for (pkg in ReqPackage) {
    if(!(pkg %in% installed.packages())){
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }else{
      library(pkg, character.only = TRUE)
    }
  }
  print("==== Finish Loading Packages ====")
}

#' Functions to get monthly return from daily return in percentage
#'
#' @param data A vector of daily return for a month
#'
#' @return monthly return
#' @export
#'
#' @examples
#' # suppose we get the daily data x for a month
#' x <- runif(31, 0, 1)
#' GetMonthReturn(x)
#'
GetMonthReturn <- function(data){
  data <- 1 + data / 100
  month.ret <- (cumprod(data)[length(data)] - 1) * 100
  return(month.ret)
}

#' Function to roll forward estimation for one portfolio
#' return: a matrix whose i-th column is i-th factor loadings
#'
#' @param port.exc.ret A vector of portfolio excess return
#' @param month.list A vector of month and year for the complete data
#' @param width A numeric specifying the width of the rolling window
#' @param yyyymm The selected year and month
#' @param func The function to be rolled
#' @param factors A matrix of covariates
#'
#' @return Estimates got by rolling forward function for a portfolio
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
#' RollForward(y, month.list.test, test.width, test.yyyymm, estLM, data.factors, include_intercept)
RollForward <- function(port.exc.ret, month.list, width, yyyymm, func, factors, params){
  est <- zoo::rollapply(month.list, width = width, FUN = func, yyyymm,
                        port.exc.ret, factors, params)
  return(est)
}

#' Monthly Rolling Approach for testing
#'
#' @param idx An indicator vector selecting the desired data
#' @param port.exc.ret A vector of portfolio excess return
#' @param fact.load.stack A matrix of factor loadings
#'
#' @return Coefficient Estimates for FM testing
#' @export
#'
#' @examples
#' n_timestamps <- 10
#' n_portfolios <- 5
#' n_params <- 2
#' idx <- 1:n_timestamps
#' y <- matrix(runif(n_timestamps * n_portfolios, 0, 1), n_timestamps, n_portfolios)
#' fact.load <- matrix(runif(n_timestamps * n_portfolios * n_params, 0, 1), n_timestamps * n_params, n_portfolios)
#' # select the first timestamp
#' MonthRoll(idx[1], y, fact.load)
MonthRoll <- function(idx, port.exc.ret, fact.load.stack){
  # number of models estimated
  num.mod <- nrow(port.exc.ret)
  port.exc.ret <- unlist(port.exc.ret[idx, ])

  # get loading for idx
  n_params <- nrow(fact.load.stack)/num.mod
  factor.loadings <- data.frame(rep(NA, length(port.exc.ret)))
  for (i in 1:n_params) {
    fact.load.idx <- num.mod * (i-1) + idx
    factor.loadings[, i] <- unlist(fact.load.stack[fact.load.idx, ])
  }
  mod <- lm(port.exc.ret ~., data = factor.loadings)
  coeff <- coef(mod)
  return(coeff)
}

#' Single Regression Approach for testing
#'
#' @param port.exc.ret.avg A vector of average excess portfolio excess return
#' @param fact.load A matrix of the factor loadings
#' @param num.mod A numeric specifying the number of models
#' @param idx.month An indicator vector selecting the month of interest
#'
#' @return The R2 of the linear regression
#' @export
#'
#' @examples
#' y <- rnorm(5, 0, 1)
#' names(y) <- paste("stock", 1:5)
#' num.mod <- 10
#' num.params <- 2
#' fact.load <- matrix(rnorm(100, 0, 1), num.mod * num.params, 5)
#' SingleReg(y, fact.load, num.mod, 1:num.mod)
#'
SingleReg <- function(port.exc.ret.avg, fact.load, num.mod, idx.month){
  # get average for each factor loading
  fact.load.avg = data.frame(rep(NA, length(port.exc.ret.avg)))
  for(i in 1:(nrow(fact.load) / num.mod)) {
    # row idx for factor i
    idx = c((1+num.mod*(i-1)):(num.mod*i))
    fact.load.avg[, i] = colMeans((fact.load[idx, ])[idx.month, ])
  }
  r2 <- summary(lm(port.exc.ret.avg ~ .,
                    data = fact.load.avg, na.action = na.omit))$r.squared
  return(r2)
}


#' Print summary for different models
#'
#' @param object list of test result, including coefficients and t-values table, R-squared, model names, and variable names
#' @param digits digits to display in the table
#'
#' @return Print the summary table
#' @export
#'
#' @examples
#' ## run fm_test and get the result
#' y <- matrix(rnorm(50, 0, 1), 10, 5)
#' fact.load.1 <- list(factor.loading = matrix(rnorm(100, 0, 1), 20, 5),
#'                     names.variable = c("p1", "p2"))
#'
#' fact.load.lst <- list(Model1 = fact.load.1)
#' test_res <- fm_test(y, fact.load.lst)
#' print.summary(test_res)
#'
#' ## already got the list object with saved results
#' n_model <- 1
#' Summary.tab <- vector("list", n_model)
#' model.names <- vector("list", n_model)
#' variables <- vector("list", n_model)
#' Summary.tab[[1]] <- matrix(1:6, 3, 2)
#' colnames(Summary.tab[[1]]) <- c("coefficient", "t.value")
#' row.names(Summary.tab[[1]]) <- c("Intercept", "p1", "p2")
#' r2 <- runif(1, 0, 1)
#' model.names[[1]] <- "Model1"
#' variables[[1]] <- c("p1", "p2")
#'
#' models <- list(Summary.models = Summary.tab,
#'                r2 = r2,
#'                model.names = model.names,
#'                variables = variables)
#' print.summary(models)
print.summary <- function(object,
                          digits = max(4L, getOption("digits")), ...){
  n_model <- length(object$r2)
  r2 <- object$r2
  model.names <- object$model.names
  variables <- object$variables
  coefficients.table <- object$Summary.models
  for (i in 1:n_model){
    cat("\nModel: ", model.names[[i]])
    cat("\nVariables Included: ", variables[[i]])
    cat("\nCoefficients: \n")
    printCoefmat(coefficients.table[[i]], digits = digits)
    cat("R-squared: ", r2[i], "\n")
  }
}


