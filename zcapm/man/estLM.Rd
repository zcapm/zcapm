% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation.R
\name{estLM}
\alias{estLM}
\title{Estimation for linear model}
\usage{
estLM(month.list, yyyymm, port.exc.ret, factors, intercept_include = FALSE)
}
\arguments{
\item{month.list}{A vector of complete month for the observations}

\item{yyyymm}{The year and month of the data to be selected}

\item{port.exc.ret}{A vector of portfolio excess return}

\item{factors}{A vector of market excess return}

\item{intercept_include}{bool, return the intercept or not}
}
\value{
A vector of linear regression w/w.o. intercepts
}
\description{
Estimation for linear model
}
\examples{
test.yyyymm <- c(rep(202001, 31), rep(202002, 29), rep(202003, 31))
month.list.test <- unique(test.yyyymm)
month.list.test <- month.list.test[-length(month.list.test)]
y <- runif(length(test.yyyymm), 0, 1)
test.width <- 2
data.factors <- matrix(runif(length(test.yyyymm) * 2, 0, 1), length(test.yyyymm), 2)
include_intercept <- FALSE
estLM(month.list.test[1], test.yyyymm, y, data.factors)
}
