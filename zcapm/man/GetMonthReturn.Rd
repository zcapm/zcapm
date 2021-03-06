% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{GetMonthReturn}
\alias{GetMonthReturn}
\title{Functions to get monthly return from daily return in percentage}
\usage{
GetMonthReturn(data)
}
\arguments{
\item{data}{A vector of daily return for a month}
}
\value{
monthly return
}
\description{
Functions to get monthly return from daily return in percentage
}
\examples{
# suppose we get the daily data x for a month
x <- runif(31, 0, 1)
GetMonthReturn(x)

}
