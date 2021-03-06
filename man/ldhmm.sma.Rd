% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ldhmm-sma.R
\name{ldhmm.sma}
\alias{ldhmm.sma}
\title{Simple moving average of a time series}
\usage{
ldhmm.sma(x, order, na.backfill = TRUE)
}
\arguments{
\item{x}{numeric, the time series.}

\item{order}{a positive integer to specify order of moving average.}

\item{na.backfill}{logical, specify whether to backfill for NA. Default is \code{TRUE}.}
}
\value{
numeric, simple moving average, same length as x.
}
\description{
This utility calculates simple moving average, with option to backfill for NA.
}
\examples{
x <- 1:100
a <- ldhmm.sma(x, 10)
}
\author{
Stephen H. Lihn
}
\keyword{data}
