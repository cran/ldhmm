#' Get log-returns from historic prices of an index
#' 
#' This utility returns the dates and log-returns of an index available in ecd package.
#'
#' @param symbol character, specify the symbol of the index, default is \code{spx}.
#' @param start.date,end.date character, specify the date range in ISO-format, 
#'                            default is from 1950-01-01 to 2016-12-31. 
#' @param on character, specify the interval, days, weeks, months. Default is \code{weeks}.
#' 
#' @return list of three vectors: \code{d} is the dates and \code{x} is log-returns and \code{p} is prices
#'
#' @keywords data
#'
#' @author Stephen H. Lihn
#'
#' @export 
#' 
#' @importFrom ecd ecd.data
#' @importFrom xts endpoints
#' @importFrom zoo index
#' @importFrom stats lag
#' @importFrom stats na.exclude
#' 
#' @examples
#' a <- ldhmm.ts_log_rtn()
### <======================================================================>
ldhmm.ts_log_rtn <- function(symbol="spx", start.date="1950-01-01", end.date="2015-12-31", on="weeks")
{
    ts <- ecd::ecd.data(symbol)
    ts1 <- ts[paste(start.date, "/", end.date, sep="")]
    ep <- xts::endpoints(ts1, on=on, k=1)
    ts2 <- ts1[ep[1:length(ep)]]
    ts2$logr <- log(ts2$Close) - stats::lag(log(ts2$Close))
    ts2 <- stats::na.exclude(ts2)

    d <- zoo::index(ts2)
    x <- as.numeric(ts2$logr)
    p <- as.numeric(ts2$Close)
    return(list(d=d, x=x, p=p))
}
