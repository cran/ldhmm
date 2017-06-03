#' Get time series from Oxford-Man Realized data set
#' 
#' This utility returns the time series from the specific column in Oxford-Man Realized data set.
#'
#' @param symbol character, specify the column name. E.g. 
#'               \code{SPX2.r} for the daily returns of SPX, 
#'               \code{SPX2.rv} for the daily realized variances of SPX.
#' @param log logical, take one plus log to convert return to log-return. Default is \code{FALSE}.
#' @param to.vol logical, take \code{sqrt(x*252)*100} to convert variance to annualized volatility. Default is \code{FALSE}.
#' @param days.pa a positive integer specifying number of days to annualize volatility. Default is 252.
#' 
#' @return an xts object containing the time series, with dates as index
#'
#' @keywords oxford
#'
#' @author Stephen H. Lihn
#'
#' @export 
#' 
#' @importFrom stats na.exclude
#' 
#' @examples
#' \dontrun{
#'     rtn <- ldhmm.oxford_man_ts("SPX2.r", log=TRUE)
#'     vol <- ldhmm.oxford_man_ts("SPX2.rv", to.vol=TRUE)
#' }
### <======================================================================>
ldhmm.oxford_man_ts <- function(symbol, log=FALSE, to.vol=FALSE, days.pa=252)
{
    rv <- ldhmm.oxford_man_realized_data()
    d <- as.Date(as.character(rv[,"DateID"]), "%Y%m%d")
    x <- as.numeric(rv[,symbol])
    if (log) x <- log(1+x)
    if (to.vol) x <- sqrt(x*days.pa)*100
    
    ts <- stats::na.exclude(xts(x,d))
    colnames(ts) <- c("x")
    return(ts)
}
