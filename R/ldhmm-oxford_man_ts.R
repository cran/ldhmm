#' Get time series from Oxford-Man Realized data set
#' 
#' This utility returns the time series from the specific column in Oxford-Man Realized data set.
#'
#' @param symbol character, specify the index name, e.g. ".SPX".
#' @param column character, the column name, e.g. "rv5".
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
#'     vol <- ldhmm.oxford_man_ts(".SPX", "rv5", to.vol=TRUE)
#' }
### <======================================================================>
ldhmm.oxford_man_ts <- function(symbol, column, log=FALSE, to.vol=FALSE, days.pa=252)
{
    rv <- ldhmm.oxford_man_realized_data()
    I <- which(rv$Symbol == symbol)
    rv1 <- rv[I,]
    
    d <- rv1$date
    x <- as.numeric(rv1[,column])

    if (log) x <- log(1+x)
    if (to.vol) x <- sqrt(x*days.pa)*100
    
    ts <- stats::na.exclude(xts(x,d))
    colnames(ts) <- c("x")
    return(ts)
}
