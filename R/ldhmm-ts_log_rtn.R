#' Get log-returns from historic prices of an index
#' 
#' This utility returns the dates and log-returns of an index available in ecd package.
#' Note that the data from ecd package is static. A limited set of live daily time series can be appended 
#' from FRED, e.g. SPX, VIX, DJIA.
#'
#' @param symbol character, specify the symbol of the index, default is \code{spx}.
#' @param start.date,end.date Date or character of ISO format (YYYY-MM-DD), to specify the date range, 
#'                            default is from 1950-01-01 to 2015-12-31. 
#'                            Set start.date and end.date to NULL or "" if you wish to get the entire time series.
#' @param on character, specify the interval, days, weeks, months. Default is \code{weeks}.
#' @param fred.data logical, specify whether to append daily time series data from FRED, default is \code{FALSE}. 
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
ldhmm.ts_log_rtn <- function(symbol="spx", start.date="1950-01-01", end.date="2015-12-31", 
                             on="weeks", fred.data=FALSE)
{
    ts <- ecd::ecd.data(symbol)
    if (fred.data) {
        map <- data.frame(symb=c(  "spx",    "vix",  "dji"), 
                          fred=c("SP500", "VIXCLS", "DJIA"))
        symb <- NULL # faking global variable for subset
        fred_symbol <- as.character(subset(map, symb==symbol)$fred)
        if (length(fred_symbol)!=1) stop(paste("ERROR: failed to locate FRED symbol for", symbol))
        # if (debug) print(paste("appending symbol", symbol, "from FRED", fred_symbol))
        max_dt <- max(index(ts))
        ts1 <- ldhmm.fred_data(fred_symbol)
        ts2 <- ts1[paste((max_dt+1),"/", sep="")]
        ts <- c(ts, ts2)
    }
    
    # It is easier to work with "" instead of NULL
    if (is.null(start.date)) start.date <- ""
    if (is.null(end.date)) start.date <- ""
    
    if (class(start.date) == "Date") start.date <- as.character(start.date)
    if (class(start.date) != "character") stop("start.date must be in ISO-date string or Date")
    if (class(end.date) == "Date") end.date <- as.character(end.date)
    if (class(end.date) != "character") stop("end.date must be in ISO-date string or Date")
    
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
