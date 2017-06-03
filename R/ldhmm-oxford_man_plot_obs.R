#' Plotting Oxford-Man realized volatility overlaid with HMM expected volatility
#' 
#' This utility plots the Oxford-Man realized volatility (from SPX2.rv) and overlays with the HMM expected volatility
#' with the observations set up SPX2.r. This graph is to show that the HMM is capable of reproducing the realized volatility.
#' Optionally the insert shows the relation between the return and volatility indicated by each state. This plot is also called
#' "volatility yield curve".
#'
#' @param object an ldhmm object with a stationary solution. If this is set to \code{NULL},
#'               an internal 10-state HMM object will be used.
#' @param days.pa a positive integer specifying trading days per year, default is 252.
#' @param start.date Date or character of ISO format (YYYY-MM-DD), 
#'                   specifying the start date of the plot, default is \code{NULL}.
#' @param end.date Date or character of ISO format (YYYY-MM-DD), 
#'                 specifying the end date of the plot, default is \code{NULL}.
#' 
#' @param index.r character, specifying index return column, default is \code{SPX2.r}.
#' @param index.rv character, specifying index realized variance column, default is \code{SPX2.rv}.
#' @param index.vol.ma.order a positive integer specifying the simple moving average of the realized volatility, default is 5.
#'                           This is needed because the realized volatility is very noisy at the daily level.
#' @param index.px character, specifying index closing price column, default to \code{"SPX2.closeprice"}.
#'                 Set this to \code{NULL} if you don't wish to see the price line.
#' @param index.px.scale numeric, specifying the scaling factor when plotting price trend, default is 15.
#'                       The closing price is converted to cumulative return by the price of the first date.
#'                       Then plot from the mid-point of volatility axis with this scale.
#' @param index.px.origin numeric, specifying the starting value of the index price line,
#'                        the default is \code{NULL}, which will start the index price line from the middle of y-axis.
#'
#' @param index.symbol character, used as a shortcut for \code{index.r, index.rv, index.px}. Once the root symbol is specified,
#'                     the three time series can be derived by appending suffix \code{.r, .rv, .closeprice}. 
#'                     This mechanism will supercede those three components 
#'                     if and only if a value is detected. Default is \code{NULL}.
#' @param vix.adj.ratio numeric, if specified, VIX index is adjusted and plotted, default is \code{NULL}.
#'                      The long-term ratio between VIX and 10-state HMM is 0.79.
#'                      The VIX data is cached when the Oxford data is downloaded. 
#' 
#' @param insert.plot logical, if true, also plot the volatility-return as insert in upper-right corner, default is \code{TRUE}.
#' @param insert.viewport optional viewport for the insert, default is \code{NULL}, 
#'                        which is internally set to \code{grid::viewport(.8, .75, .3, .3)}.
#'                        
#' @keywords oxford
#'
#' @author Stephen H. Lihn
#' 
#' @export
#' 
#' @importFrom ggplot2 qplot
#' @importFrom grid viewport
#' @importFrom scales alpha
#' 
#' @examples
#' \dontrun{
#'     ldhmm.oxford_man_plot_obs(h)
#' }
### <======================================================================>
ldhmm.oxford_man_plot_obs <- function(object, days.pa=252, 
                                      start.date=NULL, end.date=NULL,
                                      index.r="SPX2.r", 
                                      index.rv="SPX2.rv",
                                      index.px="SPX2.closeprice", 
                                      index.px.scale=15,
                                      index.px.origin=NULL,
                                      index.vol.ma.order=5,
                                      index.symbol=NULL,
                                      vix.adj.ratio=NULL,
                                      insert.plot=TRUE, 
                                      insert.viewport=NULL)
{
    if (is.null(object)) object <- ldhmm.read_sample_object()
    m  <- object@m
    ld_stats <- ldhmm.ld_stats(object)
    
    if (!is.null(start.date)) {
        if (class(start.date) == "character") start.date <- as.Date(start.date)
        if (class(start.date) != "Date") stop("start.date must be a Date object")
    }
    if (!is.null(end.date)) {
        if (class(end.date) == "character") end.date <- as.Date(end.date)
        if (class(end.date) != "Date") stop("end.date must be a Date object")
    }
    # symbol shortcut
    if (!is.null(index.symbol)) {
        index.r = paste(index.symbol, "r", sep=".")
        index.rv = paste(index.symbol, "rv", sep=".")
        index.px = paste(index.symbol, "closeprice", sep=".")   
    }
    
    # RV data
    idx_ox <- ldhmm.oxford_man_ts(index.r, log=TRUE)
    vol_ox <- ldhmm.oxford_man_ts(index.rv, to.vol=TRUE)
    cpx_ox <- if (is.null(index.px)) NULL else ldhmm.oxford_man_ts(index.px) 
    hss <- ldhmm.decoding(object, idx_ox$x)
    vols <- ldhmm.decode_stats_history(hss)
    
    # --------------------------------------------------
    max_date <- max(index(idx_ox))
    min_date <- min(index(idx_ox))
    if (!is.null(start.date)) {
        if (start.date > min_date) min_date <- start.date
    } 
    if (!is.null(end.date)) {
        if (end.date < max_date) max_date <- end.date
    } 
    
    I <- which(index(idx_ox) >= min_date & index(idx_ox) <= max_date)
    J <- which(index(vol_ox) >= min_date & index(vol_ox) <= max_date)
     
    vol_hmm <- vols[,2][I]*sqrt(days.pa)*100
    
    vol_ox_ma <- ldhmm.sma(as.numeric(vol_ox), index.vol.ma.order)
    y_min <- min(as.numeric(vol_ox_ma)[J])
    y_max <- max(as.numeric(vol_ox_ma)[J])
    y_max <- max(c(y_max, max(vol_hmm)))
    
    plot(index(idx_ox)[I], vol_hmm, 
         type="l", col="black", lwd=1, 
         ylim=c(y_min, y_max),
         xlab=sprintf("Date (%s to %s)", min_date, max_date),
         ylab=sprintf("V (Volatility)"),
         main=sprintf("%d-State HMM (%s) vs Realized Vol (%s)", hss@m, index.r, index.rv))

    # lines(index(vol_ox), vol_ox_ma, col=scales::alpha("red",0.5))
    points(index(vol_ox), vol_ox_ma, col=scales::alpha("red",0.6), cex=0.4)
    lines(index(vol_ox), as.numeric(vol_ox), col=scales::alpha("pink",0.5))

    # VIX
    if (!is.null(vix.adj.ratio)) {
        vix <- getOption("ldhmm.oxford.vix") 
        lines(vix$d, vix$p*vix.adj.ratio, col=scales::alpha("magenta",0.7), lwd=2)
        text(min_date, y_max*0.9, cex=0.8, pos=4, 
             label=sprintf("VIX adj ratio = %.3f", vix.adj.ratio))
    }
    
    # --------------------------------------------------
    for (i in 1:hss@m) {
        abline(h=ld_stats[i,2]*sqrt(days.pa)*100, lty=2, col=scales::alpha("blue",0.7))
    }
    
    # these black dots should always on top of other plots
    points(index(idx_ox)[I], vol_hmm, col="black", cex=0.4) 
    
    # price level
    if (!is.null(cpx_ox)) {
        if (is.null(index.px.origin)) index.px.origin <- y_max/2
        K <- which(index(cpx_ox) >= min_date)
        cpr_ox <- cpx_ox[K]/as.numeric(head(cpx_ox[K],1))
        cpr_ox2 <- as.numeric(cpr_ox-1)*index.px.scale + index.px.origin
        lines(index(cpr_ox), cpr_ox2, col="blue") # price
    }
    
    # --------------------------------------------------
    # plot the volatility/return insert
    if (insert.plot) {
        if (is.null(insert.viewport)) insert.viewport <- grid::viewport(.8, .75, .3, .3)
        R  <- ld_stats[,1]*days.pa
        V <- ld_stats[,2]*sqrt(days.pa)*100
        qp  <- ggplot2::qplot(V, R, data=data.frame(R=R, V=V))
        print(qp, vp=insert.viewport)
    }
}
### <---------------------------------------------------------------------->
