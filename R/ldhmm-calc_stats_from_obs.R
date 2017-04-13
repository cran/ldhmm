#' Computing the statistics for each state
#' 
#' This utility computes the statistics (mean, sd, kurtosis, length) for each state.
#' It can be based on the local or global decoding result. The concept of asymptotic 
#' statistics can be applied by which the largest N observations (in absolute term)
#' can be dropped to avoid distortion from outliers. 
#' It is assumed the object already has come with filled data in 
#' \code{observations, states.prob, states.local, states.global} slots.
#'
#' @param object an ldhmm object
#' @param drop numeric, an integer to drop the largest N observations, default is zero.
#' @param use.local logical, use local decoding result, default is \code{TURE}. 
#'                  Otherwise, use global decoding result.
#'
#' @return an ldhmm object containing results of decoding
#'
#' @keywords mllk
#'
#' @author Stephen H. Lihn
#'
#' @export ldhmm.calc_stats_from_obs
#' 
#' @importFrom utils head
#' @importFrom stats sd
#' @importFrom moments kurtosis
#' 
### <======================================================================>
ldhmm.calc_stats_from_obs <- function(object, drop=0, use.local=TRUE)
{
    m <- object@m
    x <- object@observations
    
    `%notin%` <- function (x, table) match(x, table, nomatch = 0L) == 0L
    
    calc_stats <- function(states) {
        s <- matrix(NA, nrow=m, ncol=4, byrow=TRUE)
        for (j in 1:m) {
            y <- x[states==j]
            if (drop > 0) {
                z <- abs(y)
                z1 <- rev(z[order(z)])
                y <- y[abs(y) %notin% utils::head(z1, drop)]
            }
            s[j,1] <- mean(y)
            s[j,2] <- stats::sd(y)
            s[j,3] <- moments::kurtosis(y)
            s[j,4] <- length(y)
        }
        colnames(s) <- c("mean", "sd", "kurtosis", "length")
        return(s)
    }
    
    if (use.local) {
        return(calc_stats(object@states.local))
    } else {
        return(calc_stats(object@states.global))
    }
}
### <---------------------------------------------------------------------->
