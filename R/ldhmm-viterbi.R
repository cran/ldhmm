#' Computing the global decoding by the Viterbi algorithm
#' 
#' This utility computes the global decoding by the Viterbi algorithm.
#'
#' @param object an ldhmm object
#' @param x numeric, the observations.
#'
#' @return a vector of states
#'
#' @keywords viterbi
#'
#' @author Stephen H. Lihn
#'
#' @export ldhmm.viterbi
#' 
### <======================================================================>
ldhmm.viterbi <- function(object, x)
{
    m <- object@m
    n <- length(x)
    xi <- matrix(0, n, m)
    pdf <- ldhmm.state_pdf(object, 1:m, x)
    
    phi <- object@delta * pdf[,1]
    xi[1,] <- phi/sum(phi)
    for (i in 2:n) {
        phi <- apply(xi[i-1,] * object@gamma, 2, max) * pdf[,i]
        xi[i,] <- phi/sum(phi)
    }
    iv <- numeric(n)
    iv[n] <-which.max(xi[n,])
    for (i in (n-1):1) {
        iv[i] <- which.max(object@gamma[,iv[i+1]] * xi[i,])
    }
    return(iv)
}
### <---------------------------------------------------------------------->

