#' Transforming working parameter array to natural parameters
#' 
#' This utility transforms the working parameter array back to
#' the vectors and matrix of the contrained parameters. (Zucchini, 3.3.1)
#'
#' @param object an ldhmm object that can supply m, param.nbr and stationary.
#' @param par.vector numeric, linear working parameter array. See \code{ldhmm.n2w}.
#'
#' @return an ldhmm object
#'
#' @keywords parameter
#'
#' @author Stephen H. Lihn
#'
#' @export ldhmm.w2n
#' 
### <======================================================================>
ldhmm.w2n <- function(object, par.vector)
{
    m  <- object@m
    param.nbr <- object@param.nbr
    stationary <- object@stationary
    
    N <- m*param.nbr
    idx <- seq(0, N-1)
    tparam <- par.vector[1:(m*param.nbr)]
    vparam <- ifelse(idx %% param.nbr == 0, tparam, exp(tparam)) # except mu
    param <- matrix(vparam, m, param.nbr, byrow=TRUE)
    
    # handle gamma
    gamma  <- diag(m)
    if (m==1) return(list(param=param, gamma=gamma, delta=1))
    
    N_tgamma <- m*(m-1)
    gamma[!gamma] <- exp(par.vector[(N+1):(N+N_tgamma)])
    gamma         <- gamma/apply(gamma,1,sum)
    
    # handle delta
    if (stationary) { 
        delta <- solve(t(diag(m)-gamma+1),rep(1,m)) 
    } else {
        tdelta <- par.vector[(N+N_tgamma+1):(N+N_tgamma+m-1)]
        delta1 <- c(1, exp(tdelta))
        delta <- delta1/sum(delta1)
    }
    
    h <- ldhmm(m=m, param=param, gamma=gamma, delta=delta, stationary=stationary)
    return (h)
}
### <---------------------------------------------------------------------->
