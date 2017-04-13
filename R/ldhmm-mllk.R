#' Computing the minus log-likelihood (MLLK)
#' 
#' This utility computes the MLLK. It is typically invoked by the MLE optimizer. (Zucchini, 3.2)
#'
#' @param par.vector numeric, the working parameter vector
#' @param x numeric, the observations.
#' @param ldhmm an input ldhmm object to provide static reference, 
#'              such as m, param.nbr, stationary.
#' @param mllk.print.level numeric, this argument determines the level of printing 
#'                         which is done during the minimization process. 
#'                         The default value of 0 means that no printing occurs, 
#'                         a value of 1 or greater means some tracing information is printed.
#'
#' @return an ldhmm object containing results of MLE optimization
#'
#' @keywords mllk
#'
#' @author Stephen H. Lihn
#'
#' @export 
#' 
### <======================================================================>
ldhmm.mllk <- function(par.vector, x, ldhmm, mllk.print.level=0)
{
    object <- ldhmm.w2n(ldhmm, par.vector)
    m <- object@m

    if(m==1) { return(-sum(ldhmm.state_pdf(object, 1, x))) }
    
    n <- length(x)
    pdf <- ldhmm.state_pdf(object, 1:m, x)
    
    # this is the main rescale algorithm in Zucchini, 3.2
    phi <- object@delta * pdf[,1]
    sum_phi <- sum(phi)
    log_phi_scale <- log(sum_phi)
    phi <- phi/sum_phi
    for (i in 2:n) {
        pdf_i <- if(!is.na(x[i])) pdf[,i] else rep(1,m)
        phi <- phi %*% object@gamma * pdf_i
        sum_phi <- sum(phi)
        log_phi_scale <- log_phi_scale + log(sum_phi)
        phi <- phi/sum_phi
    }
    mllk <- -log_phi_scale
    
    if (is.na(mllk)) {
        if (mllk.print.level >= 1) {
            ps <- paste(as.vector(t(object@param)), collapse=", ")
            print(paste("mllk is NaN, with param: ", ps)) 
        }
        mllk <- .Machine$double.xmax
    }
    return(mllk)
}
### <---------------------------------------------------------------------->
