#' Computing the MLEs
#' 
#' Computing the MLEs using \code{nlm} package
#'
#' @param object an ldhmm object that can supply m, param.nbr and stationary.
#' @param x numeric, the observations.
#' @param print.level numeric, this argument determines the level of printing 
#'                    which is done during the minimization process. 
#'                    The default value of 0 means that no printing occurs, 
#'                    a value of 1 means that initial and final details are printed 
#'                    and a value of 2 means that full tracing information is printed.
#' @param iterlim numeric, a positive integer specifying the maximum number of iterations 
#'                to be performed before the program is terminated.
#' @param ... additional parameters passed to the MLE optimizer 
#'
#' @return an ldhmm object containg results of MLE optimization
#'
#' @keywords mle
#'
#' @author Stephen H. Lihn
#'
#' @export
#' 
#' @examples
#' \dontrun{
#'     param0 <- matrix(c(0.003, 0.02, 1, -0.006, 0.03, 1.3), 2, 3, byrow=TRUE)
#'     gamma0 <- matrix(c(0.9, 0.1, 0.1, 0.9), 2, 2, byrow=TRUE)
#'     h <- ldhmm(m=2, param=param0, gamma=gamma0)
#'     spx <- ldhmm.ts_log_rtn()
#'     ldhmm.mle(h, spx$x)
#' }
### <======================================================================>
ldhmm.mle <- function(object, x, print.level=0, iterlim=1000, ...)
{
    m  <- object@m
    gamma <- object@gamma

    # normalize gamma so that transition probability sums up to one for each state
    one <- sapply(1:m, function(i) sum(gamma[i,]))
    for (i in 1:m) gamma[i,] <- gamma[i,]/one[i]
    object@gamma <- gamma

    # call nlm    
    v <- ldhmm.n2w(object)
    mod <- stats::nlm(ldhmm.mllk, p=v, x=x, ldhmm=object, mllk.print.level=print.level,
                      print.level=print.level, iterlim=iterlim)
    np  <- length(v)
    n   <- sum(!is.na(x))

    # construct result object
    p <- ldhmm.w2n(object, mod$estimate)
    p@mllk <- mod$minimum
    p@AIC <- 2*(p@mllk + np)
    p@BIC <- 2*p@mllk + np*log(n)
    p@return.code <- mod$code
    p@iterations <- mod$iterations 
    return (p)
}
### <---------------------------------------------------------------------->
