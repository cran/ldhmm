#' The sldhmm class
#' 
#' This S4 class is the stable lambda object class for ldhmm package
#' It uses the stable lambda distribution for more granular fit
#'
#' @name sldhmm-class
#'
#' @slot call The match.call slot
#' @slot m numeric, length 1, number of states
#' @slot dist.name character, name of distribution, default is "sld".
#'                This is in preparation as a more generic class container in the future.
#' @slot t numeric, the time period
#' @slot param.nbr numeric, number of parameters, default is 4.
#' @slot param matrix, natural parameters for sld objects, size of states times param.nbr.
#'             Each row should be 4-parameter sequences: \code{(mu, nu0, theta, convo)}.
#' @slot gamma matrix, the transition probability matrix, must be m by m.
#' @slot delta numeric, the initial distribution for each state, default is \code{NULL}.
#' @slot stationary logical, specify whether the initial distribution is stationary or not,
#'                  default is \code{TRUE}.
#' @slot mle.optimizer character, the MLE optimizer. Currently it is just set to "nlm".
#' @slot return.code numeric, the return code from the MLE optimizer.
#' @slot iterations numeric, number of iterations MLE optimizer takes.
#' @slot mllk numeric, the final mllk value.
#' @slot AIC numeric, the final AIC.
#' @slot BIC numeric, the final BIC.
#' @slot observations numeric, stores the observations post optimization
#' @slot states.prob matrix, stores the state probabilities post optimization
#' @slot states.local numeric, stores the local decoding states post optimization
#' @slot states.global numeric, stores the global decoding states post optimization (Viterbi)
#' @slot states.local.stats matrix, stores the statistics of local states post optimization
#' @slot states.global.stats matrix, stores the statistics of global states post optimization
#'
#' @keywords class constructor
#'
#' @include ldhmm-package.R
#' @include ldhmm-numericOrNull-class.R
#'
#' @exportClass sldhmm
setClass("sldhmm",
         representation(call = "call",
                        m  = "numeric",
                        dist.name = "character",
                        t = "numeric",
                        param.nbr = "numeric",
                        param = "matrix",
                        gamma = "matrix",
                        delta = "numericOrNull",
                        stationary = "logical",
                        mle.optimizer = "character",
                        return.code = "numericOrNull",
                        iterations = "numericOrNull",
                        mllk = "numericOrNull",
                        AIC = "numericOrNull",
                        BIC = "numericOrNull",
                        observations = "numeric",
                        states.prob = "matrix",
                        states.local = "numeric",
                        states.global = "numeric",
                        states.local.stats = "matrix",
                        states.global.stats = "matrix"),
          prototype(call = call("ldhmm"),
                    m = 2,
                    dist.name = "sld",
                    t = 0,
                    param.nbr = 4,
                    delta = NULL,
                    stationary = TRUE,
                    mle.optimizer = "nlm",
                    return.code = NULL,
                    iterations = NULL,
                    mllk = NULL,
                    AIC = NULL,
                    BIC = NULL)
)
### <---------------------------------------------------------------------->
