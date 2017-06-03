#' Get the index list from Oxford-Man
#' 
#' This utility shows the index list within the Oxford-Man data set.
#' 
#' @return character, list of indices
#'
#' @keywords oxford
#'
#' @author Stephen H. Lihn
#' 
#' @references Oxford-Man Institute of Quantitative Finance. Realized Library: http://realized.oxford-man.ox.ac.uk
#'
#' @export 
 
#' 
#' @examples
#' \dontrun{
#'     ldhmm.oxford_man_index_list()
#' }
### <======================================================================>
ldhmm.oxford_man_index_list <- function()
{
    get_index <- function(s) {
        has_rv <- length(grep("[.]rv$", s))
        if (has_rv > 0) {
            sp <- unlist(strsplit(s, split="[.]"))
            return(sp[1])
        }
        else return(NA)
    }

    rv <- ldhmm.oxford_man_realized_data()
    c <- colnames(rv)
    as.character(na.exclude(unique(sapply(c, get_index))))
}
### <---------------------------------------------------------------------->
