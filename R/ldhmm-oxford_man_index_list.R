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
    rv <- ldhmm.oxford_man_realized_data()
    sort(unique(rv$Symbol))
}
### <---------------------------------------------------------------------->
