#' Get the realized data from Oxford-Man
#' 
#' This utility fetches the realized data from Oxford-Man and stores the data frame in local memory.
#' It can be retrieved as \code{getOption("ldhmm.oxford.rv")}. Since the data is updated on a daily basis.
#' The user can optionally force the utility to fetch the new file in the same R-session.
#' Note that the download is network intensive. The size of file is about 10-20 MB and growing daily.
#' In addition, VIX daily data is downloaded as \code{getOption("ldhmm.oxford.vix")}.
#'
#' @param force logical, force the utility to fetch the new file. Default is \code{FALSE}.
#' @param debug logical, print debug information. Could be very verbose. Default is \code{FALSE}.
#' 
#' @return data.frame containing the raw data from Oxford-Man.
#'
#' @keywords oxford
#'
#' @author Stephen H. Lihn
#' 
#' @references Oxford-Man Institute of Quantitative Finance. Realized Library: http://realized.oxford-man.ox.ac.uk
#'
#' @export 
#' 
#' @importFrom utils download.file
#' @importFrom utils read.csv
#' @importFrom utils unzip
#' 
#' @examples
#' \dontrun{
#'     ldhmm.oxford_man_realized_data()
#' }
### <======================================================================>
ldhmm.oxford_man_realized_data <- function(force=FALSE, debug=FALSE)
{
    if (is.null(getOption("ldhmm.oxford.rv"))) {
        if (debug) print(paste("Local copy of Oxford data is not found in Option:", "ldhmm.oxford.rv"))
        force <- TRUE
    }
    if (is.null(getOption("ldhmm.oxford.vix"))) {
        if (debug) print(paste("Local copy of VIX data is not found in Option:", "ldhmm.oxford.vix"))
        force <- TRUE
    }
    
    if (force) {
        zip_url <- "https://realized.oxford-man.ox.ac.uk/images/oxfordmanrealizedvolatilityindices.zip"
        if (debug) print(paste("Fetching URL:", zip_url))
        temp <- tempfile()
        utils::download.file(zip_url, temp)
        if (debug) utils::unzip(temp, list=TRUE) 
        
        csv_file <- "oxfordmanrealizedvolatilityindices.csv"
        if (debug) print(paste("Unzip from csv:", csv_file))
        rv <- utils::read.csv(unz(temp, csv_file), header=TRUE, stringsAsFactors = FALSE)
        colnames(rv)[1] <- "date"
        rv$date <- as.Date(rv$date)
        
        unlink(temp)
        if (debug) print(paste("Store data into Options:", "ldhmm.oxford.rv"))
        options("ldhmm.oxford.rv"=rv)
        
        vix <- ldhmm.ts_log_rtn("vix", end.date="", on="days", fred.data=TRUE) # live daily
        print(paste("Max date of VIX data is", max(vix$d)))
        options("ldhmm.oxford.vix"=vix)
    }

    rv <- getOption("ldhmm.oxford.rv")
    if (debug) {
        print("List of columns:")
        print(colnames(rv))
    }
    if (force | debug) {
        print(paste("Max date of Oxford data is", max(rv$date)))
    }
    
    invisible(rv)
}
