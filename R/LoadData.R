#' loadData - Data uploading function
#'
#' @description The function allows for simple loading of user data, checking the basic parameters of the data and creating the object (data frame) attributes.
#' @usage loadData(file = "clipboard", first.col = 2, na2null = T, attrib = T)
#'
#' @param first.col Numeric (integer), the first column of samples (to skip non-relevant columns). By default, this parameter is set to ‘2’.
#' @param na2null Logical, true in case of converting NA values into null.
#' @param attrib Logical, true in case of creating attributes.
#'
#' @returns A data frame uploaded by the user containing a list of taxa in the first column, and abundance or presence/absence data in following columns, with sample names in the column header.
#' @examples
#' Uploading testing data.
#'
#' test_data <- loadData()
#'
#' @export loadData

loadData <- function(first.col = 2, na2null = T, attrib = T) {
  cat(paste("Copy data into cliboard."))
  invisible(readline(prompt = "Press [enter] to continue. "))
  tryCatch(silent = T,
           expr = {
             x <- read.table("clipboard", h = T, sep = "\t")
             message("Dataset successfully uploaded.")
           },
           error = function(e){
             message("Dataset inaccurately uploaded, Try again.")
             print(e)
           },
           warning = function(w){
             message("Warning! Dataset inaccurately uploaded. Try again.")
             print(w)
           },
           finally = {
             attr(x, "Spec_col") <- first.col:ncol(x)
             attr(x, "Number of species") <- nrow(x)
             attr(x, "Number of samples") <- ncol(x)-1
             y <- unlist(lapply(as.list(first.col:ncol(x)), function(x1) is.numeric((x[,x1]))))
             if(all(y, na.rm = TRUE) == F){
               colbad <- which(y == FALSE) + first.col-1
               cat(paste("Samples", paste(names(x)[colbad], collapse = ","), "are not numeric"))
               cat("\n")
               answ = readline(prompt = "Continue and skip non-numeric columns? y/n  ")
               if(answ == "y") x <- x[-colbad] else stop(paste("Function was terminated. Try to fix the dataset."))
             }
             if(na2null == T) x[is.na(x)]<-0
             if (attrib == T){
               cat(paste("Attributes of table"), "\n")
               print(attributes(x))
             }
             cat(paste("Short data:"), "\n")
             print(x[1:5,])
             cat("\n")
             message("You can continue.")
           }
  )
  return(x)
}


