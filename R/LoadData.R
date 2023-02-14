#' read.eco - Uploading the dataset
#'
#' @description Uploading the dataset, performing basic analyses for the loaded table, creating table attributes.
#'
#' @usage read.eco(first.col=2, na2null=T, attrib=T)
#'
#' @param type nastení např. clipboard
#' @param first.col the column from which to work with the data
#' @param na2null converting NA values to null
#' @param attrib creating attributes
#'
#' @examples x<-read.eco()
#'
#' @details The function read.eco appeals to users to copy the dataset from the spreadsheet. With uploaded dataset is worked from the user's specified column (first.col=2). This function also automatically converts NA values to null (na2null=T) and writes out attributes of the dataset.

LoadData<-function(type="clipboard",first.col=2, na2null=T, attrib=T) {
  cat(paste("Copy data into cliboard."))
  invisible(readline(prompt="Press [enter] to continue. "))
  tryCatch(silent=T,
           expr = {
             x<-read.table(type, h=T, sep = "\t")
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
             attr(x,"Spec_col")<-first.col:ncol(x)
             attr(x, "Number of species")<-nrow(x)
             attr(x,"Number of samples")<-ncol(x)-1
             y<-unlist(lapply(as.list(first.col:ncol(x)),function(x1) is.numeric((x[,x1]))))
             if(all(y, na.rm = TRUE)==F){
               colbad<-which(y==FALSE) + first.col-1
               cat(paste("Samples",paste(names(x)[colbad],collapse=","),"are not numeric"))
               cat("\n")
               answ=readline(prompt="Continue and skip non-numeric columns? y/n  ")
               if(answ=="y") x<-x[-colbad] else stop(paste("Function was terminated. Try to fix the dataset."))
             }
             if(na2null==T) x[is.na(x)]<-0
             if (attrib==T){
               print("Attributes of table")
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
