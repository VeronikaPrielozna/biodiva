#' Dominance classes analysis
#'
#' @description Calculates Tischler's dominance class analyses and displays the results of the calculation in graphical and tabular form.
#' @usage dominance(x, table = "AR", plot = "A", lehver = 10, leghor = 12, ylab = "Absolutive frequency", xlab = "Samples", col = c("gray0","gray48","gray78", "gray84", "gray90"), lncol = 6, bty = "n", ...)
#'
#' @param x A data frame uploaded by the loadData function (important because of the first column setting) containing the uploaded user dataset (list of taxa in the first column, followed by abundance columns with sample names in a header).
#' @param table Table output type. In case the table is equal to "A", the table will be created only for absolute proportion, for table equal to "R", the output will be the table for relative proportion. In the case of table equal to "AR", a sheet of two table type elements will be created for both absolute and relative proportion.
#' @param plot Plot showing the result of the calculation. If the plot equals "A" the results for the absolute proportion will be graphically represented. For a plot equal to "R", the results for relative proportion will be plotted.
#' @param legver Numeric. Setting the vertical position of the legend.
#' @param leghor Numeric. Setting the horizontal position of the legend.
#' @param ylab The text for the y axis label in plot. By default, the text is setted as ‘Absolutive frequency’.
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col A vector that contains five components (named after the five colours). By default, the colours ‘gray0’, ‘gray48’, ‘gray78’, ‘gray84’ and ‘gray90’ is used.
#' @param lncol The number of columns in the legend. By default, it is set to ‘6’.
#' @param bty The type of box to be drawn around the legend. The allowed values are "n" (the default) and "o".
#' @param ... Arguments to be passed to methods, such graphical parameters. Many methods will acccept the following arguments.
#'
#' @return A data frame consisting of a column of calculated Tischler's dominance classes (‘E’, ‘D’, ‘Sd’, ‘R’, ‘Sr’) with samples in the rows.
#'
#' @examples
#' # Calculation of the Tischler's dominance class analyses for the 'test_data' and creation of list with two elements (absolute proportions and relative proportions calculation). The position of the legend vertical is set to '50' and the position of the legend length set to '13.5'. The colour palette used is 'white', 'darkgray', 'lightgreen', 'darkcyan' and 'darkgreen'. The list of calculation results is stored in an object called 'dominance_tab'.
#'
#' dominance_tab <- dominance(test_data, table = "AR", plot = "A", lehver = 50, leghor = 13.5, col = c("white", "darkgray", "lightgreen", "darkcyan", "darkgreen"))
#'
#' # Calculation of the Tischler's dominance class analyses for the 'test_data' and creation of data frame with absolute proportions calculation and plotted results. The position of the legend height is set to '50' and the position of the legend length set to '13.5'. The colour palette used is 'white', 'darkgray', 'lightgreen', 'darkcyan' and 'darkgreen'. The data frame of calculation results is stored in an object called 'dominance_tab'.
#'
#' dominance_tab <- dominance(test_data, table = "A", plot = "A", lehver = 50, leghor = 13.5, col = c("white", "darkgray", "lightgreen", "darkcyan", "darkgreen"))
#'
#' # Calculation of the Tischler's dominance class analyses for the 'test_data' and creation of data frame with relative proportions calculation and plotted results. The name of the y axis label is set to 'Relative frequency' The position of the legend height is set to '1.3' and the position of the legend length set to '15'. The colour palette used is 'white', 'darkgray', 'lightgreen', 'darkcyan' and 'darkgreen'. The data frame of calculation results is stored in an object called 'dominance_tab'.
#'
#' dominance_tab <- dominance(test_data, table = "R", plot = "R", ylab = "Relative frequency", lehver = 1.3, leghor = 15, col = c("white", "darkgray", "lightgreen", "darkcyan", "darkgreen"))
#'
#' @export dominance

dominance<-function(x, table = "AR", plot = "A", legver = 1, leghor = 1,
                    ylab = "Absolutive frequency", xlab = "Samples",
                    col = c("gray0","gray48","gray78", "gray84", "gray90"), lncol = 6, bty = "n", ...){

  x <- x[,(attributes(x)$"First column"):ncol(x)]
  calDom <- function(x) table(cut(x, breaks = c(0, 2, 5, 12, 40, 100),
                                  labels = c("Sr", "R", "Sd", "D", "E")))
  tableA <- apply(x[-1], 2, calDom)
  tableR <- round(apply(tableA, 2, function(tableA) tableA / sum(tableA)), digits = 5)
  maxA <- max(apply(tableA, 2, function(x) sum(x)))

  if (plot == "A"){
    par(mfrow = c(1, 1), mar = c(5, 4, 2, 0.5), las = 2)
    posgr = barplot(tableA, plot = F)
    barplot(tableA, ylim = c(0, maxA * 1.2), legend = T,
            args.legend = list(x = posgr[leghor], y = legver, bty = bty, ncol = lncol, xjust = 0), col = col, ...)
    title(ylab = ylab, line = 3)
    title(xlab = xlab, line = 4)
  }

  if (plot == "R"){
    par(mfrow = c(1, 1), mar = c(5, 4, 2, 0.5), las = 2)
    posgr = barplot(tableR, plot = F)
    barplot(tableR, ylim = c(0, 1), legend = T,
            args.legend = list(x = posgr[leghor], y = legver, bty = bty, ncol = lncol, xjust = 0),
            col = col, ...)
    title(ylab = ylab, line = 3)
    title(xlab = xlab, line = 4)
  }

  if (table == "AR"){
    table1 <- list("Absolute proportion" = as.data.frame(t(tableA)), "Relative proportion" = as.data.frame(t(tableR)))
    return(table1)
  }

  if (table == "A"){
    tableA <- as.data.frame(t(tableA))
    return(tableA)
  }

  if (table == "R"){
    tableR <- as.data.frame(t(tableR))
    return(tableR)
  }
}
