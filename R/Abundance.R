#' Abundance
#'
#' @description Calculates the number of singletons and doubletons in the samples and the total abundance of the samples and displays the results of the calculation in graphical and tabular form.
#' @usage abundance(x, plot = T, lehver = 50, leghor = 10 , ylab = "Abundance", xlab = "Samples", col = c("gray0", "gray48", "gray84"), ncol = 3, ...)
#'
#' @param x A data frame uploaded by the loadData function (important because of the first column setting) containing the uploaded user dataset (list of taxa in the first column, followed by abundance columns with sample names in a header).
#' @param plot Should a barplot for results of calculations be plotted? By default, the plot is rendered.
#' @param legver Numeric. Setting the vertical position of the legend.
#' @param leghor Numeric. Setting the horizontal position of the legend.
#' @param ylab The text for the y axis label. By default, the text is setted as ‘Abundance’.
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col A vector that contains three components (named after the three colours). By default, the colours ‘gray0’, ‘gray48’ and ‘gray84’ is used.
#' @param lncol The number of columns in the legend. By default, it is set to ‘3’.
#' @param ... Arguments to be passed to methods, such graphical parameters. Many methods will acccept the following arguments.
#'
#' @return A data frame consisting of a column of calculated abundance values (‘F1’ – abundance of singletons, ‘F2’ – abundance od doubletons, ‘Other’, ‘Total’) with samples in the rows.
#' @examples
#' # Calculation of the abundance for the 'test_data' with the position of the legend vertical set to '150' and the position of the legend length set to '12'. The number of columns in the legend is set to '3' and the colour palette used is 'white', 'darkgray' and 'darkcyan'. The data frame containing the calculation results is stored in an object called 'abundance_tab'.
#'
#' abundance_tab <- abundance(test_data, lehver = 150, leghor = 12, lncol = 3, col = c("white", "darkgray", "darkcyan"))
#'
#' @export abundance

abundance <- function(x, plot = T, legver = 1, leghor = 1, ylab = "Abundance", xlab = "Samples",
                    col = c("gray0", "gray48", "gray84"), lncol = 3, ...){

  x <- x[,(attributes(x)$"First column"):ncol(x)]
  TotalA <- apply(x, 2, function(f5) sum(f5))
  F1 <- apply(x, 2, function(f1) sum(f1 == 1))
  F2A <- apply(x, 2, function(f2) sum(f2 == 2)) * 2
  OtherA <- TotalA - F1 - F2A
  Atab <- rbind(F1,F2A,OtherA,TotalA)
  rownames(Atab) <- c("F1", "F2", "Other", "Total")
  maxA <- max(Atab[1,] + Atab[2,])
  maxA2 <- max(Atab[4,])

  if (plot == T){
    par(mfrow = c(1, 1), mar = c(5, 4, 2, 0.5), las = 2)
    posgr = barplot(Atab, plot = F)
    barplot(Atab[-4,], legend.text = c(as.expression(bquote('F'['1'])), as.expression(bquote('F'['2'])), "Other"),
            ylim = c(0, maxA2 * 1.2), col = col,
            args.legend = list(bty = "n", x = posgr[leghor], y = legver, ncol = lncol, xjust = 0), ...)
    title(ylab = ylab, line = 3)
    title(xlab = xlab, line = 4)
    text(posgr, TotalA, lab = TotalA, cex = 0.7, pos = 3)
  }
  Atab <- as.data.frame(t(Atab))
  return(Atab)
}
