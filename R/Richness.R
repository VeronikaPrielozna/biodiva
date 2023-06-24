#' Species richness
#'
#' @description Calculates the species richness of the samples and displays the results of the calculation in graphical and tabular form.
#' @usage richness(x, plot = T, lehver = 10, leghor = 10, ylab = "Number of species", xlab = "Samples", col = c("gray0", "gray48", "gray84"), lncol = 3, ...)
#'
#' @param x A data frame uploaded by the loadData function (important because of the first column setting) containing the uploaded user dataset (list of taxa in the first column, followed by abundance columns with sample names in a header).
#' @param plot Should a barplot for results of calculations be plotted? By default, the plot is rendered.
#' @param legver Numeric. Setting the vertical position of the legend.
#' @param leghor Numeric. Setting the horizontal position of the legend.
#' @param ylab The text for the y axis label. By default, the text is setted as ‘Number of species’.
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col A vector that contains three components (named after the three colours). By default, the colours ‘gray0’, ‘gray48’ and ‘gray84’ is used.
#' @param lncol The number of columns in the legend. By default, it is set to ‘3’.
#' @param ... Arguments to be passed to methods, such grapfical parameters. Many methods will acccept the following arguments.
#'
#' @return A data frame consisting of a column of calculated species richness values (‘F1’ – the number of species of singletons, ‘F2’ – the number of species of doubletons, ‘Other’, ‘Total’) with samples in the rows.
#'
#' @examples
#' # Calculation of the species richness for the 'test_data' with the position of the legend vertical set to '15' and the position of the legend length set to '13'. The colour palette used is 'white', 'darkgray' and 'darkcyan'. The data frame containing the calculation results is stored in an object called 'richness_tab'.
#'
#' richness_tab <- richness(test_data, lehver = 15, leghor = 13, col = c("white", "darkgray", "darkcyan"))
#'
#' @export richness

richness<-function(x, plot = T, legver = 1, leghor = 1, ylab = "Number of species", xlab = "Samples",
                   col = c("gray0", "gray48", "gray84"), lncol = 3, ...){

  x <- x[,(attributes(x)$"First column"):ncol(x)]
  F1 <- apply(x, 2, function(f1) sum(f1 == 1))
  F2 <- apply(x, 2, function(f2) sum(f2 == 2))
  Other <- apply(x, 2, function(f4) sum(f4 > 2))
  Total <- F1 + F2 + Other
  SRtab <- rbind(F1, F2, Other, Total)
  maxS <- max(SRtab[1,] + SRtab[2,])
  maxS2 <- max(SRtab[4,])

  if (plot == T){
    par(mfrow = c(1,1), mar = c(5, 4, 2, 0.5), las = 2)
    posgr = barplot(SRtab, plot = F)
    barplot(SRtab[-4,], ylim = c(0,maxS2 * 1.2), xaxs = "r",
            legend.text = c(as.expression(bquote('F'['1'])), as.expression(bquote('F'['2'])),"Other"), col = col,
            args.legend = list(bty = "n", x = posgr[leghor], y = legver, ncol = lncol, xjust = 0), ...)
    title(ylab = ylab, line = 3)
    title(xlab = xlab, line = 4)
    text(posgr, Total, lab = Total, cex = 0.65, pos = 3)
  }
  SRtab <- as.data.frame(t(SRtab))
  return(SRtab)
}

