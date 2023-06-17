#' richness - Species richness calculating function
#'
#' @description Calculates number of singletons and doubletons in the samples and total abundance of samples.
#' @usage richness(df, first.col = 2, graph = T, legver = 10, leghor = 10, ylab = "Number of species", xlab = "Samples", col = c("gray0", "gray48", "gray84"), lncol = 3, ...)
#'
#' @param df A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).
#' @param first.col Numeric (integer), the first column of samples (to skip non-relevant columns). By default, this parameter is set to ‘2’.
#' @param plot Should a barplot for results of calculations be plotted? By default, the plot is rendered.
#' @param legver xxx
#' @param leghor xxx
#' @param ylab The text for the y axis label. By default, the text is setted as ‘Number of species’.
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col A vector that contains three components (named after the three colours). By default, the colours ‘gray0’, ‘gray48’ and ‘gray84’ is used.
#' @param lncol The number of columns in the legend. By default, it is set to ‘3’.
#' @param ... xxx
#'
#' @return A data frame consisting of a column of calculated species richness values (‘F1’, ‘F2’, ‘Other’, ‘Total’) with samples in the rows.
#'
#' @examples
#' xx
#' @export richness
#'

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

