#' richness - Species richness calculating function
#'
#' @description Calculates number of singletons and doubletons in the samples and total abundance of samples.
#' @usage
#'
#' @param df A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).
#' @param first.col Numeric (integer), the first column of samples (to skip non-relevant columns). By default, this parameter is set to ‘2’.
#' @param plot Should a barplot for results of calculations be plotted? By default, the plot is rendered.
#' @param legh
#' @param legl
#' @param ylab The text for the y axis label. By default, the text is setted as ‘Number of species’.
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col A vector that contains three components (named after the three colours). By default, the colours ‘gray0’, ‘gray48’ and ‘gray84’ is used.
#' @param lncol The number of columns in the legend. By default, it is set to ‘3’.
#' @param ...
#'
#' @return A data frame consisting of a column of calculated species richness values (‘F1’, ‘F2’, ‘Other’, ‘Total’) with samples in the rows.
#'
#' @examples
#' @export richness()

richness<-function(df, first.col = 2, plot = T, legh = 10, legl = 10, ylab = "Number of species", xlab = "Samples",
                   col = c("gray0", "gray48", "gray84"), lncol = 3, ...){

  x <- df[,first.col:ncol(df)]
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
            args.legend = list(bty = "n", x = legl, y = maxS2 * 1.2 + legh, ncol = lncol), ...)
    title(ylab = ylab, line = 3)
    title(xlab = xlab, line = 4)
    text(posgr, Total, lab = Total, cex = 0.7, pos = 3)
  }
  return(SRtab)
}

