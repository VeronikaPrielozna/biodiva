#' abundance - Abundance calculating function
#'
#' @description Calculates number of singletons and doubletons in the samples and total abundance of samples.
#' @usage abundance(df, first.col = 2, plot = T, legh = 50, legl = 10, ylab = "Abundance", xlab = "Samples", col = c("gray0", "gray48", "gray84"), ncol = 3, ...)
#'
#' @param df A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).
#' @param first.col Numeric (integer), the first column of samples (to skip non-relevant columns). By default, this parameter is set to ‘2’.
#' @param plot Should a barplot for results of calculations be plotted? By default, the plot is rendered.
#' @param legh
#' @param legl
#' @param ylab The text for the y axis label. By default, the text is setted as ‘Abundance’.
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col A vector that contains three components (named after the three colours). By default, the colours ‘gray0’, ‘gray48’ and ‘gray84’ is used.
#' @param lncol The number of columns in the legend. By default, it is set to ‘3’.
#' @param ...
#'
#' @return A data frame consisting of a column of calculated abundance values (‘F1’, ‘F2’, ‘Other’, ‘Total’) with samples in the rows.
#' @examples
#' For this function, you must have a uploaded data frame. Calculation of abundance for the ‘test_data’ data frame.
#'
#' Abundance_table <- abundance(test_data, legh = 150, legl = 12, col = c("white", "darkgray", "darkcyan"), ncol = 3)
#'
#' @export abundance()

abundance <- function(df, first.col = 2, plot = T, legh = 50, legl = 10, ylab = "Abundance", xlab = "Samples",
                    col = c("gray0", "gray48", "gray84"), lncol = 3, ...){

  x <- df[,first.col:ncol(df)]
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
            args.legend = list(bty = "n", x = legl, y = maxA2 * 1.2 + legh, ncol = lncol), ...)
    title(ylab = ylab, line = 3)
    title(xlab = xlab, line = 4)
    text(posgr, TotalA, lab = TotalA, cex = 0.7, pos = 3)
  }
  return(Atab)
}
