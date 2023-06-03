#' dominance - Dominance classes analysing function.
#'
#' @description Tischler's dominance classes analyses
#' @usage
#'
#' @param df A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).
#' @param table
#' @param plot
#' @param legh
#' @param legl
#' @param ylabA
#' @param ylabR
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col A vector that contains five components (named after the five colours). By default, the colours ‘gray0’, ‘gray48’, ‘gray78’, ‘gray84’ and ‘gray90’ is used.
#' @param lncol The number of columns in the legend. By default, it is set to ‘6’.
#' @param bty
#' @param ...
#'
#' @return
#'
#' @examples
#'
#' @export

dominance<-function(df, table = "AR", plot = "A", legh = 10, legl = 12,
                    ylabA = "Absolutive frequency", ylabR = "Relative frequency", xlab = "Samples",
                    col = c("gray0","gray48","gray78", "gray84", "gray90"), lncol = 6, bty = "n", ...){
  x <- df
  calDom <- function(x) table(cut(x, breaks = c(0, 2, 5, 12, 40, 100),
                                  labels = c("Sr", "R", "Sd", "D", "E")))
  tableA <- apply(x[-1], 2, calDom)
  tableR <- round(apply(tableA, 2, function(tableA) tableA / sum(tableA)), digits = 5)
  maxA <- max(apply(tableA, 2, function(x) sum(x)))

  if (plot == "A"){
    par(mfrow = c(1, 1), mar = c(5, 4, 2, 0.5), las = 2)
    barplot(tableA, ylim = c(0, maxA * 1.2), legend = T,
            args.legend = list(x = legl, y = maxA + legh, bty = bty, ncol = lncol), col = col, ...)
    title(ylab = ylabA, line = 3)
    title(xlab = xlab, line = 4)
  }

  if (plot == "R"){
    par(mfrow = c(1, 1), mar = c(5, 4, 2, 0.5), las = 2)
    barplot(tableR, ylim = c(0, 1), legend = T,
            args.legend = list(x = legl, y = 1 + legh, bty = bty, ncol = 1, ncol = lncol),
            col = col, ...)
    title(ylab = ylabR, line = 3)
    title(xlab = xlab, line = 4)
  }

  if (table == "AR"){
    table1 <- list( "Absolute proportion" = tableA, "Relative proportion" = tableR)
    return(table1)
  }

  if (table == "A"){
    return(tableA)
  }

  if (table == "R"){
    return(tableR)
  }
}
