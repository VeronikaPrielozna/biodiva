#' dominance - Dominance classes analysing function.
#'
#' @description Tischler's dominance classes analyses
#' @usage xxx
#'
#' @param df A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).
#' @param table xxx
#' @param plot xxx
#' @param legh xxx
#' @param legl xxx
#' @param ylabA xxx
#' @param ylabR xx
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col A vector that contains five components (named after the five colours). By default, the colours ‘gray0’, ‘gray48’, ‘gray78’, ‘gray84’ and ‘gray90’ is used.
#' @param lncol The number of columns in the legend. By default, it is set to ‘6’.
#' @param bty xxx
#' @param ... xxx
#'
#' @return xx
#'
#' @examples
#' xxx
#'
#' @export dominance
#'

dominance<-function(df, table = "AR", plot = "A", legh = 10, legl = 12,
                    ylab = "Absolutive frequency", xlab = "Samples",
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
            args.legend = list(x = legl, y = legh, bty = bty, ncol = lncol), col = col, ...)
    title(ylab = ylab, line = 3)
    title(xlab = xlab, line = 4)
  }

  if (plot == "R"){
    par(mfrow = c(1, 1), mar = c(5, 4, 2, 0.5), las = 2)
    barplot(tableR, ylim = c(0, 1), legend = T,
            args.legend = list(x = legl, y = legh, bty = bty, ncol = 1, ncol = lncol),
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
