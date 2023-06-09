#' simpson - Simpson's index calculating function
#'
#' @description Calculation of Simpson’s index and evenness
#' @usage simpson(df, first.col = 2, table = T, plot = T, ylab = "Simpson's index value", col = "gray",...)
#'
#' @param df A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).
#' @param first.col first.col 	Numeric (integer), the first column of samples (to skip non-relevant columns). By default, this parameter is set to ‘2’.
#' @param plot Should a plot for results of calculations be plotted? By default, the plot is rendered.
#' @param ylab The text for the y axis label in plot. By default, the text is setted as ‘Simpson´s index value’.
#' @param col A vector that contains five components (named after the five colours). By default, the color is set as ‘gray’.
#' @param ... xxx
#'
#' @return A data frame consisting of a column of calculated species richness values (‘F1’, ‘F2’, ‘Other’, ‘Total’) with samples in the rows.
#'
#' @examples
#' # Popsat nastavení následujících argumentů¨
#' přidat vice volání
#' simpson(test_data, col = "darkcyan")
#' @export simpson

simpson <- function(df, first.col = 2, plot = T, ylab = "Simpson's index value", col = "gray",...){
  data.simps <- df[, first.col:ncol(df)]
  mat <- matrix(nrow = 4, ncol = ncol(data.simps))
  tab <- data.frame(mat)
  D <- apply(data.simps, 2, function(x) sum((x / sum(x)) ^ 2))
  Dc <- 1 - D
  Dr <- 1 / D
  S <- apply(data.simps, 2, function(x1) sum(x1 > 0))
  Even <- Dc / S
  tab[1,] <- D
  tab[2,] <- Dc
  tab[3,] <- Dr
  tab[4,] <- Even

  colnames(tab) <- colnames(data.simps)
  rownames(tab) <- c("D", "Dc", "Dr", "Even")
  tab <- round(tab, digits = 2)

  if (plot == T){
    par(mfrow = c(1, 1), mar = c(5, 4, 0.5, 0.5), las = 2)
    maxD <- max(tab[2, ])
    posgr <- barplot(Dc, plot = F)
    barplot(Dc, ylim = c(0, maxD * 1.2), col = col, ...)
    title(ylab = ylab, line = 3)
    Dc1 <- round(Dc, digits = 2)
    text(posgr, Dc1, lab = Dc1, cex = 0.65, pos = 3)
  }
  return(tab)
}
