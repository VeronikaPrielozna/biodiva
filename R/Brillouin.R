#' brillouin - Brillouin index and evenness calculating function
#'
#' @description Calculation of Brillouin index and evenness
#' @usage rillouin(df, first.col = 2, plot = T, ylab = "Brillouin's index", col = "gray",...)
#' @param df A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).
#' @param first.col Numeric (integer), the first column of samples (to skip non-relevant columns). By default, this parameter is set to ‘2’.
#' @param plot Should a barplot for results of calculations be plotted? By default, the plot is rendered.
#' @param ylab The text for the y axis label in plot. By default, the text is setted as ‘Brillouin's index’.
#' @param col A vector that contains five components (named after the five colours). By default, the color is set as ‘gray’.
#' @param ... xxx
#'
#' @return A data frame consisting of a column of calculated species richness values (‘F1’, ‘F2’, ‘Other’, ‘Total’) with samples in the rows.

#' @export brillouin()
#'
#' @examples
#' Popsat nastavení následujících argumentů
#' přidat vice volání
#' brillouin(test_data.1, col = "darkcyan")

brillouin <- function(df, first.col = 2, plot = T, ylab = "Brillouin's index", col = "gray",...){
  data <- df[, first.col : ncol(df)]
  tab <- data.frame(mat <- matrix(nrow = 3))

  for (i in 1:ncol(data)) {
    com <- data[,i]
    N <- sum(com)
    Hb <- (lfactorial(N) - sum(lfactorial(com))) / N
    k <- length(com)
    c <- N %/% k
    d <- N %% k
    Hbmax <- (lfactorial(N) - (k - d) * lfactorial(c) - d * lfactorial(c + 1)) / N
    Even <- Hb / Hbmax
    vec <- c(Hb, Hbmax, Even)
    tab <- cbind(tab, new_col = vec)
  }
  tab <- tab[, -1]
  colnames(tab) <- colnames(data)
  rownames(tab) <- c("HB", "Hbmax", "Even")
  tab <- round(tab, digits = 2)

  if (plot == T){
    HB <- as.matrix(tab[1,])
    Hbmax <- tab[2,]
    maxB <- max(Hbmax)
    par(mfrow = c(1,1), mar = c(5, 4, 0.5, 0.5), las = 2)
    barplot(HB, ylim = c(0, maxB * 1.2), col = col, ...)
    title(ylab = ylab, line = 2.5)
    posgr = barplot(HB, plot = F)
    points(posgr, Hbmax, pch = 20, cex = 1)
    text(posgr, Hbmax, lab = Hbmax, cex = 0.7, pos = 3)
  }
  return(tab)
}
