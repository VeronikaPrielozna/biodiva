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
#'
#' @examples
#' Popsat nastavení následujících argumentů
#' přidat vice volání
#' brillouin(test_data.1, col = "darkcyan")
#'
#' @export brillouin
#'

brillouin <- function(x, first.col = 2, plot = T, ylab = "Brillouin's index", xlab = "Samples", col = "black",...){
  data <- x[,(attributes(x)$"First column"):ncol(x)]
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
  rownames(tab) <- c("HB", "HBmax", "Even")
  tab <- round(tab, digits = 2)

  if (plot == T){
    HB <- as.matrix(tab[1,])
    Hbmax <- tab[2,]
    maxB <- max(Hbmax)
    par(mfrow = c(1,1), mar = c(5, 4, 0.5, 0.5), las = 2)
    plot(NULL,ylim = c(min(HB) * 0.8, maxB * 1.2), xlim = c(1, ncol(HB)), xlab = "", xaxt = "n",
         ylab = "",...)
    title(ylab = ylab, line = 2.5)
    title(xlab = xlab, line = 3.5)
    posgr = barplot(HB, plot = F)
    points(c(1:ncol(HB)), tab[1,], pch = 20, cex = 1, col = col)
    axis(1, at = 1:ncol(HB), lab = colnames(HB))
    for (i in 1:ncol(HB)){
      arrows(i, HB[,i], i, Hbmax[,i], angle = 90, code = 2, length = 0.05, col = col)
    }
    text(1 : ncol(HB), Hbmax, lab = Hbmax, cex = 0.65, adj = c(-0.25,0.3), srt = 90)
  }
  tab <- as.data.frame(t(tab))
  return(tab)
}
