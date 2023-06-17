#' jacren - Jaccard and Renconen similarity index calculating function
#'
#' @description Calculation of Jaccard and Renconen similarity index
#' @usage jacren(df, plot = T, cex = 0.75)
#' @param df A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).
#' @param plot Should a plot for results of calculations be plotted? By default, the plot is rendered.
#' @param cex Numeric. Size of the text in the plot cells.
#'
#' @return xxx
#'
#' @examples
#' jacren(test_data)
#'
#' @export jacren
#'

jacren <- function(x, plot = T, cex = 0.4){
  data.jac <- x[, attributes(x)$"First column" : ncol(x)]
  ncplot <- ncol(data.jac)
  m1 <- matrix(0, ncplot, ncplot)

  for (i in 1:(ncplot - 1)){
    x1 <- data.jac[,i]
    for (j in (i + 1) : ncplot){
      x2 <- data.jac[,j]
      m1[i,j] <- sum(x1 > 0 & x2 > 0) / sum(x1 > 0 | x2 > 0)
      m1[j,i] <- sum(apply(cbind(a = x1 / sum(x1), b = x2 / sum(x2)), 1, min))
      m1 <- round(m1, digits = 2)
    }
  }
  m1 <- round(m1, digits = 2)
  par(mfrow = c(1,1), mar = c(1, 4, 4, 1), mgp = c(1, 0, 0))
  plot(0 : ncplot, 0 : ncplot, type = "n", axes = 0, xlab = "",
       ylab = "")
  uv <- ceiling(max(m1[upper.tri(m1)]) * ncplot) / ncplot
  lv <- floor(min(m1[upper.tri(m1)]) * ncplot) / ncplot
  m2 <- seq(lv - (uv - lv) / ncplot, uv, length = 12)
  uv <- ceiling(max(m1[lower.tri(m1)]) * ncplot) / ncplot
  lv <- floor(min(m1[lower.tri(m1)]) * ncplot) / ncplot
  m3 <- seq(lv - (uv - lv) / ncplot, uv, length = 12)

  if (plot == T){
    for (i in 1:ncplot) {
      for(j in 1:ncplot) {
        if(j > i) mcol = as.character(cut(m1[i, j], breaks=m2, labels = heat.colors(11)[11 : 1]))
        if(j == i) mcol = "black"
            if (j < i) mcol = as.character(cut(m1[i, j], breaks = m3, labels = heat.colors(11)[11 : 1]))

            rect(j - 1, ncplot - i, j, ncplot - i + 1, border = "white", col = mcol)
            text(j - 0.5, ncplot - i + 0.5, label = m1[i, j], cex = cex)
      }
    }
    axis(3, at = seq(0.5, ncplot, by = 1), lab = colnames(data.jac), las = 2, lty = 0)
    axis(2, at = seq(ncplot - 0.5, 0), lab = colnames(data.jac), las = 2, lty = 0)
    axis(4, at = ncplot / 2, lab = "Jaccard index", las = 3, line = -0.5, tick = F)
    axis(1, at = ncplot / 2, lab = "Renkonen index", las = 1, line = -0.25, tick = F)
  }
  colnames(m1) <- colnames(data.jac)
  rownames(m1) <- colnames(data.jac)

  return(m1)
}
