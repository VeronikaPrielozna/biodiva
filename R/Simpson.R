#' simpson - Simpson's index calculating function
#'
#' @param df
#' @param first.col
#' @param table
#' @param plot
#' @param ylab
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

simpson <- function(df, first.col = 2, table = T, plot = T, ylab = "Simpson´s index value", ...){
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
    par(mfrow = c(1, 1), mar = c(5, 4, 0.5, 0.5))
    maxD <- max(tab[2, ])
    posgr <- barplot(Dc, plot = F)
    barplot(Dc, ylim = c(0, maxD * 1.2), col = "lightcyan3")
    title(ylab = ylab, line = 3)
    Dc1 <- round(Dc, digits = 2)
    text(posgr, Dc1, lab = Dc1, cex = 0.65, pos = 3)
  }

  if (table == T){
    return(tab)
  }
}
