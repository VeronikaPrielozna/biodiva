#' menhinick - Margalef’s index calculating function
#'
#' @param df xxx
#' @param first.col xxx
#' @param plot xxx
#' @param col xxx
#' @param ylab xxx
#' @param xlab xx
#' @param ... xx
#'
#' @return xxx
#'
#' @examples
#' xxx
#' @export menhinick
#'

menhinick <- function(x, first.col = 2, plot = T, col = "gray",
                      ylab = "Menhinick´s index value", xlab = "Samples", ...){
  x <- x[,(attributes(x)$"First column"):ncol(x)]
  S <- apply(x, 2, function(x1) sum(x1 > 0))
  N <- apply(x, 2, sum, na.rm = TRUE)
  tab <- rbind(S, N)
  tabS <- matrix(S, nrow = 1)
  tabN <- matrix(N, nrow = 1)
  DMn_m <- matrix()

  for (i in 1:ncol(tab)){
    DMn <- (tabS[,i]) / sqrt(tabN[,i])
    DMn_m <- rbind(DMn_m, DMn)
  }

  DMn_m <- as.data.frame(DMn_m[2:nrow(DMn_m),1])
  colnames(DMn_m) <- "D"
  DMn_m <- round(DMn_m, digits = 2)
  DMn <- DMn_m[,1,drop = F]
  rownames(DMn) <- as.vector(colnames(tab))
  maxMn <- max(DMn)
  plot_Dmn <- as.table(t(DMn_m))
  if (plot == T){
    par(mfrow = c(1,1), mar = c(5, 4, 0.5, 0.5), las = 2)
    posgr <- barplot(plot_Dmn, plot = F)
    barplot(plot_Dmn, ylim = c(0, maxMn * 1.25), names.arg = rownames(DMn), col = col, xaxs = "r", ...)
    text(posgr, plot_Dmn, lab = plot_Dmn, cex = 0.65, adj = c(-0.2,0.3), srt = 90)
    title(ylab = ylab, line = 2.5)
    title(xlab = xlab, line = 4)
  }

  return(DMn)
}
