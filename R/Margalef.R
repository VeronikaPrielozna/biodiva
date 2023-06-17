#' margalef - Menhinick's index calculating function
#'
#' @param df xxx
#' @param first.col xxx
#' @param plot xxx
#' @param ylab xxx
#' @param xlab xxx
#' @param col xxx
#' @param ... xxx
#'
#' @return xxx
#'
#' @examples
#' xxx
#' @export margalef
#'

margalef <- function(x, plot = T, ylab = "Margalef's index value", xlab = "Samples", col = "gray", ...){
  x <- x[,(attributes(x)$"First column"):ncol(x)]
  S <- apply(x, 2, function(x1) sum(x1 > 0))
  N <- apply(x, 2, sum, na.rm = TRUE)
  tab <- rbind(S, N)
  tabS <- matrix(S, nrow = 1)
  tabN <- matrix(N, nrow = 1)
  DMg_m <- matrix()

  for (i in 1:ncol(tab)){
    DMg <- (tabS[,i] - 1) / log(tabN[,i])
    DMg_m <- rbind(DMg_m, DMg)
  }

  DMg_m <- as.data.frame(DMg_m[2:nrow(DMg_m),1])
  colnames(DMg_m) <- "D"
  DMg_m <- round(DMg_m, digits = 2)
  DMg <- DMg_m[,1,drop = F]
  rownames(DMg) <- as.vector(colnames(tab))
  maxMg <- max(DMg)
  print(DMg)
  DMg_m_posgr <- as.table(t(DMg_m))
    if (plot == T){
    par(mfrow = c(1,1), mar = c(5, 4, 0.5, 0.5), las = 2)
    posgr <- barplot(DMg_m_posgr, plot = F)
    barplot(DMg_m_posgr, ylim = c(0, maxMg * 1.25), names.arg = rownames(DMg), col = col, xaxs = "r", ...)
    text(posgr, DMg_m_posgr, lab = DMg_m_posgr, cex = 0.65, adj = c(-0.2,0.3), srt = 90)
    title(ylab = ylab, line = 3)
    title(xlab = xlab, line = 4)
  }
   return(DMg)
}

