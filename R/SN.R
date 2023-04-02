#' sn.eco - Species richness and abundance
#'
#' @description Calculation of species richness and abundance.
#'
#' @usage  sn.eco(x, first.col = 2, table = T, graph = T)
#' @param x data
#' @param first.col the column from which to work with the data
#' @param table creating (T) or uncreating (F) table
#' @param graph creating (T) or uncreating (F) graph
#'
#' @examples sn.eco(x)
#'
#' @details The function sn.eco provides the calculations of species richness and abundance for uploaded dataset (x). With dataset is worked from the user's specified column (first.col=2). This function provides tabular (table=T) and graphical outputs (graph=T).
#'

SN<-function(x, first.col = 2, table = T, graph = T, col = "gray", ylimS = c(0,maxS + 5), ylimN = c(0, maxN + 50),
             ylabS = "Number of species", ylabN = "Abundance", ...){
  x<-x[,first.col:ncol(x)]
  S<-apply(x, 2, function(x1) sum(x1>0))
  N<-apply(x, 2, sum, na.rm = TRUE)
  tab<-rbind(S,N)
  rownames(tab)<-c("S (number of species)", "N (abundance)")

  if (graph == T){
    par(mfrow=c(2,1), mar=c(3.85,4,0,1), las = 2)
    maxS <- max(tab[1,])
    maxN <- max(tab[2,])
    barplot(tab[1,], ylim = ylimS, ylab = ylabS, xaxt = "n", col = col)
    barplot(tab[2,], ylim = ylimN, ylab = ylabN, col = col)

  }
  if (table == T){
    return(tab)
  }
}
