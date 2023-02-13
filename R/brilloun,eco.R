#' brillouin.eco - Brillouin index and evenness for Brillouin index
#'
#' @description Calculation and graphical visualization of Brillouin index and evenness for Brillouin index
#' @usage brillouin.eco(x, first.col = 2, table = T, graph = T)
#'
#' @param x data
#' @param first.col integer, the first column for the analysis
#' @param table logical, if TRUE table will be creating (T) or uncreating (F)
#' @param graph logical, if TRUE graphical output will be drawn graph
#'
#' @examples brillouin.eco(x)

#' @details The function brillouin.eco provides the calculations of Brillouin index (HB), maximal diversity value (Hbmax) and evenness for Brillouin index for every sample in the uploaded dataset (x). With dataset is worked from the user's specified column (first.col=2). This function returns tabular (table=T) and graphical outputs (graph=T).

#' @references
#' Krebs C.J., 2014: Ecological Methodology. 3rd ed. (in prep).
#' Pielou E.C., 1966: The measurement of diversity in different types of biological collections. Journal of Theoretical Biology 13: 131–144.

brillouin.eco<-function(x, first.col = 2, table = T, graph = T){
  data.br<-x[,first.col:ncol(x)]
  mat<-matrix(nrow = 3)
  tab<-data.frame(mat)

  for (i in 1:ncol(data.br)) {
    x1<-data.br[,i]
    N<-sum(x1)
    Hb<-(lfactorial(N) - sum(lfactorial(x1)))/N
    k <- length(x1)
    c <- N %/% k
    d <- N %% k
    Hbmax<-(lfactorial(N) - (k-d)*lfactorial(c) - d*lfactorial(c+1))/N
    Even<-Hb/Hbmax
    vec<-c(Hb,Hbmax, Even)
    tab<-cbind(tab, new_col = vec)
  }
  tab<-tab[,-1]
  colnames(tab)<-colnames(data.br)
  rownames(tab)<-c("HB","Hbmax","Even")
  tab<-round(tab, digits = 2)

  if (table == T){
    print("Brillouin´s calculations")
    print(tab)
  }

  if (graph == T){
    maxB<-max(tab[2,])

    tab1<-as.matrix(tab[1,])

    par(mfrow=c(1,1), mar=c(3.85,4,0.5,0.5))
    barplot(tab1, ylim = c(0, maxB + 1), col = "lightcyan3", ylab="Brilloun´s index")

    posgr = barplot(tab1, plot = F)
    tab2<-as.matrix(tab[2,])
    points(posgr,tab[2,], pch = 20, cex = 1.5)
    text(posgr,tab[2,],lab=tab[2,],cex=0.9,pos=3)
  }
}
