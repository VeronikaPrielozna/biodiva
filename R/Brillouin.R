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

Brillouin<-function(x, first.col = 2, graph = T, colour="gray", note="Brillouin´s index"){
  data<-x[,first.col:ncol(x)]
  tab<-data.frame(mat<-matrix(nrow = 3))

  for (i in 1:ncol(data)) {
    com<-data[,i] # com = specific community
    N<-sum(com)
    Hb<-(lfactorial(N) - sum(lfactorial(com)))/N
    k <- length(com)
    c <- N %/% k
    d <- N %% k
    Hbmax<-(lfactorial(N) - (k-d)*lfactorial(c) - d*lfactorial(c+1))/N
    Even<-Hb/Hbmax
    vec<-c(Hb,Hbmax, Even)
    tab<-cbind(tab, new_col = vec)
  }
  tab<-tab[,-1]
  colnames(tab)<-colnames(data)
  rownames(tab)<-c("HB","Hbmax","Even")
  tab<-round(tab, digits = 2)

  if (graph == T){
    HB<-as.matrix(tab[1,])
    Hbmax<-tab[2,]
    maxB<-max(Hbmax)

    par(mfrow=c(1,1), mar=c(3.85,4,0.5,0.5))
    barplot(HB, ylim = c(0, maxB + 1), col = colour, ylab=note, las = 2)
    posgr = barplot(HB, plot = F)
    points(posgr,Hbmax, pch = 20, cex = 1.5)
    text(posgr,Hbmax,lab=Hbmax,cex=0.9,pos=3)
  }
  return(tab)
}
