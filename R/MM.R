#' mm.eco - Margalef's and Menhinick's index
#'
#' @description Calculation of species richness indices (Margalef's and Menhinick's index)
#' @usage mm.eco(x, first.col=2, table=1, graph=1)
#'
#' @param x data
#' @param first.col the column from which to work with the data
#' @param table creating table
#' @param graph creating graph
#'
#' @examples mm.eco(x)
#' mm_index.eco(x, table = 2, graph = 2)
#' mm_index.eco(x, table = 3, graph = 3)

#' @details The function mm.eco provides the calculations of Margalef's (DMg) and Menhinick's (DMm) index for uploaded dataset (x). With dataset is worked from the user's specified column (first.col=2). This function returns tabular (table=T) and graphical outputs (graph=T). Users can set the outputs as:
#' 1 = table and graph are made for both indices
#' 2 = table and graph are made only for Margalef's index
#' 3 = table and graph are made only for Menhinick's index

#' @references
#' Clifford H.T. & Stephenson W., 1975: An introduction to numerical classification. Academic Press: New York, San Francisco, ISBN 978-0-12-176750-1.
#' Magurran A.E., 2013: Measuring Biological Diversity. Wiley-Blackwell: Malden, Oxford, Carlton, ISBN 978-1-118-68792-5.
#' Whittaker R., 1977: Evolution of species diversity in land communities. Evolutionary Biology 10: 1–67.


MM<-function(x, first.col=2, table = 1, graph = 1, colour="gray"){
  x<-x[,first.col:ncol(x)]
  S<-apply(x, 2, function(x1) sum(x1>0))
  N<-apply(x, 2, sum, na.rm = TRUE)
  tab<-rbind(S,N)
  tabS<-matrix(S, nrow = 1)
  tabN<-matrix(N, nrow = 1)
  tabDMg<-matrix()
  tabDMn<-matrix()

  for (i in 1:ncol(tab)){
    DMg<-(tabS[,i]-1)/log(tabN[,i])
    tabDMg<-cbind(tabDMg, DMg)

    DMn<-(tabS[,i])/sqrt(tabN[,i])
    tabDMn<-cbind(tabDMn, DMn)
  }

  tabDMg<-tabDMg[,-1]
  tabDMn<-tabDMn[,-1]

  DMg<-round(as.vector(tabDMg), digits = 2)
  DMn<-round(as.vector(tabDMn), digits = 2)

  tab_Mg_Mn<-rbind(DMg, DMn)
  colnames(tab_Mg_Mn)<-colnames(tab)

  maxMg<-max(tab_Mg_Mn[1,])
  maxMn<-max(tab_Mg_Mn[2,])

  textMg<-"Margalef´s index"
  textMn<-"Menhinick´s index"

  if (table == 1){
    cat(paste(tab_Mg_Mn), "\n")
  }

  if (table == 2){
    cat(paste(textMg),"\n")
    print(tab_Mg_Mn[1,])
  }

  if (table == 3){
    cat(paste(textMn), "\n")
    print(tab_Mg_Mn[2,])
  }

  if (graph == 1){
    par(mfrow=c(2,1), mar=c(3.8,4,2,1))

    barplot(tab_Mg_Mn[1,], ylim = c(0,maxMg + 1), las = 2, ylab = textMg, font.main = 1, xaxt = "n",
            col = colour)
    barplot(tab_Mg_Mn[2,], ylim = c(0,maxMn + 1), las = 2, ylab = textMn, font.main = 1,
            col = colour)
  }

  if (graph == 2){
    par(mfrow=c(1,1), mar=c(3.8,4,4,1))
    barplot(tab_Mg_Mn[1,], ylim = c(0,maxMg + 1), las = 2, main = textMg, cex.main=1.5, font.main = 1,
            col = colour)
  }

  if (graph == 3){
    par(mfrow=c(1,1), mar=c(3.8,4,4,1))
    barplot(tab_Mg_Mn[2,], ylim = c(0,maxMn + 1), las = 2, main = textMn,cex.main=1.5, font.main = 1,
            col = colour)
  }
}
