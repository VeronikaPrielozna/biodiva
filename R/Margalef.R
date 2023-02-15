Margalef<-function(x, first.col=2, graph = T, colour="gray"){
  x<-x[,first.col:ncol(x)]
  S<-apply(x, 2, function(x1) sum(x1>0))
  N<-apply(x, 2, sum, na.rm = TRUE)
  tab<-rbind(S,N)
  tabS<-matrix(S, nrow = 1)
  tabN<-matrix(N, nrow = 1)
  tabDMg<-matrix()

  for (i in 1:ncol(tab)){
    DMg<-(tabS[,i]-1)/log(tabN[,i])
    tabDMg<-cbind(tabDMg, DMg)
  }

  tabDMg<-tabDMg[,-1]
  DMg<-round(as.vector(tabDMg), digits = 2)
  tab_Mg<-rbind(DMg)
  colnames(tab_Mg)<-colnames(tab)

  maxMg<-max(tab_Mg[1,])
  textMg<-"Margalef´s index"

  if (graph == T){
    par(mfrow=c(1,1), mar=c(3.8,2,4,1))
    barplot(tab_Mg[1,], ylim = c(0,maxMg + 1), las = 2, main = textMg, cex.main=1.5, font.main = 1,
            col = colour)
  }
  return(tab_Mg[1,])
}
