Menhinick<-function(x, first.col=2, graph = T, colour="gray"){
  x<-x[,first.col:ncol(x)]
  S<-apply(x, 2, function(x1) sum(x1>0))
  N<-apply(x, 2, sum, na.rm = TRUE)
  tab<-rbind(S,N)
  tabS<-matrix(S, nrow = 1)
  tabN<-matrix(N, nrow = 1)
  tabDMn<-matrix()

  for (i in 1:ncol(tab)){
    DMn<-(tabS[,i])/sqrt(tabN[,i])
    tabDMn<-cbind(tabDMn, DMn)
  }

  tabDMn<-tabDMn[,-1]
  DMn<-round(as.vector(tabDMn), digits = 2)
  tab_Mn<-rbind(DMn)
  colnames(tab_Mn)<-colnames(tab)
  maxMn<-max(tab_Mn[1,])
  textMn<-"Menhinick´s index"

  if (graph == T){
    par(mfrow=c(1,1), mar=c(3.8,2,4,1))
    barplot(tab_Mn[1,], ylim = c(0,maxMn + 1), las = 2, main = textMn,cex.main=1.5, font.main = 1,
            col = colour)
  }

  return(tab_Mn[1,])
}
