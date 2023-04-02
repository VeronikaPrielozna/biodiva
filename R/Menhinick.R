Menhinick<-function(x, first.col=2, graph = T, col="gray", textMn = "Menhinick´s index", main = textMn, cex.main=1.5, font.main = 1, ...){
  x<-x[,first.col:ncol(x)]
  S<-apply(x, 2, function(x1) sum(x1>0))
  N<-apply(x, 2, sum, na.rm = TRUE)
  tab<-rbind(S,N)
  tabS<-matrix(S, nrow = 1)
  tabN<-matrix(N, nrow = 1)
  vec_DMn<-c()

  for (i in 1:ncol(tab)){
    DMn<-(tabS[,i])/sqrt(tabN[,i])
    vec_DMn<-append(vec_DMn, DMn)
  }

  DMn<-round(vec_DMn, digits = 2)
  names(DMn)<-colnames(tab)
  maxMn<-max(DMn)

  if (graph == T){
    par(mfrow=c(1,1), mar=c(3.8,3,4,1), las = 2)
    barplot(DMn, ylim = c(0,maxMn + 1), main = main, cex.main = cex.main, font.main = font.main,
            col = col)
  }

  return(DMn)
}
