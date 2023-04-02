Margalef<-function(x, first.col=2, graph = T, col="gray", textMg = "Margalef´s index", main = textMg, cex.main = 1.5, font.main = 1, ...){
  x<-x[,first.col:ncol(x)]
  S<-apply(x, 2, function(x1) sum(x1>0))
  N<-apply(x, 2, sum, na.rm = TRUE)
  tab<-rbind(S,N)
  tabS<-matrix(S, nrow = 1)
  tabN<-matrix(N, nrow = 1)
  vec_DMg<-c()

  for (i in 1:ncol(tab)){
    DMg<-(tabS[,i]-1)/log(tabN[,i])
    vec_DMg<-append(vec_DMg, DMg)
  }

  DMg<-round(vec_DMg, digits = 2)
  names(DMg)<-colnames(tab)
  maxMg<-max(DMg)

  if (graph == T){
    par(mfrow=c(1,1), mar=c(3.8,3,4,1), las = 2)
    barplot(DMg, ylim = c(0,maxMg + 1), main = main, cex.main = cex.main, font.main = font.main,
            col = col)
  }
  return(DMg)
}
