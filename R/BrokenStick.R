BrokenStick<-function(x, first.col = 2, table=T, graph = T, ...){
  x<-x[,first.col:ncol(x)]
  data.abund<-x
  S<-sum(apply(x, 1, sum, na.rm = TRUE)>0)
  N<-apply(x, 2, sum, na.rm = TRUE)
  Bs<-rep(0,S)
  N<-sum(data.abund)

  for (i in 1:S) {
    Bs[i]<-sum(1/(i:S))/S * N
  }

  Bst<-data.frame(Bs)
  Bst<-cbind(Bst, new_col = log10(Bs))
  colnames(Bst)<-c("Bs", "Log10(Bs)")
  Bst<-round(Bst, digits = 2)

  if (graph == T){
    par(mfrow=c(2,1), mar=c(3,4,1,0.3), las = 2)
    maxB1<-max(Bs)
    maxB2<-max(Bst[,2])
    minB<-min(Bst[,2])
    plot(Bs, ylim = c(0, maxB1 + 2), xlim = c(0,S))
    plot(log10(Bs), ylim = c(minB, maxB2 + 0.2), xlim = c(0,S))
  }

  if (table==T){
    return(Bst)
  }
}
