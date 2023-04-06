Chao1<-function(x, first.col = 2, graph = T, legh = 50, legl = 10, ylab = "Abundance",
                  col = c("gray0", "gray48", "gray84"), ...){

  x<-x[,first.col:ncol(x)]

  F1<-apply(x, 2, function(f1) sum(f1==1))
  F2<-apply(x, 2, function(f2) sum(f2==2))
  S<-apply(x, 2, function(x1) sum(x1>0))

  Sobs<-(F1^2)/(2*F2)

  table1<-rbind(F1,F2,Sobs)

  rownames(table1)<-c("F1", "F2", "Chao1")
  table1<-round(table1, digits = 2)
  print(table1)
  print(S)

  if (graph == T){
    par(mfrow=c(1,1), mar=c(3.85,4,4,0.5), las = 1)


  }

  return(table1)
}
