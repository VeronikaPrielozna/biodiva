abundance<-function(x, first.col = 2, graph = T, legh = 50, legl = 10, ylab = "Abundance",
                    col = c("gray0", "gray48", "gray84"), ...){
  x<-x[,first.col:ncol(x)]

  TotalA<-apply(x, 2, function(f5) sum(f5))
  F1<-apply(x, 2, function(f1) sum(f1==1))
  F2A<-apply(x, 2, function(f2) sum(f2==2))*2

  OtherA<-TotalA-F1-F2A
  Atab<-rbind(F1,F2A,OtherA,TotalA)
  rownames(Atab)<-c("F1", "F2", "Other", "Total")
  maxA<-max(Atab[1,] + Atab[2,])
  maxA2<-max(Atab[4,])

  if (graph == T){
    par(mfrow=c(1,1), mar=c(3.85,4,4,0.5), las = 2)
    posgr = barplot(Atab, plot = F)
    barplot(Atab[-4,],legend.text = c(as.expression(bquote('F'['1'])), as.expression(bquote('F'['2'])), "Other"),
            ylab = ylab, ylim = c(0, maxA2 + 30), col = col,
            args.legend = list(bty = "n", x = legl, y = maxA2 + legh, ncol = 3), ...)
    text(posgr, TotalA, lab = TotalA, cex = 0.9, pos = 3)
  }

  return(Atab)
}
