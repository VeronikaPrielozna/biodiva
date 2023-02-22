Richness<-function(x, first.col = 2, graph = T, legh = 10, legl = 10, col = c("gray0", "gray48", "gray84")){
  x<-x[,first.col:ncol(x)]

  F1<-apply(x, 2, function(f1) sum(f1==1))
  F2<-apply(x, 2, function(f2) sum(f2==2))
  Other<-apply(x, 2, function(f4) sum(f4>2))
  Total<-F1 + F2 + Other
  SRtab<-rbind(F1,F2,Other,Total)
  maxS<-max(SRtab[1,] + SRtab[2,])
  maxS2<-max(SRtab[4,])

  if (graph == T){
    par(mfrow=c(1,1), mar=c(4,3,3,0.5))
    posgr = barplot(SRtab, plot = F)
    barplot(SRtab[-4,], ylim = c(0,maxS2 + 5), las = 2, ylab = "Number of species",
            legend.text = c("F1", "F2", "Other"), col=col,
            args.legend = list(bty = "n", x = legl, y = maxS2 + legh, ncol = 3))
    text(posgr, Total, lab=Total, cex=0.9, pos=3)
  }

  return(SRtab)
}
