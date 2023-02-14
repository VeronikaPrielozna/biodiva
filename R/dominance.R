Dominance<-function(x, first.col = 2, table = 1, graph = 1, pos_leg = 11){
  calDom<-function(x) table(cut(x,breaks=c(0,2,5,12,40,100), labels = c("Sr", "R", "Sd.", "D", "E")))
  tableA<-apply(x[-1],2,calDom)
  tableR<-apply(tableA,2,function(tableA) tableA/sum(tableA))
  tableR1<-round(tableR, digits = 2)
  maxA<-max(apply(tableA, 2, function(x) sum(x)))

  textA<-"Absolute proportion of species richness for classes of dominance: "
  textB<-"Relative proportion of species richness for classes of dominance: "

  if (table == 1){
    cat(paste(textA, "\n\n"))
    print(tableA)
    cat("\n")
    cat(paste(textB, "\n\n"))
        print(tableR1)
  }

  if (table == 2){
    cat(paste(textA, "\n\n"))
    print(tableA)
  }

  if (table == 3){
    cat(paste(textB, "\n\n"))
    print(tableR1)
  }

  if (graph == 1){
    par(mfrow=c(2,1), mar=c(3.8,4,2,1))

    barplot(tableA, ylim = c(0,maxA + 5), ylab = "Total number of species", legend=TRUE,
            args.legend = list(x = pos_leg, y = -10, bty = "n", ncol = 6),las = 2,
            col = c("white", "snow2", "grey", "snow4", "gray24"), xaxt = "n")

    barplot(tableR,ylab = "Relative frequency", legend=F,
            col = c("white", "snow2", "grey", "snow4", "gray24"), las = 2)
  }

  if (graph == 2){
    par(mfrow=c(1,1), mar=c(3.8,4,4,1))
    barplot(tableA, ylim = c(0,maxA + 5), ylab = "Total number of species", legend = T,
            args.legend = list(x = pos_leg, y = maxA + 8, bty = "n", ncol = 6),
            las = 2, col = c("white", "snow2", "grey", "snow4", "gray24"))
  }

  if (graph == 3){
    par(mfrow=c(1,1), mar=c(3.8,4,4,1))
    barplot(tableR, legend=TRUE,
            args.legend = list(x = pos_leg, y = 1.2, bty = "n", ncol = 1, ncol = 6),
            col = c("white", "snow2", "grey", "snow4", "gray24"), las = 2)
  }
}
