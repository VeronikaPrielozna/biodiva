Dominance<-function(x, first.col = 2, table = 1, graph = 1, legl = 11, col = col){
  calDom<-function(x) table(cut(x,breaks=c(0,2,5,12,40,100), labels = c("Sr", "R", "Sd.", "D", "E")))
  tableA<-apply(x[-1],2,calDom)
  tableR<-round(apply(tableA,2,function(tableA) tableA/sum(tableA)), digits = 2)
  maxA<-max(apply(tableA, 2, function(x) sum(x))) # max in tableA

  textA<-"Absolute proportion of species richness for classes of dominance: "
  textB<-"Relative proportion of species richness for classes of dominance: "
  textA1<-"Total number of species"
  textB1<-"Relative frequency"

  if (table == 1){
    cat(paste(textA, "\n\n"))
    print(tableA)
    cat("\n")
    cat(paste(textB, "\n\n"))
    print(tableR)
  }

  if (table == 2){
    cat(paste(textA, "\n\n"))
    print(tableA)
  }

  if (table == 3){
    cat(paste(textB, "\n\n"))
    print(tableR)
  }

  if (graph == 1){
    par(mfrow=c(2,1), mar=c(3.8,4,2,1))

    barplot(tableA, ylim = c(0,maxA + 5), ylab = textA1, legend=TRUE,
            args.legend = list(x = legl, y = -10, bty = "n", ncol = 6),las = 2,
            col = col, xaxt = "n")

    barplot(tableR,ylab = textB1, legend=F,
            col = col, las = 2)
  }

  if (graph == 2){
    par(mfrow=c(1,1), mar=c(3.8,4,4,1))
    barplot(tableA, ylim = c(0,maxA + 5), ylab = textA1, legend = T,
            args.legend = list(x = legl, y = maxA + 8, bty = "n", ncol = 6),
            las = 2, col = col)
  }

  if (graph == 3){
    par(mfrow=c(1,1), mar=c(3.8,4,4,1))
    barplot(tableR, legend=TRUE,
            args.legend = list(x = legl, y = 1.2, bty = "n", ncol = 1, ncol = 6),
            col = col, las = 2)
  }
}