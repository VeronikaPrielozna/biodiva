sd.eco<-function(x, first.col = 2, table = 1, graph = 1, pos_leg = 8.5){
  x<-x[,first.col:ncol(x)]

  # Species richness
  F1<-apply(x, 2, function(f1) sum(f1==1))
  F2<-apply(x, 2, function(f2) sum(f2==2))
  Other<-apply(x, 2, function(f4) sum(f4>2))
  Total<-F1 + F2 + Other
  table1<-rbind(F1,F2,Other,Total)
  maxS<-max(table1[1,] + table1[2,])
  maxS2<-max(table1[4,])

  # Abundance
  TotalA<-apply(x, 2, function(f5) sum(f5))
  F2A<-F2*2
  OtherA<-TotalA-F1-F2A
  table2<-rbind(F1,F2A,OtherA,TotalA)
  rownames(table2)<-c("F1", "F2", "Other", "Total")
  maxA<-max(table2[1,] + table2[2,])
  maxA2<-max(table2[4,])

  if (table == 1){
    print("Species richness")
    print(table1)
    print(" ")
    print("Abundance")
    print(table2)
  }

  if (table == 2){
    print("Species richness")
    print(table1)
  }

  if (table == 3){
    print("Abundance")
    print(table2)
  }

  if(graph == 1){
    par(mfrow=c(2,1), mar=c(3.5,5,0.3,0.3))
    posgr = barplot(table1, plot = F)
    barplot(table1[-c(3,4),], ylim = c(0,maxS + 5),las = 2, ylab = "Number of species", xaxt = "n",
            legend.text = c("F1", "F2"), col = c( "lightcyan4", "lightcyan3"),
            args.legend = list(bty = "n", x = pos_leg, y = -4, ncol = 3))
    text(posgr, F1+F2, lab=Total, cex=0.9, pos=3)

    posgr = barplot(table2, plot = F)
    barplot(table2[-c(3,4),], ylim = c(0,maxA + 5),las = 2, las = 2, ylab = "Abundance",
            legend = F, col = c("lightcyan4", "lightcyan3"))
    text(posgr, F1+2*F2, lab = TotalA, cex=0.9, pos=3)
  }

  if (graph == 2){
    par(mfrow=c(1,1), mar=c(3.85,4,4,0.5))
    posgr = barplot(table1, plot = F)
    barplot(table1[-4,], ylim = c(0,maxS2 + 5), las = 2, ylab = "Number of species",
            legend.text = c("F1", "F2", "Other"), col = c("lightcyan4", "lightcyan3","azure2"),
            args.legend = list(bty = "n", x = pos_leg, y = maxS2 + 7, ncol = 3))
    text(posgr, Total, lab=Total, cex=0.9, pos=3)
  }

  if (graph == 3){
    par(mfrow=c(1,1), mar=c(3.85,4,4,0.5))
    posgr = barplot(table2, plot = F)
    barplot(table2[-4,], ylim = c(0, maxA2 + 30), las = 2, ylab = "Abundance",
            legend.text = c("F1", "F2", "Other"), col = c("lightcyan4", "lightcyan3","azure2"),
            args.legend = list(bty = "n", x = pos_leg, y = maxA2+20, ncol = 3))
    text(posgr, TotalA, lab=TotalA, cex=0.9, pos=3)
  }
}

