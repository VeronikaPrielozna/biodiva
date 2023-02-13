shannon.eco<-function(x, first.col = 2, table = 1, graph = T, arrow = T, log_base = 1){
  data.shan<-x[,first.col:ncol(x)]
  mat1<-matrix(nrow = 3)
  Htable<-data.frame(mat1)

  mat2<-matrix(nrow = 2)
  Etable<-data.frame(mat2)

  if (log_base == 1){
    Base<-sum(1/factorial(0:100))
  }

  if (log_base == 2){
    Base<-2
  }

  for(x in 1:ncol(data.shan)){
    x<-data.shan[,x]
    n<-sum(x[x>0])
    S<-length(x[x>0])
    Hshan<--sum(x[x>0]/n*log(x[x>0]/n,Base))
    Hmin<--((n-S+1)/n*log((n-S+1)/n,Base)+(S-1)/n*log(1/n,Base))
    Hmax<-log(S,Base)

    Even1<-Hshan/(Hmax)
    Even2<-(Hshan-Hmin)/(Hmax-Hmin)

    vec1<-c(Hshan,Hmin,Hmax)
    Htable<- cbind(Htable, new_col = vec1)

    vec2<-c(Even1,Even2)
    Etable<- cbind(Etable, new_col = vec2)

  }
  Htable<-Htable[,-1]
  colnames(Htable)<-colnames(data.shan)
  rownames(Htable)<-c("H´","Hmin","Hmax")
  Htable<-round(Htable, digits = 2)

  Etable<-Etable[,-1]
  colnames(Etable)<-colnames(data.shan)
  rownames(Etable)<-c("Even1","Even2")
  Etable<-round(Etable, digits = 2)

  if (table == 1){
    print("Table of Shannon calculations")
    print(Htable)
    print(" ")
    print("Shannon evenness")
    print(Etable)
  }

  if(table == 2){
    print("Table of Shannon calculations")
    print(Htable)
  }

  if(table == 3){
    print("Shannon evenness")
    print(Etable)
  }

  if (graph == T){
    par(mfrow=c(1,1), mar=c(4,4,2,1))
    minH<-min(Htable[2,])
    maxH<-max(Htable[3,])
    posgr = barplot(as.matrix(Htable[1,]), plot = F)
    plot(NULL,ylim = c(minH,maxH),xlim = c(1,ncol(data.shan))
         , xlab = "", xaxt = "n", ylab = "Shannon index")

    points(c(1:ncol(Htable)), Htable[1,], pch = 16)

    table1<-as.matrix(Htable[2,])
    table2<-as.matrix(Htable[3,])
    axis(1,at=1:ncol(data.shan),lab=colnames(data.shan),las=2)

    if (arrow == T){
      for (i in 1:ncol(table1)){
        arrows(i,table1[,i],i,table2[,i],angle=90,code=3,length=0.08)
      }
    }
  }
}
