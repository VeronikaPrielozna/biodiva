soren.eco<-function(x, first.col=2, table=T, graph=T, txt=T, txs=1){
#notes Pavel
    data.jac<-x[,first.col:ncol(x)]
  m1<-matrix(0,ncol(data.jac), ncol(data.jac))
  for (i in 1:(ncol(data.jac)-1)){
    x1<-data.jac[,i]
    for (j in (i+1):ncol(data.jac)){
      x2<-data.jac[,j]
      m1[i,j]<-2*sum(x1>0 & x2>0)/(sum(x1>0) + sum(x2>0))
      m1[j,i]<-sum(abs(x1 - x2)) / sum(x1 + x2)
    }
  }
  m1<-round(m1, digits = 2)
  par(mfrow=c(1,1), mar=c(0.5,4,3,1),mgp=c(1,0,0))
  plot(0:ncol(data.jac),0:ncol(data.jac),type="n",axes=0,xlab="",ylab="")
  uv<-ceiling(max(m1[upper.tri(m1)])*10)/10
  lv<-floor(min(m1[upper.tri(m1)])*10)/10
  m2<-seq(lv-(uv-lv)/10,uv,length=12)
  uv<-ceiling(max(m1[lower.tri(m1)])*10)/10
  lv<-floor(min(m1[lower.tri(m1)])*10)/10
  m3<-seq(lv-(uv-lv)/10,uv,length=12)

  if (graph==T){
    for (i in 1:10) {
      for(j in 1:10) {
        if(j>i) mcol=as.character(cut(m1[i,j],breaks=m2,labels=heat.colors(11)[11:1]))
        if(j==i) mcol="black"
            if (j<i) mcol=as.character(cut(m1[i,j],breaks=m3,labels=heat.colors(11)[11:1]))

            rect(j-1,10-i,j,10-i+1,border="white", col=mcol)
            text(j-0.5,10-i+0.5,label=m1[i,j], cex = txs)
      }
    }
    axis(3,at = seq(0.5, ncol(data.jac), by = 1),lab = colnames(data.jac),las = 1, lty = 0)
    axis(2, at = seq(ncol(data.jac)-0.5, 0), lab = colnames(data.jac), las = 2, lty = 0)
  }
  colnames(m1)<-colnames(data.jac)
  rownames(m1)<-colnames(data.jac)

  if (table==T){
    print(m1)
  }
}
