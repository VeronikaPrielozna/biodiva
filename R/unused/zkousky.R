data<-LoadData()

Ab<-Abundance(data)

Ab<-Abundance(data,xlab="samples",ylab="početnost")

DBI<-LoadDBI()

install.packages("available")
library(available)
available::available("biodiva")
