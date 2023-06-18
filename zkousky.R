library(readxl)
test_data <- read_excel("data-raw/test_data.xlsx")
test_data.1<-loadData()
test_data.2<-loadData()
attributes(test_data.1)

Ab<-abundance(test_data.1, col = c("white", "darkgray", "darkcyan"),legh = 150, legl = 12, ncol = 3)

Ri<-richness(test_data.1, col = c("white", "darkgray", "darkcyan"), legh = 15, legl = 12)

Do <- dominance(test_data.1, table = "AR", plot = "A", legh = 50, legl = 13.5, col = c("white", "darkgray", "lightgreen", "darkcyan", "darkgreen"))
Do <- dominance(test_data.1, table = "A", plot = "A", legh = 50, legl = 13.5, col = c("white", "darkgray", "lightgreen", "darkcyan", "darkgreen"))
Do <- dominance(test_data.1, table = "R", plot = "R", ylab = "Relative frequency", legh = 0.3, legl = 15, col = c("white", "darkgray", "lightgreen", "darkcyan", "darkgreen"))

M <- margalef(test_data.1, col = "darkcyan")
M <- menhinick(test_data.1, col = "darkcyan")
str(M)

S <- shannon(test_data.1, log = "2", table = "C")

sim <- simpson(test_data.1, col = "darkcyan")

Br <- brillouin(test_data.1, col = "darkcyan")


JR <- jacren(test_data.1)
JR <- jacren(test_data.2)
SO <- soren(test_data.2)

data <- loadData()

AB <- abundance(data, legver = 100, leghor = 1)
RI <- richness(data, legver = 30, leghor = 1)
DO <- dominance(data, leghor = 1, legver = 30)
DO <- dominance(data, table = "A", leghor = 1, legver = 30)
DO <- dominance(data, table = "R", plot = "R", leghor = 1, legver = 1.2)
MA <- margalef(data)
ME <- menhinick(data)
SA <- shannon(data, table = "E")
SI <- simpson(data)
BR <- brillouin(data)
JA <- jacren(data, cex = 0.7)
SO <- soren(data)

attributes(data)







data[,1:11]


