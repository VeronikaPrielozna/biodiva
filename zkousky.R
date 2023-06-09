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

AB <- abundance(data, legh = 10, legl = 29)
RI <- richness(data, legl = 29, legh = 3)
DO <- dominance(data, legl = 31, legh = 8)
MA <- margalef(data)
ME <- menhinick(data)
SA <- shannon(data)
SI <- simpson(data)
BR <- brillouin(data)
JA <- jacren(data)
SO <- soren(data)



