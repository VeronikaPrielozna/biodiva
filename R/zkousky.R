library(readxl)
test_data <- read_excel("data-raw/test_data.xlsx")
test_data.1<-loadData()
attributes(test_data.1)

Ab<-abundance(test_data.1, col = c("white", "darkgray", "darkcyan"),legh = 150, legl = 12, ncol = 3)

Ri<-richness(test_data.1, col = c("white", "darkgray", "darkcyan"), legh = 15, legl = 12)

Do <- dominance(test_data.1, table = "AR", plot = "A", legh = 50, legl = 13.5, col = c("white", "darkgray", "lightgreen", "darkcyan", "darkgreen"))
Do <- dominance(test_data.1, table = "A", plot = "A", legh = 50, legl = 13.5, col = c("white", "darkgray", "lightgreen", "darkcyan", "darkgreen"))
Do <- dominance(test_data.1, table = "R", plot = "R", legh = 0.3, legl = 13.5, col = c("white", "darkgray", "lightgreen", "darkcyan", "darkgreen"))
Do[2]

apply(Do, 2, sum)
