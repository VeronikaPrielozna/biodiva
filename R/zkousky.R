library(readxl)
test_data <- read_excel("data-raw/test_data.xlsx")
test_data.1<-loadData()
attributes(test_data.1)

abundance(test_data.1, col = c("white", "darkgray", "darkcyan"),legh = 150, legl = 12, ncol = 3)

