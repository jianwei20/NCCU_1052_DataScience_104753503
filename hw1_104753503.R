require(RCurl)
URL <- "https://raw.githubusercontent.com/winterthink/NCCU_1052_DataScience_104753503/master/test.1.csv"
hw1 <- getURL(URL)
hw1 <- read.csv(textConnection(hw1))
weight = round(max(hw1$weight),2)
height = round(max(hw1$height),2)
set <- tools::file_path_sans_ext(basename(URL)) #get filename without ext
result = c("set", colnames(hw1[2:3]), set,weight, height)
matrix(result, nrow = 2, ncol = 3, byrow = T)
write.table(matrix(result, nrow = 2, ncol = 3, byrow = T), file = "104753503_result.csv",sep = ",", row.names = F, col.names = F)