args = commandArgs(trailingOnly=TRUE) 
if (length(args)==0) {
  stop("USAGE: Rscript hw1_104753503.R input", call.=FALSE) 
} 
i <- 1
while (i< length(args)){
 if (args[i] == "-files") {
  inputfile <- args[i+1]
  i <- i+1
 } else if (args[i] == "-out"){
   outputfile <- args[i+1]
   i <- i+1
 }
i <- i+1
}
hw1 <- read.csv(inputfile)
weight <- round(max(hw1$weight),2)
height <- round(max(hw1$height),2)
result_set <- tools::file_path_sans_ext(basename(inputfile)) #get filename without ext
result = c("set", colnames(hw1[2:3]), result_set,weight, height)
matrix(result, nrow = 2, ncol = 3, byrow = T)
write.table(matrix(result, nrow = 2, ncol = 3, byrow = T), file = outputfile ,sep = ",", row.names = F, col.names = F)
