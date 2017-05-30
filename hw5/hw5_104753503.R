library("e1071")

#read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw5_104753503.R -fold n -out performance.csv", call.=FALSE)
}

#parse parameters
i <- 1 
while(i < length(args))
{
  if(args[i] == "-fold"){
    query_f <- args[i+1]
    i <- i+1
  }else if(args[i] == "-out"){
    out_f<-args[i+1]
    i <- i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i <- i+1
}

#check query
print("PROCESS")
print(paste("fold :", query_f))
print(paste("output file:", out_f))

d <- read.csv("./Archaeal_tfpssm.csv",header=F)
d <- d[,-5603]

testAcc <- c()
trainingAcc <- c()
calibrationAcc <- c()

sample_data <- d[sample(1:query_f, nrow(d), replace = TRUE)]
d_index <- sample(seq_len(nrow(d)), size = round(0.8*nrow(d)))
training_set <- sample_data[d_index, ]
sample_data2 <- sample_data[-d_index, ]
d_index2 <- sample(seq_len(nrow(sample_data2)), size = round(0.5*nrow(sample_data2)))
test_set <- sample_data2[d_index2, ]
calibration_set <- sample_data2[-d_index2, ]


training_model <- svm(V2 ~ ., data = training_set)
accuracy <- function(data_set){
  pred <- predict(training_model, data_set)
  accuracy <- sum(pred == data_set$V2)/length(pred)
  return(accuracy)
}

trainingAcc <-  c(trainingAcc,accuracy(training_set))
calibrationAcc <-  c(calibrationAcc,accuracy(calibration_set))
testAcc <-  c(testAcc,accuracy(test_set))


print(paste("trainingAcc:",trainingAcc))
print(paste("calibrationAcc:",calibrationAcc))
print(paste("testAcc:",testAcc))

row_name <- c("trainning","calibration","test")
accuracy_mean <- c(mean(trainingAcc),mean(calibrationAcc),mean(testAcc))
print(paste("accuracy_mean:",accuracy_mean))

out_data <- data.frame("set" = row_name, "accuracy" = accuracy_mean, stringsAsFactors = F)
write.csv(out_data, file=out_f, sep=",", row.names = FALSE, col.names = FALSE)

