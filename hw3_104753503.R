library('ROCR')

calConfusionMatrix <- function(pred, ref, target) { 
  tandf = c(pred == ref) 
  pandn = c(pred == target) 
  confusionmatrix <- table(truth = tandf, prediction = pandn) 
  return (confusionmatrix) 
} 

calsen <- function(pred, ref, target) { 
  confusionmatrix <- calConfusionMatrix(pred, ref, target) 
  return (confusionmatrix[4] / (confusionmatrix[4] + confusionmatrix[1])) 
} 

calspe <- function(pred, ref, target) { 
  confusionmatrix <- calConfusionMatrix(pred, ref, target) 
  return (confusionmatrix[4] / (confusionmatrix[4] + confusionmatrix[3])) 
} 

calper <- function(pred, ref, target) { 
  precision = calspe(pred, ref, target) 
  recall = calsen(pred, ref, target) 
  return (2 * precision * recall / (precision + recall)) 
} 

calauc <- function(predscore, ref) { 
  eval <- prediction(predscore, ref) 
  auc <- attributes(performance(eval, 'auc'))$y.values[[1]] 
  return (auc) 
} 


# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_104753503.R --target male|female --files file1 file2 ... filen –-out out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--files"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

# read files
names<-c()
perResult <- c() 
aucResult <- c() 
senResult <- c() 
speResult <- c() 

for(file in files)
{
  name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  
  #cal round value
  per <- round(calper(d$prediction, d$reference, query_m), digit=2) 
  auc <- round(calauc(d$pred.score, d$reference), digit=2) 
  sen <- round(calsen(d$prediction, d$reference, query_m), digit = 2) 
  spe <- round(calspe(d$prediction, d$reference, query_m), digit = 2) 
  
  names<-c(names,name)
  perResult <- c(perResult, per) 
  aucResult <- c(aucResult, auc) 
  senResult <- c(senResult, sen) 
  speResult <- c(speResult, spe)
}

out_data<-data.frame(method = names, sensitivity = sen, specificity = spe, per = perResult, stringsAsFactors = F)
index<-apply(out_data[,-1], 2, which.max)


per.sorted <- sort(out_data$per)
len <- length(out_data$per)
sec_per <- per.sorted[(len-1)]

file1 <- read.table(index[3], header = T, sep = ",")
file2 <- read.table(sec_per, header = T, sep = ",")

ResultTable <- table(file1$prediction, file2$prediction)
print(ResultTable)
print(fisher.test(ResultTable)$p.value)

# output file
out_data<-rbind(out_data,c("highest",names[index]))
write.table(out_data, file=out_f, sep = ",", row.names = F, quote = F)