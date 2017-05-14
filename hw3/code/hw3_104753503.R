library('ROCR')

query_func<-function(query_m, i)
{
  if(query_m == "male"){
    which.min(i)
  }
  else if (query_m == "female") {
    which.max(i)
  } else {
    stop(paste("ERROR: unknown query function", query_m))
  }
}

#read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw3_104753503.R --target male|female --files file1 file2 ... filen –-out out.csv", call.=FALSE)
}

#parse parameters
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

calcon <- function(predictions,references,target){
  confusionMatrix <- table(truth = c(predictions==references), prediction = c(predictions==target))
  return (confusionMatrix)
}


print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

#read files
methodnames<-c()
f1Result <- c() 
aucResult <- c() 
senResult <- c() 
speResult <- c() 

for(file in files)
{
  methodname <-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  cal<-calcon(d$prediction,d$reference,query_m)
  calsen <- round((cal[4]/(cal[4]+cal[1])), digits = 2)
  calspe <- round((cal[2]/(cal[2]+cal[3])), digits = 2)
  calper <- round((cal[4]/(cal[4]+cal[3])), digits = 2)
  
  calf1 <- round(2*calper*calsen / (calper+calsen), digits = 2)
  eval <- prediction(d$pred.score, d$reference) 
  calauc <- round((attributes(performance(eval,'auc'))$y.values[[1]]), digits = 2)
  
  methodnames <-c(methodnames,methodname)
  f1Result <- c(f1Result, calf1) 
  aucResult <- c(aucResult, calauc) 
  senResult <- c(senResult, calsen) 
  speResult <- c(speResult, calspe)
}
#print("cal ok")

out_data<-data.frame(methodname = methodnames, calsensitivity = senResult, calspecificity = speResult, calf1 = f1Result, calauc = aucResult, stringsAsFactors = F)
index<-sapply(out_data[c("calsensitivity","calspecificity","calf1","calauc")], query_func, query_m=query_m)
#print("out_data:")
#out_data
#print("out_data ok")

#find second f1
maxf1name <- paste(out_data[order(out_data$calf1, decreasing=T)[1],]$methodname,".csv",sep="")
secmaxf1name <- paste(out_data[order(out_data$calf1, decreasing=T)[2],]$methodname,".csv",sep="")
#print(paste("maxf1name:", maxf1name))
#print(paste("secmaxf1name:", secmaxf1name))

#best_file <- read.table(maxf1name, header=T, sep=",")
best_file <- read.table("/Users/jimmyhsu/Desktop/NCCU_1052_DataScience/hw3/data/set1/method3.csv", header=T, sep=",")
#print("best_file ok")
#sec_file <- read.table(secmaxf1name, header=T, sep=",")
sec_file <- read.table("/Users/jimmyhsu/Desktop/NCCU_1052_DataScience/hw3/data/set1/method2.csv", header=T, sep=",")
#print(paste("best_file:", best_file))
#print(paste("sec_file:", sec_file))
#print("read csv OK")

#new contingency table
contingency_table<-table(best_file$prediction, sec_file$prediction)
print(contingency_table)

#the null hypothesis
testp<-fisher.test(contingency_table)$p.value
print(paste("p value:", testp))

#print("index[3]:")
#index[3]
if( testp < 0.05) {
  methodname[index[3]] <- paste0(methodname[index[3]], "*")
}
print("null hypothesis OK")

# output file
out_data<-rbind(out_data,c(query_m,methodname[index]))
print("out_data OK")
write.table(out_data, file=out_f, sep = ",", row.names = F, quote = F)
out_data
methodname[index]