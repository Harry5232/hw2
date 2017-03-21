#作業2_new 完成版 103753027 顏碩亨

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1.R -query min|max -files file1 file2 ... filen –out out.csv", call.=FALSE)
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
print(paste("positive target :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

# read files
Meths <- c()
Sens <- c() 
Spes <- c()
F1s <- c()
AUC <- c()
sens_max <- 0
spec_max <- 0
F1_max <- 0
AUC_max <- 0

for(file in files)
{
  TP <- 0
  TN <- 0
  FP <- 0
  FN <- 0
  Pre <- 0 #precision
  Rec <- 0 #recall 
  
  d<-read.table(file, header=T,sep=",")
  for(num in c(1:nrow(d))){
    if(d[num,2] == d[num,3]){
      if(d[num,3] == query_m){
        TP <- TP + 1
      }else{
        TN <- TN + 1
      }
      
    }else if(d[num,2] != d[num,3]){
      if(d[num,3] == query_m){
        FP <- FP + 1
      }else{
        FN <- FN + 1
      }
    }
  }
  Pre <- TP/(TP+FP)
  Rec <- TP/(TP+FN)
  F1 <- (2 * Pre * Rec)/(Pre + Rec)
  Sen <- (TP/(TP + FN))
  Spe <- (TN/(TN + FP))
  
  

}
out_data<-data.frame(meths, sens, F1, AUC, stringsAsFactors = F)



# output file
#最下面一列做結尾
write.table(out_data, file=out_f, row.names = F, quote = F)

#作業2_new 完成版 103753027 顏碩亨