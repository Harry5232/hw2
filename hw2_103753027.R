#作業2_new 完成版 103753027 顏碩亨


library('ROCR')
#library(pROC)
#------------------read parameters---------------
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("", call.=FALSE)
}else{
  
  #----------------parse parameters----------------
  i<-1 
  while(i < length(args))
  {
    if(args[i] == "--target"){
      query_m<-args[i+1]
      i<-i+1
    }else if(args[i] == "--files"){
      j<-grep("--", c(args[(i+1):length(args)], "--"))[1]
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
  
  #-----------show status-----------
  print("PROCESS")
  print(paste("positive target :", query_m))
  print(paste("output file:", out_f))
  print(paste("files      :", files))
  print("Please wait...")
  
  
  method <- c()
  sensitivity <- c() 
  specificity <- c()
  F1 <- c()
  AUCs <- c()
  sens_max <- list(num=0,name="")
  spec_max <- list(num=0,name="")
  F1_max <- list(num=0,name="")
  AUC_max <- list(num=0,name="")
  
  #-----------read files------------
  for(file in files)
  {
    TP <- 0
    TN <- 0
    FP <- 0
    FN <- 0
    Pre <- 0 #precision
    Rec <- 0 #recall 
    AUC <- 0 #AUC
    Pre.score <- c()
    Pre.label <- c()
    
    #------------build confusion table---------
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
          FN <- FN + 1
        }else{
          FP <- FP + 1
        }
      }
      
    }

    #-----------calculate sensitivity  specificity AUC ----------
    Pre <- TP/(TP+FP)
    Rec <- TP/(TP+FN)
    f1 <- (2 * Pre * Rec)/(Pre + Rec)
    f1 <- round(f1,digits = 2)
    F1 <- c(F1,f1)
    sen <- (TP/(TP + FN))
    sen <- round(sen,digits = 2)
    sensitivity <- c(sensitivity,sen)
    Spe <- (TN/(TN + FP))
    Spe <- round(Spe,digits = 2)
    specificity <- c(specificity,Spe)
    file <- strsplit(file,".csv")[[1]]
    method <- c(method,file)
    Pre.score <- c(d$pred.score)
    Pre.label <- c(d$reference)
    Pre.label <- Pre.label - 1
    #AUC <- auc(multiclass.roc(Pre.score,Pre.label), min = 0, max = 1)
    AUC <- prediction(Pre.score,Pre.label)
    AUC <- round(attributes(performance(AUC, 'auc'))$y.values[[1]], 2)
    #AUC <- round(AUC,digits = 2)
    AUCs <- c(AUCs,AUC)
   
    
    #-----------calculate MAX----------
    if(sen > sens_max$num){
      sens_max$num <- sen
      sens_max$name <- file
    }
   
    if(Spe > spec_max$num){
      spec_max$num <- Spe
      spec_max$name <- file
    }
    
    if(f1 > F1_max$num){
      F1_max$num <- f1
      F1_max$name <- file 
    }
    
    
    if(AUC > AUC_max$num){
      AUC_max$num <- AUC
      AUC_max$name <- file
    }
    
  }
  
  #------------------write data frame -----------------
  out_data<-data.frame(method, sensitivity, specificity, F1, AUC = AUCs, stringsAsFactors = F)
  
  #------------- add final row to the end of the table-----------------
  out_data[nrow(out_data)+1,] <- c("highest",sens_max$name,spec_max$name,F1_max$name,AUC_max$name)
 
  #------------- output file-----------------
  write.table(out_data, file=out_f, row.names = F, quote = F)
  
}

#作業2_new 完成版 103753027 顏碩亨
