#作業2_new 完成版 103753027 顏碩亨

#處理command下的變數
#Rscript hw2_yourID.R --target male/female --files meth1 meth2 … methx
#--out result.csv

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("USAGE: Rscript hw1_exam.R input", call.=FALSE)
}else if(args[1] == '--files'){
  
  i <- 2
  while(i){
    
    if(args[i] == '--out'){
      i <- i + 1
      break
    }
    f(args[i])
    i <- i + 1
  }
  
  wr(args[i], p)
  
   
}else if(args[1] == '--out'){
  
  if(args[3] == "--files"){
    i <- 4
    while(i){
      
      if(is.na(args[i])){
        wr(args[2], p)
        break
      }
      f(args[i])
      i <- i + 1
      
    }
  }
  
}else {
  stop("Rscript hw2_yourID.R --target male/female --files meth1 meth2 … methx --out result.csv", call.=FALSE)
}
#作業2_new 完成版 103753027 顏碩亨