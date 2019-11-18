protein_file_check<-function(file,sep,header){
  errors<-NULL
  d<-read.table(file,sep = sep,header = header)
  if(ncol(d)==1)
    errors<-"Please select correct separator for your file. One sample is not allowed"
  else if (nrow(d)!=length(unique(d[,1]))){
    errors<-"Duplicated values in first column is not allowed "
  }
  else if (!is.numeric(d[,-2])){
    errors<-"Non numeric value is not allowed"
  }
  return(errors)
}
technical_file_check<-function(file,sep,header){
  
}
batch_file_check<-function(file,sep,header){
  
}
batch_design_check<-function(file,sep,header){
  errors<-NULL
  d<-read.table(file,sep = sep,header = header)
  if(ncol(d)==1){
    errors<-"Please select correct separator for your file. One sample is not allowed"
  }else if(nrow(d)!=length(unique(d[,1]))){
    errors<-"Duplicated values in first column is not allowed "
  }
  return(errors)
}