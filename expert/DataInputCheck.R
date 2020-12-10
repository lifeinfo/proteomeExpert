protein_file_check<-function(file,sep,header){
  errors<-NULL
  d<-read.table(file,sep = sep,header = header)
  if(ncol(d)==1)
    errors<-"Please select correct separator for your file. One sample is not allowed"
  else if (nrow(d)!=length(unique(d[,1]))){
    errors<-"Duplicated values in first column is not allowed "
  }
  else if (!is.numeric(as.vector(unlist(d[,-c(1,2)])))){
    errors<-"Non numeric value is not allowed"
  }
  return(errors)
}
technical_file_check<-function(file,sep,header){
  
}
batch_file_check<-function(file,sep,header){
  
}
batch_design_check<-function(file,sep,header,colname,weight_sep){
  errors <- NULL

  if (nchar(weight_sep)<1) {
    errors <- "Weights for columns are requried"
  } else if (!grepl(",", weight_sep) & !grepl("^\\d+$", weight_sep)) {
    errors <- "Please use comma to separate each weight"
  } else if (length(colname) != length(unlist(strsplit(weight_sep, ",")))) {
    errors <- "Number of selected columns didn't equal number of weights"
  } else if (sum(grepl("\\s+",colname))>0) {
    errors <- "Column name shouldn't include blank spaces"
  }else{
    d <- read.table(file, sep = sep, header = header,check.names = F,encoding = "UTF-8")
    if (ncol(d) == 1) {
      errors <-
        "Please select correct separator for your file. One sample is not allowed"
    } else if (nrow(d) != length(unique(d[, 1]))) {
      errors <- "Duplicated values in first column is not allowed"
    } else if (sum(is.na(d[, colname])) > 0) {
      errors <- "Missing value is not allowed in your upload file"
    }
  }
  return(errors)
}