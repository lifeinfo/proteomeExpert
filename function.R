#myhead function


myhead<-function(d){
  if(ncol(d)>10){
    t=head(d[,1:10])
  }
  else t=head(d)
  return(t)
}