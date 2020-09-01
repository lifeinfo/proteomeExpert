drawviolin <-
  function(value,
           sample,
           ylabel,
           strTitle = "Abundance",
           cluster_row = T,
           cluster_col = F) {
    df <- data.frame(Type = sample , value = value)
    df %>%
    plot_ly(
      x = ~Type,
      y = ~value,
      color = df$Type,
      type = 'violin',
      box = list(
        visible = T
      ),
      meanline = list(
        visible = T
      )
    )%>%
      layout(
        yaxis = list(
          title = strTitle,
          zeroline = F
        )
      )
    
  }

drawviolin_cv<-function(myList){
  protM<-myList$data
  type.group<-split(colnames(protM),myList$label)
  df<-data.frame()
  for(i in 1:length(type.group)){
    label<-names(type.group[i])
    intensity<-protM[,type.group[[i]]]
    cv<-apply(intensity,1,function(x){sd(x)/mean(x)})
    df.temp<- data.frame(cv,rep(label,length(cv)))
    df<-rbind(df,df.temp)
  }
  colnames(df)<-c("CV","Type")
  df$CV<-round(df$CV*100,2)
  drawviolin(df$CV,df$Type,ylabel="CV")
}
