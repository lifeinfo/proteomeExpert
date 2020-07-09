drawviolin <-
  function(value,
           sample,
           ylabel,
           strTitle = "Violin plot",
           cluster_row = T,
           cluster_col = F) {
    df <- data.frame(type = sample , value = value)
    # ggplot(df, aes(x = type, y = value, fill = type)) +
    #   geom_violin(trim = FALSE) +
    #   geom_boxplot(width = 0.05, position = position_dodge(0.9)) +
    #   ylab(ylabel) + xlab("") +
    #   theme(
    #     legend.direction = 'horizontal',
    #     legend.position = 'top',
    #     panel.grid.major = element_blank(),
    #     panel.grid.minor = element_blank(),
    #     panel.background = element_blank(),
    #     axis.line = element_line(colour = "black")
    #   )
    df %>%
    plot_ly(
      x = ~type,
      y = ~value,
      color = df$type,
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

##
# protM<-read.table("../demo/prot.txt",sep = "\t",header = T,row.names = 1)
# sample<-read.csv("../demo/sampleInfo.csv",header = T)
# individual<-read.csv("../demo/IndividualInfo.csv",header = T)
# anno<-merge(sample,individual,by="Sample_Id")
# myList<-list(data=protM,label=anno$TissueType)
