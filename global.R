options(encoding = "UTF-8")
options(shiny.maxRequestSize=300*1024^2)
options(stringsAsFactors = F)

# loading model
# eg: source("model.R")
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sva)
library(DT)
library(htmltools)
library(preprocessCore)
library(httr)
library(jsonlite)
library(xml2)

#template
library(shinyBS)
library(bsplus)
#library(colourpicker)
#library(ECharts2Shiny)

#drag
#library(dragulaR)

#d3
#library(networkD3)
#library(igraph)
#library(ggvis)

#plotly provides high-level bindings for working directly with plotly.js.
library(ggplot2)
library(plotly)

#table
library(rhandsontable)

#model
source("lr_2prot.R")
source("combat.R")
source("test.R")
source("preprocess.R")
source("featureSelection.R")
source("pca.R")
#######################################global variable
protM_name<-c("original","featureSelected")
anno_name<-"Please annotation first"

#library(keras)
# ML
#library(keras)
#install
#1.install.packages("keras")
#2.library(keras)
#3.install_keras()

myhead<-function(d){
  if(ncol(d)>10){
    t=head(d[,1:10])
  }
  else t=head(d)
  return(t)
}