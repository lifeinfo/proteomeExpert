options(encoding = "UTF-8")
options(shiny.maxRequestSize=300*1024^2)
options(stringsAsFactors = F)
set.seed(1)
#defalut value for na
NA_VALUE<-0

#################################
# loading model
#################################
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
###start featrue selection
library(glmnet)
library(doParallel)
registerDoParallel(cores = 5)
library(foreach)
library(pROC)
library(heatmaply)
###end featrue selection
#################################
# template
#################################
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

#################################
# plot
#################################
#plotly provides high-level bindings for working directly with plotly.js.
library(ggplot2)
library(plotly)
library(corrplot)
library(canvasXpress)

#table
library(rhandsontable)

#################################
# source file
#################################
source("expert/lr_2prot.R")
source("expert/combat.R")
source("expert/test.R")
source("expert/preprocess.R")
source("expert/featureSelection.R")
source("expert/pca.R")
source("expert/umap.R")
source("expert/tsne.R")
source("expert/volcano.R")
source("expert/corrplot.R")
source("expert/radar.R")
source("expert/heatmap.R")
source("expert/violin.R")
source("expert/missingValueExplore_zts.R")
source("function.R")
#################################
# global variable
#################################
protM_name<-c("original","featureSelected")
anno_name<-"Please annotation first"

#################################
# ML
#################################
library(tsne)
library(umap)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)


#library(keras)
# ML
#library(keras)
#install
#1.install.packages("keras")
#2.library(keras)
#3.install_keras()
