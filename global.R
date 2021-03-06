options(encoding = "UTF-8")
options(shiny.maxRequestSize= 10*1024^3)
options(stringsAsFactors = F)
options(digits=3)
set.seed(1)
#defalut value for na
NA_VALUE<-0

library(sqldf)
# Load libraries for genetic algorithem
library(caret)
library(randomForest)
library(funModeling)
library(tidyverse)
library(GA)
library(compiler)
source("expert/lib_ga.R")
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
##COMBAT
library(sva)
###start featrue selection
library(glmnet)
library(doParallel)
library(parallel)
registerDoParallel(cores = 5)
library(speedglm)
library(biglm)

library(foreach)
library(pROC)
library(pheatmap)
#library(heatmaply)
###end featrue selection
#################################
# template
#################################
library(shinyBS)
library(bsplus)

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
source("expert/data_preprocess.R")
source("expert/combat.R")
source("expert/t_test.R")
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
source("expert/BatchGenerator.R")
source("expert/missingRatio.R")
source("expert/DataInputCheck.R")

#################################
# global variable
#################################
#protM_name<-c("uploadedProtMatrix","featureSelected")
#protM_name<-c("uploadedProtMatrix","featureSelected","preprocessedOriginal","preprocessedFeatureSeleceted")
protM_name<-c("uploadedProtMatrix","comming soon")
anno_name<-"Please upload your annotation file in data console first"
#batch design use
BDcol_name<-"Please upload your file first"
#label vector
label_vector<-"Please upload your annotation file in data console first"
#################################
# ML
#################################
library(tsne)
library(umap)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)

# ML
library(shinyjs)
library(stringr)
library(shinycssloaders)
#source("expert/pulse_dia_combine.R")
library(Matrix)
library(xgboost)
source("expert/xgboost.R")
library(openxlsx)
