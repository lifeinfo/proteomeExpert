
####install dependent libraries
req.pcg.install <- function(pcg){
  new <- pcg[!(pcg %in% installed.packages()[, "Package"])]
  if (length(new)) install.packages(new, dependencies = T)
  sapply(pcg, require, ch = T)
}


install.pcg <- c("sqldf","caret","randomForest","funModeling","tidyverse","GA","compiler","shiny","shinythemes","RColorBrewer","scales","lattice","dplyr", "DT","htmltools","httr","jsonlite","xml2","glmnet","doParallel","parallel","speedglm","biglm","foreach","pROC","pheatmap","shinyBS","bsplus","ggplot2","plotly","corrplot","canvasXpress","rhandsontable","tsne","umap","rpart","rpart.plot","shinyjs","stringr","shinycssloaders","Matrix","xgboost","openxlsx","BiocManager")

req.pcg.install(install.pcg)

req.pcg.BiocManager <- function(pcg){
  new <- pcg[!(pcg %in% installed.packages()[, "Package"])]
  if (length(new)) BiocManager::install(new)
  sapply(pcg, require, ch = T)
}
BiocManager.pcg <- c("preprocessCore","sva")

req.pcg.BiocManager(BiocManager.pcg)

