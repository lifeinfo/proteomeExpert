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
source(file = "test.R")
source(file = "preprocess.R")

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
