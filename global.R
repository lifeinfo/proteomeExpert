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
source(file = "test.R")
source(file = "preprocess.R")

