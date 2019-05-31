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
<<<<<<< HEAD
library(DT)
=======
library(htmltools)
library(preprocessCore)
source("lr_2prot.R")
source("combat.R")
#library(keras)
>>>>>>> 154fd24a5abda76c04e57994fb78f35e6f1810a4
source(file = "test.R")
source(file = "preprocess.R")

