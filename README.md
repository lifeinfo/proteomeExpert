# proteomeExpert - A user friendly Web for protein analysis using R and Shiny.


proteomeExpert is an open-source platform-independent browser-based interface for protein analytics in R. 
The application is based on the Shiny package and can be run locally or on a server. 

# Guomics Team

Team:http://www.guomics.com/col.jsp?id=108

web: http://www.guomics.com

# How to Use

* 
* 
* 
* 


# Development Guide

## Note:

All across sessions object should be put in the global.R, such as global functions
other session level object should not be in the global.R

## session level object

### getAnnoTable():

* reactive function return a dataframe which store sample and individual information

* important column name {sampleId, sampleType,batchId,technicalId,individualId,individualType}

### readProteinM()

* reactive function return a dataframe which store protein matrix