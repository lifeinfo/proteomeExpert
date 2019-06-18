# proteomeExpert - A user friendly Tools for protein analysis.


proteomeExpert is an open-source platform-independent browser-based interface for protein analytics in R. 
The application is based on the Shiny package and can be run locally or on a server. 

# Guomics Team

Team:http://www.guomics.com/col.jsp?id=108

web: http://www.guomics.com


<iframe width="640" height="375" src="http://www.guomics.com" frameborder="0" allowfullscreen></iframe>


# Key Features

* Faster

* Deeper

* Smarter

* Machine Learning

# How to Use

* step 1. upload your protein matrix(*.TXT or *.csv)

* step 2. click submit & analysis

* step 3. browser diffrent tab

* step 4. download 


# How to install

- Required: [R](https://cran.r-project.org/) version 3.4.0 or later

- Required: [Rstudio](https://www.rstudio.com/products/rstudio/download/) version 1.1.453 or later


# Development Guide

Although proteomeExpert web-interface can handle quite a few data and analysis tasks, you may prefer to write your own R-code. proteomeExpert provides a bridge to programming in R(studio) by exporting the functions used for analysis (i.e., you can conduct your analysis using the proteomeExpert web-interface or by calling proteomeExpert's functions directly from R-code). For more information about programming with proteomeExpert see the [programming](https://github.com/lifeinfo/proteomeExpert) page on the documentation site.


## Note:

All across sessions object should be put in the global.R, such as global functions
other session level object should not be in the global.R

## session level object

### getAnnoTable():

* reactive function return a dataframe which store sample and individual information

* important column name {sampleId, sampleType,batchId,technicalId,individualId,individualType}

### readProteinM()

* reactive function return a dataframe which store protein matrix