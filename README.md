# proteomeExpert - A user friendly Tools for protein analysis.
![guomics](http://19241930.s21i.faiusr.com/4/ABUIABAEGAAgsdzx5QUomeWW4QMwbjhu.png)

proteomeExpert is an open-source platform-independent browser-based interface for protein analytics in R. 
The application is based on the Shiny package and can be run locally or on a server. 

# Guomics Team

Team:http://www.guomics.com/col.jsp?id=108

web: http://www.guomics.com


![](http://19241930.s21i.faiusr.com/2/ABUIABACGAAg9qrY5wUo4JuMgAYw3BE4sQc.jpg)


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

## Reporting issues

Please use the GitHub issue tracker at https://github.com/lifeinfo/proteomeExpert/issues/new if you have any problems.

## Note:

All across sessions object should be put in the global.R, such as global functions
other session level object should not be in the global.R

## session level object

### getAnnoTable():

* reactive function return a dataframe which store sample and individual information

* important column name {sampleId, sampleType,batchId,technicalId,individualId,individualType}

### readProteinM()

* reactive function return a dataframe which store protein matrix

