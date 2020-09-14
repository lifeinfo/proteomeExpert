# ProteomeExpert - A user friendly Tool for quantitive proteome data analysis.
![guomics](http://www.guomics.com/assets/img/home/home3.jpg)

ProteomeExpert is an open-source platform-independent browser-based interface for proteome analytics in R. 
The application is based on the Shiny package and can be run locally or on a server. 

# Guomics Team

web: http://www.guomics.com


![](http://19241930.s21i.faiusr.com/2/ABUIABACGAAg9qrY5wUo4JuMgAYw3BE4sQc.jpg)



# How to Use

* step 1. upload your protein matrix(*.TXT or *.csv or xls/xlsx) and sample information (individual file if has)

* step 2. click submit & analysis

* step 3. browser diffrent tab

* step 4. download 


# How to install

There are two different ways to launch ProteomeExpert (PE):
## 1. Docker-based installation
This way is easier and recommended.
* [1]	Install Docker (version >= 17.12.0-ce)
Download and install Docker from https://docs.docker.com/install/
* [2]	Get and run docker image
`docker run -p 80:3838 proteomeexpert/latest` #on command line
* [3]	Open the web browser, go to http://ipOfDockerImageHost/pe2

## 2. R-based installation
* [1]	download the source code from github
* [2]	Install R (v >= 3.2)
Download and install R from http://cran.us.r-project.org/
* [3]	Install R studio (Optional)
Download and install RStudio Desktop from http://rstudio.org/download/desktop
* [4]	Install packages in install_packages.R
`source(“install_packages.R”)` #inside R console
* [5]	`source(“app.R”)` #inside R console

Using either way, users can launch PE in a web browser with the address of the installed PE computer. The detailed help pages of how to use PE are in the launched web page. We provide a demo server at https://proteomic.shinyapps.io/peserver/. This server is single-thread and of low-level hardware, we do recommend users to analyze the data using the demo server with small data sets. An upgraded hardware is necessary, according to the possible computational cost of the data, to reach the potential of PE.


# Development Guide

Although ProteomeExpert web-interface can handle quite a few data and analysis tasks, you may prefer to write your own R-code. ProteomeExpert provides a bridge to programming in R(studio) by exporting the functions used for analysis (i.e., you can conduct your analysis using the ProteomeExpert web-interface or by calling ProteomeExpert's functions directly from R-code). For more information about programming with ProteomeExpert see the [programming](https://github.com/lifeinfo/proteomeExpert) page on the documentation site.

## Reporting issues

Please use the GitHub issue tracker at https://github.com/lifeinfo/proteomeExpert/issues/new if you have any problems.

