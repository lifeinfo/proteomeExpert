# Guomics Team

proteomeExpert: A user friendly Web for protein analysis.


# How to Use




# Development Guide

## Note:
This file will be deleted after publishing

All across sessions object should be put in the global.R, such as global functions
other session level object should not be in the global.R

session level object
###getAnnoTable(): 
reactive function return a dataframe which store sample and individual information
important column name {sampleId, sampleType,batchId,technicalId,individualId,individualType}
###readProteinM()
reactive function return a dataframe which store protein matrix