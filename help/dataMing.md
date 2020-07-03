# ProteomeExpert Data Mining

## Overview
Data mining module includes fetarue selection, clustering and classification.
In the feature selection module, users not only use filter methods to filter  features of near zero variance and high correlation, but also exercise additional feature selection methods: LASSO (Tibshirani, 1996), genetic algorithm, and random forest. As in clinical application classifying disease into subtypes is of great interest in the fields of diagnose and prognosis, users can perform various machine learning analyses for classification: unsupervised analysis PCA, t-SNE, and UMAP for clustering, and decision tree, random forest, and XGBoost.
<br />

## Parameters
### Data Mining : feature selection
The feature selection is the process that choose a reduced number of explanatory variables to describe a response variable. The feature selection is even more important for the high-dimensional datasets, always higher than the number of observations in genomics and proteomics data, which more than thousands features, and the number of samples sometimes hundreds even dozens. The main goal of proteomics biomarker discovery is to identify which are the most importance proteins contribute to the disease. Here we used three well known feature selection methods: LASSO, genetic algorithm, and random forest. We are showing the process of these feature selection and describing how to use them to biomarker discover. 
LASSO is short of Least Absolute Shrinkage and Selection Operator. We used glmnet package in R (employ cv.glmnet function to choose the most appropriate tuning parameter λ, that controls the strength of the penalty and set α = 1 for LASSO regularization). Once the λ is set, glmnet function is used to do the feature selection according to this λ.
Genetic Algorithm (GA) are a is a stochastic optimization method inspired by the famous Charles Darwin’s idea of natural selection. Here we used the GA to select the right number of proteins in order to find positive biomarkers. We defined fitness function as the ROC divided number of features for two classification and accuracy divided number of features for multiple classification. Selection, crossover and mutation were done automatically by the ga function in GA package. Random forest is very popular in bioinformatics area and achieved fantastic results. Function sbf and rfe in randomForest package was applied here to afford feature selection using cross validation.

 

### Data Mining : clustering
It includes the following algorithem:

- Heatmap
- PCA
- t-SNE 
- UMAP

### Data Mining : classification
It includes the following algorithm:

- Decision Tree
- Random forest
- XGBoost