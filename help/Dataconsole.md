# ProteomeExpert-Data upload

## Overview

### Data Upload
Data Upload is the core data input interface for user to upload your own data file. The Data Upload module allows uploading your specific protein matrix and sample annotation file (including experiment run sample file and individual file if it has) as the input data for most of modules. Moreover, it interactively merges the sample and individual information into one file which is required by some modules such as statistics, data mining, data pre-processing etc. It includes two ways to upload data: Two files format; Three files format. Choose two or three files format depends on whether the sample file stores the enough annotation you want. 

#### Two files format
Upload your files (`protein`, `sample`) .txt or .csv format with the Browse button. Choose separator for the file according to its format. `Comma` for .csv, `Semicolon`, `Comma` or `Tab` for .txt. file.

#### Three files format
Upload your files (`protein`, `sample`, `individual`) .txt or .csv format with the bottom Browse . Choose separator for the file according to its format. `Comma` for .csv, `Semicolon`, `Comma`  or  `Tab` for .txt. file. Select `sample id` (protein file should have the same sample id as sample file) columns for further analysis, `individual id/name` in sample file and individual file as reference for data merge. This tool will merge multiple files  as template for further analysis.


## Tutorial
### Two files format
Upload your protein matrix file and sample information file as follows:
	![image.png](twoFilesFormat.png)
	
### Three files format
1. Download the protein, sample and individual test data<br/>Download _test_prot.txt_, _test_sample.csv_ and _test_individual.csv_ files from "Online Help - Test data files used for batch design - Get", _test_individual.csv_ files comprise 21 individual information, _test_sample.csv _contain individual and sample information. All of the test data are from Delayed post-hypoxic leukoencephalopathy (DPHL) dataset.<br/>
2. Select your protein file: click on the  `Browse..` to upload the _test_prot.txt_ file, choose `Tab` as separator.
3. Select your sample file: click on the  `Browse..` to upload the _sample_individual.csv_, choose `Comma` as separator.
4. Select your individual file click on the  `Browse..` to upload the _individual_prot.txt_ file, choose `Comma` as separator.
5. Annotate sample columns: select `SampleName` as sample id; select `Individual_ID` as individual id/name, click on `Submit` .
6. Annotate individual columns: select `Individual_ID` as individual id/name, click on `Submit` .
7. Click on `Merge`, result would be shown on the bottom on the page. After merge, all the data uploaded would save in the sever for further analysis.

	![image.png](threeFilesFormat.png)

