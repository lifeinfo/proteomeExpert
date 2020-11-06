# ProteomeExpert-Experimental Design
### Power Analysis

Power analysis is a statistical strategy that allows us to determine the sample size required to detect a preset effect under a given test statistics, such as Chi-square test or t-test. In particular, we need to pay attention to the calculated sample size and the actual sample size in an experiment. As observed empirically, when the expression of a protein is not that high, say less than 17 after log2 scale transformation, the required sample size will be compromised due to missing data and the statistical power is compromised too.

Power analysis, set parameters:<br />`Number of Proteins (default=5000)` is the estimated number of proteins identified of your dataset. <br />`Mean abundance (default=13)` is the average intensity (take log2)of proteins in the experimental group. <br />`Mean abundance 0 (default=13.5)` is the average intensity (log2 transformed)of proteins in the control group.<br />`Alpha (default=0.05)` is the significance level of the test (the _p_ value). <br />`Beta (default=0.2)` is the
probability of accepting the null hypothesis even though the null hypothesis is false (i.e. false negative),
when the real difference is equal to the minimum effect size. Beta = 1 - power.<br />`Standard
deviations (default=0.75)` have to be estimated for measured variables, usually we assumed it equals to the standard deviation of control group. <br />Click on the `Submit` and the estimated sample size and figure will be shown on the right side of the browser window. 

<img src="poweranalysis.png" align=center>

### Batch Design
The main purpose for batch design is to allocate samples into balanced groups to minimize technical bias, say instrumental variations, for large cohort studies. <br />Upload your sample data matrix in **.txt** or **.csv** format with the bottom `Browse`. Choose separator for the file according to its format. `Comma` for .csv, `Semicolon` ,  `Comma` or  `Tab`  for .txt. file.<br />`Select columns for balanced batch design` means choosing the column names (attributes) of your sample dataset that need to be balanced.<br />`Weights for columns` is the different weights between different attributes when the influencers  are considered together.  For example, if you submit `1,1,2` as input for selected columns A, B, C. This means the weight of column A is 25%, of column B is 25% and of column C is 50%. Normalize with <img src="gongshi.jpg" width="133" height = "53" align=center>, n is the number of attributes.<br />`Number of samples in each batch` means how many samples you expected in each balanced group.<br />`Select numeric columns for balanced batch design` means if the attribute is numeric type, such as age, tumor node, metastasis, et.al, it needed to be clarified.


1. Download the Batch Design test data 

    For this demo, we will be using a Delayed post-hypoxic leukoencephalopathy (not published) dataset, comprised of 168 samples' information, along with characters of "Sample ID", "Type", "Sex", "Age", "TNM" and "Fuhrman". Download the _batchdesign.csv_ file from "Online Help - Test data files used for batch design - Get"
	
2. Click on `Browse..` to upload the _batch_design.csv_ file, choose `Comma` as separator.
3. Select "Type", "Sex", "Age", "TNM" and "Fuhrman" for balanced batch design.
4. Input weights "1,1,1,1,1" for "Type", "Sex", "Age", "TNM" and "Fuhrman".
5. Input "15" as number of samples in each batch.
6. Select "Age" and "TNM" as numeric columns for balance batch design.
7. Click on `Submit`, waiting for the result that would be shown on the right side. batchId shows divided group number.

    <img src="exprimentaldesigh-pic.png" align=center>
	
8. Click on Download to get the _BatchDesignResult.txt_ file.
