# IPS-approach

This project is using Interpenetrating Subsampling (IPS) approach to incorporate interpreter variability into estimation of the total variance of land cover area estimates. Two sampling design -- simple random sampling and sratified random sampling were considered. 
The R code file SRS_6group.R demostrate the estimation of total variance using IPS of simple random samples with 6 groups, the example dataset used in this case is Groups_6_Binary.csv. In Groups_6_Binary.csv, the binary reference labels provided by six different interpreters was recorded, with 1--target land cover class and 0--nontarget land cover class (for example, forest=1, nonforest=0). 
