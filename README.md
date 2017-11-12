# pakistan-routinevacc
Code use for analysis of MICS data from Pakistan

# Introduction
For analysis of MICS data the availbale data first needs to be reformatted into a useable format, and secondly the actual analysis needs to be carried out. The formatting of the data was done mostly in _R_, including adjustment of the survey weights. Accounting for the survey weights is an important aspect of the study design. Currently there are not reliable packages within _R_ to deal with survey weights. The regression analysis was carried out in STATA, usng the _svy_ toolbox (https://www.stata.com/manuals13/svy.pdf). 

## Formatting of data from SPSS format to R

The code makes use of the R library _foreign_ which reads into R the SPSS files. As we are only interested in attributes related to children undr 5, we assign the additional data (from the household, women and household members survey) to the child via use of hte unique identifier HH1 and HH2. The sample weights (_chweight_) are used to infer the population level weights for each child surveyed.

## Data analysis in R

Comparing AFP and MICS data

## Data analysis in STATA
