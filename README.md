# pakistan-routinevacc
Code use for analysis of MICS data from Pakistan

# Introduction
For analysis of MICS data the availbale data first needs to be reformatted into a useable format, and secondly the actual analysis needs to be carried out. The formatting and analysis of the data was done mostly in R, with some additional analysis in STATA (code not provided). 

## Formatting of data from SPSS format to R

The code makes use of the R library _foreign_ which reads into R the SPSS files. As we are only interested in attributes related to children undr 5, we assign the additional data (from the household, women and household members survey) to the child via use of hte unique identifier HH1 and HH2. The sample weights (_chweight_) are used to infer the population level weights for each child surveyed.

## Data analysis in R

To account for the sampling weights within surveyed children, the package _survey_ was used. Essentially, the regression analysis is adapted to account for unequal sampling of surveyed individuals, in order to infer a population effect. Much more information about the approach can be found here http://r-survey.r-forge.r-project.org/survey/
