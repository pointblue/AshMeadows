# ASH MEADOWS NATIONAL WILDLIFE REFUGE 
### Status report

## Objective
The files in this repository are the R scripts used to prepare the key indicator metrics for the Natural Resources Management Plan for Ash Meadows National Wildlife Refuge.
These metrics were developed during the completion of the Refuge NRMP, in 2018. Details on the metrics are provided in the NRMP.
For more information about the scripts, please contact Kara Moore-O'Leary (kara_moore-oleary@fws.gov) or Leo Salas (lsalas@pointblue.org)

## About running the scripts
The scripts are divided into the two basic categories of metrics: for landbirds, and for marshbirds. There is a sequence of execution for both, along with clear annotation of the code.
NOTE: all file paths must be edited as needed to ensure the code finds the necessary data and scripts.

## Marshbird indicators scripts
There are three files of code. One file is a set of utility functions: AshMeadows_AnalysisUtils.R. This file is required by the file AshMeadows_Marsh_Metrics.R. The latter is the set
of code that fits hierarchical imperfect detection abundance models for RIRA, VIRA and SORA, and saves estimates into the data file marshIndicators_Data.RData
This data file is used by the file makePlots.R (along with the utility scripts) to generate the plots of the report.

### How to run
Run the file AshMeadows_Marsh_Metrics.R first, then the file makePlots.R 

## Landbird indicators scripts
We provide a script to explore the GBBO landbird data (GBBO_dataAnalyses.R). It is provided for guidance and completeness. Similarly, the file selectTransects_toSurvey.R is provided to
document how we selected the transects to include in landbird analyses.  
  
Habitat covariates were added to the selected survey locations with the script pointHabitatAttribution.R. It requires the utility functions script in the Marshbird script set.
Once attributed with habitat data, all analyses are done in the script landbirdIndicators.R

### How to run
Run the script pointHabitatAttribution.R, and then the script landbirdIndicators.R
  
  
#### Note for the R savvy
A lot of the code can be simplified with plyr and dplyr functions, and magritte pipes, but was intentionally kept simple to make the logic easier to follow by those not familiar with R
