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

### Data needs
  * Download the latest version of the marshbird warehouse from the AKN using the Downloader tool (https://data.pointblue.org/apps/downloader/) as a csv file. Point to it in line 61 of the script file AshMeadows_Marsh_Metrics.R. Alternatively, remark out that line and enable line 63 and use the RData file provided by Point Blue (AshMeadows_marshbird_indicatorData.RData).
  * On line 111 of AshMeadows_Marsh_Metrics.R you will also need the file marshPoints_attributed_200.RData. This file is provided by Point Blue, but can be generated with the script pointHabitatAttribution.R (in the folder for Landbirds), using the marshbird data as the source (replace accordingly on line 28 of that script) - keep in mind that coordinates in the AKN are latlon, not UTM, so edit code accordingly. The pointHabitatAttribution.R script also needs the geospatial vegetation dataset (AHEM_Vegetation_BW_20100308/VegHabitat,layer Veg_HabitatTypes - on line 35) provided by FWS to Point Blue.
  * The script file AshMeadows_Marsh_Metrics.R crates and saves the file marshIndicators_Data.RData. This dataset is used in the script makePlots.R (and is also provided by Point Blue)

### How to run
Run the file AshMeadows_Marsh_Metrics.R first, then the file makePlots.R 

## Landbird indicators scripts
We provide a script to explore the GBBO landbird data (GBBO_dataAnalyses.R). It is provided for guidance and completeness. Similarly, the file selectTransects_toSurvey.R is provided to
document how we selected the transects to include in landbird analyses.  
  
Habitat covariates were added to the selected survey locations with the script pointHabitatAttribution.R. It requires the utility functions script in the Marshbird script set.
Once attributed with habitat data, all analyses are done in the script landbirdIndicators.R

### Data needs
  * The script landbirdIndicators.R needs the .xls or .xlsx version of the Great Basin Bird Observatory (GBBO) database on line 79. This file, named "Ash Meadows NWR NBC data through 2016.xlsx" in the script, is provided by Point Blue but can be obtained from GBBO. The file must have a sheet named "Point Count Data" that contains the landbird data.
  * On line 81 the script also loads the file landbirdPoints_attributed_100.RData, which Point Blue provides. However, this file is also generated with the script pointHabitatAttribution.R, which uses the same .xls version of the GBBO data (on line 28 of that script), and the geospatial vegetation dataset AHEM_Vegetation_BW_20100308/VegHabitat,layer Veg_HabitatTypes (on line 35) provided by FWS to Point Blue.
  * The script landbirdIndicators.R also needs the file DiversityIndicators.csv on line 243. This is a simple list of bird indicator species and is provided by Point Blue 

### How to run
Run the script pointHabitatAttribution.R, and then the script landbirdIndicators.R
  
