#For each day of the KAOS survey export integration by cells in Echoview
#Author: Lisa-Marie Harrison
#Date: 01/10/2014
library(RDCOMClient)

#get survey dates
file.dates <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_survey_dates.csv", header = T)[, 1]

#create an Echoview object and open Echoview
EVAppObj=COMCreate('EchoviewCom.EvApplication')

#read in an Echoview file
EVFile= EVOpenFile(EVAppObj, fileName = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos-transect-swarmIDR1.EV")$EVFile

#add calibration file
EVAddCalibrationFile(EVFile, "038-120-200", "C:/Users/Lisa/Desktop/KAOS/20120326_KAOS_SimradEK5.ecs")

#add regions definition file
EVImportRegionDef(EVFile = EVFile, evr = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/regions/aggregations.evr", "false_bottom_regions")
EVImportRegionDef(EVFile = EVFile, evr = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/regions/off_transect.evr", "off_transect")

#add extra regions definitions files for off transect times
off_transect_files <- list.files("C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/regions/off transect", full.names = T)

for (i in 1:length(off_transect_files)) {
  EVImportRegionDef(EVFile, off_transect_files[i], "off_transect")
}


#loop over each date and export integration by cells separately
for (i in 8:length(file.dates)) {
  
  #clear raw data from fileset
  EVClearRawData(EVFile, "038-120-200")
  
  #add multiple raw data file to EV test
  raw.files <-list.files(path = "H:/KAOS/kaos_raw", pattern = paste("(", file.dates[i], ").*\\.raw$", sep = ""), full.names = T)
  EVAddRawData(EVFile = EVFile, filesetName = "038-120-200", dataFiles = raw.files)
  
  #change grid of Krill aggregations
  varObj = EVAcoVarNameFinder(EVFile, acoVarName = "Krill aggregations")$EVVar
  EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, verticalType = 4, verticalDistance = 7, horizontalType = 1, horizontalDistance = 1)

  #create new region classes for medium and high aggregations
  EVNewRegionClass(EVFile, "medium aggregations")
  EVNewRegionClass(EVFile, "high aggregations")
  
  #Run schools detection on 120 7x7 convolution for each type of aggregation (low, medium and high)
  schDet<-EVSchoolsDetect(EVFile = EVFile,
                          acoVarName='120 7x7 convolution',
                          outputRegionClassName='aggregations',
                          deleteExistingRegions = TRUE,
                          distanceMode="GPS distance",
                          maximumHorizontalLink=15,#m
                          maximumVerticalLink=5,#m
                          minimumCandidateHeight=1,#m
                          minimumCandidateLength=10,#m
                          minimumSchoolHeight=2,#m
                          minimumSchoolLength=15, #m
                          dataThreshold= -80)

  medSchDet<-EVSchoolsDetect(EVFile = EVFile,
                          acoVarName='120 7x7 convolution',
                          outputRegionClassName='medium aggregations',
                          deleteExistingRegions = TRUE,
                          distanceMode="GPS distance",
                          maximumHorizontalLink=15,#m
                          maximumVerticalLink=5,#m
                          minimumCandidateHeight=1,#m
                          minimumCandidateLength=10,#m
                          minimumSchoolHeight=2,#m
                          minimumSchoolLength=15, #m
                          dataThreshold= -65)
  
  highSchDet<-EVSchoolsDetect(EVFile = EVFile,
                          acoVarName='120 7x7 convolution',
                          outputRegionClassName='high aggregations',
                          deleteExistingRegions = TRUE,
                          distanceMode="GPS distance",
                          maximumHorizontalLink=15,#m
                          maximumVerticalLink=5,#m
                          minimumCandidateHeight=1,#m
                          minimumCandidateLength=10,#m
                          minimumSchoolHeight=2,#m
                          minimumSchoolLength=15, #m
                          dataThreshold= -58)
  
  #reset the 120 7x7 convolution threshold
  varObj <- EVAcoVarNameFinder(EVFile, acoVarName = "120 7x7 convolution")$EVVar
  EVminThresholdSet(varObj, -80)
  
  #export integration by cells for 38kHz and 120kHz
  return.wd <- "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/schools detection"
  EVIntegrationByRegionsByCellsExport(EVFile = EVFile, acoVarName = "Krill aggregations", regionClassName = "aggregations", exportFn = paste(return.wd, "/low_aggregations_", file.dates[i], ".csv", sep = ""))
  EVIntegrationByRegionsByCellsExport(EVFile = EVFile, acoVarName = "Krill aggregations", regionClassName = "medium aggregations", exportFn = paste(return.wd, "/medium_aggregations_", file.dates[i], ".csv", sep = ""))
  EVIntegrationByRegionsByCellsExport(EVFile = EVFile, acoVarName = "Krill aggregations", regionClassName = "high aggregations", exportFn = paste(return.wd, "/high_aggregations_", file.dates[i], ".csv", sep = ""))
  
  #export integration by regions for 38kHz and 120kHz
  EVIntegrationByRegionsExport(EVFile = EVFile, acoVarName = "Krill aggregations", regionClassName = "aggregations", exportFn = paste(return.wd, "/low_aggregations_by_region", file.dates[i], ".csv", sep = ""))
  EVIntegrationByRegionsExport(EVFile = EVFile, acoVarName = "Krill aggregations", regionClassName = "medium aggregations", exportFn = paste(return.wd, "/medium_aggregations_by_region", file.dates[i], ".csv", sep = ""))
  EVIntegrationByRegionsExport(EVFile = EVFile, acoVarName = "Krill aggregations", regionClassName = "high aggregations", exportFn = paste(return.wd, "/high_aggregations_by_region", file.dates[i], ".csv", sep = ""))
  
}















