#For each day of the KAOS survey export integration by cells in Echoview
#Author: Lisa-Marie Harrison
#Date: 01/10/2014
library(RDCOMClient)

#get survey dates
file.dates <- read.csv("F:/KAOS/kaos_survey_dates.csv", header = T)[, 1]

#create an Echoview object and open Echoview
EVAppObj=COMCreate('EchoviewCom.EvApplication')

#read in an Echoview file
EVFile= EVOpenFile(EVAppObj, fileName = "F:/KAOS/RAW/20120519_KAOS_all_2.ev")$EVFile

#add calibration file
EVAddCalibrationFile(EVFile, "038-120-200", "F:/KAOS/RAW/20120326_KAOS_SimradEK5.ecs")

#add regions definition file
EVImportRegionDef(EvFile = EVFile, evr = "F:/KAOS/regions/aggregations.evr", "false_bottom_regions")
EVImportRegionDef(EvFile = EVFile, evr = "F:/KAOS/regions/off_transect.evr", "off_transect")

#add extra regions definitions files for off transect times
off_transect_files <- list.files("F:/KAOS/regions/off transect", full.names = T)

for (i in 1:length(off_transect_files)) {
  EVImportRegionDef(EVFile, off_transect_files[i], "off_transect")
}


#loop over each date and export integration by cells separately
for (i in 5:length(file.dates)) {
  
  #clear raw data from fileset
  EVClearRawData(EVFile, "038-120-200")
  
  #add multiple raw data file to EV test
  raw.files <-list.files(path = "F:/KAOS/RAW", pattern = paste("(", file.dates[i], ").*\\.raw$", sep = ""), full.names = T)
  EVAddRawData(EVFile = EVFile, filesetName = "038-120-200", dataFiles = raw.files)
  
  #change grid of 38kHz and 120kHz to 5m * 50 pings
  varObj = EVAcoVarNameFinder(EVFile, acoVarName = "38 seabed and surface excluded")$EVVar
  EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, verticalType = 5, verticalDistance = 500, horizontalType = 1, horizontalDistance = 5)
  varObj = EVAcoVarNameFinder(EVFile, acoVarName = "120 seabed and surface excluded")$EVVar
  EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, verticalType = 5, verticalDistance = 500, horizontalType = 1, horizontalDistance = 5)
  
  #export integration by region for 38kHz and 120kHz
  return.file.120 <- paste("C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/schools_detection/kaos_120kHz_sd_", file.dates[i], ".csv", sep = "")
  EVIntegrationByRegionsExport(EVFile = EVFile, "120 seabed and surface excluded", "aggregations", return.file.120)
  
}


#combine all exported data into one file
krill.files <- list.files("C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/schools_detection/", full.names = TRUE)
for (i in 1:length(krill.files)) {
  dat <- read.csv(file = krill.files[i])
  write.table(dat, "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_combined_schools_detection.csv", row.names = F, col.names = F, append = T, sep=",")
}











