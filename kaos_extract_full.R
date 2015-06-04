#For each day of the KAOS survey export integration by cells in Echoview
#Author: Lisa-Marie Harrison
#Date: 01/10/2014
library(RDCOMClient)
library(EchoviewR)

#get survey dates
file.dates <- read.csv("H:/KAOS/kaos_survey_dates.csv", header = T)[, 1]

#create an Echoview object and open Echoview
EVAppObj=COMCreate('EchoviewCom.EvApplication')

#read in an Echoview file
EVFile= EVOpenFile(EVAppObj, fileName = "H:/KAOS/20120519_KAOS_all_2.ev")$EVFile

#add calibration file
EVAddCalibrationFile(EVFile, "038-120-200", "H:/KAOS/20120326_KAOS_SimradEK5.ecs")

#add regions definition file
EVImportRegionDef(EVFile = EVFile, evr = "H:/KAOS/regions/aggregations.evr", "false_bottom_regions")
EVImportRegionDef(EVFile = EVFile, evr = "H:/KAOS/regions/off_transect.evr", "off_transect")

#add extra regions definitions files for off transect times
off_transect_files <- list.files("H:/KAOS/regions/off transect", full.names = T)

for (i in 1:length(off_transect_files)) {
  EVImportRegionDef(EVFile, off_transect_files[i], "off_transect")
}


#loop over each date and export integration by cells separately
for (i in 1:length(file.dates)) {
    
  #clear raw data from fileset
  EVClearRawData(EVFile, "038-120-200")
  
  #add multiple raw data file to EV test
  raw.files <-list.files(path = "H:/KAOS/RAW", pattern = paste("(", file.dates[i], ").*\\.raw$", sep = ""), full.names = T)
  EVAddRawData(EVFile = EVFile, filesetName = "038-120-200", dataFiles = raw.files)

  #change grid of 38kHz and 120kHz to 5m * 50 pings
  varObj = EVAcoVarNameFinder(EVFile, acoVarName = "38 seabed and surface excluded")$EVVar
  EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, verticalType = 5, verticalDistance = 2000, horizontalType = 1, horizontalDistance = 5)
  varObj = EVAcoVarNameFinder(EVFile, acoVarName = "120 seabed and surface excluded")$EVVar
  EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, verticalType = 5, verticalDistance = 2000, horizontalType = 1, horizontalDistance = 5)
  
  #export integration by cells for 38kHz and 120kHz
  return.file.38 <- paste("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/exported_integrations_2km/kaos_38kHz_integration_", file.dates[i], ".csv", sep = "")
  return.file.120 <- paste("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/exported_integrations_2km/kaos_120kHz_integration_", file.dates[i], ".csv", sep = "")
  EVExportIntegrationByCells(EVFile = EVFile, variableName = "38 seabed and surface excluded", filePath = return.file.38)
  EVExportIntegrationByCells(EVFile = EVFile, variableName = "120 seabed and surface excluded", filePath = return.file.120)
    
}
















