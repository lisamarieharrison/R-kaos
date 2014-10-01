#For each day of the KAOS survey export integration intervals in Echoview
#Author: Lisa-Marie Harrison
#Date: 01/10/2014
library(RDCOMClient)

#get survey dates
file.dates <- read.csv("C:/Users/43439535/Documents/Lisa/KAOS/kaos_survey_dates.csv", header = T)[, 1]

#create an Echoview object and open Echoview
EVAppObj=COMCreate('EchoviewCom.EvApplication')

#read in an Echoview file
EVFile= EVOpenFile(EVAppObj, fileName = "C:/Users/43439535/Desktop/RAW/20120519_KAOS_all_2.ev")$EVFile

#add calibration file
EVAddCalibrationFile(EVFile, "038-120-200", "C:/Users/43439535/Desktop/RAW/20120326_KAOS_SimradEK5.ecs")

#add regions definition file
EVImportRegionDef(EVFile = EVFile, evrFile = "C:/Users/43439535/Documents/Lisa/KAOS/regions/aggregations.evr", "false_bottom_regions")

#loop over each date and export integration by cells separately
for (i in 2:length(file.dates)) {
    
  #clear raw data from fileset
  EVClearRawData(EVFile, "038-120-200")
  
  #add multiple raw data file to EV test
  raw.files <-list.files(path = "C:/Users/43439535/Desktop/RAW", pattern = paste("(", file.dates[i], ").*\\.raw$", sep = ""), full.names = T)
  EVAddRawData(EVFile = EVFile, filesetName = "038-120-200", dataFiles = raw.files)

  #change grid of 38kHz and 120kHz to 5m * 50 pings
  varObj = EVAcoVarNameFinder(EVFile, acoVarName = "38 seabed and surface excluded")$EVVar
  EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, horizontalType = 4, horizontalDistance = 50, verticalType = 1, verticalDistance = 5)
  varObj = EVAcoVarNameFinder(EVFile, acoVarName = "120 seabed and surface excluded")$EVVar
  EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, horizontalType = 4, horizontalDistance = 50, verticalType = 1, verticalDistance = 5)
  
  #export integration by cells for 38kHz and 120kHz
  return.file.38 <- paste("C:/Users/43439535/Documents/Lisa/KAOS/exported_integrations/kaos_38kHz_integration_", file.dates[i], ".csv", sep = "")
  return.file.120 <- paste("C:/Users/43439535/Documents/Lisa/KAOS/exported_integrations/kaos_120kHz_integration_", file.dates[i], ".csv", sep = "")
  EVExportIntegrationByCells(EVFile = EVFile, variableName = "38 seabed and surface excluded", filePath = return.file.38)
  EVExportIntegrationByCells(EVFile = EVFile, variableName = "120 seabed and surface excluded", filePath = return.file.120)
  
}
















