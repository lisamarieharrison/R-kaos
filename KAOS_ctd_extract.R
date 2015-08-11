# Extracts data around multiple CTD stations from Echoview using the R acoustic package
#Author: Lisa-Marie Harrison
#Date: 11/08/2015
library(EchoviewR)

#create an Echoview object and open Echoview
EVAppObj <- COMCreate('EchoviewCom.EvApplication')

#get list of EV files
file.dates <- list.files(path = "~//EV", full.names = T, pattern = paste(".*\\.ev$", sep = ""))

#read in the .csv file containing the start and end times for each CTD drop
ctd.dat <- read.csv("kaos_ctd.csv", header = T)
ctd.info <- ctd.dat[ctd.dat$depth == 2, ] #subset to get 1 line for each station

#add zeros at the start of single digit dates and times and make into a single string
EVctdTimes <- function(ctd.info){
  
  for(i in 1:nrow(ctd.info)){
    
    while(nchar(ctd.info$start_time[i]) < 6){
      ctd.info$start_time[i] <- paste(0, ctd.info$start_time[i], sep = "")
    }
    while(nchar(ctd.info$bottom_time[i]) < 6){
      ctd.info$bottom_time[i] <- paste(0, ctd.info$bottom_time[i], sep = "")
    }
    
    #pad times with 0's to make correct length for .evr file
    ctd.info$start_time[i]  <- paste(ctd.info$start_time[i], 0, 0, 0, 0, sep = "")
    ctd.info$bottom_time[i] <- paste(ctd.info$bottom_time[i], 0, 0, 0, 0, sep = "")
    
  }
  return(list(ctd.info.R = ctd.info))  
}

ctd.date <- ctd.info$date
ctd.info <- EVctdTimes(ctd.info)$ctd.info.R

for (i in 1:nrow(ctd.info)){
  tryCatch({

    
    #create an Echoview object and open Echoview
    EVAppObj <- COMCreate('EchoviewCom.EvApplication')
    
    #open new echoview object
    EVFile <- EVOpenFile(EVAppObj, fileName = "~//20120519_KAOS_all_2.EV")$EVFile
    
    #add a calibration file to the fileset
    EVAddCalibrationFile(EVFile, "38H-120H-200H", "~//20120326_KAOS_SimradEK5.ecs")
    
    #clear all current raw data from fileset
    EVClearRawData(EVFile, "38H-120H-200H")
    
    #read in raw data files around CTD station
    raw.file.pattern <- paste("38H_120H_200H-D", ctd.date[i], ".*\\.raw$", sep = "")
    raw.files <- list.files(path = "G:/RAW/", pattern = raw.file.pattern, full.names = T) 
    EVAddRawData(EVFile = EVFile, filesetName = "38H-120H-200H", dataFiles = raw.files)
    
    #check the start and end time of the fileset
    fileset.times <- EVFindFilesetTime(EVFile, "38H-120H-200H")
    
    #create a new region class called CTD
    EVAddNewClass(EVFile, "CTD")  
    
    #change start time to 1 hour before CTD drop
    ctd.start.time.new <- ChangeStartTime(ctd.info, ctd.date, h = 1, m = 0)$ctd.time
    ctd.date.new       <- ChangeStartTime(ctd.info, ctd.date, h = 1, m = 0)$ctd.date
    
    #change end time to 1 hour after CTD drop
    ctd.end.time.new <- ChangeEndTime(ctd.info, ctd.date.new, h = 1, m = 0, ctd.start.time.new)$ctd.time
    ctd.date.end     <- ChangeEndTime(ctd.info, ctd.date.new, h = 1, m = 0, ctd.start.time.new)$ctd.date.end
    
    #write echoview region definitions file for the CTD region
    line_1 <- paste("EVRG 7 5.4.96.24494", sep = "")
    line_2 <- "1"
    line_3 <- ""
    line_4 <- paste("13 4 1 0 3 -1 1", ctd.date.new[i], ctd.start.time.new[i],  "0", ctd.date.end[i], ctd.end.time.new[i], "250", sep = " ")
    line_5 <- "0"
    line_6 <- "0"
    line_7 <- "CTD class"
    line_8 <- paste(ctd.date.new[i], ctd.start.time.new[i],  "0", ctd.date.new[i], ctd.start.time.new[i],  "250", ctd.date.end[i], ctd.end.time.new[i], "250", ctd.date.end[i], ctd.end.time.new[i], "0", "1", sep = " ")
    line_9 <- paste("CTD_", i, sep = "")
    file_lines <- c(line_1, line_2, line_3, line_4, line_5, line_6, line_7, line_8, line_9)
    
    dummy.def <- file(paste("G:/Region_definitions_files/region_def_file_stn_", i, ".evr", sep = ""), 'w')
    for (j in 1:9){
      writeLines(file_lines[j], con = dummy.def)
    }
    close(dummy.def)
    
    #import the region definitions file into Echoview
    EVImportRegionDef(EVFile, paste("G:/Region_definitions_files/region_def_file_stn_", i, ".evr", sep = ""), line_9)
    
    #change grid of 38kHz and 120kHz to 5m * 50 pings
    varObj = EVAcoVarNameFinder(EVFile, acoVarName = "38H hri ex noise")$EVVar
    EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, verticalType = 5, verticalDistance = 500, horizontalType = 1, horizontalDistance = 2)
    varObj = EVAcoVarNameFinder(EVFile, acoVarName = "120H hri ex noise")$EVVar
    EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, verticalType = 5, verticalDistance = 500, horizontalType = 1, horizontalDistance = 2)
    
    #export the Sv values for the region as a csv file
    EVExportIntegrationByRegionByCells(EVFile, '120H hri ex noise', paste("CTD_", i, sep = ""), paste("~//stn_", i, "_extracted_120khz.csv", sep =""))
    EVExportIntegrationByRegionByCells(EVFile, '38H hri ex noise', paste("CTD_", i, sep = ""), paste("~//stn_", i, "_extracted_38khz.csv", sep =""))
    
    msg <- paste("Finished extracting data for station", i, sep = " ")
    print(msg)
    if(1 == 1) flush.console()
    
    EVCloseFile(EVAppObj, EVFile)
  }, warning = function(w) {
    msg <- paste("WARNING: Got some warning, but continuing", w, sep = " ")
    print(msg)
  }, error = function(e) {
    msg <- paste("ERROR: Looks like we got an error, rerunning station number", e, i, sep = " ")
    print(msg)
    i <- i - 1
  })
}












