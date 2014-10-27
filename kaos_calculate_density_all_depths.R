#read in each kaos integration by cells file and calculate density for each integration interval
#write into a single file
#Author: Lisa-Marie Harrison
#Date: 01/10/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/exported_integrations")

#get dates of files in folder
survey.dates <- as.numeric(substr(list.files(path = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/exported_integrations", pattern = paste("(", "38kHz", ").*\\.csv$", sep = ""), full.names = F), start = 24, stop = 31))

#for each date, read in the 38 and 120kHz files and process the data

for (i in 1:length(survey.dates)) {
  
  
  acoustic_38  <- read.csv(file = paste("kaos_38kHz_integration_", survey.dates[i], ".csv", sep = ""), header = T) #5m*50ping integration interval
  acoustic_120 <- read.csv(file = paste("kaos_120kHz_integration_", survey.dates[i], ".csv", sep = ""), header = T) #5m*50ping integration interval
  
  #sort by interval
  acoustic_38 <- acoustic_38[order(acoustic_38$Interval), ]
  acoustic_120 <- acoustic_120[order(acoustic_120$Interval), ]
  acoustic_120 <- acoustic_120[c(1:nrow(acoustic_38)), ]
  
  #remove layers of -1 
  acoustic_38 <- acoustic_38[acoustic_38$Layer > 0, ]
  acoustic_120 <- acoustic_120[acoustic_120$Layer > 0, ]  
  
  #calculate 120kHz - 38kHz for each 10x50 window
  sv_38 <- acoustic_38$Sv_mean
  sv_120 <- acoustic_120$Sv_mean
  sv_38[sv_38 > 500 | sv_38 < -500] <- NA
  sv_120[sv_120 > 500 | sv_120 < -500] <- NA
  sv_diff <- sv_120 - sv_38
  
  date  <- acoustic_38$Date_M
  lat  <- acoustic_38$Lat_S
  long  <- acoustic_38$Lon_E
  start_time  <- as.character(acoustic_38$Time_S)
  end_time  <- as.character(acoustic_38$Time_E)
  layer <- acoustic_38$Layer
  
  #remove 120 - 38 kHz values outside of [1.02, 14.75] because these are unlikely to be krill
  #dB difference window is from Potts AAD report for KAOS data
  sv_diff[sv_diff < 1.02 | sv_diff > 14.75] <- NA
  sv_120[is.na(sv_diff)] <- NA
  
  sv <- 10^(sv_120/10)

  mvbs  <- 10*log10(sv)
  mvbs[mvbs == -Inf] <- NA
  
  #convert to density using target strength (kg/m2 per interval)
  p <- 5*10 ^((mvbs - -42.22)/10)*1000
  
  #plot density (kg/m2 per interval) along the transect
  plot(ksmooth(c(1:length(p)), p, bandwidth = 15), type = "l")
  
  dat <- cbind(time, date, lat, long, start_time, end_time, layer, p)
  
  #append to current file
  write.table(dat, file = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_combined_density_intervals_all_depths.csv", row.names = F, col.names = F, sep = ",", append = TRUE)
  
  msg <- paste("Finished calculating krill density for date ", survey.dates[i], sep = "")
  message(msg)
  
}






