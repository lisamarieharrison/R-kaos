#read in each kaos integration by cells file and calculate density for each integration interval
#write into a single file
#Author: Lisa-Marie Harrison
#Date: 01/10/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/exported_integrations")

file.create("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/kaos_combined_density_intervals.csv")

#get dates of files in folder
survey.dates <- as.numeric(substr(list.files(path = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/exported_integrations", pattern = paste("(", "38kHz", ").*\\.csv$", sep = ""), full.names = F), start = 24, stop = 31))

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
  noise <- sv_120 > 500
  sv_38[sv_38 > 500 | sv_38 < -500] <- NA
  sv_120[sv_120 > 500 | sv_120 < -500] <- NA
  sv_diff <- sv_120 - sv_38
  
  time  <- as.character(acoustic_38$Time_M[seq(1, nrow(acoustic_38), by = max(acoustic_38$Layer))])
  date  <- acoustic_38$Date_M[seq(1, nrow(acoustic_38), by = max(acoustic_38$Layer))]
  lat  <- acoustic_38$Lat_S[seq(1, nrow(acoustic_38), by = max(acoustic_38$Layer))]
  long  <- acoustic_38$Lon_E[seq(1, nrow(acoustic_38), by = max(acoustic_38$Layer))]
  start_time  <- as.character(acoustic_38$Time_S[seq(1, nrow(acoustic_38), by = max(acoustic_38$Layer))])
  end_time  <- as.character(acoustic_38$Time_E[seq(1, nrow(acoustic_38), by = max(acoustic_38$Layer))])
  
  #remove 120 - 38 kHz values outside of [1.02, 14.75] because these are unlikely to be krill
  #dB difference window is from Potts AAD report for KAOS data
  sv_diff[sv_diff < 1.02 | sv_diff > 14.75] <- NA
  sv_120[is.na(sv_diff)] <- NA
  
  sv <- 10^(sv_120/10)
  
  mvbs  <- 10*log10(aggregate(matrix(sv, ncol = 1), by = list(rep(c(1:(length(sv)/max(acoustic_38$Layer))), each = max(acoustic_38$Layer))), sum, na.rm = T)$V1/max(acoustic_38$Layer))
  mvbs[mvbs == -Inf] <- NA
  
  
  #convert to density using target strength (kg/m2 per interval)
  p <- 250*10 ^((mvbs - -42.22)/10)*1000
  
  #add zero krill intervals back in
  p[is.na(p)] <- 0
  
  #remove noise intervals
  p[noise] <- NA
  
  #calculate interval length (m)
  interval_length <- 0
  for (k in unique(acoustic_38$Interval)) {
    interval_length[k] <- (acoustic_38$Dist_E[acoustic_38$Layer == 1 & acoustic_38$Interval == k] - acoustic_38$Dist_S[acoustic_38$Layer == 1 & acoustic_38$Interval == k])*1000
  }
  
  #calculate interval weighting
  interval_weight <- interval_length/sum(interval_length)
  
  #calculate mean weighted density
  transect_mean <- sum(na.omit(p*interval_weight))
  

  dat <- cbind(time, date, lat, long, start_time, end_time, p, interval_weight)
  
  #append to current file
  write.table(dat, file = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/kaos_combined_density_intervals.csv", row.names = F, col.names = F, sep = ",", append = TRUE)
  
  msg <- paste("Finished calculating krill density for date ", survey.dates[i], sep = "")
  message(msg)
  
}

