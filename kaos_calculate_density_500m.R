#read in each kaos integration by cells file and calculate density for each integration interval
#write into a single file
#Author: Lisa-Marie Harrison
#Date: 01/10/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/exported_integrations_500m")

file.create("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/kaos_combined_density_intervals_500m.csv")

#get dates of files in folder
survey.dates <- as.numeric(substr(list.files(path = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/exported_integrations_500m", pattern = paste("(", "38kHz", ").*\\.csv$", sep = ""), full.names = F), start = 24, stop = 31))

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
  sv_120[sv_120 > 500 | sv_120 < -100] <- NA
  noise <- is.na(sv_120)
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
  p[table(acoustic_120$Interval, noise)[, 1] == length(unique(acoustic_38$Layer))] <- NA
  
  #calculate interval length (m)
  interval_length <- 0
  for (k in 1:length(unique(acoustic_38$Interval))) {
    interval_length[k] <- acoustic_38$Dist_E[acoustic_38$Layer == 1 & acoustic_38$Interval == unique(acoustic_38$Interval)[k]] - acoustic_38$Dist_S[acoustic_38$Layer == 1 & acoustic_38$Interval == unique(acoustic_38$Interval)[k]]
  }
  
  #calculate interval weighting
  interval_weight <- interval_length/sum(interval_length)
  
  #calculate mean weighted density
  transect_mean <- sum(na.omit(p*interval_weight))
    
  dat <- cbind(time, date, lat, long, start_time, end_time, p, interval_weight, interval_length, i)
  
  #append to current file
  write.table(dat, file = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/kaos_combined_density_intervals_500m.csv", row.names = F, col.names = F, sep = ",", append = TRUE)
  
  msg <- paste("Finished calculating krill density for date ", survey.dates[i], sep = "")
  message(msg)
  
}

#add names to columns
dat <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/kaos_combined_density_intervals_500m.csv", header = F)
names(dat) <- c("time", "date", "lat", "long", "start_time", "end_time", "p", "interval_weight", "interval_length", "i")
write.csv(dat, "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/kaos_combined_density_intervals_500m.csv", row.names = F)

dat <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/kaos_combined_density_intervals_500m.csv", header = T)
dat$p[dat$p > 5000] <- NA

transect_density <- 0
for(i in unique(dat$i)) {
  transect_density[i] <- sum(na.omit(dat$p[dat$i == i]*dat$interval_weight[dat$i == i]))
}

sum(na.omit(dat$p*dat$interval_length/sum(dat$interval_length)))

#calculate biomass (kt)
(sum(na.omit(dat$p*dat$interval_length/sum(dat$interval_length)))*10500e6)/1000/1000/1000
