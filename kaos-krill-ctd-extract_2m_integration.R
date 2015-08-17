setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/2m_500m_integrations")
ctd <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/kaos_ctd.csv", header = T)
library(chron)

file.create("kaos_krill_ctd_2m.csv")


for (i in c(4, 6, 7, 8, 9)) {

  stn <- unique(ctd$stn)[i]
  date <- unique(ctd$date[ctd$stn == stn])
    
  krill_120 <- read.csv(paste("CTD_STN_", i, "_extracted_120khz.csv", sep = ""), header = T)
  krill_38 <- read.csv(paste("CTD_STN_", i, "_extracted_038khz.csv", sep = ""), header = T)
  
  krill_120 <- krill_120[krill_120$Region_name %in% c(paste(" CTD_", i, sep = ""), paste("CTD_", i, sep = "")), ]
  krill_38 <- krill_38[krill_38$Region_name %in% c(paste(" CTD_", i, sep = ""), paste("CTD_", i, sep = "")), ]
  
  start_time <- as.character(unique(ctd$start_time[ctd$stn == stn]))
  if (nchar(start_time) != 6) {
    start_time <- paste(0, start_time, sep = "")
  }
  
  start_time <- chron(times. = start_time, format = "hms")
  
  bottom_time <- as.character(unique(ctd$bottom_time[ctd$stn == stn]))
  if (nchar(bottom_time) != 6) {
    bottom_time <- paste(0, bottom_time, sep = "")
  }
  
  bottom_time <- chron(times. = bottom_time, format = "hms")
  
  
  krill_38$Time_S <- chron(times. = krill_38$Time_S, format = "h:m:s")
  krill_38$Time_E <- chron(times. = krill_38$Time_E, format = "h:m:s")
  krill_120$Time_S <- chron(times. = krill_120$Time_S, format = "h:m:s")
  krill_120$Time_E <- chron(times. = krill_120$Time_E, format = "h:m:s")
  
  krill_38 <- krill_38[krill_38$Time_S > start_time & krill_38$Time_E < (bottom_time), ]  
  krill_120 <- krill_120[krill_120$Time_S > start_time & krill_120$Time_E < (bottom_time), ]  
  
  if(nrow(krill_38) == 0) {
    next()
  }
  
  #remove layers of -1 
  krill_38 <- krill_38[krill_38$Layer > 0, ]
  krill_120 <- krill_120[krill_120$Layer > 0, ]  
  
  #calculate 120kHz - 38kHz for each 2*500m window
  sv_38 <- krill_38$Sv_mean
  sv_120 <- krill_120$Sv_mean
  sv_38[sv_38 > 500 | sv_38 < -500] <- NA
  sv_120[sv_120 > 500 | sv_120 < -100] <- NA
  sv_diff <- sv_120 - sv_38
  
  #remove 120 - 38 kHz values outside of [1.02, 14.75] because these are unlikely to be krill
  #dB difference window is from Potts AAD report for KAOS data
  sv_diff[sv_diff < 1.02 | sv_diff > 14.75] <- NA
  sv_120[is.na(sv_diff)] <- NA
  
  #convert to density using target strength (kg/m2 per interval)
  sv <- 10^(sv_120/10)
  
  mvbs <- 10*log10(aggregate(matrix(sv, ncol = 1), by = list(krill_38$Layer), sum, na.rm = T)$V1/table(krill_38$Layer)[1])
  mvbs[mvbs == -Inf] <- NA
  

  #convert to density using target strength (kg/m2 per interval)
  p <- 2*10 ^((mvbs - -42.22)/10)*1000
  p[is.na(p)] <- 0
  
  depth <- round(aggregate(krill_38$Depth_mean, by = list(krill_38$Layer), FUN = "mean"))$x
  
  out <- cbind(rep(stn, length(p)), p, depth)
  
  write.table(out, "kaos_krill_ctd_2m.csv", sep = ",", row.names = F, col.names = F, append = T)  
  
  print(i)
  
}


dat <- read.csv("kaos_krill_ctd_2m.csv", header = F)
names(dat) <- c("stn", "p", "depth")
write.csv(dat, "kaos_krill_ctd_2m.csv", row.names = F)
