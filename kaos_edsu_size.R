#analysis of edsu size for kaos data originally exported at 50ping x 5m integration intervals
#author: Lisa-Marie Harrison
#date: 11/06/2015
library(chron)

#remove null rows from acoustic files
# files <- list.files("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/exported_integrations/", full.names = T)
# for (i in files) {
#   
#   dat <- read.csv(i, header = T)
#   w <- !dat$Sv_mean == 9999
#   dat <- dat[w, ]
#   
#   write.csv(dat, i, row.names = F)
#   
# }

date <- "20030118" #specify date as a character

#read all acoustic data files and combine into one
acoustic_38 <- matrix(0, ncol = 82)
acoustic_120 <- matrix(0, ncol = 82)
files_38 <- list.files("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/exported_integrations/", full.names = T, pattern = paste("38kHz.*", date, sep = ""))
files_120 <- list.files("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/exported_integrations/", full.names = T, pattern = paste("120kHz.*", date, sep = ""))

for (i in 1:length(files_38)) {
  dat_38 <- read.csv(files_38[i], header = T)
  dat_120 <- read.csv(files_120[i], header = T)
  
  dat_38 <- dat_38[dat_38$Interval %in% unique(dat_120$Interval), ]
  dat_120 <- dat_120[dat_120$Interval %in% unique(dat_38$Interval), ]
  
  if(nrow(dat_120) != nrow(dat_38)) {
    
    compare <- table(dat_120$Interval, dat_120$Layer) == table(dat_38$Interval, dat_38$Layer)
    inds <- which(compare == FALSE, arr.ind=TRUE)
    for (j in 1:nrow(inds)) {
      dat_120 <- dat_120[!(dat_120$Interval == unique(rbind(dat_38, dat_120)$Interval)[inds[j, 1]] & dat_120$Layer == unique(dat_38$Layer)[inds[j, 2]]), ]
      dat_38 <- dat_38[!(dat_38$Interval == unique(rbind(dat_38, dat_120)$Interval)[inds[j, 1]] & dat_38$Layer == unique(dat_38$Layer)[inds[j, 2]]), ]
      
    }
  }
  
  colnames(acoustic_38) <- colnames(dat_38)
  acoustic_38 <- rbind(acoustic_38, dat_38)
  colnames(acoustic_120) <- colnames(dat_120)
  acoustic_120 <- rbind(acoustic_120, dat_120) 
}
acoustic_38 <- acoustic_38[-1, ]
acoustic_120 <- acoustic_120[-1, ]


max_layer <- max(acoustic_38$Layer)
change_loc <- which(acoustic_38$Layer == max_layer)
acoustic_38$unique_interval <- rep(1, nrow(acoustic_38))
for (i in 1:(length(change_loc) - 1)) {
  acoustic_38$unique_interval[(change_loc[i] + 1):change_loc[i+1]] <- i + 1
}

#calculate 120kHz - 38kHz for each 10x50 window
sv_38 <- acoustic_38$Sv_mean
sv_120 <- acoustic_120$Sv_mean
sv_38[sv_38 > 500 | sv_38 < -500] <- NA
sv_120[sv_120 > 500 | sv_120 < -80] <- NA
sv_diff <- sv_120 - sv_38


#remove 120 - 38 kHz values outside of [1.02, 14.75] because these are unlikely to be krill
#dB difference window is from Potts AAD report for KAOS data
sv_diff[sv_diff <= 1.02 | sv_diff >= 14.75] <- NA
sv_120[is.na(sv_diff)] <- NA

sv <- 10^(sv_120/10)

sv_mat <- as.data.frame(cbind(sv, acoustic_38$unique_interval))
colnames(sv_mat) <- c("sv", "unique_interval")
mvbs <- 10*log10(ddply(sv_mat, "unique_interval", numcolwise(sum), na.rm = TRUE)$sv/length(unique(acoustic_38$Layer)))
mvbs[mvbs == -Inf] <- NA

abc <- 10 ^((mvbs)/10)*250

deg2rad <- function(deg) {
  #converts degrees to radians
  #input: degree coordinate
  #returns: radian coordinate 
  
  return(deg*pi/180)
}

gcd.hf <- function(lat1, long1, lat2, long2) {
  #calculates distance between two coordinates using the Haversine formula (hf)
  #input: radian latitude and longitude coordinates
  #returns: distance between coordinates in m
  
  R <- 6371 # Earth mean radius [km]
  delta.long <- (deg2rad(long2) - deg2rad(long1))
  delta.lat  <- (deg2rad(lat2) - deg2rad(lat1))
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1, sqrt(a)))
  d = R * c * 1000
  return(d) 
  
}


#calculate interval length (m) and time
interval_length <- 0
interval_time <- 0
int_matrix <- acoustic_38[acoustic_38$Layer == 5, ]
for (k in 1:nrow(int_matrix)) {
  interval_length[k] <- gcd.hf(int_matrix$Lat_S[k], int_matrix$Lon_S[k], int_matrix$Lat_E[k], int_matrix$Lon_E[k])
  time_start <- chron(times. = int_matrix$Time_S[k], format = "h:m:s")
  time_end <- chron(times. = int_matrix$Time_E[k], format = "h:m:s")
  interval_time[k] <- as.character(time_end - time_start)
  if(time_start > time_end) {
    interval_time[k] <- "00:00:00"
  }
}
interval_length[is.nan(interval_length)] <- 0

set_edsu_length <- 200 #choose the edsu length in m

abc_nm <- 0
j <- 1
n_int <- 0
int_time <- rep("00:00:00", round((sum(interval_length)/set_edsu_length)))
for (i in 1:(sum(interval_length)/set_edsu_length)) {
  abc_nm[i]  <- 0
  cumulative_length <- 0
  n_int[i] <- 0
  while (cumulative_length < set_edsu_length) {
    abc_nm[i] <- abc_nm[i] + abc[j]
    j <- j + 1
    cumulative_length <- cumulative_length + interval_length[j]
    n_int[i] <- n_int[i] + 1
    int_time[i] <- as.character(chron(times.= int_time[i], format = "h:m:s") + chron(times. = interval_time[j], format = "h:m:s"))
    if (j >= length(abc)) {
      stop()
    }
  }
}

nasc <- (abc_nm/n_int)*4*pi*1852^2
nasc <- nasc[-!chron(times. = int_time, format = "h:m:s") > chron(times. = "01:00:00", format = "h:m:s")]


#method 2
p <- nasc*0.6697
p[p > 600] <- NA
p[is.na(p)] <- 0
mean(p)



