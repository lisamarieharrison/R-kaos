#KAOS air-breathing predator preliminary analysis
#assesses association between krill biomass derived from acoustic data, 
#underway fluorescence data and air-breathing predator observations
#Author: Lisa-Marie Harrison
#Date: 07/08/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data")
dat   <- read.csv(file = "kaos_formatted.csv", header = T, fill = T) #abp observations
track <- read.csv(file  = "kaos_cruise_track.csv", header = T) #location of cruise track
acoustic_38  <- read.csv(file = "kaos_38_sv.csv", header = T) #50x10m integration interval
acoustic_120 <- read.csv(file = "kaos_120_sv.csv", header = T) #50x10m integration interval
sc <- read.csv(file = "schools_detection_38.csv", header = T)
phyto <- read.csv(file = "kaos_underway_fluoro.csv", header = T)
library(chron)
library(oce)


#--------------------------- EXPLORATORY PLOTS --------------------------------#


#plot locations - full data set
dat$Latitude <- as.numeric(as.character(dat$Latitude))
plot(dat$Longitude, dat$Latitude, xlab = "Longitude", ylab = "Latitude")
title("Locations of air-breathing predator sightings - full data set")

#plot locations - only antarctica
plot(dat$Longitude, dat$Latitude, xlab = "Longitude", ylab = "Latitude",
     xlim = c(61.5, 65.5), ylim = c(-67.5, -65.5))
title("Locations of air-breathing predator sightings - Antarctica")

#check time range of sightings
hist(dat$Time, xlab = "Time (hhmm)", main = "Histogram of observation times (UTC)")

#check times against acoustic data (only 3 days of acoustic data is available)
obs  <- c(829: 976)
num.obs <- length(obs)

#subset data to only include times that overlap with the acoustic survey
dat_sub <- dat[obs, ]
dat_sub <- dat_sub[dat_sub$Longitude < 64.55, ]

#table of species seen during acoustic survey
sp.seen <- table(dat_sub$Species, dat_sub$Count)
sightings <- unname(apply(sp.seen, 1, function(x)sum(x != 0)))
ind <- unname(rowSums(sp.seen))
sp <- cbind(sightings, ind)
rownames(sp) <- levels(dat_sub$Species)
sp <- sp[sightings != 0, ]
colnames(sp) <- c("Sightings", "Individuals")

#plot of track with sighting locations superimposed
gps <- unique(track[,c('Latitude','Longitude')]) #filter unique track values
plot(gps$Longitude, gps$Latitude, xlab = "Longitude", ylab = "Latitude")
points(dat_sub$Longitude, dat_sub$Latitude, col = "red", pch = 19)
title("Cruise track (black) with sighting locations superimposed (red)")

#---------------- CALCULATE KRILL BIOMASS DURING EACH INTERVAL ----------------#


#calculate 120kHz - 38kHz for each 10x50 window
sv_38 <- acoustic_38$Sv_mean
sv_120 <- acoustic_120$Sv_mean
sv_38[sv_38 < -500] <- NA
sv_120[sv_120 < -500] <- NA
sv_diff <- sv_120 - sv_38

int_depth <- acoustic_38$Depth_mean
int_time  <- acoustic_38$Time_M
int_date  <- acoustic_38$Date_M
int_td <- paste(int_date, int_time) #date time for each integration interval

#histogram of mean Sv in each interval
#red lines show min and max cutoff for krill
hist(sv_diff)
abline(v = c(2, 12), col = "red")
#write.csv(sv_diff, "sv_diff_na.csv", row.names = F)

#remove 120 - 38 kHz values outside of [2, 12] because these are unlikely to be krill
sv_diff[sv_diff < 2 | sv_diff > 12] <- NA
nasc_120 <- acoustic_120$NASC
nasc_120[sv_diff == NA] <- 0
nasc_120[int_depth < 10] <- 0 #remove the first interval because of noise
nasc_120[acoustic_120$Good_samples < 2000] <- 0 

#convert to density using target strength
#use 0.028*sv_120 (Demer & Hewitt 1995) as an approximation of krill lengths
p <- nasc_120*0.028 #average volumetric density/interval
b <- (p*10*50)/1000 #biomass in kg/interval


#sum density through depths for each time point to find biomass in water column at each time
pt <- colSums(matrix(b, nrow = 25), na.rm = T)
plot(ksmooth(c(1:length(pt)), pt, bandwidth = 7), type = "l")

#------------------------------------------------------------------------------#

#find date and middle time during an integration interval for unique intervals
int_d <- unlist(strsplit(unique(int_td), "  "))[seq(1, length(unlist(strsplit(unique(int_td), "  "))), by = 2)]
int_t <- unlist(strsplit(unique(int_td), "  "))[seq(2, length(unlist(strsplit(unique(int_td), "  "))), by = 2)]

#find start and end times for each unique interval (rather than down all depths)
#air breathing predator observations will be assigned to an interval using these times
int.start.time <- acoustic_38$Time_S[seq(1, length(acoustic_38$Time_S), by = 25)]
int.end.time   <- acoustic_38$Time_E[seq(1, length(acoustic_38$Time_E), by = 25)]
t_s <- chron(times. = as.character(int.start.time), format = "h:m:s")
t_e <- chron(times. = as.character(int.end.time), format = "h:m:s")

#find latitude, longitude and time at the middle of each interval
int.lat.mid <- acoustic_38$Lat_M[seq(1, length(acoustic_38$Lat_M), by = 25)]
int.long.mid  <- acoustic_38$Lon_M[seq(1, length(acoustic_38$Lon_M), by = 25)]
int.time.mid <- acoustic_38$Time_M[seq(1, length(acoustic_38$Time_M), by = 25)]
int.date.mid <- acoustic_38$Date_M[seq(1, length(acoustic_38$Date_M), by = 25)]

#find time at middle of integration interval and remove : and .
t_m <- gsub("[: -]", "" , int.time.mid, perl=TRUE)
t_m <- gsub("[. -]", "" , t_m, perl=TRUE)
t_axt <- as.numeric(substr(t_m, start = 1, stop = 4))

#find angle of the sun at each sighting
t.str <- strptime(paste(int.date.mid, int.time.mid), format='%Y%m%d %H:%M:%S', tz = "UTC")
sun.angle <- sunAngle(t.str, int.long.mid, int.lat.mid) 
light.level <- sun.angle$altitude
h.str <- as.numeric(format(t.str, "%H")) + as.numeric(format(t.str, "%M"))/60
plot(h.str, light.level)

#---------------------- PREDATOR SIGHTINGS ~ KRILL BIOMASS --------------------#


#assign predator sightings to intervals
#pred is a 0 1 vector of whether there any sightings during an interval
#multiple sightings are not reported, only presence/absence of sightings
pred <- rep(0, length(unique(int_td)))
for (i in 1:length(dat_sub$d)) {

  w <- which(int_d == dat_sub$d[i] & t_e >= as.character(dat_sub$t[i])
             & t_s <= as.character(dat_sub$t[i]))[1]
  pred[w] <- 1
}

#plot density (summed through all depths) for each time interval
#times of predator observations are shown with a red vertical line
plot(pt, xlab = "time of day", ylab = "krill biomass", xaxt = "n")
axis(1, at = seq(1, length(pt), by = 100), labels = t_axt[seq(1, length(pt), by = 100)])
title("Krill biomass summed in top 250m and timing of predator sightings in red")
abline(v = which(pred == 1), col = "red")


#assign predator sightings to intervals
#pred_num is the number of sightings during an interval
pred_num <- rep(0, length(unique(int_td)))
for (i in 1:length(dat_sub$d)) {
  
  w <- which(int_d == dat_sub$d[i] & t_e >= as.character(dat_sub$t[i])
             & t_s <= as.character(dat_sub$t[i]))[1]
  pred_num[w] <- pred_num[w] + 1
}

#plot of krill biomass against number of sightings at that time
plot(pt, pred_num, xlab = "krill biomass", ylab = "number of sightings")
title("krill biomass (top 250m) against the number of predator sightings at that time")


#assign predator sightings to intervals
#pred_count is the count during sightings in an interval
pred_count <- rep(0, length(unique(int_td)))
for (i in 1:length(dat_sub$d)) {
  
  w <- which(int_d == dat_sub$d[i] & t_e >= as.character(dat_sub$t[i])
             & t_s <= as.character(dat_sub$t[i]))[1]
  pred_count[w] <- pred_count[w] + dat_sub$Count[i]
}



#plot of krill biomass against number of individuals seen at that time
plot(pt, pred_count, xlab = "krill biomass", ylab = "number of individuals")
title("krill biomass (top 250m) against the number of predator individuals seen at that time")

#same plot as above but with only presences included
plot(pt[pred_count > 0], pred_count[pred_count> 0], xlab = "krill biomass", ylab = "number of individuals")
title("krill biomass (top 250m) against the number of predator individuals seen at that time")


#image of krill biomass along transect
p_mat <- matrix(p, nrow = 25)
image(t(p_mat)[,nrow(p_mat):1])
title("krill biomass in each 10x50m integration interval")

#plot krill biomass with predator locations in black
#add 1s to first row at intervals where there are predators
p_ext_pred <- matrix(rep(pred, 25), nrow = 25, byrow = T) 
p_ext_pred[p_ext_pred == 0 ] <- NA
image(t(p_mat)[, nrow(p_mat):1])
image(t(p_ext_pred)[, nrow(p_ext_pred):1], add = T, col = "black")
title("krill biomass with predator locations overlayed")

#----------------------- USING SCHOOLS DETECTION IN EV ------------------------#

#find time, date and depth of for each detected krill swarm
start.date <- sc$Date_S
start.time <- sc$Time_S
end.date   <- sc$Date_E
end.time   <- sc$Time_E
height     <- sc$Height_mean
depth      <- sc$Depth_mean

#integration interval = 0 if no krill present or = 1 if krill present
krill <- matrix(0, ncol = length(int_td)/25, nrow = 25) 
s <- chron(times. = as.character(start.time), format = "h:m:s")
e <- chron(times. = as.character(end.time), format = "h:m:s")
for (i in 1:length(start.date)) {
  w <- which(int_date == start.date[i] & as.character(int_time) >= s[i] & as.character(int_time) <= e[i] & 
          int_depth >= (depth[i] - height[i]/2) & int_depth <= (depth[i] + height[i]/2))
  krill[w] <- 1
  print(i)
  if(1 == 1) flush.console()
}
krill_int <- colSums(krill)

#plot krill biomass with predator locations in black
#add 1s to first row at intervals where there are predators
p_ext_pred <- matrix(rep(pred, 25), nrow = 25, byrow = T) 
p_ext_pred[p_ext_pred == 0 ] <- NA
image(t(krill)[,nrow(krill):1])
image(t(p_ext_pred)[, nrow(p_ext_pred):1], add = T, col = "black")

#find number of krill swarms during interval
krill_times <- colSums(krill)
plot(krill_times, pred)
points(pred, col = "red")

#---------------------- MODELS FOR PREDATOR ~ KRILL BIOMASS -------------------# 

#glm for biomass ~ predator presence/absence
biomass.glm <- glm(pred ~ pt, family = binomial)
summary(biomass.glm)

#plot observed and fitted values against time
plot(pt, pred, xlab = "krill biomass", ylab = "predator presence/absence")
title("Observed data with fitted values (red line) for biomass GLM pred ~ biomass")
points(pt, fitted(biomass.glm), col = "red", type = "l")


#------------------------ UNDERWAY FLUORESCENCE DATA --------------------------#

#underway data was sourced from the AAD data centre
#find date and time for each fluorescence reading
fluoro <- phyto$FLUORESCENCE_NOUNIT[phyto$Date >= 20030115 & phyto$Date <= 20030117]
fluoro.date <- phyto$Date[phyto$Date >= 20030115 & phyto$Date <= 20030117]
fluoro.time <- phyto$Time[phyto$Date >= 20030115 & phyto$Date <= 20030117]

#bin underway fluorescence data into 50m intervals using krill interval times
#calculate average fluorescence throughout each interval
#surface intervals have an average depth of 8m
fluoro.sum <- rep(0, length(t_s))
for (i in 1:(length(fluoro.time) - 1)) {
  
  w <- which(int_d == as.character(fluoro.date[i]) & t_s <= as.character(fluoro.time)[i]
             & t_e >= as.character(fluoro.time)[i])
  
  fluoro.sum[w] <- fluoro.sum[w] + fluoro[i]  
  
  print(i)
  if(1 == 1) flush.console()
}
fluoro.sum[fluoro.sum == 0] <- NA #remove zero values because they are data deficient, not zero

#plot krill biomass in each interval with summed fluorescence and predator locations overlayed
#krill biomass is summed through all depths during the integration interval
plot(ksmooth(c(1:length(pt)), pt, bandwidth = 50)$y, xlab = "time of day", ylab = "krill density", xaxt = "n")
points(fluoro.sum, col = "red")
points(light.level, col = "orange", type = "l", lwd = 2)
abline(v = which(pred == 1), col = "blue")
title("Krill density (black), phytoplankton fluorescence*100 (red), predator locations (blue) and light levels (yellow)")
axis(1, at = seq(1, length(pt), by = 100), labels = t_axt[seq(1, length(pt), by = 100)])

plot(ksmooth(c(1:length(pt)), pt, bandwidth = 50), type = "l")
points(fluoro.sum*10, col = "red")

plot(fluoro.sum, ksmooth(c(1:length(pt)), pt, bandwidth = 50)$y)


plot(krill_times*40)
points(fluoro.sum, col = "red")

#find times when phytoplankton blooms occur
bloom.times <- t_s[which(fluoro.sum > 15)]

#plot variables against light level
plot(light.level, pt, xlab = "light level", ylab = "krill biomass (g)")
plot(light.level, fluoro.sum, xlab = "light level", ylab = "fluorescence")








