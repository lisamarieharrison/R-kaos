#KAOS full survey predator/krill analysis
#Author: Lisa-Marie Harrison
#Date: 02/10/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data")
krill <- read.csv(file = "kaos_combined_density_intervals.csv", header = T)
pred <- read.csv(file = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_20030115_20030125_abp.csv", header = T)
phyto <- read.csv(file = "kaos_underway_fluoro.csv", header = T)
library(chron)
library(oce)

#convert times into time object
pred$t <- chron(times.= pred$t, format = "h:m:s")
krill$start_time <- chron(times.= krill$start_time, format = "h:m:s")
krill$end_time <- chron(times.= krill$end_time, format = "h:m:s")

#subset data set to include only Adelie penguins
pred <- pred[pred$Species == "Pygoscelis adeliae (Hombron and Jacquinot,1841) (Adelie Penguin)", ]

#remove acoustic intervals that don't have a gps position
krill <- krill[krill$lat < 900, ]
krill <- krill[krill$long < 900, ]

#remove noise values
krill$p[krill$p > 2000] <- NA

#bin predator sightings into intervals corresponding with integration interval times
sightings <- rep(0, length(krill$p))
for (i in 1:nrow(pred)) {
  w <- which(krill$date == pred$d[i] & krill$start_time <= pred$t[i] & 
               krill$end_time >= pred$t[i])
  sightings[w] <- sightings[w] + 1
}

#bin individuals into intervals corresponding with integration interval times
counts <- rep(0, length(krill$p))
for (i in 1:nrow(pred)) {
  w <- which(krill$date == pred$d[i] & krill$start_time <= pred$t[i] & 
               krill$end_time >= pred$t[i])
  counts[w] <- counts[w] + pred$Count[i]
}


#remove sightings that are during off transect times
sightings[is.na(krill$p)] <- 0

#remove krill data that is outside of air breathing predator sightings times
krill$p[krill$start_time < "00:00:00" | krill$start_time > "17:00:00"] <- NA


#find angle of the sun during the survey as a proxy for daylight hours
t.str <- strptime(paste(krill$date, krill$start_time), format='%Y%m%d %H:%M:%S', tz = "UTC")
sun.angle <- sunAngle(t.str, krill$long, krill$lat) 
light.level <- sun.angle$altitude
light.level[is.na(as.character(t.str))] <- NA

#plot krill with sighting times overlayed in red
plot(ksmooth(c(1:length(krill$p)), krill$p, bandwidth = 5), xaxt = "n")
abline(v = which(sightings > 0), col = "red")
points(ksmooth(c(1:length(krill$p)), krill$p, bandwidth = 5), pch = 19)
axis(1, at = seq(1, length(krill$start_time), by = 1000), krill$date[seq(1, length(krill$start_time), by = 1000)])
points(light.level, col = "darkgoldenrod2", type = "l")
points(fluoro.sum*10, col = "green")

density <- ksmooth(c(1:length(krill$p)), krill$p, bandwidth = 20)$y
radius <- sqrt(density/pi)
sym_col <- rep("gray85")
sym_col[density > 100] <- "gray40"
symbols(krill$long, krill$lat, circles=radius, inches = 0.2, bg = sym_col,
        xlab = "Longitude", ylab = "Latitude")
title("krill densities for 50 ping intervals")

#calculate biomass for entire survey area
biomass_kg <- (mean(na.omit(density)) * 10500 * 1e6)/1000
biomass_kt <- biomass_kg/1e6





#------------------------ UNDERWAY FLUORESCENCE DATA --------------------------#

#underway data was sourced from the AAD data centre
#find date and time for each fluorescence reading
fluoro <- phyto$FLUORESCENCE_NOUNIT[phyto$Date >= 20030115 & phyto$Date <= 20030125]
fluoro.date <- phyto$Date[phyto$Date >= 20030115 & phyto$Date <= 20030125]
fluoro.time <- phyto$Time[phyto$Date >= 20030115 & phyto$Date <= 20030125]

#bin underway fluorescence data into 50ping intervals using krill interval times
#calculate average fluorescence throughout each interval
#surface intervals have an average depth of 8m
fluoro.sum <- rep(0, length(krill$p))
for (i in 1:length(fluoro.time)) {
  
  if(is.na(fluoro[i]) == FALSE) {
  
    w <- which(krill$date == as.character(fluoro.date[i]) & krill$start_time<= as.character(fluoro.time[i])
               & krill$end_time >= as.character(fluoro.time[i]))
    
    if(length(w) != 0 & fluoro[i] > 0) {
      fluoro.sum[w] <- fluoro.sum[w] + fluoro[i]  
    }
  
  }
  if(i %% 100 == 0) print(i)
}
fluoro.sum[fluoro.sum == 0] <- NA #remove zero values because they are data deficient, not zero
fluoro.sum[fluoro.sum > 1000] <- NA #remove noise values

plot(density)
points(fluoro.sum, col = "red")

plot(fluoro.sum, krill$p)


#--------------------------- HURDLE AND MIXTURE MODELS -------------------------------#

f1 <- formula(sightings ~ density)
pred_hurdle <- hurdle(f1, dist = "poisson", link = "logit", data = data.frame(sightings, density))
summary(pred_hurdle)

pred_zip <- zeroinfl(f1, dist = "poisson", link = "logit", data = data.frame(sightings, density))
summary(pred_zip)



#------------------------- KRILL PRESENCE/ABSENCE MODELS-----------------------#

krill_present <- 0
for (i in 1:length(density)) {
  if (is.na(density[i]) == FALSE) {
    if (density[i] > 5) {
      krill_present[i] <- 1
    }
  } else {
    krill_present[i] <- 0
  }
}



#-------------------------- KRILL FROM 0-100M DEPTH ----------------------------#
#same approach as above, but uses interval aeral densities that are calculated using
#krill acoustic data collected between 0-100m depth (foraging range of Adelie penguins)

#krill density for 50ping intervals calculated using only depths of <100m
krill_100m <- read.csv(file = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_combined_density_intervals_100m.csv", header = T)

#convert times into time object
krill_100m$start_time <- chron(times.= krill_100m$start_time, format = "h:m:s")
krill_100m$end_time <- chron(times.= krill_100m$end_time, format = "h:m:s")

#remove krill data that is outside of air breathing predator sightings times
krill_100m$p[krill_100m$start_time < "00:00:00" | krill_100m$start_time > "17:00:00"] <- NA

#plot krill with sighting times overlayed in red
plot(ksmooth(c(1:length(krill_100m$p)), krill_100m$p, bandwidth = 5), xaxt = "n", xlab = "date", ylab = "krill density (g/m2)")
abline(v = which(sightings > 0), col = "red")
points(ksmooth(c(1:length(krill_100m$p)), krill_100m$p, bandwidth = 5), pch = 19)
axis(1, at = seq(1, length(krill_100m$start_time), by = 1000), krill_100m$date[seq(1, length(krill_100m$start_time), by = 1000)])
points(light.level, col = "blue", type = "l", lwd = 2)
title("Krill density from 0-100m depth (black), predator sighting locations (red) and light levels (blue)")

#plot densities by day
for (i in unique(krill_100m$date)) {
  
  krill_sub <- krill_100m[krill_100m$date == i, ]
  
  if (length(na.omit(krill_sub$p)) > 0) {
    plot(krill_sub$p, ylab = "krill density (g/m2)")
    abline(v = which(sightings[krill_100m$date == i] > 0), col = "red")
    points(ksmooth(c(1:length(krill_sub$p)), krill_sub$p, bandwidth = 5), pch = 19)
    title(paste("Krill density for date", i))
  }
}











