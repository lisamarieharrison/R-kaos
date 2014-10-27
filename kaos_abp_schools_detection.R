#KAOS full survey predator/krill analysis using schools detection data
#Author: Lisa-Marie Harrison
#Date: 21/10/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data")
krill <- read.csv(file = "kaos_combined_schools_detection.csv")
pred <- read.csv(file = "kaos_20030115_20030125_abp.csv", header = T)
phyto <- read.csv(file = "kaos_underway_fluoro.csv", header = T)
intervals <- read.csv(file = "kaos_combined_density_intervals_500m.csv", header = T)
library(chron)

intervals <- intervals[intervals$date < 20030126, ]

#convert times into time object
pred$t <- chron(times.= pred$t, format = "h:m:s")
krill$Time_S <- chron(times.= krill$Time_S, format = "h:m:s")
krill$Time_E <- chron(times.= krill$Time_E, format = "h:m:s")

#subset data set to include only Adelie penguins
pred <- pred[pred$Species == "Pygoscelis adeliae (Hombron and Jacquinot,1841) (Adelie Penguin)" |
               pred$Species == "Phoebetria palpebrata Forster,1785 (Light-mantled Sooty Albatross)", ]


#calculate mean areal density for each aggregation
sv <- 10^(krill$Sv_mean/10)

mvbs  <- 10*log10(sv)

#convert to density using target strength (g/m2 per interval)
p <- krill$Height_mean*10 ^((mvbs - -42.22)/10)*1000
p[p > 2000] <- NA

dt <- as.factor(paste(pred$d, pred$t))

#plot krill locations with sighting times overlayed in red
plot(krill$dt[krill$Depth_mean < 100], p[krill$Depth_mean < 100], pch = 19, xaxt = "n")
axis(1, at = seq(1, length(krill$Time_S), by = 100), krill$Date_S[seq(1, length(krill$Time_S), by = 100)])






intervals$start_time <- chron(times.= intervals$start_time, format = "h:m:s")
intervals$end_time <- chron(times.= intervals$end_time, format = "h:m:s")

#remove acoustic intervals that don't have a gps position
intervals <- intervals[intervals$lat < 900, ]
intervals <- intervals[intervals$long < 900, ]

#bin predator sightings into intervals corresponding with integration interval times
sightings <- rep(0, length(intervals$p))
for (i in 1:nrow(pred)) {
  w <- which(intervals$date == pred$d[i] & intervals$start_time <= pred$t[i] & 
               intervals$end_time >= pred$t[i])
  sightings[w] <- sightings[w] + 1
}

krill <- krill[krill$Depth_mean < 50, ]

#bin krill aggregations into intervals corresponding with integration interval times
density <- rep(0, length(intervals$p))
for (i in 1:length(density)) {
  w <- which(krill$Date_S == intervals$date[i] &  krill$Time_S >= intervals$start_time[i] & 
               krill$Time_S <= intervals$end_time[i]) 
  if (length(w) > 0) {
    if (length(na.omit(p[w])) > 0) {
      density[i] <- mean(na.omit(p[w]))
    }
  } else {
    density[i] <- 0
  }
}
#remove krill data that is outside of air breathing predator sightings times
density[intervals$start_time > "17:00:00"] <- NA




plot(density)
abline(v = which(sightings > 0), col = "red")
points(fluoro.sum, col = "green")
points(density, pch = 19)






#------------------------ UNDERWAY FLUORESCENCE DATA --------------------------#

#underway data was sourced from the AAD data centre
#find date and time for each fluorescence reading
fluoro <- phyto$FLUORESCENCE_NOUNIT[phyto$Date >= 20030115 & phyto$Date <= 20030125]
fluoro.date <- phyto$Date[phyto$Date >= 20030115 & phyto$Date <= 20030125]
fluoro.time <- phyto$Time[phyto$Date >= 20030115 & phyto$Date <= 20030125]

#bin underway fluorescence data into 50ping intervals using krill interval times
#calculate average fluorescence throughout each interval
#surface intervals have an average depth of 8m
fluoro.sum <- rep(0, length(intervals$p))
for (i in 1:length(fluoro.time)) {
  
  if(is.na(fluoro[i]) == FALSE) {
    
    w <- which(intervals$date == as.character(fluoro.date[i]) & intervals$start_time<= as.character(fluoro.time[i])
               & intervals$end_time >= as.character(fluoro.time[i]))
    
    if(length(w) != 0 & fluoro[i] > 0) {
      fluoro.sum[w] <- fluoro.sum[w] + fluoro[i]  
    }
    
  }
  if(i %% 100 == 0) print(i)
}
fluoro.sum[fluoro.sum == 0] <- NA #remove zero values because they are data deficient, not zero
fluoro.sum[fluoro.sum > 1000] <- NA #remove noise values


#--------------------------- HURDLE AND MIXTURE MODELS -------------------------------#

dat <- data.frame(sightings, density, fluoro.sum)
library(pscl)

f1 <- formula(sightings ~ density + fluoro.sum)
pred_hurdle <- hurdle(f1, dist = "poisson", link = "logit", data = dat)
summary(pred_hurdle)


plot(fluoro.sum, krill$Height_mean)


