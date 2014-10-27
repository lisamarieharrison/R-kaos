#runs abp analysis for krill density data that is not aggregated into integration intervals
#check whether abp are near shallower krill assemblages
#Author: Lisa-Marie Harrison
#Date: 20/10/2014

krill <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_combined_density_intervals_all_depths.csv", header = F)
pred <- read.csv(file = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_20030115_20030125_abp.csv", header = T)
library(chron)


names(krill) <- c("time", "date", "long", "start_time", "end_time", "layer", "p")

#convert times into time object
pred$t <- chron(times.= pred$t, format = "h:m:s")
krill$start_time <- chron(times.= krill$start_time, format = "h:m:s")
krill$end_time <- chron(times.= krill$end_time, format = "h:m:s")

#subset data set to include only Adelie penguins
pred <- pred[pred$Species == "Pygoscelis adeliae (Hombron and Jacquinot,1841) (Adelie Penguin)", ]

#remove acoustic intervals that don't have a gps position
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

#calculate biomass for entire survey area
biomass_kg <- (mean(na.omit(krill$p)) * 10500 * 1e6)/1000
biomass_kt <- biomass_kg/1e6





