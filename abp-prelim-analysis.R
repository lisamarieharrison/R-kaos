# KAOS air-breathing predator preliminary analysis
setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data")
dat <- read.csv(file = "kaos_formatted.csv", header = T, fill = T)
dat$Latitude <- as.numeric(as.character(dat$Latitude))

#plot locations - full data set
plot(dat$Longitude, dat$Latitude, xlab = "Longitude", ylab = "Latitude")
title("Locations of air-breathing predator sightings - full data set")

#plot locations - only antarctica
plot(dat$Longitude, dat$Latitude, xlab = "Longitude", ylab = "Latitude",
     xlim = c(61.5, 65.5), ylim = c(-67.5, -65.5))
title("Locations of air-breathing predator sightings - Antarctica")


