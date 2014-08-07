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

#check time range of sightings
hist(dat$Time, xlab = "Time (hhmm)", main = "Histogram of observation times (UTC)")

#check times against acoustic data (only 3 days of acoustic data is available)
obs <- which(as.character(dat$Date_UTC) >= "15/01/2003 16:08" & as.character(dat$Date_UTC) <= "17/01/2003 16:15")
num.obs <- length(obs)

#subset data to only include times that overlap with the acoustic survey
dat_sub <- dat[obs, ]

#table of species seen during acoustic survey
sp.seen <- table(dat_sub$Species, dat_sub$Count)
sightings <- unname(apply(sp.seen, 1, function(x)sum(x != 0)))
ind <- unname(rowSums(sp.seen))

sp <- cbind(sightings, ind)
rownames(sp) <- levels(dat_sub$Species)
sp <- sp[sightings != 0, ]
colnames(sp) <- c("Sightings", "Individuals")



