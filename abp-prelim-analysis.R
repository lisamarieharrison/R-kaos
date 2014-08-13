# KAOS air-breathing predator preliminary analysis
#Lisa-Marie Harrison
#07/08/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data")
dat <- read.csv(file = "kaos_formatted.csv", header = T, fill = T)
track <- read.csv(file  = "kaos_cruise_track.csv", header = T)
acoustic_38  <- read.csv(file = "kaos_38_sv.csv", header = T)
acoustic_120 <- read.csv(file = "kaos_120_sv.csv", header = T)
dat$Latitude <- as.numeric(as.character(dat$Latitude))
library(chron)

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


#calculate 120kHz - 38kHz for each 10x50 window
sv_diff <- acoustic_120$Sv_mean - acoustic_38$Sv_mean
sv_diff[sv_diff < -500 | sv_diff > 500] <- NA

int_depth <- acoustic_38$Depth_mean
int_time  <- acoustic_38$Time_M
int_date  <- acoustic_38$Date_M
int_td <- paste(int_date, int_time) #date time for each integration interval

hist(sv_diff)
abline(v = c(2, 12), col = "red")
#write.csv(sv_diff, "sv_diff_na.csv", row.names = F)

#convert to density using target strength
sv_diff[sv_diff < 2 | sv_diff > 12] <- NA
pv <- 0.028*sv_diff #g\m3
p <- pv*(10*50) #g/interval

#sum p through depths for each time point to find density in water column at each time
pt <- rep(0, length(unique(int_td)))
for (i in 1:length(unique(int_td))) {
  w <- unique(int_td)[i]
  t <- which(int_td == w)
  pt[i] <- sum(na.omit(p[t]))
  print(i)
  if (1 == 1) flush.console()
}

#find date and middle time during an integration interval for unique intervals
int_d <- unlist(strsplit(unique(int_td), "  "))[seq(1, length(unlist(strsplit(unique(int_td), "  "))), by = 2)]
int_t <- unlist(strsplit(unique(int_td), "  "))[seq(2, length(unlist(strsplit(unique(int_td), "  "))), by = 2)]

#find start and end times for each unique interval (rather than down all depths)
#air breathing predator observations can then be assigned to an interval using these times
t_s <- acoustic_38$Time_S[seq(1, length(acoustic_38$Time_S), by = 25)]
t_e <- acoustic_38$Time_E[seq(1, length(acoustic_38$Time_E), by = 25)]

#find time at middle of integration interval and remove : and .
t_m <- acoustic_38$Time_M[seq(1, length(acoustic_38$Time_E), by = 25)]
t_m <- gsub("[: -]", "" , t_m, perl=TRUE)
t_m <- gsub("[. -]", "" , t_m, perl=TRUE)
t_axt <- as.numeric(substr(t_m, start = 1, stop = 4))

#assign predator sightings to intervals
#pred is a 0 1 vector of whether there any sightings during an interval
#multiple sightings are not reported, only presence/absence of sightings
pred <- rep(0, length(unique(int_td)))
for (i in 1:length(dat_sub$d)) {
  t_s <- chron(times. = as.character(t_s), format = "h:m:s")
  t_e <- chron(times. = as.character(t_e), format = "h:m:s")
  
  w <- which(int_d == dat_sub$d[i] & t_e > as.character(dat_sub$t[i])
             & t_s < as.character(dat_sub$t[i]))
  pred[w] <- 1
}

#plot density (summed through all depths) for each time interval
#times of predator observations are shown with a red vertical line
plot(pt, xlab = "time of day", ylab = "krill biomass", xaxt = "n")
axis(1, at = seq(1, length(pt), by = 100), labels = t_axt[seq(1, length(pt), by = 100)])
title("Krill biomass summed in top 250m and timing of predator sightings in red")
abline(v = which(pred == 1), col = "red")







