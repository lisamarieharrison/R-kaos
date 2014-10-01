#KAOS analysis with false bottom removed in Echoview.
#Author: Lisa-Marie Harrison
#Date: 17/09/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data")
fb_38  <- read.csv(file = "38kHz_fbr.csv", header = T) #50pingx5m integration interval
fb_120 <- read.csv(file = "120kHz_fbr.csv", header = T) #50pingx5m integration interval



#calculate 120kHz - 38kHz for each 10x50 window
sv_38 <- fb_38$Sv_mean
sv_120 <- fb_120$Sv_mean
sv_38[sv_38 < -500 | sv_38 > 500] <- NA
sv_120[sv_120 < -500 | sv_120 > 500] <- NA
sv_diff <- sv_120 - sv_38

int_depth <- fb_38$Depth_mean
int_time  <- fb_38$Time_M
int_date  <- fb_38$Date_M
int_td <- paste(int_date, int_time) #date time for each integration interval

#histogram of mean Sv in each interval
#red lines show min and max cutoff for krill
hist(sv_diff)
abline(v = c(2, 12), col = "red")

#remove 120 - 38 kHz values outside of [2, 12] because these are unlikely to be krill
sv_diff[sv_diff < 2 | sv_diff > 12] <- NA
nasc_120 <- fb_120$NASC
nasc_120[sv_diff == NA] <- 0
nasc_120[int_depth < 10] <- 0 #remove the first interval because of noise


#convert to density using target strength
#use 0.028*sv_120 (Demer & Hewitt 1995) as an approximation of krill lengths
p <- nasc_120*0.028 #average volumetric density/interval
b <- (p*10*50)/1000 #biomass in kg/interval


#sum density through depths for each time point to find biomass in water column at each time
pt <- colSums(matrix(b, nrow = 25), na.rm = T)
plot(ksmooth(c(1:length(pt)), pt, bandwidth = 7), type = "l")

