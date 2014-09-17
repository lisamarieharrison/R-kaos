#KAOS analysis with false bottom removed in Echoview.
#Author: Lisa-Marie Harrison
#Date: 17/09/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data")
fb_38  <- read.csv(file = "38_fbr.csv", header = T) #50pingx5m integration interval
fb_120 <- read.csv(file = "120_fbr.csv", header = T) #50pingx5m integration interval
