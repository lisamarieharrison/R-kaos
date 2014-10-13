#KAOS full survey predator/krill analysis
#Author: Lisa-Marie Harrison
#Date: 02/10/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data")
krill <- read.csv(file = "kaos_combined_density_intervals.csv", header = T)
pred <- read.csv(file = "kaos_formatted.csv", header = T, fill = T)

#subset data to only include times that overlap with the acoustic survey#check times against acoustic data (only 3 days of acoustic data is available)
pred_sub <- pred[pred$d >= 20030113 & pred$d <= 20030131, ]
