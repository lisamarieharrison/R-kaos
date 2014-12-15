#presence and absence of kaos swarms linked to predator presence absence
#Author: Lisa-Marie Harrison
#Date: 15/12/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/schools detection")
high <- read.csv("high_aggregations_by_region20030116.csv", header= T)
med <- read.csv("medium_aggregations_by_region20030116.csv", header = T)
low <- read.csv("low_aggregations_by_region20030116.csv", header= T)
pred <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_20030115_20030125_abp.csv")[1:39, ]
library(chron)

#exclude other dates
low <- low[low$Date_S == 20030116 & low$Date_E == 20030116, ]
med <- med[med$Date_S == 20030116 & med$Date_E == 20030116, ]
high <- high[high$Date_S == 20030116 & high$Date_E == 20030116, ]



high$Time_S <- chron(times. = high$Time_S, format = "h:m:s")
high$Time_E <- chron(times. = high$Time_E, format = "h:m:s")
med$Time_S <- chron(times. = med$Time_S, format = "h:m:s")
med$Time_E <- chron(times. = med$Time_E, format = "h:m:s")
low$Time_S <- chron(times. = low$Time_S, format = "h:m:s")
low$Time_E <- chron(times. = low$Time_E, format = "h:m:s")


plot(c(high$Time_S[i], high$Time_E[i]), c(3, 3), type = "l", xlim = c(min(low$Time_S), max(low$Time_E)), lwd = 2, xaxt = "n", ylim = c(0, 4), yaxt = "n", xlab = "time", ylab = "swarm")
for (i in 1:nrow(low)) {
  points(c(high$Time_S[i], high$Time_E[i]), c(3, 3), type = "l", lwd = 5, col = "red")
  points(c(med$Time_S[i], med$Time_E[i]), c(2, 2), type = "l", lwd = 5, col = "orange")
  points(c(low$Time_S[i], low$Time_E[i]), c(1, 1), type = "l", lwd = 5)
  
}

axis(1, at = seq(from = min(low$Time_S), to = max(low$Time_E), length.out = 10), 
     labels = chron(times. = seq(from = chron(times. = min(low$Time_S), format = "h:m:S"), to = chron(times. = max(low$Time_E), format = "h:m:s"), length.out = 10)))
axis(2, at = c(1:3), labels = c("low", "medium", "high"))


pred <- pred[pred$Species == "Pygoscelis adeliae (Hombron and Jacquinot,1841) (Adelie Penguin)", ]
pred$t <- chron(times. = pred$t, format = "h:m:s")

for (i in 1:nrow(pred)) {

  points(pred$t[i], 4, pch = "x", lwd = 5, col = "blue")
  
}


