#presence and absence of kaos swarms linked to predator presence-absence
#Author: Lisa-Marie Harrison
#Date: 15/12/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/schools detection")
kaos_dates <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_survey_dates_sd.csv", header = F)$V1

for(i in 1:length(kaos_dates)) {
  
  high <- read.csv(paste("high_aggregations_by_region", kaos_dates[i], ".csv", sep = ""), header= T)
  med <- read.csv(paste("medium_aggregations_by_region", kaos_dates[i], ".csv", sep = ""), header = T)
  low <- read.csv(paste("low_aggregations_by_region", kaos_dates[i], ".csv", sep = ""), header= T)
  pred <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_20030115_20030125_abp.csv")
  library(chron)
  
  pred <- pred[pred$d == kaos_dates[i], ]
  
  #exclude other dates
  low <- low[low$Date_S == kaos_dates[i] & low$Date_E == kaos_dates[i], ]
  med <- med[med$Date_S == kaos_dates[i] & med$Date_E == kaos_dates[i], ]
  high <- high[high$Date_S == kaos_dates[i] & high$Date_E == kaos_dates[i], ]
  
  
  
  high$Time_S <- chron(times. = high$Time_S, format = "h:m:s")
  high$Time_E <- chron(times. = high$Time_E, format = "h:m:s")
  med$Time_S <- chron(times. = med$Time_S, format = "h:m:s")
  med$Time_E <- chron(times. = med$Time_E, format = "h:m:s")
  low$Time_S <- chron(times. = low$Time_S, format = "h:m:s")
  low$Time_E <- chron(times. = low$Time_E, format = "h:m:s")
  
  
  plot(c(high$Time_S[1], high$Time_E[1]), c(3, 3), type = "l", xlim = c(min(low$Time_S), max(low$Time_E)), lwd = 2, xaxt = "n", ylim = c(0, 4), yaxt = "n", xlab = "time", ylab = "swarm")
  for (j in 1:nrow(low)) {
    points(c(high$Time_S[j], high$Time_E[j]), c(3, 3), type = "l", lwd = 5, col = "red")
    points(c(med$Time_S[j], med$Time_E[j]), c(2, 2), type = "l", lwd = 5, col = "orange")
    points(c(low$Time_S[j], low$Time_E[j]), c(1, 1), type = "l", lwd = 5)
    
  }
  title(paste("Date", kaos_dates[i]))
  
  axis(1, at = seq(from = min(low$Time_S), to = max(low$Time_E), length.out = 10), 
       labels = chron(times. = seq(from = chron(times. = min(low$Time_S), format = "h:m:S"), to = chron(times. = max(low$Time_E), format = "h:m:s"), length.out = 10)))
  axis(2, at = c(1:3), labels = c("low", "medium", "high"))
  
  pred <- pred[pred$Species == "Pygoscelis adeliae (Hombron and Jacquinot,1841) (Adelie Penguin)", ]
  pred$t <- chron(times. = pred$t, format = "h:m:s")
  
  if (nrow(pred) != 0) {
    for (j in 1:nrow(pred)) {
      
      points(pred$t[j], 4, pch = "x", cex = 3, col = "blue")
      
    }
  }
  
}
