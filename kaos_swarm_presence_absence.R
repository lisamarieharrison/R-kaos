#presence and absence of kaos swarms linked to predator presence-absence
#Author: Lisa-Marie Harrison
#Date: 15/12/2014

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/schools detection")
kaos_dates <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_survey_dates_sd.csv", header = F)$V1
library(chron)

for(i in 1:length(kaos_dates)) {
  
  high <- read.csv(paste("high_aggregations_by_region", kaos_dates[i], ".csv", sep = ""), header= T)
  med <- read.csv(paste("medium_aggregations_by_region", kaos_dates[i], ".csv", sep = ""), header = T)
  low <- read.csv(paste("low_aggregations_by_region", kaos_dates[i], ".csv", sep = ""), header= T)
  pred <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/kaos_20030115_20030125_abp.csv")
  
  #put all densities into one list
  krill <- list(high, med, low)
  names(krill) <- c("high", "med", "low")
  
  for (j in names(krill)) {
    
    #exclude other dates
    krill[j][[1]] <- krill[j][[1]][krill[j][[1]]$Date_S == kaos_dates[i] & krill[j][[1]]$Date_E == kaos_dates[i], ]
    
    #remove null values (-999)
    krill[j][[1]] <- krill[j][[1]][which(krill[j][[1]]$Sv_mean != -999), ]
    
    #convert krill times to a chron object
    krill[j][[1]]$Time_S <- chron(times. = krill[j][[1]]$Time_S, format = "h:m:s")
    krill[j][[1]]$Time_E <- chron(times. = krill[j][[1]]$Time_E, format = "h:m:s")
    
  }
  
    
  plot(c(krill["high"][[1]]$Time_S[1], krill["high"][[1]]$Time_E[1]), c(3, 3), type = "l", xlim = c(min(krill["low"][[1]]$Time_S), max(krill["low"][[1]]$Time_E)), lwd = 2, xaxt = "n", ylim = c(0, 4), yaxt = "n", xlab = "time", ylab = "swarm")
  for (j in 1:nrow(low)) {
    points(c(krill["high"][[1]]$Time_S[j], krill["high"][[1]]$Time_E[j]), c(3, 3), type = "l", lwd = 5, col = "red")
    points(c(krill["med"][[1]]$Time_S[j], krill["med"][[1]]$Time_E[j]), c(2, 2), type = "l", lwd = 5, col = "orange")
    points(c(krill["low"][[1]]$Time_S[j], krill["low"][[1]]$Time_E[j]), c(1, 1), type = "l", lwd = 5)
    
  }
  title(paste("Date", kaos_dates[i]))
  
  axis(1, at = seq(from = min(krill["low"][[1]]$Time_S), to = max(krill["low"][[1]]$Time_E), length.out = 10), 
       labels = chron(times. = seq(from = chron(times. = min(krill["low"][[1]]$Time_S), format = "h:m:S"), to = chron(times. = max(krill["low"][[1]]$Time_E), format = "h:m:s"), length.out = 10)))
  axis(2, at = c(1:3), labels = c("low", "medium", "high"))
  
  #subset predator data to include only the correct date and Adelie Penguins
  pred <- pred[pred$d == kaos_dates[i], ]
  pred <- pred[pred$Species == "Pygoscelis adeliae (Hombron and Jacquinot,1841) (Adelie Penguin)", ]
  pred$t <- chron(times. = pred$t, format = "h:m:s")
  
  if (nrow(pred) != 0) {
    for (j in 1:nrow(pred)) {
      
      points(pred$t[j], 4, pch = "x", cex = 3, col = "blue")
      
    }
  }
  
}
