#extract krill acoustic data around ctd station for kaos data
#author: Lisa-Marie Harrison
#date: 18/06/2015


setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/")
ctd <- read.csv("kaos_ctd.csv", header = T)

for (i in 1:length(unique(ctd$stn))) {
  
  stn <- unique(ctd$stn)[i]
  date <- unique(ctd$date[ctd$stn == stn])
  
}