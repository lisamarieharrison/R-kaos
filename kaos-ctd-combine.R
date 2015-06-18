#reads kaos ctd .all files and combines into one csv
#author: Lisa-Marie Harrison
#date: 18/06/2015


setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/kaos/kaos_ctd/")
ctd_files <- list.files(pattern = ".all")

file.create("kaos_ctd.csv")

for (i in ctd_files) {
  
  dat <- read.table(i, skip = 15, header = F)
  
  time <- read.csv(i, header = F)
  
  date <- substr(time[3, 1], start = 21, stop = 31)
  start_time <- substr(time[4, 1], start = 21, stop = 26)
  bottom_time <- substr(time[5, 1], start = 21, stop = 26)
    
  out <- cbind(dat, rep(which(ctd_files == i), nrow(dat)), rep(start_time, nrow(dat)), rep(bottom_time, nrow(dat)), rep(date, nrow(dat)))
  
  write.table(out, "kaos_ctd.csv", row.names = F, col.names = F, sep = ",", append = T)
  
}

#add headers to the completed file
dat <- read.csv("kaos_ctd.csv", header = F)
names(dat) <- c("depth", "temp", "cond", "sal", "oxy", "fluoro", "npts", "stn", "start_time", "bottom_time", "date")
write.csv(dat, "kaos_ctd.csv", row.names = F)
