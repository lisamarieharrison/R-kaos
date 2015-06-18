#analysis of krill around kaos ctd stations
#author: Lisa-Marie Harrison
#date: 18/06/2015

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/")
ctd <- read.csv("kaos_ctd.csv", header = T)
library(lattice)

ctd <- ctd[ctd$depth <= 500, ]

lat.plot <- xyplot(ctd$fluoro ~ ctd$depth | ctd$stn,
                   ylab = "fluoro", outer = FALSE, type = "l")
update(lat.plot)
