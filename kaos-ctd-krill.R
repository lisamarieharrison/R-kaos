#analysis of krill around kaos ctd stations
#author: Lisa-Marie Harrison
#date: 18/06/2015

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/")
ctd <- read.csv("kaos_ctd.csv", header = T)
krill <- read.csv("kaos_krill_ctd.csv", header = T)
library(lattice)
library(latticeExtra)

#remove stations where there was no acoustic data
ctd <- ctd[ctd$stn %in% unique(krill$stn), ]

ctd <- ctd[ctd$depth <= 250, ]

#pad krill to 5m depths
p <- rep(NA, nrow(ctd))
for (i in unique(ctd$stn)) {
  p[ctd$stn == i][seq(1, sum(ctd$stn == i), by = 2)][1:49] <- krill$p[krill$stn == i]
}

lat.plot <- xyplot(ctd$fluoro + p ~ ctd$depth | ctd$stn,
                   ylab = "fluoro", outer = FALSE, type = "l")
lat.plot2 <- xyplot(p*2 ~ ctd$depth | ctd$stn,
                   ylab = "fluoro", outer = FALSE, type = "p", col = "red", pch = 19)
lat.plot + lat.plot2



