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

#remove error values
ctd$fluoro[ctd$fluoro == -9] <- NA

#remove station 11 because no krill data
ctd <- ctd[ctd$stn != 11, ]
krill <- krill[krill$stn != 11, ]

krill$p[is.na(krill$p)] <- 0

krill <- krill[krill$depth > 0, ]
krill <- krill[!is.na(krill$depth), ]

#pad krill to 5m depths
p <- rep(NA, nrow(ctd))
for (i in unique(ctd$stn)) {
  p[ctd$stn == i & ctd$depth %in% (2*round(krill$depth/2)[krill$stn == i])] <- krill$p[krill$stn == i]
}
p[p == 0] <- NA

lat.plot <- xyplot(log(ctd$fluoro + 1) ~ ctd$depth | ctd$stn,
                   ylab = "fluoro", outer = FALSE, type = "l")
lat.plot2 <- xyplot(krill$p + 2 ~ krill$depth | krill$stn,
                   ylab = "fluoro", outer = FALSE, type = "p", col = "red", pch = 19)
lat.plot + lat.plot2


#linear model for log-log krill fluoro relationship
fluoro.lm <- lm(log(p) ~ log(ctd$fluoro + 1))
summary(fluoro.lm)


plot(log(ctd$fluoro + 1), log(p), xlab = "log(fluoro)", ylab = "log(krill density)")
y <- fluoro.lm$coefficients[1] + fluoro.lm$coefficients[2]*log(ctd$fluoro + 1)
x <- log(ctd$fluoro + 1)
xy <- cbind(x, y)
xy <- xy[order(xy[, 1]), ]
points(xy[, 1], xy[, 2], col = "red", type = "l")






