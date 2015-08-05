#analysis of krill around kaos ctd stations
#author: Lisa-Marie Harrison
#date: 18/06/2015

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/")
ctd <- read.csv("kaos_ctd.csv", header = T)
krill <- read.csv("kaos_krill_ctd.csv", header = T)
library(lattice)
library(latticeExtra)
library(nlme)
library(rgl)

#remove stations where there was no acoustic data
ctd <- ctd[ctd$stn %in% unique(krill$stn), ]

ctd <- ctd[ctd$depth <= 250, ]

#remove error values
ctd$fluoro[ctd$fluoro == -9] <- NA
ctd$temp[ctd$temp == -9] <- NA
ctd$oxy[ctd$oxy < 0] <- NA
ctd$sal[ctd$sal < 0] <- NA

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
ctd$stn <- as.factor(ctd$stn)
fluoro.lm <- lm(log(p) ~ log(ctd$fluoro + 1))
summary(fluoro.lm)


plot(log(ctd$fluoro + 1), log(p), xlab = "log(fluoro)", ylab = "log(krill density)", pch = 19)
y <- fluoro.lm$coefficients[1] + fluoro.lm$coefficients[2]*log(ctd$fluoro + 1)
x <- log(ctd$fluoro + 1)
xy <- cbind(x, y)
xy <- xy[order(xy[, 1]), ]
points(xy[, 1], xy[, 2], col = "red", type = "l")
title("log(krill) vs log(phyto) for 13 KAOS CTD stations")


#plot coloured by station
plot(log(ctd$fluoro + 1), log(p), xlab = "log(fluoro)", ylab = "log(krill density)", pch = 19, col = as.factor(ctd$stn), cex = 2)
title("log(krill) vs log(phyto) coloured by station (n = 13)")


#model with station random intercept
fluoro.lm <- lme(log(p) ~ log(fluoro + 1), random = ~1 | stn, data = ctd, na.action = na.omit)
summary(fluoro.lm)


plot(log(ctd$fluoro + 1), log(p), ylim = c(-7, 1), xlab = "log(fluoro)", ylab = "log(krill density)", pch = 19, col = ctd$stn)
y <- fluoro.lm$coefficients$fixed[1] + fluoro.lm$coefficients$fixed[2]*log(ctd$fluoro + 1)
x <- log(ctd$fluoro + 1)
xy <- cbind(x, y)
xy <- xy[order(xy[, 1]), ]
points(xy[, 1], xy[, 2], col = "black", type = "l", lwd = 4)
title("log(krill) vs log(phyto) for 13 KAOS CTD stations")


for(i in 1:nlevels(ctd$stn)) {
  y <- fluoro.lm$coefficients$fixed[1] + fluoro.lm$coefficients$random$stn[i] + fluoro.lm$coefficients$fixed[2]*log(ctd$fluoro + 1)[ctd$stn == levels(ctd$stn)[i]]
  x <- log(ctd$fluoro + 1)[ctd$stn == levels(ctd$stn)[i]]
  xy <- cbind(x, y)
  xy <- xy[order(xy[, 1]), ]
  points(xy[, 1], xy[, 2], type = "l", col = i)
}



#model with station random intercept and slope
fluoro.lm <- lme(log(p) ~ log(fluoro + 1), random = ~1 + log(fluoro + 1) | stn, data = ctd, na.action = na.omit)
summary(fluoro.lm)


plot(log(ctd$fluoro + 1), log(p), ylim = c(-7, 1), xlab = "log(fluoro)", ylab = "log(krill density)", pch = 19, col = ctd$stn)
y <- fluoro.lm$coefficients$fixed[1] + fluoro.lm$coefficients$fixed[2]*log(ctd$fluoro + 1)
x <- log(ctd$fluoro + 1)
xy <- cbind(x, y)
xy <- xy[order(xy[, 1]), ]
points(xy[, 1], xy[, 2], col = "black", type = "l", lwd = 4)
title("log(krill) vs log(phyto) with random intercept and slope")


for(i in 1:nlevels(ctd$stn)) {
  y <- fluoro.lm$coefficients$fixed[1] + fluoro.lm$coefficients$random$stn[i, 1] + (fluoro.lm$coefficients$fixed[2] + fluoro.lm$coefficients$random$stn[i, 2])*log(ctd$fluoro + 1)[ctd$stn == levels(ctd$stn)[i]]
  x <- log(ctd$fluoro + 1)[ctd$stn == levels(ctd$stn)[i]]
  xy <- cbind(x, y)
  xy <- xy[order(xy[, 1]), ]
  points(xy[, 1], xy[, 2], type = "l", col = i)
}

#add coloured numbers for each station (set original point colour to white)
text(log(ctd$fluoro + 1), log(p), ctd$stn, col = as.numeric(ctd$stn))


#calculate marginal and conditional R-squared for mixed model
source("C:/Users/Lisa/Documents/phd/southern ocean/Mixed models/R code/R-mixed-models/calc_conditional_marginal_Rsquared.R")
r.squared.lme(fluoro.lm)


#------------------------------- PRESENCE/ABSENCE -----------------------------#

p <- rep(NA, nrow(ctd))
for (i in unique(ctd$stn)) {
  p[ctd$stn == i & ctd$depth %in% (2*round(krill$depth/2)[krill$stn == i])] <- krill$p[krill$stn == i]
}

pa <- p
pa[p > 0] <- 1
plot(ctd$fluoro, pa)

boxplot(ctd$fluoro ~ pa, xlab = "Krill (0 = absent, 1 = present)", ylab = "phytoplankton fluoro")

#binomial glm for presence/absence
pa.lm <- glm(pa ~ fluoro - 1, dat = ctd, family = binomial)
summary(pa.lm)

MyData <- data.frame(fluoro = seq(from = -0.6, to =  13, by = 0.1))
Pred <- predict(pa.lm, newdata = MyData, type = "response")
plot(ctd$fluoro, pa, xlab = "fluoro", ylab = "Probability of krill presence", ylim = c(0, 1))
lines(MyData$fluoro, Pred)


#3d plot of temperature, salinity and oxygen with krill presence/absence as colour
dat <- data.frame(cbind(ctd$oxy, ctd$temp, ctd$sal, pa, p, ctd$depth))
colnames(dat) <- c("oxy", "temp", "sal", "pa", "p", "depth")
dat <- dat[!is.na(dat$pa), ]

plot3d(x = dat$depth, y = dat$temp, z = dat$oxy, col = (dat$pa + 1), pch = 19, type = "s", 
       size = 0.5, xlab = "Oxygen", ylab = "Temp", zlab = "Salinity", box = FALSE)

#add 95% confidence ellipsoids for presence and absence
pa_0 <- dat[dat$pa == 0, ] #only absent
ellips <- ellipse3d(cov(cbind(pa_0$depth, pa_0$temp, pa_0$oxy)), 
                    centre = c(mean(pa_0$depth), mean(pa_0$temp), 
                               mean(pa_0$oxy)), level = 0.95)
plot3d(ellips, col = "black", alpha = 0.2, add = TRUE)

pa_1 <- na.omit(dat[dat$pa == 1, ]) #only present
ellips <- ellipse3d(cov(cbind(pa_1$depth, pa_1$temp, pa_1$oxy)), 
                    centre = c(mean(pa_1$depth), mean(pa_1$temp), 
                               mean(pa_1$oxy)), level = 0.95)
plot3d(ellips, col = "red", alpha = 0.2, add = TRUE)

#add legend
legend3d("topright", c("present", "absent"), col = c("red", "black"), bty = "n", pch = 19)


pa.lm <- glm(pa ~ temp + sal, dat = ctd, family = binomial)
summary(pa.lm)

MyData <- data.frame(cbind(seq(from = -2, to =  1, length.out = 100), seq(from = 33.2, to = 34.7, length.out = 100)))
colnames(MyData) = c("temp", "sal")
Pred <- predict(pa.lm, newdata = MyData, type = "response")



plot(ctd$fluoro, pa, xlab = "fluoro", ylab = "Probability of krill presence", ylim = c(0, 1))
lines(MyData$fluoro, Pred)







