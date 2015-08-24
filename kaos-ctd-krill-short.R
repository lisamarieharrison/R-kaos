#analysis of krill around kaos ctd stations - short fluoro
#author: Lisa-Marie Harrison
#date: 24/06/2015

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


p <- rep(NA, nrow(ctd))
for (i in unique(ctd$stn)) {
  p[ctd$stn == i & ctd$depth %in% (2*round(krill$depth/2)[krill$stn == i])] <- krill$p[krill$stn == i]
}

pa <- p
pa[p > 0] <- 1

dat <- data.frame(cbind(ctd$oxy, ctd$temp, ctd$sal, pa, p, ctd$depth, ctd$stn, ctd$fluoro))
colnames(dat) <- c("oxy", "temp", "sal", "pa", "p", "depth", "stn", "fluoro")
dat <- dat[!is.na(dat$pa), ]
d <- dat[dat$pa == 1, ]
d <- d[, c(1:3, 5:7)]
d$p <- log(d$p)
d$stn <- as.factor(d$stn)

plot(d[, 1:5])
plot(dat[, c(1:4, 6)])



pa.lm <- glm(pa ~ oxy + sal + temp + depth, data = dat, family = binomial)
summary(pa.lm)


par(mfrow = c(1, 4))
boxplot(dat$oxy ~ as.factor(dat$pa), ylab = "Oxygen (umol/L)", main = "Oxygen")
boxplot(dat$sal ~ as.factor(dat$pa), ylab = "Salinity (PSS78)", main = "Salinity")
boxplot(dat$depth ~ as.factor(dat$pa), ylab = "Depth (m)", main = "Depth")
boxplot(dat$temp ~ as.factor(dat$pa), ylab = "Temp", main = "Temperature")



table(na.omit(dat)$pa, round(fitted(pa.lm)))

library(caret)
sensitivity(as.factor(round(fitted(pa.lm))), as.factor(na.omit(dat)$pa))
specificity(as.factor(round(fitted(pa.lm))), as.factor(na.omit(dat)$pa))

roc.curve <- function(s, print = FALSE) {
  Ps <- (S > s)*1
  FP <- sum((Ps == 1)*(Y == 0))/sum(Y == 0)
  TP <- sum((Ps == 1)*(Y == 1))/sum(Y == 1)
  if (print) {
    print(table(Observed = Y, Predicted = Ps))
  }
  vect <- c(FP, TP)
  names(vect) <- c("FPR", "TPR")
  return(vect)
}

S <- predict(pa.lm, type = "response")
threshold <- 0.5
Y <- na.omit(dat)$pa
roc.curve(threshold, print = TRUE)
ROC.curve <- Vectorize(roc.curve)
M.ROC <- ROC.curve(seq(0, 1, by = 0.01))
library(flux)
auc(M.ROC[1,], M.ROC[2,])


p.lm <- lme(p ~ oxy, data = d, random =~ 1 + oxy| stn, na.action = na.omit)
summary(p.lm)
r.squared.lme(p.lm)
