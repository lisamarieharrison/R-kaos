#analysis of krill around kaos ctd stations
#author: Lisa-Marie Harrison
#date: 18/06/2015

setwd(dir = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS/")
ctd <- read.csv("kaos_ctd.csv", header = T)
krill <- read.csv("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/2m_500m_integrations/kaos_krill_ctd_2m.csv", header = T)
source("C:/Users/Lisa/Documents/phd/southern ocean/Mixed models/R code/R-mixed-models/calc_conditional_marginal_Rsquared.R")
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

#pad krill to 2m depths
p <- rep(NA, nrow(ctd))
for (i in unique(ctd$stn)) {
  krill_depths <- round(krill$depth)[krill$stn == i]
  p[ctd$stn == i & ctd$depth %in% (krill_depths + 1)] <- krill$p[krill$stn == i]
}

#plot p against environmental variables
plot(cbind(ctd[, c(1, 2, 4, 5)], p))
plot(cbind(ctd[, c(1, 2, 4, 5)], log(p)))

#------------------------ KRILL DENSITY AND OXYGEN ----------------------------#

pa <- rep(0, length(p))
pa[p > 0] <- 1
pa[is.na(p)] <- NA

dat <- data.frame(cbind(pa, ctd$oxy, ctd$sal, ctd$depth, ctd$temp, log(p), ctd$stn))
colnames(dat) <- c("pa", "oxy", "sal", "depth", "temp", "p", "stn")
dat <- na.omit(dat)

#two-part model for krill presence/absence and density

pa.lm <- glm(pa ~ oxy + depth + sal, data = dat, family = binomial)
summary(pa.lm)


par(mfrow = c(1, 3))
boxplot(dat$oxy ~ as.factor(dat$pa), ylab = "Oxygen (umol/L)", main = "Oxygen")
boxplot(dat$sal ~ as.factor(dat$pa), ylab = "Salinity (PSS78)", main = "Salinity")
boxplot(dat$depth ~ as.factor(dat$pa), ylab = "Depth (m)", main = "Depth")

d <- dat[dat$pa == 1, ] #subset to only get presence data
d$stn <- as.factor(d$stn)
p.lm <- lme(p ~ oxy, data = d, random =~ 1 + oxy| stn, na.action = na.omit)
summary(p.lm)
r.squared.lme(p.lm)

table(dat$pa, round(fitted(pa.lm)))

#calculate the Variance Inflation Factor (VIF) for the binomial glm covariates
#Lower values = good, High = ~5-10
library(car)
vif(pa.lm)

#calculate sensitivity and specificity of binomial glm
#sensitivity = predicted 1/true 1
#speicificity = predicted 0/true 0
library(caret)
sensitivity(as.factor(round(fitted(pa.lm))), as.factor(na.omit(dat)$pa))
specificity(as.factor(round(fitted(pa.lm))), as.factor(na.omit(dat)$pa))

#plot a ROC curve for the binomial glm
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

par(mfrow = c(1, 1))
plot(M.ROC[1, ], M.ROC[2, ], lwd = 2, type = "l", xlab = "False Positive Rate", ylab = "True Positive Rate")
title("ROC curve")

#calculate the area under the ROC curve (0.5 = bad, 1 = perfect)
library(flux)
auc(M.ROC[1,], M.ROC[2,])

library(caret)
RMSE(fitted(p.lm), na.omit(d)$p)


#-------------------------- PLOTS ON LOG SCALE ----------------------------#

par(mfrow = c(1, 1))

#plot only relationship
plot(d$oxy, d$p, pch = 19)

#plot of model with random intercept and slope on log scale
p.lm <- lme(p ~ oxy, random = ~1 + oxy | stn, data = d, na.action = na.omit)

plot(d$oxy, d$p, xlab = "oxygen", ylab = "krill density", pch = 19, col = "white")
y <- p.lm$coefficients$fixed[1] + p.lm$coefficients$fixed[2]*d$oxy
x <- d$oxy
xy <- cbind(x, y)
xy <- xy[order(xy[, 1]), ]
points(xy[, 1], xy[, 2], col = "black", type = "l", lwd = 4)
title("log(krill) vs oxygen for 13 KAOS CTD stations")


for(i in 1:nlevels(d$stn)) {
  y <- p.lm$coefficients$fixed[1] + p.lm$coefficients$random$stn[i, 1] + (p.lm$coefficients$fixed[2] + p.lm$coefficients$random$stn[i, 2])*d$oxy[d$stn == levels(d$stn)[i]]
  x <- d$oxy[d$stn == levels(d$stn)[i]]
  xy <- cbind(x, y)
  xy <- xy[order(xy[, 1]), ]
  points(xy[, 1], xy[, 2], type = "l", col = i)
}

text(d$oxy, d$p, d$stn, col = as.numeric(d$stn))



#model with station random intercept on log scale
p.lm <- lme(p ~ oxy, random = ~1 | stn, data = d, na.action = na.omit)

plot(d$oxy, d$p, xlab = "oxygen", ylab = "krill density", pch = 19, col = "white")
y <- p.lm$coefficients$fixed[1] + p.lm$coefficients$fixed[2]*d$oxy
x <- d$oxy
xy <- cbind(x, y)
xy <- xy[order(xy[, 1]), ]
points(xy[, 1], xy[, 2], col = "black", type = "l", lwd = 4)
title("log(krill) vs oxygen for 13 KAOS CTD stations")


for(i in 1:nlevels(d$stn)) {
  y <- p.lm$coefficients$fixed[1] + p.lm$coefficients$random$stn[i] + p.lm$coefficients$fixed[2]*d$oxy[d$stn == levels(d$stn)[i]]
  x <- d$oxy[d$stn == levels(d$stn)[i]]
  xy <- cbind(x, y)
  xy <- xy[order(xy[, 1]), ]
  points(xy[, 1], xy[, 2], type = "l", col = i)
}

text(d$oxy, d$p, d$stn, col = as.numeric(d$stn))



#-------------------------- PLOTS ON NATURAL SCALE ----------------------------#

#plot of model with random intercept and slope on natural scale
plot(d$oxy, exp(d$p), xlab = "oxygen", ylab = "krill density", pch = 19, col = "white")
y <- exp(p.lm$coefficients$fixed[1] + p.lm$coefficients$fixed[2]*d$oxy)
x <- d$oxy
xy <- cbind(x, y)
xy <- xy[order(xy[, 1]), ]
points(xy[, 1], xy[, 2], col = "black", type = "l", lwd = 4)
title("log(krill) vs oxygen for 13 KAOS CTD stations")


for(i in 1:nlevels(d$stn)) {
  y <- exp(p.lm$coefficients$fixed[1] + p.lm$coefficients$random$stn[i, 1] + (p.lm$coefficients$fixed[2] + p.lm$coefficients$random$stn[i, 2])*d$oxy[d$stn == levels(d$stn)[i]])
  x <- d$oxy[d$stn == levels(d$stn)[i]]
  xy <- cbind(x, y)
  xy <- xy[order(xy[, 1]), ]
  points(xy[, 1], xy[, 2], type = "l", col = i)
}

text(d$oxy, exp(d$p), d$stn, col = as.numeric(d$stn))



#model with station random intercept on natural scale
p.lm <- lme(p ~ oxy, random = ~1 | stn, data = d, na.action = na.omit)
summary(p.lm)


plot(d$oxy, exp(d$p), xlab = "oxygen", ylab = "krill density", pch = 19, col = "white")
y <- exp(p.lm$coefficients$fixed[1] + p.lm$coefficients$fixed[2]*d$oxy)
x <- d$oxy
xy <- cbind(x, y)
xy <- xy[order(xy[, 1]), ]
points(xy[, 1], xy[, 2], col = "black", type = "l", lwd = 4)
title("log(krill) vs oxygen for 13 KAOS CTD stations")


for(i in 1:nlevels(d$stn)) {
  y <- exp(p.lm$coefficients$fixed[1] + p.lm$coefficients$random$stn[i] + p.lm$coefficients$fixed[2]*d$oxy[d$stn == levels(d$stn)[i]])
  x <- d$oxy[d$stn == levels(d$stn)[i]]
  xy <- cbind(x, y)
  xy <- xy[order(xy[, 1]), ]
  points(xy[, 1], xy[, 2], type = "l", col = i)
}

text(d$oxy, exp(d$p), d$stn, col = as.numeric(d$stn))







