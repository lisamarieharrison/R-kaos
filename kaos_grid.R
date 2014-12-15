#transfers the schools detection (integration by regions by cells) for each day on to 
#a presence/absence grid. 
#zero cells are no krill and numbers represent the swarm ID
#Author: Lisa-Marie Harrison
#Date: 15/12/2014

dat <- read.csv(file = "C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/schools detection/high_aggregations_20030115.csv")
library(chron)
colnames(dat)[1] <- "Region_ID"


#create empty grid
n.col <- max(dat$Interval) - min(dat$Interval) + 1
n.row <- max(dat$Layer) - min(dat$Layer) + 1
swarm.grid <- matrix(0, nrow = n.row, ncol = n.col) 

#fill with region ID for krill occupied cells
for (i in 1:nrow(dat)) {
  swarm.grid[dat$Layer[i] - min(dat$Layer) + 1, dat$Interval[i] - min(dat$Interval) + 1] <- dat$Region_ID[i]
}

swarm.time <- chron(times. = dat$Time_S, format = "h:m:s")

x <- dat$Time_S[duplicated(dat$Interval) == FALSE]
empty <- zoo(order.by = unique(dat$Interval))
na.locf(merge(x, empty))

x <- which(is.duplicated(dat$Interval))
colnames(swarm.grid) <- as.character(dat$Time_S[])


