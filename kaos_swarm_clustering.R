#read in data files - the results of integration by regions - use kaos data in the vignette though
f038=read.csv('C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/038-aggregations.csv')
f120=read.csv('C:/Users/Lisa/Documents/phd/southern ocean/KAOS data/120-aggregations.csv')
Sv038=cbind.data.frame(Region_name=f038$Region_name,
                       Sv038=f038$Sv_mean)
#merge Sv_mean from 38 to 120 kHz data: 
ag=merge(f120,Sv038,by='Region_name')
#calculate dB difference:
ag$dBdiff=ag$Sv_mean-ag$Sv038
#isolate krill swarms
nrow(ag)
swarms=subset(ag,ag$dBdiff>2 & ag$dBdiff<16)
nrow(swarms)

#select swarm metrics for PCA - there are loads more we coudl chose, these are just examples:
swarms=swarms[,c("Sv_mean","Sv_max","Sv_min","Corrected_length","Height_mean",
                 "Depth_mean","Corrected_thickness","Corrected_perimeter",             
                 "Corrected_area","Image_compactness",               
                 "Corrected_mean_amplitude","Coefficient_of_variation",
                 "Horizontal_roughness_coefficient",
                 "Vertical_roughness_coefficient")]
#scale the data
swarms$Horizontal_roughness_coefficient <- as.numeric(swarms$Horizontal_roughness_coefficient) #convert factor to numeric
scaleSwarm=scale(swarms)
#determine number of clusters in scaled krill swarm data using the gap-stat. see:
#Tibshirani, R., Walther, G., Hastie, T. (2001), Estimating the number of clusters in a data set via the gap statistic, "Journal of the Royal Statistical Society", ser. B, vol. 63, part 2, 411-423.


library(clusterSim)
#the code that follows is lifted from the 
#index.Gap function in clusterSim:
# nc - number_of_clusters
min_nc=1
max_nc=10
min <- 0
clopt <- NULL
res <- array(0, c(max_nc-min_nc+1, 2))
res[,1] <- min_nc:max_nc
found <- FALSE
for (nc in min_nc:max_nc){
  cl1 <- pam(scaleSwarm, nc, diss=FALSE)
  cl2 <- pam(scaleSwarm, nc+1, diss=FALSE)
  clall <- cbind(cl1$clustering, cl2$clustering)
  gap <- index.Gap(scaleSwarm,clall,B=20,method="pam")
  res[nc-min_nc+1, 2] <- diffu <- gap$diffu
  if ((res[nc-min_nc+1, 2] >=0) && (!found)){
    nc1 <- nc
    min <- diffu
    clopt <- cl1$cluster
    found <- TRUE
  }
}
if (found){
  print(paste("Minimal nc where diffu>=0 is",nc1,"for diffu=",round(min,4)),quote=FALSE)
}else{
  print("I have not found clustering with diffu>=0", quote=FALSE)
}
plot(res,type="p",pch=0,xlab="Number of clusters",ylab="diffu",xaxt="n")
abline(h=0, untf=FALSE)
axis(1, c(min_nc:max_nc))
#this example has found two clusters - might only find one in kaos data though.
swarms$type=  pam(scaleSwarm, 2, diss=FALSE)$clustering
table(swarms$type) #krill swarm types determined by PAM
#example results summary for depth, height, length by type
lapply(split(swarms[,c("Depth_mean" ,'Height_mean','Corrected_length')],swarms$type),summary)
