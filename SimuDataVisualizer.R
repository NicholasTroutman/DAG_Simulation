##Nicholas Troutman
##Visualization of CSV SimuData

library(ggplot2)



##upload csv as simuData
simuData=test

#get number of agents
lastAgentName=names(simuData[,ncol(simuData)])
numAgents=as.numeric(substr(lastAgentName,7,100))


simuData$count = numAgents-rowSums(is.na(simuData)) #count how many found it


plot(simuData$txID, simuData$adoption_rate, col=simuData$agent, pch=16)
#plot(simuDataOpt$txID, simuDataOpt$adoption_rate, col=simuData$agent, pch=16) #SaME

plot(simuData$txID, simuData$count, col=simuData$agent, pch=16)
#plot(simuDataOpt$txID, simuDataOpt$count, col=simuData$agent, pch=16) #SAME

plot(simuData$tsa_time, col=simuData$agent, pch=16)
#plot(simuDataOpt$tsa_time, col=simuData$agent, pch=16)
##get tip time difference
simuData2=simuData
simuData2$timeDiff=99999
for (i in 2:nrow(simuData2)){
  #print("i Value")
  print(i)
  tips = simuData2[i,2]

  gsubbby= gsub("\\[|\\]", "", tips$tips)
  tipList=as.numeric(strsplit(gsubbby, split=",", fixed=TRUE)[[1]])
  #print("TIpList: ")
  #print(tipList)
  minTime=99999
  for (tip in 1:length(tipList)){
    #print("TIP: ")
    #print(tipList[tip])
    tempTime = as.numeric(simuData2[tipList[tip]+1,3])
    #print(tempTime)
    if (tempTime< minTime){

      minTime=tempTime
      }

  }
  simuData2[i,numAgents+9]=simuData2[i,3]-minTime
  #break
}



plot(simuData2$txID[2:nrow(simuData2)], simuData2$timeDiff[2:nrow(simuData2)], col=simuData$agent, pch=16)

##get timeLat ordered set
simuData2=simuData
timeLat = function(x){
  #print(x)
  agents = x[9:ncol(x)-1]
  oTime = agents[order(as.matrix(agents),decreasing=FALSE)]
  #oTimeLat = oTime[2:ncol(oTime)]-oTime[1]

  #lat = map_dfc(select(otime, starts_with("agent")), ~ .x - otime[1])[2:ncol(otime)]
  lat = t(apply(oTime, 1,  function(x, na.rm = TRUE) x - min(oTime,na.rm=TRUE)))
  #print(oTime)
  mean50=mean(as.matrix(lat[1:((ncol(lat)+1)*0.5)]))
  mean75=mean(as.matrix(lat[1:((ncol(lat)+1)*0.75)]))
  mean90=mean(as.matrix(lat[1:((ncol(lat)+1)*0.9)]))
  return(c(mean50,mean75, mean90))

}


simuData3=simuData
simuData3$lat50=NA
simuData3$lat75=NA
simuData3$lat90=NA
for (i in 2:nrow(simuData2)){
  print(i)
  means=timeLat(simuData2[i,])
  #print(means[1])
  simuData3$lat50[i]=means[1]
  simuData3$lat75[i]=means[2]
  simuData3$lat90[i]=means[3]
}


##Plot lat

#plot(simuData3$txID, simuData3$lat50, col=simuData3$agent, pch=16)
#plot(simuData3$txID, simuData3$lat75, col=simuData3$agent, pch=16)
#plot(simuData3$txID, simuData3$lat90, col=simuData3$agent, pch=16)

##get ecdf's
cdf50=ecdf(simuData3$lat50)
#ecdf50Big=apply()

##plot latency
h50<- hist( simuData3$lat50, breaks=50, col=16)

#lines(x = knots(cdf50), y=1:length(simuData3$lat50))/length(simuData3$txID) * max(h50$density), col='red' )
lines(x = knots(cdf50),
      y=(1:length(simuData3$lat50))/length(simuData3$lat50) * max(h50$density),
      col ='red')

hist( simuData3$lat75,  breaks=50)
hist( simuData3$lat90,  breaks=50)

##plot all histograms
b<- min(c(simuData3$lat50,simuData3$lat75 ,simuData3$lat90),na.rm=TRUE)
e <- max(c(simuData3$lat50,simuData3$lat75 ,simuData3$lat90),na.rm=TRUE)
ax <- pretty(b:e, n = 60) # Make a neat vector for the breakpoints

c1 <- rgb(173,216,230,max = 255, alpha = 100, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 100, names = "lt.pink")
c3 <- rgb(34,212,46, max = 255, alpha = 100, names = "lt.green")

hg50 <- hist(simuData3$lat50, breaks=ax, plot=FALSE)
hg75 <- hist(simuData3$lat75, breaks=ax, plot=FALSE)
hg90 <- hist(simuData3$lat90, breaks=ax, plot=FALSE)
plot.new()
plot(hg50, col = c1) # Plot 1st histogram using a transparent color
plot(hg75, col = c2, add = TRUE) # Add 2nd histogram using different color
plot(hg90, col = c3, add = TRUE) # Add 3rd histogram using different color

##histogram+density curve
plotDistribution = function (x) {
  N = length(x)
  x <- na.omit(x)
  hist( x,col = "light blue", probability = TRUE, breaks=50)
  lines(density(x), col = "red", lwd = 3)
  rug(x)
  print(N-length(x))
}
plot.new()
plotDistribution(simuData3$lat50)
plotDistribution(simuData3$lat75)
#par(new = TRUE)
plotDistribution(simuData3$lat90)

##boxPlot + histogram
hist(simuData3$lat50, freq = FALSE, main = "Density curve")
axis(1) # Adds horizontal axis
par(new = TRUE)
boxplot(simuData3$lat50, horizontal = TRUE, axes = FALSE,lwd = 2, col = rgb(0, 0, 0, alpha = 0.2))





##Test
data <- data.frame(simuData3$lat50, simuData3$lat75, simuData3$lat90)
colnames(data) <- c("50%", "75%", "90%")
title=sprintf("Transaction Latency: %d Agents", numAgents)
boxplot(data, main=title, ylab="Latency Time (Sec)", xlab="Network Penetration",col=c1)
mean(simuData3$lat50,na.rm=TRUE)
mean(simuData3$lat75,na.rm=TRUE)
mean(simuData3$lat90,na.rm=TRUE)
