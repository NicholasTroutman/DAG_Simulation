##Nicholas Troutman
##Visualization of CSV SimuData

library(ggplot2)
library(reshape2)


##upload csv as simuData
simuData=test

#get number of agents
agentSeen = grepl("agent_", names(simuData)) #index for 
numAgents=sum(grepl("agent_",names(simuData))) #grep "agent_" from headers of simuData
simuData$singleAgent = gsub("\\[|\\]", "", simuData$agent) #change agent owner to single and remove brakcets []
simuData$singleBlockLink = gsub("\\[|\\]", "", simuData$tips) #change agent owner to single and remove brakcets []\

simuData$transaction_creation_time = gsub("\\[|\\]", "", simuData$transaction_creation_time) #change agent owner to single and remove brakcets []\

simuData$count = numAgents-rowSums(is.na(simuData[agentSeen])) #count how many found it

simuData$timeToConfirm = simuData$confirmationTime-simuData$arrival_time

plot(simuData$ID, simuData$adoption_rate, col=simuData$singleAgent, pch=16)
#plot(simuDataOpt$txID, simuDataOpt$adoption_rate, col=simuData$agent, pch=16) #SaME

plot(simuData$ID, simuData$count, col=simuData$singleAgent, pch=16)
#plot(simuDataOpt$txID, simuDataOpt$count, col=simuData$agent, pch=16) #SAME

##time to share blocks:
agentSeendf = data.frame(simuData[agentSeen])[-1,]
shareTimes = apply(agentSeendf, 1, FUN = max)  - apply(agentSeendf, 1, FUN = min)
hist(shareTimes, breaks=20)


##PROPORTION OF CONFIRMED BLOCKS/Tot(blocks)
percentConfirmedBlocks = sum(simuData$confirmedBlock=="True")/nrow(simuData)
print(percentConfirmedBlocks)

##Proportion of confirmed Txs
unconfirmedBlocks = simuData[simuData$confirmedBlock=="False",]
numUncomfirmedTxs = 0
for (i in 2:nrow(unconfirmedBlocks)){
  tx_creation_time = strsplit(unconfirmedBlocks$transaction_creation_time[i], ",")[[1]] #returns 1, 2 3
  for (tct in tx_creation_time){
    numUncomfirmedTxs = numUncomfirmedTxs +1
    } 
  }
print(numUncomfirmedTxs) #num of unconfirmed Txs, divide by #stuff


###PLOTS:

##Get Agent/ID col=confirmed
plot(simuData$ID,simuData$singleAgent,col = ifelse(simuData$confirmedBlock =="False",'red','green'), pch = 16 )
for (i in 2:nrow(simuData)){
 # print(i)
  linkedBlock = simuData$singleBlockLink[i]
  linkedBlock = strtoi(linkedBlock)+1
  #print(linkedBlock)
  
 segments(x0=strtoi(simuData$ID[linkedBlock]), y0= strtoi(simuData$singleAgent[linkedBlock]), x1 = strtoi(simuData$ID[i]), y1 = strtoi(simuData$singleAgent[i]), col="black" )
   
}
points(simuData$ID,simuData$singleAgent,col = ifelse(simuData$confirmedBlock =="False",'red','green'), pch = 19, cex=1.9 ) 


##Histogram of transaction submission time
#for each row, get difference between simuData$arrival_time - simuData$transaction_creation_time
txSubmissionTimeConfirmed = list()
txSubmissionTime = list()
#for (i in 39:39){
for (i in 2:nrow(simuData)){
  #print(i)
    tx_creation_time = strsplit(simuData$transaction_creation_time[i], ",")[[1]] #returns 1, 2 3
  for (tct in tx_creation_time){
    newTST = ceiling(as.numeric(simuData$arrival_time[i])) - strtoi(tct)
    
    if (newTST < 1){
      newTST=1
    }
    
    #print(newTST)
    if (simuData$confirmedBlock[i] == "True"){
      txSubmissionTimeConfirmed <- append(txSubmissionTimeConfirmed, newTST)
      
   }else{
    txSubmissionTime <- append(txSubmissionTime, newTST)
    }    
  }
}

txSubmissionTime2 <- unlist(txSubmissionTime, use.names = FALSE)
txSubmissionTimeConfirmed2 <- unlist(txSubmissionTimeConfirmed, use.names = FALSE)
##two individual HISTOGRAMS
hist(txSubmissionTime2, breaks=100)
hist(txSubmissionTimeConfirmed2, breaks=100)

##STACKED HISTOGRAM
bucket <-list(  Confirmed=txSubmissionTimeConfirmed2,UnConfirmed = txSubmissionTime2)
thruPlot<-ggplot(melt(bucket), aes(value, fill = L1))+ geom_histogram(position = position_stack(reverse=TRUE), binwidth=7)
thruPlot+ggtitle("Throughput of Transactions (Linear Blockchain, DC)")+xlab("Time to Submission")+ylab("Count") +  labs(fill = "Types of Txs") +  geom_vline(xintercept= mean(txSubmissionTimeConfirmed2),linetype="solid", color = "black", size=0.65)



# ##LOG PLOT
#txTimes <- data.frame(matrix(ncol=2,nrow=0))
#colnames(txTimes) <- c("time", "confirmed")
#for (i in 1:length(txSubmissionTime2)){
#  txTimes[nrow(txTimes) + 1,] = list(txSubmissionTime2[i], "Unconfirmed")
#}

#for (i in 1:length(txSubmissionTimeConfirmed2)){
#  txTimes[nrow(txTimes) + 1,] = list(txSubmissionTimeConfirmed2[i], "Confirmed")
#}

#thruPlot<-ggplot(melt(txTimes), aes(txTimes$time, fill = L1))+ geom_histogram(position = "stack", binwidth=3)
#LOG WOKRED



#dfr <- data.frame(x = rlnorm(length(bucket),sdlog = 5))
#logPlot <- ggplot(dfr,aes(bucket))+ geom_histogram() + scale_x_log10()
#logPlot


#boxplot/violin plot
library(vioplot)
vioplot(txSubmissionTime2, txSubmissionTimeConfirmed2, names=c("Unconfirmed Txs", "Confirmed Txs"), col="gold")
title("Violin plot of Txs")
ylab("Time")




###Plot Confirmation Time
library(plyr)
#h <- hist(simuData$timeToConfirm,breaks=50)
top = round_any( max(simuData$timeToConfirm,na.rm=TRUE),10)
h <- hist(
  simuData$timeToConfirm,
  breaks = seq(0, top+5 , 5),
  xlim = c(0,top))

par(new = T)
ec <- ecdf(simuData$timeToConfirm)

plot(h)
#plot(x = h$mids, y=ec(h$mids)*max(h$counts), col = rgb(0,0,0,alpha=0), axes=F, xlab=NA, ylab=NA)
lines(x = h$mids, y=ec(h$mids)*max(h$counts), col ='red', lwd=2)
axis(4, at=seq(from = 0, to = max(h$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'red', col.axis = 'red')
mtext(side = 4, line = 3, 'Cumulative Density', col = 'red')









##get tip time difference DOES NOT WORK CURRENTLY
simuData2=simuData
simuData2$timeDiff=99999
for (i in 2:nrow(simuData2)){
  #print("i Value")
  print(i)
  tips = simuData2$tips[i]

  gsubbby= gsub("\\[|\\]", "", tips$tips)
  tipList=as.numeric(strsplit(gsubbby, split=",", fixed=TRUE)[[1]])
  #print("TIpList: ")
  #print(tipList)
  minTime=99999
  for (tip in 1:length(tipList)){
    print("TIP: ")
    print(tipList[tip])
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











found=0

##find 6171 tx
for (i in 2:nrow(simuData)){
 txs = simuData$block_transactions[i]
 if (!is.null(txs)){
   if (nchar(txs)>5){
     if (grepl( "6171", txs, fixed=TRUE)){
       print(i)
      }
    }
  }
}

