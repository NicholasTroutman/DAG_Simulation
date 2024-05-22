##Nicholas Troutman
##Visualization of CSV SimuData

##\\wsl$\Ubuntu\home\ntroutm\git\DAG_Simulation\SimuData

library(ggplot2)
library(reshape2)


##upload csv as simuData

simuData=test2
#simuData=dhtTest



# Section PreProcesssing -------------
#get number of agents
numAgents=sum(grepl("agent_",names(simuData))) #grep "agent_" from headers of simuData
simuData$singleAgent = gsub("\\[|\\]", "", simuData$agent) #change agent owner to single and remove brakcets []
simuData$singleBlockLink = gsub("\\[|\\]", "", simuData$tips) #change agent owner to single and remove brakcets []\

simuData$transaction_creation_time = gsub("\\[|\\]", "", simuData$transaction_creation_time) #change agent owner to single and remove brakcets []\

simuData$timeToConfirm = simuData$confirmationTime-simuData$arrival_time

plot(simuData$ID, simuData$adoption_rate, col=simuData$singleAgent, pch=16)
#plot(simuDataOpt$txID, simuDataOpt$adoption_rate, col=simuData$agent, pch=16) #SaME


#plot(simuDataOpt$txID, simuDataOpt$count, col=simuData$agent, pch=16) #SAME


agentSeen = grepl("agent_", names(simuData)) #index for 
simuData$count = numAgents-rowSums(is.na(simuData[agentSeen])) #count how many found it
plot(simuData$ID, simuData$count, col=simuData$singleAgent, pch=16)
print(mean(simuData$count))
##time to share blocks:
agentSeen = grepl("agent_", names(simuData)) #index for 
agentSeendf = data.frame(simuData[agentSeen])[-1,]
agentSeendf = data.frame(simuData[agentSeen])[2:200,]
shareTimes = apply(agentSeendf, 1, FUN = max)  - apply(agentSeendf, 1, FUN = min)
print(max(shareTimes,na.rm=TRUE))
shareTimesMedian = apply(agentSeendf, 1, FUN = median)  - apply(agentSeendf, 1, FUN = min)
hist(shareTimes,200)
(sum(sTimes,na.rm=TRUE) + sum(is.na(sTimes))*max(shareTimes,na.rm=TRUE))/(199*100)


sTimes = apply(agentSeendf, 1, function(x){x-min(x, na.rm=TRUE)})
sTimes2=data.frame(x=unlist(sTimes))
hist(sTimes,1000,xlim=c(0,10000), main = "Network Gossip Times", xlab="Time to Reach New Agent since Creation")
#hist(sTimesBroke,60, main = "Network Gossip Times", xlab="Time to Reach New Agent since Creation")

simuData$maxShareTime = c(0,shareTimes)
d <- density(simuData$maxShareTime, na.rm=TRUE)
dmedian <- density(shareTimesMedian, na.rm=TRUE)
dx <- diff(d$x)[1]

h<-hist(shareTimes, breaks=50,main="Highway Full Network Penetration Time",col=rgb(0,0,1,0.5), xlab="Time to Penetrate ")
h<-hist(shareTimesMedian, breaks=20,main="Highway Half Network Penetration Time",col=rgb(1,0,0,0.5),add=T)
h<-hist(shareTimesMedian, breaks=50,main="Highway Half Network Penetration Time")
#overlay kernel density plot
#lines(x=d$x,y=max(h$counts)*d$y/dx)
lines(x=d$x,y=d$y*length(shareTimes)*(h$breaks[2] - h$breaks[1]))
#new plot
plot(d)
lines()
# Data
set.seed(5)

df <- data.frame(shareTimes)

# Histogram with kernel density
ggplot(df, aes(x = shareTimes)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)


##Compare two kernels
plot(dhwy, "Map Difference ~ Full Network Penetration Time",xlab="Time", col="blue", xlim=c(0,300))
lines(d,col="red")
legend(225,0.039, legend=c("DownTown", "HighWay"), col=c("red","blue"), lty=1, cex=1)




# Section PROPORTION OF CONFIRMED BLOCKS/Tot(blocks) ------
library( plyr )


file_list = list.files(pattern="*.csv")
#data_list <- vector("list", "length" = length(file_list))
blockTimeList <- data.frame(matrix(ncol = 2, nrow = length(file_list)))
colnames(blockTimeList) <- c("BlockTime", "ConfirmationPercentage")
currentBlockTime = 10

#loop through all files
for (i in seq_along(file_list)) {
  filename = file_list[[i]]
  print(filename)
  ## Read data in
  df1 <- read.csv(filename, header = TRUE)
  
  ## Extract year from filename
  #year = gsub("yob", "", filename)
  #df[["Filename"]] = year
  df = head(df1,-10)
  percentConfirmedBlocks = sum(df$confirmedBlock=="True")/nrow(df)
  #print(percentConfirmedBlocks)
  ## Add year to data_list
  blockTimeList$ConfirmationPercentage[i] = percentConfirmedBlocks
  blockTimeList$BlockTime[i] = currentBlockTime
  currentBlockTime = currentBlockTime+5
}


plot(blockTimeList$BlockTime,blockTimeList$ConfirmationPercentage)
plot(blockTimeList$BlockTime,1-blockTimeList$ConfirmationPercentage, main="Block Orphanage Rate Vs. Mean Block Time", xlab="Mean Block Time (Exponential Distribution)", ylab="Orphanage Rate", ylim=c(0,1), pch=19)






# Section Time to confirm each TX (1 data frame) ---------
txConfirmationTime =  c()
##add to listblockConfirmationTime-txCreationTime
confirmedBlocks = simuData[simuData$confirmedBlock=="True",] #Sometimes "TRUE"
for (i in 2:nrow(confirmedBlocks)){
  #print(confirmedBlocks$ID[i])
  tx_creation_time = strsplit(confirmedBlocks$transaction_creation_time[i], ",")[[1]] #returns 1, 2 3
  #print(tx_creation_time)
  ###tx_creation_time is list of strings
  for (tct in tx_creation_time){
    tct = gsub("[^0-9.-]", "", tct)
    #print(strtoi(tct))
    txConfirmationTime = append( txConfirmationTime, strtoi(confirmedBlocks$confirmationTime[i]) - strtoi(tct) )
    #txConfirmationTime = c( txConfirmationTime, confirmedBlocks$confirmationTime - strtoi(tct) )
  } 
}


#plot h50
#h<-hist(txConfirmationTime, breaks=100, main="50 tx/Block")
plot(h50)
d <- density(txConfirmationTime, na.rm=TRUE)
lines(x=d$x,y=d$y*length(txConfirmationTime)*(h50$breaks[2] - h$breaks[1]), col="blue", lwd=2)

##plot h200
#h<-hist(txConfirmationTime, breaks=100, main="50 tx/Block")
plot(h50)
d <- density(txConfirmationTime, na.rm=TRUE)
lines(x=d$x,y=d$y*length(txConfirmationTime)*(h$breaks[2] - h$breaks[1]), col="blue", lwd=2)









# Section Compare Network Penetration w/ different Agent #s ------

file_list = list.files(pattern="*.csv")
#data_list <- vector("list", "length" = length(file_list))
numAgentsData <- data.frame(matrix(ncol = 7, nrow = length(file_list)))
colnames(numAgentsData) <- c("numAgents", "distributionX","distributionY", "meanMaximumPenetrationTime", "times", "expectedConfirmation", "orphanageRate")
currentNumAgents = 10

#loop through all files
for (i in seq_along(file_list)) {
  filename = file_list[[i]]
  print(filename)
  ## Read data in
  df <- read.csv(filename, header = TRUE)
  
  
  #Get data
  agentSeen = grepl("agent_", names(df)) #index for 
  agentSeendf = data.frame(df[agentSeen])[-1,]
  shareTimes = apply(agentSeendf, 1, FUN = max)  - apply(agentSeendf, 1, FUN = min)
  d <- density(shareTimes, na.rm=TRUE, from=0, to = 200 )
  #d <- density(shareTimes, na.rm=TRUE )
  
  ##save data
  numAgentsData$numAgents[i] = currentNumAgents
  numAgentsData$distributionX[i] = list(d$x)
  numAgentsData$distributionY[i] = list(d$y)
  numAgentsData$meanMaximumPenetrationTime[i] = d$x[match(max(d$y),d$y)]
  numAgentsData$times[i] = list(shareTimes)
  
  
  ##Get TX Confirmation TIme data
  confirmedBlocks = df[df$confirmedBlock=="True",]
  for (j in 2:nrow(confirmedBlocks)){
    tx_creation_time = strsplit(confirmedBlocks$transaction_creation_time[j], ",")[[1]] #returns 1, 2 3
    #print(tx_creation_time)
    ###tx_creation_time is list of strings
    for (tct in tx_creation_time){
      #print(tct)
      tct = gsub("[^0-9.-]", "", tct)
      txConfirmationTime = append( txConfirmationTime, strtoi(confirmedBlocks$confirmationTime[j]) - strtoi(tct) )
      #txSubmissionTime =   append(txSubmissionTime,    strtoi(confirmedBlocks$arrival_time[j])    - strtoi(tct))
    } 
  }
  
  
  
  #Get Expected Confirmation Time
  numAgentsData$expectedConfirmation[i] = 0
  meanTime = mean(txConfirmationTime, na.rm=TRUE)
  orphanRate = sum(df$confirmedBlock=="False")/nrow(df)
  numAgentsData$orphanageRate[i]=orphanRate
  for (n in 1:50){
    temp= sum( (meanTime+ reSubmitTime*(n-1)) * (1-orphanRate)*(orphanRate**(n-1) ))
    #print(temp)
    numAgentsData$expectedConfirmation[i] = numAgentsData$expectedConfirmation[i] + temp
  }
  
  
  currentNumAgents = currentNumAgents +5
  if (i==1){
    plot(d,ylim=c(0,0.05))
  }
  else{
    lines(d,add=TRUE)
  }
}





###heatmap
library(ggplot2)
library(hrbrthemes)



## real data
x <- d$x
y <- c(10, 15 ,20,25,30,35,40,45,50,55,60,100)
data <- expand.grid(X=x, Y=y)
data$Z <- c(numAgentsData$distributionY[[1]],  numAgentsData$distributionY[[2]], numAgentsData$distributionY[[3]], numAgentsData$distributionY[[4]], numAgentsData$distributionY[[5]], numAgentsData$distributionY[[6]], numAgentsData$distributionY[[7]],  numAgentsData$distributionY[[8]],  numAgentsData$distributionY[[9]],  numAgentsData$distributionY[[10]],  numAgentsData$distributionY[[11]], numAgentsData$distributionY[[12]])

# Heatmap 
ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile() + scale_fill_gradient(low="white",high="red")





### Network Penetration
boxplot(numAgentsData$times, names = numAgentsData$numAgents, main="Network Penetration ~ Number of Agents", horizontal=TRUE, ylab="Number of Agents", xlab="Maximum Network Penetration Time")
library(vioplot)
vioplot(numAgentsData$times, names = numAgentsData$numAgents, main="Network Penetration ~ Number of Agents", horizontal=TRUE, ylab="Number of Agents", xlab="Maximum Network Penetration Time")

##Tx Confirmation Time
ggplot(numAgentsData, aes( y=expectedConfirmation, x=numAgents)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("BlockChain Expected Confirmation Time ~ Number of Agents") +
  theme(plot.title = element_text(size=30))



# Section DAG References VS txConfirmationTime VS submission Time ----------

file_list = list.files(pattern="*.csv")
#data_list <- vector("list", "length" = length(file_list))
refData <- data.frame(matrix(ncol = 4, nrow = length(file_list)))
colnames(refData) <- c("references", "cTime", "orphanRate", "expectedConfirmation")
numReferences = 1
#######Time to confirm each TX:


#loop through all files
for (i in seq_along(file_list)) {
  txConfirmationTime =  c()
  #txSubmissionTime = c()
  filename = file_list[[i]]
  print(filename)
  ## Read data in
  df <- read.csv(filename, header = TRUE)
  df = head(df,-1)
  confirmedBlocks = df[df$confirmedBlock=="True",]
  
  
  ##Get Data
  for (j in 2:nrow(confirmedBlocks)){
    tx_creation_time = strsplit(confirmedBlocks$transaction_creation_time[j], ",")[[1]] #returns 1, 2 3
    #print(tx_creation_time)
    ###tx_creation_time is list of strings
    for (tct in tx_creation_time){
      tct = gsub("[^0-9.-]", "", tct)
      #print(tct)
      txConfirmationTime = append( txConfirmationTime, strtoi(confirmedBlocks$confirmationTime[j]) - strtoi(tct) )
      #txSubmissionTime =   append(txSubmissionTime,    strtoi(confirmedBlocks$arrival_time[j])    - strtoi(tct))
    } 
  }
  
  
  ##save data
  refData$references[i] = numReferences
  refData$cTime[i] = list(na.omit(txConfirmationTime))
  refData$orphanRate[i] = 1-(nrow(confirmedBlocks)/nrow(df))
  meanTime =mean(txConfirmationTime, na.rm=TRUE)
  reSubmitTime = mean(txConfirmationTime, na.rm=TRUE) + sd(txConfirmationTime, na.rm=TRUE)*2
  
  #get expected Confirmation Time
  refData$expectedConfirmation[i] = 0
  for (n in 1:50){
    temp= sum( (meanTime+ reSubmitTime*(n-1)) * (1-refData$orphanRate[i])*(refData$orphanRate[i]**(n-1) ))
    
    #print(temp)
    refData$expectedConfirmation[i] = refData$expectedConfirmation[i] + temp
     }
    
  numReferences = numReferences + 1
}

##Boxplots

##confirmation Time
boxplot(refData$cTime, names = refData$references, main="Reference Tx Confirmation Time (PoL)", horizontal=TRUE, ylab="Number of References", xlab="Transaction Confirmation Time")
text(x=fivenum(refData$cTime), labels=fivenum(refData$cTime), y=1.25)
library(vioplot)
vioplot(refData$cTime, names = refData$references, main="Reference Tx Confirmation Time", horizontal=TRUE, ylab="Number of References", xlab="Transaction Confirmation Time")

##Submission Time
boxplot(refData$sTime, names = refData$references, main="Reference Tx Submission Time", horizontal=TRUE, ylab="Number of References", xlab="Transaction Submission Time")
library(vioplot)
vioplot(refData$sTime, names = refData$references, main="Reference Tx Submission Time", horizontal=TRUE, ylab="Number of References", xlab="Transaction Submission Time")










# Section time to share blocks: ---------------
agentSeendf = data.frame(simuData[agentSeen])[-1,]
shareTimes = apply(agentSeendf, 1, FUN = max)  - apply(agentSeendf, 1, FUN = min)
shareTimesMedian = apply(agentSeendf, 1, FUN = median)  - apply(agentSeendf, 1, FUN = min)

simuData$maxShareTime = c(0,shareTimes)
d <- density(simuData$maxShareTime, na.rm=TRUE)
dmedian <- density(shareTimesMedian, na.rm=TRUE)
dx <- diff(d$x)[1]

h<-hist(shareTimes, breaks=30)

#overlay kernel density plot
#lines(x=d$x,y=max(h$counts)*d$y/dx)
lines(x=d$x,y=d$y*length(shareTimes)*(h$breaks[2] - h$breaks[1]))







# Section Proportion of confirmed Txs ------------
unconfirmedBlocks = simuData[simuData$confirmedBlock=="False",]
numUncomfirmedTxs = 0
for (i in 1:nrow(unconfirmedBlocks)){
  tx_creation_time = strsplit(unconfirmedBlocks$transaction_creation_time[i], ",")[[1]] #returns 1, 2 3
  for (tct in tx_creation_time){
    numUncomfirmedTxs = numUncomfirmedTxs +1
    } 
  }
print(numUncomfirmedTxs) #num of unconfirmed Txs, divide by #stuff


# Section et Agent/ID col=confirmed ------------
plot(simuData$ID,simuData$singleAgent,col = ifelse(simuData$confirmedBlock =="False",'red','green'), pch = 16 )
for (i in 2:nrow(simuData)){
 # print(i)
  linkedBlock = simuData$singleBlockLink[i]
  linkedBlock = strtoi(linkedBlock)+1
  #print(linkedBlock)
  
 segments(x0=strtoi(simuData$ID[linkedBlock]), y0= strtoi(simuData$singleAgent[linkedBlock]), x1 = strtoi(simuData$ID[i]), y1 = strtoi(simuData$singleAgent[i]), col="black" )
   
}
points(simuData$ID,simuData$singleAgent,col = ifelse(simuData$confirmedBlock =="False",'red','green'), pch = 19, cex=1.9 ) 



# Section Transaction Submission Time -------

##Histogram of transaction submission time
#for each row, get difference between simuData$arrival_time - simuData$transaction_creation_time
txSubmissionTimeConfirmed = list()
txSubmissionTime = list()
#for (i in 39:39){
for (i in 2:nrow(simuData)){
  #print(i)
    tx_creation_time = strsplit(simuData$transaction_creation_time[i], ",")[[1]] #returns 1, 2 3
  for (tct in tx_creation_time){
    tct = gsub("[^0-9.-]", "", tct)
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




#boxplot/violin plot
library(vioplot)
vioplot(txSubmissionTime2, txSubmissionTimeConfirmed2, names=c("Unconfirmed Txs", "Confirmed Txs"), col="gold")
title("Violin plot of Txs")
ylab("Time")








# Section Find blocks Including a specific TX ------

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


# Section grouped Bargraphs ----------


##grouped barGraph
library(ggplot2)
#mock Data
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)


#real data
links <- unlist(rep(list("1","2","3","4","5"),3))
consensus <- c(rep("30sec", 5), rep("10sec", 5), rep("PoL", 5))
orphanRate <- c( c(0.614, 0.0715, 0, 0, 0), c(0.36, 0.195, 0.00025, 0,0), c(0.36, 0, 0, 0, 0) )
eConf <- c( c(1074, 855, 608, 601, 598), c(1075, 543, 543, 505, 498 ), c(221, 161, 161, 161, 161))
dagData <- data.frame(links, consensus, orphanRate, eConf)

ggplot(dagData, aes( y=eConf, x=links, fill=consensus)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("DAG Expected Confirmation Time") +
  theme(plot.title = element_text(size=30))





# Section Get Penetration Distribution sfrom folder ----------


file_list = list.files(pattern="*.csv")
#data_list <- vector("list", "length" = length(file_list))
#refData <- data.frame(matrix(ncol = 4, nrow = length(file_list)))
refData <- data.frame(matrix(ncol = 10 , nrow = length(file_list)))
#colnames(refData) <- c("blockTime", "cTime", "orphanRate", "expectedConfirmation")
colnames(refData) <- c("blockTime", "numAgents","maps","dlt", "refs", "cTime", "orphanRate", "meanPenTime", "penTimes", "group")
#######Time to confirm each TX:


#loop through all files
for (i in seq_along(file_list)) {
#for (i in 1) {
  econfList = c()
  txConfirmationTime =  c()
  #txSubmissionTime = c()
  filename = file_list[[i]]
  print(paste(filename," ~ ",i/length(file_list)))
  ## Read data in
  df <- read.csv(filename, header = TRUE)
  df = head(df,-50)
  
  agentSeen = grepl("agent_", names(df)) #index for 
  agentSeendf = data.frame(df[agentSeen])[-1,]
  shareTimes = apply(agentSeendf, 1, FUN = max)  - apply(agentSeendf, 1, FUN = min)
  confirmedBlocks = df[df$confirmedBlock=="True",]
  
  
  ##Get Data
  for (j in 2:nrow(confirmedBlocks)){
    tx_creation_time = strsplit(confirmedBlocks$transaction_creation_time[j], ",")[[1]] #returns 1, 2 3
    #print(tx_creation_time)
    ###tx_creation_time is list of strings
    for (tct in tx_creation_time){ #tct = Transaction creation time
      tct = gsub("[^0-9.-]", "", tct) 
      #print(tct)
      #print(strtoi(confirmedBlocks$confirmationTime[j]) - strtoi(tct) )
      txConfirmationTime = append( txConfirmationTime, strtoi(confirmedBlocks$confirmationTime[j]) - strtoi(tct) )
      #txSubmissionTime =   append(txSubmissionTime,    strtoi(confirmedBlocks$arrival_time[j])    - strtoi(tct))
    } 
  }
  
  
  ##save data
  #refData$blockTime[i] = as.numeric(strsplit(gsub("[^0-9.-]", " ", file_list[[i]]), " +")[[1]][5])
  refData$cTime[i] = list(na.omit(txConfirmationTime))
  refData$orphanRate[i] = 1-((nrow(confirmedBlocks)+1)/nrow(df))
  refData$numAgents[i] =   as.numeric(strsplit(gsub("[^0-9.-]", " ", file_list[[i]]), " +")[[1]][3]) ##numAgents
  refData$dlt[i] =  strsplit(gsub("_", " ",filename), " +")[[1]][2]
  refData$maps[i] = strsplit(gsub("_", " ",filename), " +")[[1]][13]
  refData$refs[i] = as.numeric(substr(strsplit(gsub("_", " ",filename), " +")[[1]][17], 1, 1))
  refData$group[i] = as.numeric(substr(strsplit(gsub("_", " ",filename), " +")[[1]][19], 1, 1))
  refData$meanPenTime[i] = mean(shareTimes, na.rm=TRUE)
  refData$penTimes[i] = list(na.omit(shareTimes))
  
  meanTime =mean(txConfirmationTime, na.rm=TRUE)
  reSubmitTime = meanTime + sd(txConfirmationTime, na.rm=TRUE)*2
  #get expected Confirmation Time
  #refData$expectedConfirmation[i] = meanTime
  refData$expectedConfirmation[i] = 0
  for (n in 1:20){
    temp= sum( (meanTime+ reSubmitTime*(n-1)) * (1-refData$orphanRate[i])*(refData$orphanRate[i]**(n-1) ))
    econfList = append(econfList,temp)
    refData$expectedConfirmation[i] = refData$expectedConfirmation[i] + temp
  }
  
  plot(seq_along(econfList), econfList, type="l", main=paste("BlockTime: ",refData$blockTime[i], " orphanRate: ",refData$orphanRate[i]))
  
}
refData$color="darkred"

for (i in 1:nrow(refData)){
 # print(i)
  
  
  print(refData$group[i])
  
  if (refData$group[i]==1){
    refData$color[i]="darkred"
  }
  else if (refData$group[i]==3){
    refData$color[i]="darkgreen"
  }
  else if (refData$group[i]==4){
    refData$color[i]="darkblue"
  }
  else if (refData$group[i]==5){
    refData$color[i]="darkorange"
  }
}

refDatatot=refData
refDataDownTown=refData[refData$maps=="Houstonredblue",]
refDataHwy = refData[refData$maps=="HoustonHwyredblue",]


##SPlit by refs
refDataDownTown2refs = refDataDownTown[refDataDownTown$refs==2,]
refDataDownTown3refs = refDataDownTown[refDataDownTown$refs==3,]

library(ggrepel)
library(ggplot2)
ggplot(refDataDownTown2refs, aes( y=expectedConfirmation, x=numAgents, group=group, col=group, label=group), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  geom_label_repel(aes(label = group),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')+
  ggtitle(paste("Downtown 2 Refs, Minting Time ~ ")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  scale_color_gradient(low="blue", high="red") + 
  ylim(0,200)+
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.8)

ggplot(refDataDownTown2refs, aes( y=expectedConfirmation, x=numAgents, group=group, col=group, label=group), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  geom_label_repel(aes(label = group),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')+
  ggtitle(paste("Downtown 2 Refs, Minting Time ~ ")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Number of Agents") + ylab("Expected Transaction Confirmation Time") +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  scale_color_gradient(low="blue", high="red") + 
  ylim(0,200)



#INDIVIDUAL WORKS
ggplot(refDataDownTown2refs, aes( y=expectedConfirmation, x=numAgents, group=group, col=group, label=group), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste("Downtown 2 Refs, Minting Time ~ ")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Number of Agents") + ylab("Expected Transaction Confirmation Time") +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  scale_color_gradient(low="blue", high="red") + 
  ylim(0,200)+
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.7) + 
  geom_label_repel(data=refDataDownTown3refs[c(1:3,10,11,12,13,16),], aes(label = group),box.padding   = 0.4, point.padding = 0.4, label.size=0.75,direction=("y"),nudge_y=10)+
  scale_x_continuous(breaks = round(seq(25, 200, by = 25),1))
 


ggplot(refDataDownTown3refs, aes( y=expectedConfirmation, x=numAgents, group=group, col=group, label=group), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste("Downtown 3 Refs, Minting Time ~ ")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Number of Agents") + ylab("Expected Transaction Confirmation Time") +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  scale_color_gradient(low="blue", high="red") + 
  ylim(0,200)+
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.7 ,linetype="solid") + 
  geom_label_repel(data=refDataDownTown3refs[c(1:3,10,11,12,13,16),], aes(label = group),box.padding   = 0.4, point.padding = 0.4, label.size=0.75,direction=("y"),nudge_y=10)+
  scale_x_continuous(breaks = round(seq(25, 200, by = 25),1)) 

##combiNED
ggplot() + 
  geom_point(data = refDataDownTown2refs,aes( y=expectedConfirmation, x=numAgents, group=group, col=group), pch = 5,  position="dodge", stat="identity") +
  geom_smooth(data= refDataDownTown2refs, se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.7 ,aes( y=expectedConfirmation, x=numAgents, group=group, col=group,linetype='2'), size=0.9) +
  geom_point(data = refDataDownTown3refs,aes( y=expectedConfirmation, x=numAgents, group=group, col=group), pch = 19,  position="dodge", stat="identity") + 
  geom_smooth(data= refDataDownTown3refs, se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.7,aes( y=expectedConfirmation, x=numAgents, group=group, col=group,linetype='3'), size=1.6)+
  ggtitle(paste("Downtown DAG PoL ~ ")) +
  labs(x="Number of Agents", y="Expected Transaction Confirmation", col="Groups",linetype="# of References")+  #xlab("Number of Agents") + ylab("Expected Transaction Confirmation Time")+
  scale_color_gradient(low="blue", high="red") +
  scale_linetype_manual(values=c('2'='dashed','3'='solid')) + 
  ylim(0,175) + 
  geom_label_repel(data=refDataDownTown3refs[c(1:3,10,11,12,13,16),], aes( y=expectedConfirmation, x=numAgents, group=group, col=group, label=group),box.padding   = 0.4, point.padding = 0.4, label.size=0.75,direction=("y"),nudge_y=10) + 
  scale_x_continuous(breaks = c(25,50,100,150,200))+ 
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9), axis.line = element_line(colour = "black"), legend.title=element_text(face = "bold"),legend.key.width = unit(2,"cm"))

#Linear Near Plot
ggplot(refData, aes( y=expectedConfirmation, x=numAgents, group=group, col=group, label=group), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste("Downtown 3 Refs, Minting Time ~ ")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Number of Agents") + ylab("Expected Transaction Confirmation Time") +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  scale_color_gradient(low="blue", high="red") + 
  ylim(0,200)+
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.7 ,linetype="solid") + 
  geom_label_repel(data=refData[c(1:3,10,11,12,13,16),], aes(label = group),box.padding   = 0.4, point.padding = 0.4, label.size=0.75,direction=("y"),nudge_y=10)+
  scale_x_continuous(breaks = round(seq(25, 200, by = 25),1)) 



#theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) +
#geom_text(hjust=1,vjust=1) +
#geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.5) +
#scale_color_manual(name="Group",values=c('darkred','darkgreen','darkblue'), labels=c("25","50","100")) +

##density plots
d25 =  density(refDataDownTown$penTimes[[1]])
d50 =  density(refDataDownTown$penTimes[[2]])
d100 = density(refDataDownTown$penTimes[[3]])

plot(d100, xlim=c(0,500))
lines(d50)
lines(d25)

##Both hwy + Downtown
#refDataHwy


library(ggplot2)

##plot expectedConfirmation Time
refData=refDataHwy
refData=refDataDownTown
ggplot(refData, aes( y=expectedConfirmation, x=blockTime,group=numAgents,col = ifelse(numAgents < 50,'red',ifelse(numAgents>50,'blue','green'))), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste(refData$maps[1]," Minting Time ~ Expected Transaction Confirmation Time")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.5) +
  scale_color_manual(name="Number of Agents",values=c('darkred','darkgreen','darkblue'), labels=c("25","50","100")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 



##Plot ExpectedConfirmation Time by refs
#refData=refData[refD]

refData=refData[!refData$refs==4,]
refData25=refData[refData$numAgents==25,]
refData50=refData[refData$numAgents==50,]
refData100=refData[refData$numAgents==100,]

min(refData25$expectedConfirmation)
min(refData50$expectedConfirmation)
min(refData100$expectedConfirmation)

refData2 =refDatatot[refDatatot$refs==2,]
refData3 =refDatatot[refDatatot$refs==3,]
refData4 =refDatatot[refDatatot$refs==4,]
refData5 =refDatatot[refDatatot$refs==5,]
refData6 =refDatatot[refDatatot$refs==6,]
refData7 =refDatatot[refDatatot$refs==7,]
#refData3s = rbind(refData25,refData50,refData100)
library(ggplot2)




ggplot(refData25, aes( y=expectedConfirmation, x=numAgents,group=refs, col = ifelse(refs == 2,'red','blue')), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste("Downtown DAG 25 Agents, Minting Time ~ Expected Transaction Confirmation Time")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.35) +
  scale_color_manual(name="Number of Refs",values=c('darkred','darkblue'), labels=c("3","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime))))+ 
  ylim(0,50)


ggplot(refData50, aes( y=expectedConfirmation, x=blockTime,group=refs,col = ifelse(refs == 2,'red','blue')), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste("Downtown DAG 50 Agents, Minting Time ~ Expected Transaction Confirmation Time")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab(" 50 Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.35) +
  scale_color_manual(name="Number of Refs",values=c('darkred','darkblue'), labels=c("3","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime))))+ 
  ylim(0,550)


ggplot(refData100, aes( y=expectedConfirmation, x=blockTime,group=refs,col = ifelse(refs == 2,'red','blue')), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste("Downtown DAG 100 Agents, Minting Time ~ Expected Transaction Confirmation Time")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.4) +
  scale_color_manual(name="Number of Refs",values=c('darkred','darkblue'), labels=c("3","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  ylim(0,550)


ggplot(refData, aes( y=expectedConfirmation, x=blockTime,group=refs,col = ifelse(refs == 2,'red','blue')), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste("Downtown DAG 200 Agents, Minting Time ~ ")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.2) +
  scale_color_manual(name="Number of Refs",values=c('darkred','darkblue'), labels=c("3","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  ylim(0,1000)


##Plot 2/3/4 attachment time

ggplot(refDataHwy, aes( y=expectedConfirmation, x=blockTime, group=refs, col = ifelse(refs == 2, ifelse(refs == 3, 'green','blue'),'red')), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste("Downtown DAG 200 Agents, Minting Time ~ ")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  scale_color_manual(name="Number of Refs",values=c('darkred','darkblue'), labels=c("4","3","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  ylim(0,250)
##geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.2) +


refDataHwy50=refDataHwy[refDataHwy$numAgents==50,]
refDataHwy100=refDataHwy[refDataHwy$numAgents==100,]


ggplot(refDataHwy50, aes( y=expectedConfirmation, x=blockTime,group=refs, col = ifelse(refs == 2,'red',ifelse(refs==3,'blue','green'))), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle("50 AGents Minting Time ~ Expected Transaction Confirmation Time") +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.5) +
  scale_color_manual(name="Number of Agents",values=c('darkred','darkgreen','darkblue'), labels=c("4","3","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime))))



ggplot(refDataHwy100, aes( y=expectedConfirmation, x=blockTime,group=refs, col = ifelse(refs == 2,'red',ifelse(refs==3,'blue','green'))), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle("100 AGents Minting Time ~ Expected Transaction Confirmation Time") +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.5) +
  scale_color_manual(name="Number of Agents",values=c('darkred','darkgreen','darkblue'), labels=c("4","3","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime))))


refDataDownTown50=refDataDownTown[refDataDownTown$numAgents==50,]
refDataDownTown100=refDataDownTown[refDataDownTown$numAgents==100,]

ggplot(refDataDownTown50, aes( y=expectedConfirmation, x=blockTime,group=refs, col = ifelse(refs == 2,'red',ifelse(refs==3,'blue','green'))), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle("50 Agents Minting Time ~ Time to Reference") +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.4)) + 
  xlab("Block Minting Time") + ylab("Expected Time to Reference (with resubmission)") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.5) +
  scale_color_manual(name="Number of References",values=c('darkred','darkgreen','darkblue'), labels=c("3","4","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime))))



ggplot(refDataDownTown100, aes( y=expectedConfirmation, x=blockTime,group=refs, col = ifelse(refs == 2,'red',ifelse(refs==3,'blue','green'))), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle("100 AGents Minting Time ~ Expected Transaction Confirmation Time") +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.5) +
  scale_color_manual(name="Number of References",values=c('darkred','darkgreen','darkblue'), labels=c("3","4","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime))))



# Section Attachment Time of Folder


# Section Get PenChart, Confirmation Chart % Penetration


## Get ------------


# Create List: row=chainNUm={blocks in that layer}


# Check 10 layers if they 

file_list = list.files(pattern="*.csv")
penChart <- data.frame(matrix(ncol = 8 , nrow = length(file_list)))

#data_list <- vector("list", "length" = length(file_list))
#refData <- data.frame(matrix(ncol = 4, nrow = length(file_list)))
#refData <- data.frame(matrix(ncol = 10 , nrow = length(file_list)))
#colnames(refData) <- c("blockTime", "cTime", "orphanRate", "expectedConfirmation")
colnames(penChart) <- c("blockTime", "numAgents","maps","dlt", "refs", "confirmationNumber", "consensus", "group")
#######Time to confirm each TX:
library(stringr)

#loop through all files
for (i in seq_along(file_list)) {
  
  #for (i in 1) {
  
  ##Step 1. Create Layer DataFrame
  #txSubmissionTime = c()
  filename = file_list[[i]]
  print(paste(filename," ~ ",i/length(file_list)))
  ## Read data in
  df <- read.csv(filename, header = TRUE)
  df = head(df,-25)
  
  layers = data.frame(matrix(ncol = 1, nrow = df$chainNum[nrow(df)]+10)) #dataframe made
  colnames(layers)<-"layer"
  ##Step 1.a, Fill Layer DataFrame
  print("    Creating Layer Dataframe")
  for (j in 2:nrow(df)){
    #if (j%%1000==0)
    #{
     # print(j/nrow(df))
    #}
  chainNum =df[j,]$chainNum   #ChainNum
    if (is.na(layers$layer[chainNum])){
      layers$layer[chainNum]=list(df[j,]$ID)
    }
    else{
      
      layers$layer[chainNum][[1]] <- append(layers$layer[chainNum][[1]],  df[j,]$ID)
    }
  }
  
  
  ##Step 2. Find Longest Chain for each block
  #For 10 blocks,
  print("    tFinding Penetration")
  totPenetration=c()
  #for each layer Row:
  for(k in 1:(nrow(layers)-15)){
    #if (k %% 400 == 0){
      #print(k/(nrow(layers)-15))
    #}
    #print(paste("Layer:",k))
    #currentLayer to reference easily
    currentLayer2 = layers$layer[k][[1]]
    
    
    for (currentLayer in currentLayer2){
      penetration=c()
      #go 10 rows in the future
      for (kk in 1:15){
        #print(paste( " Current layer",currentLayer))
        nextLayer=c()
        
        #for each element in future list
        for (kkk in layers$layer[k+kk][[1]]){
        #print(paste(" Check this Block:",kkk))
          tip = df$tips[kkk+1] #get tip
          tip = str_sub(tip,2,-2) #remove '[]' 
          tip = as.numeric(tip)
          #print(paste("TIP: ",tip))
            if (tip %in% currentLayer){
            #add to penetration
            penetration = append(penetration, kk)
            #append to next Layer
            nextLayer = append(nextLayer,kkk) 
            #print(nextLayer)
            #print(penetration)
            
            }
          else{
            next
          }
        }
        currentLayer=nextLayer #increment currentLayer for next thingamabob
        
        
      }
      ##add penetration to totPenetration
      penetration=unique(penetration)
      totPenetration = append(totPenetration, penetration)
    }
    
  }
  
  #plot and find confirmation number
  h= hist(totPenetration, labels=TRUE, breaks=c(0:15))
  
  lastCount = h$counts[ length(h$counts)]
  cutoff = lastCount/0.95

  colors = h$counts>cutoff
  numColors = sum(colors)
  confirmationNumber = numColors
  colors[1:numColors] = "gray"
  colors[(numColors+1):length(colors)]="red"
  hist(totPenetration, main=paste("Confirmation Number: ",confirmationNumber), labels=TRUE, breaks=c(0:15), col=colors)
  
  ##save data
  penChart$blockTime[i] = as.numeric(strsplit(gsub("[^0-9.-]", " ", file_list[[i]]), " +")[[1]][5])
  penChart$numAgents[i] =   as.numeric(strsplit(gsub("[^0-9.-]", " ", file_list[[i]]), " +")[[1]][3]) ##numAgents
  penChart$maps[i] = strsplit(gsub("_", " ",filename), " +")[[1]][13]
  penChart$dlt[i] =  strsplit(gsub("_", " ",filename), " +")[[1]][2]
  penChart$refs[i] = as.numeric(substr(strsplit(gsub("_", " ",filename), " +")[[1]][17], 1, 1))
  penChart$confirmationNumber[i] = confirmationNumber
  penChart$consensus[i] = strsplit(gsub("_", " ",filename), " +")[[1]][3]
  penChart$group[i] = as.numeric(substr(strsplit(gsub("_", " ",filename), " +")[[1]][19], 1, 1))
  
  
  
  
  } 









# Section Storage ----------


storage=v50
storage=v150
#plot transactions
library(ggplot2)
#txs
p = ggplot() + 
  geom_line(data = storage, aes(x = time, y = maxNumTxs), color = "blue") +
  geom_line(data = storage, aes(x = time, y = numTxs), color = "red") +
  xlab('Time') +
  ylab('Number') + 
  ggtitle("100 Tx Storage") + 
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.4))
  

print(p)

#blocks

p = ggplot() + 
  geom_line(data = storage, aes(x = time, y = maxNumBlocks), color = "blue") +
  geom_line(data = storage, aes(x = time, y = numBlocks), color = "red") +
  xlab('Time') +
  ylab('Number') + 
  ggtitle('100 Block Storage')
  

print(p)

storage=d500

#plot transactions
library(ggplot2)
#txs
p = ggplot() + 
  geom_line(data = storage, aes(x = time, y = maxNumTxs), color = "blue") +
  geom_line(data = storage, aes(x = time, y = numTxs), color = "red") +
  xlab('Time') +
  ylab('Number') + 
  ggtitle("500 Tx Storage") + 
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.4))


print(p)

#blocks

p = ggplot() + 
  geom_line(data = storage, aes(x = time, y = maxNumBlocks), color = "blue") +
  geom_line(data = storage, aes(x = time, y = numBlocks), color = "red") +
  xlab('Time') +
  ylab('Number') + 
  ggtitle('500 Block Storage')


print(p)



##100
storage=d1000

p = ggplot() + 
  geom_line(data = storage, aes(x = time, y = maxNumTxs), color = "blue") +
  geom_line(data = storage, aes(x = time, y = numTxs), color = "red") +
  xlab('Time') +
  ylab('Number') + 
  ggtitle("1000 Tx Storage") + 
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.4))


print(p)

#blocks

p = ggplot() + 
  geom_line(data = storage, aes(x = time, y = maxNumBlocks), color = "blue") +
  geom_line(data = storage, aes(x = time, y = numBlocks), color = "red") +
  xlab('Time') +
  ylab('Number') + 
  ggtitle('1000 Block Storage')


print(p)

storage=d2000

p = ggplot() + 
  geom_line(data = storage, aes(x = time, y = maxNumTxs, color = "Seen")) +
  geom_line(data = storage, aes(x = time, y = numTxs, color = "Currently Stored")) +
  xlab('Time') +
  ylab('Number') + 
  ggtitle("2000 Keep Time ~ Tx Storage") + 
  labs(colour="Txs:")+
  scale_color_manual(values=c("red", "blue"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.4))
print(p)

#blocks
p = ggplot() + 
  geom_line(data = storage, aes(x = time, y = maxNumBlocks, color = "Seen")) +
  geom_line(data = storage, aes(x = time, y = numBlocks, color = "Currently Stored")) +
  xlab('Time') +
  ylab('Number') +
  labs(colour="Blocks:")+
  scale_color_manual(values=c("red", "blue"))+
  ggtitle('2000 Keep Time ~ Block Storage')
print(p)


##COMPARISON
p = ggplot() + 
  geom_line(data = d50, aes(x = time, y = numBlocks, color = "50")) +
  #geom_line(data = d100, aes(x = time, y = numTxs, color = factor(100))) +
  geom_line(data = d500, aes(x = time, y = numBlocks, color = "500")) +
  geom_line(data = d1000, aes(x = time, y = numBlocks, color = "1000")) +
  geom_line(data = d2000, aes(x = time, y = numBlocks, color = "2000")) + 
  xlab('Time') +
  ylab('Number') + 
  scale_colour_manual(values = c( "blue", "black", "red", "green")) +#scale_color_gradientn(colors=c('red','black','blue')) +
  scale_fill_discrete(breaks=c("50","500", "1000", "2000")) +
  labs(colour="Keep Time")+
  theme(legend.position = c(0, 1),legend.justification = c(-0.15, 1.1))+
  ggtitle('Block Storage Comparison')
print(p)

p = ggplot() + 
  geom_line(data = d50, aes(x = time, y = numTxs, color = "50")) +
  #geom_line(data = d100, aes(x = time, y = numTxs, color = factor(100))) +
  geom_line(data = d500, aes(x = time, y = numTxs, color = "500")) +
  geom_line(data = d1000, aes(x = time, y = numTxs, color = "1000")) +
  geom_line(data = d2000, aes(x = time, y = numTxs, color = "2000")) + 
  xlab('Time') +
  ylab('Number') + 
  scale_colour_manual(values = c( "blue", "black", "red", "green")) +#scale_color_gradientn(colors=c('red','black','blue')) +
  scale_fill_discrete(breaks=c("50","500", "1000", "2000")) +
  labs(colour="Keep Time")+
  theme(legend.position = c(0, 1),legend.justification = c(-0.15, 1.1))+
  ggtitle('TX Storage Comparison')
print(p)


p = ggplot() + 
  geom_line(data = d10, aes(x = time, y = numTxs), color = "red") +
  geom_line(data = d50, aes(x = time, y = numTxs), color = "black") +
  geom_line(data = d100, aes(x = time, y = numTxs), color = "blue") +
  xlab('Time') +
  ylab('Number') + 
  ggtitle('TX Storage Comparison')
print(p)



##COMPARISON OF DIFFERENT BaseStation #'s
r3d1000=d1000
#r10d1000
#r6d1000
#r1d1000

p = ggplot() + 
  geom_line(data = r1d1000,  aes(x = time, y = numTxs, color = "1")) +
  geom_line(data = r3d1000,  aes(x = time, y = numTxs, color = "3")) +
  geom_line(data = r6d1000,  aes(x = time, y = numTxs, color = "6")) +
  geom_line(data = r10d1000, aes(x = time, y = numTxs, color = "10")) +
  xlab('Time') +
  ylab('Number of Stored Txs') + 
  ggtitle("1000 Keep Time ~ Varying Base Station #'s") + 
  labs(colour="# of Base Stations:")+
  scale_color_manual(values=c("red", "blue", "green","black"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.4))
print(p)

p = ggplot() + 
  geom_line(data = r1d1000,  aes(x = time, y = numBlocks, color = "1")) +
  geom_line(data = r3d1000,  aes(x = time, y = numBlocks, color = "3")) +
  geom_line(data = r6d1000,  aes(x = time, y = numBlocks, color = "6")) +
  geom_line(data = r10d1000, aes(x = time, y = numBlocks, color = "10")) +
  xlab('Time') +
  ylab('Number of Stored Blocks') + 
  ggtitle("1000 Keep Time ~ Varying Base Station #'s") + 
  labs(colour="# of Base Stations:")+
  scale_color_manual(values=c("red", "blue", "green","black"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.4))
print(p)






###PRUNING TEST
data=prune10

data=prune30
p = ggplot() + 
  geom_line(data = prune10,  aes(x = time, y = numTxs, color = "10")) +
  geom_line(data = prune30,  aes(x = time, y = numTxs, color = "30")) +
  xlab('Time') +
  ylab('Number of Stored Txs') + 
  ggtitle("Pruning, Varying Size") + 
  labs(colour="# of Agents:")+
  scale_color_manual(values=c("red", "blue", "green","black"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.4))
print(p)

p = ggplot() + 
  geom_line(data = prune10,  aes(x = time, y = numBlocks, color = "10")) +
  geom_line(data = prune30,  aes(x = time, y = numBlocks, color = "30")) +
  xlab('Time') +
  ylab('Number of Stored Blocks') + 
  ggtitle("Pruning, Varying Size") + 
  labs(colour="# of Agents:")+
  scale_color_manual(values=c("red", "blue", "green","black"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.4))
print(p)







###STORAGE NUMBER:
##BLOCK HEADER: 240
##TX Size: 196 (with 3 inputs/outputs)
library(ggplot2)
#blockSize=8+32+32+32+8+256*2
blockSize=8+32+32+32+8+192*2
#txSize = (32+4+3*36)+  (4*16) + (256) #tx data + tag
txSize = (32+4+3*36)+  (4*16) + (192) #tx data + tag #truncate to 192 bits, or 24 bytes


data50=v50
data70=v70
data100=v100
data150=v150

#Preprocessing
data50$blockStorage=data50$numBlocks*blockSize
data50$txStorage=data50$numTxs*txSize
data50$blockTxStorage=data50$blockTxs*txSize
#Preprocessing
data70$blockStorage=data70$numBlocks*blockSize
data70$txStorage=data70$numTxs*txSize
data70$blockTxStorage=data70$blockTxs*txSize
#Preprocessing
data100$blockStorage=data100$numBlocks*blockSize
data100$txStorage=data100$numTxs*txSize
data100$blockTxStorage=data100$blockTxs*txSize
#Preprocessing
data150$blockStorage=data150$numBlocks*blockSize
data150$txStorage=data150$numTxs*txSize
data150$blockTxStorage=data150$blockTxs*txSize
#Preprocessing
data200$blockStorage=data200$numBlocks*blockSize
data200$txStorage=data200$numTxs*txSize
data200$blockTxStorage=data200$blockTxs*txSize



##total storage in kilobytes
p = ggplot() + 
  geom_line(data = data50,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125+txStorage*0.000125, color = "50")) +
  geom_line(data = data70,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125+txStorage*0.000125, color = "70")) +
  geom_line(data = data100,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125+txStorage*0.000125, color = "100")) +
  geom_line(data = data150,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125+txStorage*0.000125, color = "150")) +
  #geom_line(data = data200,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125+txStorage*0.000125, color = "200")) +
  xlab('Time') +
  ylab('Storage in KiloBytes') + 
  ggtitle("Total Storage w/ Different Block Tx Limts") + 
  labs(colour="Max Txs / Block")+
  scale_color_manual(values=c("black","maroon","red", "blue", "green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9))
print(p)

##COMPARE 2
##total storage in kilobytes
p = ggplot() + 
  geom_line(data = X25v100,  aes(x = time, y = (blockTxStorage+blockStorage)+txStorage, color = "25Block")) +
  geom_line(data = X50v100,  aes(x = time, y = (blockTxStorage+blockStorage)+txStorage, color = "50Block")) +
  geom_line(data = X25v100,  aes(x = time, y = txStorage, color = "25Tx")) +
  geom_line(data = X50v100,  aes(x = time, y = txStorage, color = "50Tx")) +
  #geom_line(data = data200,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125+txStorage*0.000125, color = "200")) +
  xlab('Time') +
  ylab('Storage in KiloBytes') + 
  ggtitle("Total Storage w/ Different Block Tx Limts") + 
  labs(colour="Max Txs / Block")+
  scale_color_manual(values=c("black","red", "blue", "green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9))
print(p)

p = ggplot() + 
  geom_line(data = X25v100,  aes(x = time, y = numBlocks, color = "25Block")) +
  geom_line(data = X50v100,  aes(x = time, y = numBlocks, color = "50Block")) +
  
  #geom_line(data = X25v100,  aes(x = time, y = numTxs, color = "25Tx")) +
  #geom_line(data = X50v100,  aes(x = time, y = numTxs, color = "50Tx")) +
  
  #geom_line(data = X25v100,  aes(x = time, y = blockTxs, color = "25Tx")) +
  #geom_line(data = X50v100,  aes(x = time, y = blockTxs, color = "50Tx")) +
  
  
  #geom_line(data = data200,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125+txStorage*0.000125, color = "200")) +
  xlab('Time') +
  ylab('Number (frequency)') + 
  ggtitle("Total Storage w/ Different Block Tx Limts") + 
  labs(colour="Max Txs / Block")+
  scale_color_manual(values=c("black","red"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9))
print(p)


#block storage in kB
p = ggplot() + 
  #geom_line(data = X30v100,  aes(x = time, y = numBlocks, color = "30Block")) +
  #geom_line(data = X45V100,  aes(x = time, y = numBlocks, color = "45Block")) +
  #geom_line(data = X130v100,  aes(x = time, y = numBlocks, color = "130Block")) +
  geom_line(data = v20,  aes(x = time, y = blockTxs, color = "20")) +
  geom_line(data = v130,  aes(x = time, y = blockTxs, color = "130")) +
  #
  #geom_line(data = X130v100,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125, color = "130Block")) 
  #geom_line(data = X30v100,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125, color = "30Block")) +
  #geom_line(data = X130v100,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125, color = "130Block")) +
  #geom_line(data = X30v100,  aes(x = time, y = (txStorage)*0.000125, color = "30Tx")) +
  #geom_line(data = X130v100,  aes(x = time, y = (txStorage)*0.000125, color = "130Tx")) +
  xlab('Time') +
  ylab('# Txs in Tx Pool') + 
  ggtitle("blockTxs") + 
  labs(colour="Max Txs / Block")+
  scale_color_manual(values=c("black","red","blue"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9))
print(p)

#tx pool storage in  kb
p = ggplot() + 
  #geom_line(data = data50,  aes(x = time, y = txStorage*0.000125, color = "50")) +
  #geom_line(data = data100,  aes(x = time, y = txStorage*0.000125, color = "100")) +
 # geom_line(data = data150,  aes(x = time, y =txStorage*0.000125, color = "150")) +
 # geom_line(data = data200,  aes(x = time, y = txStorage*0.000125, color = "200")) +
  xlab('Time') +
  ylab('Storage in KiloBytes') + 
  ggtitle("Tx Pool Storage w/ Different Block Tx Limits") + 
  labs(colour="Max Txs / Block")+
  scale_color_manual(values=c("black","red", "blue", "green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9))
print(p)




#numBlocks
p = ggplot() + 
  geom_line(data = data50,  aes(x = time, y = blockTxs, color = "50")) +
  geom_line(data = data100,  aes(x = time, y = blockTxs, color = "100")) +
  geom_line(data = data150,  aes(x = time, y =blockTxs, color = "150")) +
  #geom_line(data = data200,  aes(x = time, y = blockTxs, color = "200")) +
  xlab('Time') +
  ylab('Storage in #') + 
  ggtitle("blockTxs") + 
  labs(colour="Max Txs / Block")+
  scale_color_manual(values=c("black","red", "blue", "green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9))
print(p)


#numBlocks
p = ggplot() + 
  geom_line(data = data50,  aes(x = time, y = blockStorage, color = "50")) +
  geom_line(data = data100,  aes(x = time, y = blockStorage, color = "100")) +
  geom_line(data = data150,  aes(x = time, y =blockStorage, color = "150")) +
  #geom_line(data = data200,  aes(x = time, y = blockTxs, color = "200")) +
  xlab('Time') +
  ylab('Storage in #') + 
  ggtitle("blockStorage") + 
  labs(colour="Max Txs / Block")+
  scale_color_manual(values=c("black","red", "blue", "green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9))
print(p)










## Section Get Tx statistics of folder with varying maxTxs/Block -----

maxTxsStats <- function(title){
library(rlist)
library(ggplot2)

file_list = list.files(pattern="*.csv")
file_list25=c()
file_list25V=c()

file_list50=c()
file_list50V=c()

file_list100=c()
file_list100V=c()
for (i in seq_along(file_list)) {
  if (grepl("size_25_",file_list[i])){
    if (grepl("VOLUME",file_list[i])){
      file_list25V=c(file_list25V,file_list[i])
    }
    else{
      file_list25=c(file_list25,file_list[i])
    }
  }
  
  if (grepl("size_50_",file_list[i])){
    if (grepl("VOLUME",file_list[i])){
      file_list50V=c(file_list50V,file_list[i])
    }
    else{
      file_list50=c(file_list50,file_list[i])
    }
  }
  
  if (grepl("size_100_",file_list[i])){
    if (grepl("VOLUME",file_list[i])){
      file_list100V=c(file_list100V,file_list[i])
    }
    else{
      file_list100=c(file_list100,file_list[i])
    }
  }
}

file_lists=data.frame(matrix(ncol=2, nrow=3))
colnames(file_lists) <- c("txs","volume")
file_lists$txs[1]=list(file_list25)
file_lists$volume[1]=list(file_list25V)

file_lists$txs[2]=list(file_list50)
file_lists$volume[2]=list(file_list100V)

file_lists$txs[3]=list(file_list100)
file_lists$volume[3]=list(file_list100V)


##for loop through the 3 numAgents
for (nAgents in 3:nrow(file_lists)){


currentFiles=file_lists$tx[nAgents][[1]]
currentVFiles=file_lists$volume[nAgents][[1]]

#data_list <- vector("list", "length" = length(file_list))
#refData <- data.frame(matrix(ncol = 4, nrow = length(file_list)))
refData <- data.frame(matrix(ncol = 10 , nrow = length(currentFiles)))
#colnames(refData) <- c("blockTime", "cTime", "orphanRate", "expectedConfirmation")
colnames(refData) <- c("numAgents","maxTxs", "dlt", "maps", "NumTxs","TxsPerBlock","txConfirmationTime", "cTime","uniqueTxs", "numUnique")
#######Time to confirm each TX:


#loop through all files
for (i in seq_along(currentFiles)) {
  #for (i in 1) {
  econfList = c()
  txConfirmationTime =  c()
  #txSubmissionTime = c()
  filename = currentFiles[[i]]
  print(paste(filename," ~ ",i/length(currentFiles)))
  ## Read data in
  df <- read.csv(filename, header = TRUE)
  #df = head(df,-1)
  df = df[-1,]
  
  
  agentSeen = grepl("agent_", names(df)) #index for 
  agentSeendf = data.frame(df[agentSeen])[-1,]
  shareTimes = apply(agentSeendf, 1, FUN = max)  - apply(agentSeendf, 1, FUN = min)
  confirmedBlocks = df[df$confirmedBlock=="True",]
  unconfirmedBlocks = df[df$confirmedBlock=="False",]
  alltxs=c()
  
  
  ##Get tx Confirmation Time and Count
  for (j in 2:nrow(confirmedBlocks)){
    tx_creation_time = strsplit(confirmedBlocks$transaction_creation_time[j], ",")[[1]] #returns 1, 2 3
    txs = strsplit(confirmedBlocks$block_transactions[j], ",")[[1]] #returns tx ids, 1,2,3,4....
    #print(tx_creation_time)
    for (tx in txs){
      t = strtoi(gsub("[^0-9.-]", "", tx))
      t = strtoi(gsub("[\r\n]", "", t))
      alltxs = append(alltxs, t)
    }
   # alltxs = unique(alltxs)
    
    ###tx_creation_time is list of strings
    for (tct in tx_creation_time){ #tct = Transaction creation time
      tct = gsub("[^0-9.-]", "", tct) 
      #print(tct)
      #print(strtoi(confirmedBlocks$confirmationTime[j]) - strtoi(tct) )
      temp = strtoi(confirmedBlocks$confirmationTime[j]) - strtoi(tct)
      #print(temp)
      #if (is.numeric(temp)==FALSE){
      #  print(temp)
      #  }
      txConfirmationTime = append( txConfirmationTime, temp )
      #txSubmissionTime =   append(txSubmissionTime,    strtoi(confirmedBlocks$arrival_time[j])    - strtoi(tct))
    } 
  }
  
  unconfirmedTxs=c()
    if (nrow(unconfirmedBlocks)>0) {
    for (j in 1:nrow(unconfirmedBlocks)){
      txs = strsplit(unconfirmedBlocks$block_transactions[j], ",")[[1]] #returns tx ids, 1,2,3,4....
      
      for (tx in txs){
        t = strtoi(gsub("[^0-9.-]", "", tx))
        t = strtoi(gsub("[\r\n]", "", t))
        #print(t)
        unconfirmedTxs = append(unconfirmedTxs, t)
      }
    unconfirmedTxs=unconfirmedTxs
      
    }
  }
  
  
  totUniqueTxs = length(unique(c(unconfirmedTxs,alltxs)))
  #totTxs = length(unique(c(unconfirmedTxs,uniquetxs)))
  confirmedTxsRate = length(unique(alltxs))/totUniqueTxs
  
  ##save data
  #refData$blockTime[i] = as.numeric(strsplit(gsub("[^0-9.-]", " ", file_list[[i]]), " +")[[1]][5])
  ##"dlt", "maps","maxTxs", "numAgents","NumTxs","TxsPerBlock","txConfirmationTime", "cTime")
  refData$txConfirmationRate[i] = confirmedTxsRate
  refData$unconfirmedUniqueTxs[i] = list(unconfirmedTxs)
  refData$unconfirmedNumUnique[i] = length(unconfirmedTxs)
  #refData$uniqueTxs[i] = list(unique(alltxs))
  #refData$numUnique[i] = length(unique(alltxs))
  refData$numUniqueTxs[i] = unique(totUniqueTxs) ##all unique txs in blocks (conf+unconf)
  refData$totConfTxs[i] = list(alltxs) ##all confirmed Txs
  refData$uniqueTotConfTxs[i] = unique(list(alltxs)) ##all unique confirmed txs
  refData$numConfTxs[i] = length(alltxs)
  refData$numUniqueConfTxs[i] = length(unique(alltxs))
  
  
  refData$cTime[i] = list(na.omit(txConfirmationTime))
  refData$dlt[i] =  strsplit(gsub("_", " ",filename), " +")[[1]][2]
  refData$maps[i] = strsplit(gsub("_", " ",filename), " +")[[1]][13]
  #refData$numTxs[i] =  strsplit(gsub("_", " ",filename), " +")[[1]][5]
  refData$NumTxs[i] = length(txConfirmationTime)
  refData$TxsPerBlock[i] = length(txConfirmationTime)/(nrow(df)-1)
  refData$maxTxs[i] = strtoi(gsub("\\..*", "",strsplit(gsub("_", " ",filename), " +")[[1]][27]))
  refData$minTxs[i] = strtoi(gsub(".csv","",strsplit(gsub("_", " ",filename), " +")[[1]][29]))
  refData$txConfirmationTime[i] = mean(refData$cTime[i][[1]], na.rm=TRUE)
  refData$numAgents[i]=strsplit(gsub("_", " ",filename), " +")[[1]][9]
  refData$rsus[i] = strtoi(strsplit(gsub("_", " ",filename), " +")[[1]][21]) ##Rsu
  
  
  orphanRate = sum(df$confirmedBlock=="False")/nrow(df)
  
  ##print results real quick
  #print(paste("O: ",orphanRate," # Unique Txs: ",refData$numUnique[i], " allTxs: ",length(refData$totTxs[i][[1]]), "Repeat %: ",1-refData$numUnique[i]/length(refData$totTxs[i][[1]]) ))
  
  refData$blockOrphanageRate[i]=orphanRate
  reSubmitTime = mean(txConfirmationTime,na.rm=TRUE) + sd(txConfirmationTime, na.rm=TRUE)*2
  meanTime =mean(txConfirmationTime, na.rm=TRUE)
  refData$meanTime[i]=meanTime
  refData$resubmitTime[i] = reSubmitTime
  refData$expectedConfirmation[i]=0
  orphanRate=1-confirmedTxsRate
  for (n in 1:40){
    temp= sum( (meanTime+ reSubmitTime*(n-1)) * (1-orphanRate)*(orphanRate**(n-1) ))
    #print(temp)
    refData$expectedConfirmation[i] = refData$expectedConfirmation[i] + temp
  }

  
    
  ##calculate Storage Max
  blockSize=208
  #txSize = (32+4+3*36)+  (4*16) + (256) #tx data + tag
  txSize = 244+32*8 #tx data + tag #truncate to 192 bits, or 24 bytes
  
  #df <- read.csv(filename, header = TRUE)
  data=read.csv(currentVFiles[i], header = TRUE)
  
  #Preprocessing
  data$blockStorage=(data$numBlocks)*blockSize ##get rid of first block
  data$txStorage=data$numTxs*txSize
  data$blockTxStorage=data$blockTxs*txSize
  data$totStorage=(data$blockTxStorage+data$blockStorage)/8192+data$txStorage/8192
  data$totBlockStorage = (data$blockTxStorage+data$blockStorage)/8192
  data$poolStorage = data$txStorage/8192
  
  
  refData$maxStorage[i]=max(data$totStorage)
  refData$meanStorage[i]=mean(data$totStorage)
  refData$maxTotBlockStorage[i] = max(data$totBlockStorage)
  refData$maxPoolStorage[i]= max(data$poolStorage) #data$poolStorage[nrow(data)]

  refData$maxSpacePerTx[i] = refData$maxStorage[i]/refData$numUniqueConfTxs[i] ##latest
  refData$maxBitsPerTx[i] = 8192*refData$maxSpacePerTx[i]
  refData$DLTStoragePerTx[i] =   refData$maxTotBlockStorage[i]/refData$numUniqueConfTxs[i]#(refData$TxsPerBlock[i]*500+208)/refData$TxsPerBlock[i]
  
  #refData$meanSpacePerTx[i] = refData$meanStorage[i]/refData$numUniqueConfTxs[i]
}

volumePreProcessing <- function(data){
  
  #Preprocessing
  blockSize=208
  txSize=500 # 244+32*8
  data$blockStorage=data$numBlocks*blockSize ##size of all block headers
  data$txStorage=data$numTxs*txSize ##size of numTxs in pool 
  data$blockTxStorage=data$blockTxs*txSize ##Size of all txs in blocks
  data$totStorage=(data$blockTxStorage+data$blockStorage)/8192+data$txStorage/8192
  data$totBlockStorage = (data$blockTxStorage+data$blockStorage)/8192
  return(data)
}



#ratio=floor
#ratio=0.9*max(refData$expectedConfirmation)/max(refData$maxStorage)
ratio=0.9*max(refData$expectedConfirmation)/max(refData$DLTStoragePerTx)
p = ggplot() + 
  #geom_line(data = refData, size=2, aes(x = maxTxs, minTxs y = TxsPerBlock*40, color = "Txs/Block")) +
  geom_line(data = refData, size=2, aes(x = minTxs, y = expectedConfirmation, color = "Confirmation Time")) +
  #geom_line(data = refData, size=2, aes(x = minTxs, y = maxStorage*ratio, color = "Maximum Storage")) +
  #geom_line(data = refData, size=2, aes(x = minTxs, y = meanStorage*ratio, color = "Mean Storage")) +
  geom_line(data = refData, size=2, aes(x = minTxs, y = DLTStoragePerTx*ratio, color = "Bits/Tx on-Chain")) +
  #geom_line(data = refData, size=2, aes(x = minTxs, y = maxSpacePerTx*ratio, color = "Maximum Storage")) +
  #geom_line(data = refData, size=2, aes(x = minTxs, y = meanSpacePerTx*ratio, color = "Mean Storage")) +
  xlab('Min Txs/Block') +
  ylab('') + 
  ggtitle(paste(title, " # Agents: ",refData$numAgents[1])) + 
  labs(colour="Metrics")+
  scale_color_manual(values=c("blue","red", "green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,2), legend.position=c(0.9,0.6)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Expected Confirmation Time",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./ratio, name="Total Storage (KB)")
  )
print(p)

} ##end of file_lists loop

return(refData)

}


#numUniqueConfTxs
title="DAG PoW Unique Conf Txs:"
#ratio=(max(refData$expectedConfirmation)/max(refData$maxStorage))*0.9
ratio = (max(refData$expectedConfirmation)/max(refData$maxStorage))*0.9
p = ggplot() + 
  #geom_line(data = refData, size=2, aes(x = maxTxs, y = TxsPerBlock*40, color = "Txs/Block")) +
  geom_line(data = refData, size=2, aes(x = minTxs, y = numConfTxs, color = "# Confirmed Txs")) +
 # geom_line(data = refData, size=2, aes(x = minTxs, y = numUniqueConfTxs, color = "# of Unique Confirmed Txs")) +
  geom_line(data = refData, size=2, aes(x = minTxs, y = maxStorage*ratio, color = "Total Storage")) +
  xlab('Max Txs/Block') +
  ylab('') + 
  ggtitle(paste("DAG PoL RSUs, MinTxs, Agents: ",refData$numAgents[1])) + 
  labs(colour="Metrics")+
  scale_color_manual(values=c("blue","red"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,2), legend.position=c(0.9,0.9)) 
print(p)


##txs/block
ratio=0.9*max(refData$expectedConfirmation)/max(refData$maxStorage)
p = ggplot() + 
  #geom_line(data = refData, size=2, aes(x = maxTxs, minTxs y = TxsPerBlock*40, color = "Txs/Block")) +
  #geom_line(data = refData, size=2, aes(x = rsus, y = expectedConfirmation, color = "Confirmation Time")) +
  #geom_line(data = refData, size=2, aes(x = minTxs, y = maxStorage*ratio, color = "Maximum Storage")) +
  #geom_line(data = refData, size=2, aes(x = minTxs, y = meanStorage*ratio, color = "Mean Storage")) +

  geom_line(data = refData, size=2, aes(x = rsus, y = maxStorage, color = "Max Storage")) +
    #geom_line(data = refData, size=2, aes(x = minTxs, y = maxSpacePerTx*ratio, color = "Maximum Storage")) +
  
  #geom_line(data = refData, size=2, aes(x = minTxs, y = maxStorage*ratio, color = "Max Storage")) +
  geom_line(data = refData, size=2, aes(x = rsus, y = meanStorage, color = "Mean Storage")) +
  xlab('Number of RSUs') +
  ylab("Max Storage (Kb)") + 
  ggtitle(paste(title)) + 
  labs(colour="Metrics")+
  scale_color_manual(values=c("blue","red", "green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,2), legend.position=c(0.9,1.1)) 
print(p)



##Storage over time
ratio=0.9*max(refData$expectedConfirmation)/max(refData$maxStorage)
title="Tree-Chain, Balance, 100 Agents"
p = ggplot() + 
  #geom_line(data = test70v, size=2, aes(x = maxTxs, minTxs y = TxsPerBlock*40, color = "Txs/Block")) +
  #geom_line(data = test70v, size=2, aes(x = minTxs, y = expectedConfirmation, color = "Confirmation Time")) +
  #geom_line(data = test70v, size=2, aes(x = minTxs, y = maxStorage*ratio, color = "Maximum Storage")) +
  #geom_line(data = test70v, size=2, aes(x = minTxs, y = meanStorage*ratio, color = "Mean Storage")) +
  
  geom_line(data = test30v, size=1, aes(x = time, y = totStorage, color = "Total Storage")) +
  #geom_line(data = test70v, size=2, aes(x = minTxs, y = maxSpacePerTx*ratio, color = "Maximum Storage")) +
  
  #geom_line(data = test70v, size=2, aes(x = minTxs, y = maxStorage*ratio, color = "Max Storage")) +
  geom_line(data = test30v, size=1, aes(x = time, y = totBlockStorage, color = "Block Storage")) +
  geom_line(data = test30v, size=1, aes(x = time, y = txStorage/8129, color = "Pool Storage")) +
  xlab('Min Txs/Block') +
  ylab("Max Storage (Kb)") + 
  ggtitle(paste(title)) + 
  labs(colour="Metrics")+
  scale_color_manual(values=c("blue","red", "green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,2), legend.position=c(0.25,1.15))
print(p)




## SECTION HASHLENGTH ~ BLOCKSIZE with txs
hashLength = 1:256 #hashLength
collisions=2^hashLength
blockSize = 8+32+32+hashLength*2+32 + 8
txSize = 32 + 4+4 + 16*3 + (hashLength+4)*3
hashRate=1300000 #bytes per second
hashFrame  = data.frame(hashLength,collisions,blockSize,txSize)

for (i in 1:nrow(hashFrame)){
  ##hash per second
  hashFrame$numTrials[i] = log(0.5)/log(1-1/hashFrame$collisions[i])
  hashFrame$hashperSecond[i]=hashRate/(hashFrame$txSize[i]/32)

  
  #seconds per collision
  hashFrame$secPerCollisison[i]=hashFrame$numTrials[i]/hashFrame$hashperSecond[i]
  hashFrame$minPerCollisison[i]=hashFrame$secPerCollisison[i]/60
  hashFrame$hoursPerCollisison[i] = hashFrame$minPerCollisison[i]/60
  hashFrame$daysPerCollisison[i] = hashFrame$hoursPerCollisison[i]/24
  hashFrame$txSizesScale[i] = hashFrame$txSize[i]/868
  hashFrame$blockSizesScale[i] = hashFrame$blockSize[i]/624
  hashFrame$bdayCollision[i] = hashFrame$numTrials[i]/hashFrame$hashperSecond
  }


#(data$blockTxStorage+data$blockStorage)*0.000125+data$txStorage*0.000125
p = ggplot() + 
  #geom_line(data = hashFrame, size=2, aes(x = hashLength, y = collisions, color = "Collisions")) +
  geom_line(data = hashFrame, size=2, aes(x = hashLength, y = daysPerCollisison, color = "Time per Collission")) +
  geom_line(data = hashFrame, size=2, aes(x = hashLength, y = txSizesScale*10000, color = "Scaled Tx Size")) +
  geom_line(data = hashFrame, size=2, aes(x = hashLength, y = blockSizesScale*10000, color = "Scaled Block Size")) +
  xlab('Hash Length') +
  ylab('Days') + 
  ylim(0,10000)+
  ggtitle("Hash Truncation Collision & Storage") + 
  labs(colour="Metrics")+
  scale_color_manual(values=c("red","blue","green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,2), legend.position=c(0.9,0.9))
print(p)


##line of best fit for dayscollisison
model <- lm(log(hashFrame$daysPerCollisison) ~ hashFrame$hashLength)
summary(model)





## SECTION P2p -- P2p statistics


p2pProcessor <- function(data){
  ##txs
  ##return list of number of #s exchanged, and recent time diff of those exchanged items
  
  
  tVisTxTimes=c()
  for (j in 1:nrow(data)){
    ##tVisTxs
    times=gsub("\\[|\\]", "", data$tVisTx[j])
    times = strsplit(times, ",")[[1]] #returns tx ids, 1,2,3,4....
    
    for (t in times){
      tVisTxTimes = append(tVisTxTimes, as.numeric(t))
    }
  }
  hist(tVisTxTimes,breaks=max(tVisTxTimes)+1)
  
  ##save numDIff + time difference of each exchange
  ret=c(list(dplyr::pull(data, dVisTx)), list(tVisTxTimes))
  
  tSubTxTime=c()
  for (j in 1:nrow(data)){
    ##tVisTxs
    times=gsub("\\[|\\]", "", data$tSubTx[j])
    times = strsplit(times, ",")[[1]] #returns tx ids, 1,2,3,4....
    
    for (t in times){
      tSubTxTime = append(tSubTxTime, as.numeric(t))
    }
  }
  hist(tSubTxTime,breaks=max(tSubTxTime)+1)
  
  ##save numDIff + time difference of each exchange
  ret=c(ret, list(dplyr::pull(data, dSubTx)), list(tSubTxTime))
  
    ##should be 0
    tConTxTime=c()
    for (j in 1:nrow(data)){
      print(j/nrow(data))
      ##tVisTxs
      times=gsub("\\[|\\]", "", data$tConTx[j])
      times = strsplit(times, ",")[[1]] #returns tx ids, 1,2,3,4....
      
      for (t in times){
        tConTxTime = append(tConTxTime, as.numeric(t))
      }
    }
    
    
    if (length(tConTxTime)>0){
      hist(tConTxTime,breaks=max(tConTxTime)+1)
    }
    
    
    
    tSubTxTime=c()
  for (j in 1:nrow(data)){
    ##tVisTxs
    times=gsub("\\[|\\]", "", data$tSubTx[j])
    times = strsplit(times, ",")[[1]] #returns tx ids, 1,2,3,4....
    
    for (t in times){
      tSubTxTime = append(tSubTxTime, as.numeric(t))
    }
  }
  hist(tSubTxTime,breaks=max(tSubTxTime)+1)
  
  ##save numDIff + time difference of each exchange
  ret=c(ret, list(dplyr::pull(data, dSubTx)), list(tSubTxTime))
    
    
    ##blocks
    tVisBlockTime=c()
    for (j in 1:nrow(data)){
      ##tVisTxs
      times=gsub("\\[|\\]", "", data$tVisBlock[j])
      times = strsplit(times, ",")[[1]] #returns tx ids, 1,2,3,4....
      
      for (t in times){
        tVisBlockTime = append(tVisBlockTime, as.numeric(t))
      }
    }
    hist(tVisBlockTime, breaks=max(tVisBlockTime)+1)
    
    ##save numDIff + time difference of each exchange
    ret=c(ret, list(dplyr::pull(data, dVisBlock)), list(tVisBlockTime))
    
    
    tLinkBlockTime=c()
    for (j in 1:nrow(data)){
      ##tVisTxs
      times=gsub("\\[|\\]", "", data$tLinkBlock[j])
      times = strsplit(times, ",")[[1]] #returns tx ids, 1,2,3,4....
      
      for (t in times){
        tLinkBlockTime = append(tLinkBlockTime, as.numeric(t))
      }
    }
    hist(tLinkBlockTime,breaks=max(tLinkBlockTime)+1)
    
    ##save numDIff + time difference of each exchange
    ret=c(ret, list(dplyr::pull(data, dLinkBlock)), list(tLinkBlockTime))
  
    #Function to listify a column
    ##dplyr::pull(test, dVisTx)
    
  return(ret)
}




p2pProcessorSimple <- function(data){
  ##txs
  ##return list of number of #s exchanged, and recent time diff of those exchanged items
  
  
  ##txs
  
  tTxTimes=c()
  for (j in 1:nrow(data)){
    ##tVisTxs
    times=gsub("\\[|\\]", "", data$tTxs[j])
    times = strsplit(times, ",")[[1]] #returns tx ids, 1,2,3,4....
    
    for (t in times){
      tTxTimes = append(tTxTimes, as.numeric(t))
      #if (as.numeric(t)>500){
      #  print(j)
      #}
    }
  }
  hist(tTxTimes,breaks=max(tTxTimes)+1)
  
  ##save numDIff + time difference of each exchange
  ret=c(list(dplyr::pull(data, uTxs)), list(dplyr::pull(data, dTxs)), list(tTxTimes))
  
  
  
  
  tBlockTimes=c()
  for (j in 1:nrow(data)){
    ##tVisTxs
    times=gsub("\\[|\\]", "", data$tBlocks[j])
    times = strsplit(times, ",")[[1]] #returns tx ids, 1,2,3,4....
    
    for (t in times){
      tBlockTimes = append(tBlockTimes, as.numeric(t))
    }
  }
  hist(tBlockTimes,breaks=max(tBlockTimes)+1)
  
  ret=c(ret, list(dplyr::pull(data, uBlocks)), list(dplyr::pull(data, dBlocks)), list(tBlockTimes))
  
  return(ret)
}


##test=p2pProcessor(test)

##p2pStats, find relevant data from all p2p csv files
p2pstats <- function(title){
  library(rlist)
  library(ggplot2)
  
  file_list = list.files(pattern="*.csv")
  p2pfiles=c()
  for (i in seq_along(file_list)) {
    if (grepl("P2P",file_list[i])){
      p2pfiles=c(p2pfiles,file_list[i])
    }
  }
  print(p2pfiles)
  
  refData <- data.frame(matrix(ncol = 9 , nrow = length(p2pfiles)))
  #colnames(refData) <- c("blockTime", "cTime", "orphanRate", "expectedConfirmation")
  #colnames(refData) <- c("numAgents", "keep", "maps", "numVisTxsD", "timeVisTxsD",  "numSubTxsD", "timeSubTxsD", "numVisBlocksD", "timeVisBlocksD", "numLinkBlocksD", "timeLinkBlocksD", "uTxs", "dTxs", "tTxs", "uBlocks", "dBlocks", "tBlocks" )
  colnames(refData) <- c("numAgents", "keep", "maps", "numTxsU", "numTxsD", "timeTxsD", "numBlocksU", "numBlocksD", "timeBlocksD")
  #######
  
  ##for each p2pfile, record statistics into refData
  for (i in seq_along(p2pfiles)){
    filename = p2pfiles[i]

    print(paste(filename," ~ ",i/length(p2pfiles)))
    ## Read data in
    df <- read.csv(filename, header = TRUE)
    ret=p2pProcessorSimple(df)
    
    ##data
    refData$maps[i] = strsplit(gsub("_", " ",filename), " +")[[1]][13]
    refData$numAgents[i]=strsplit(gsub("_", " ",filename), " +")[[1]][9]
    refData$keep[i] =strsplit(gsub("_", " ",filename), " +")[[1]][31]
    
    refData$numTxsU[i] = list(ret[1][[1]])
    refData$numTxsD[i] = list(ret[2][[1]])
    refData$timeTxsD[i]= list(ret[3][[1]])
    
    refData$numBlocksU[i] = list(ret[4][[1]])
    refData$numBlocksD[i] = list(ret[5][[1]])
    refData$timeBlocksD[i]= list(ret[6][[1]])
    
   
    
    
    
  }
  return(refData)
}




### Section --- Plot Redundant and New Txs

ssFilter=lambda1
ssFilterTrue = ssFilter[ssFilter$new=="TRUE",]

hist(ssFilter$age,100,col=rgb(0,0,1,0.25),main="Recent Txs pre-filter, Redundant & New P2P Transactions, Lambda 1", xlab="Transaction Age")
hist(ssFilterTrue$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

lambda2=d
ssFilter=lambda2
ssFilterTrue = ssFilter[ssFilter$new=="True",]

hist(ssFilter$age,100,col=rgb(0,0,1,0.25),main="Recent Txs pre-filter, Redundant & New P2P Transactions, Lambda 2", xlab="Transaction Age")
hist(ssFilterTrue$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))



ssFilter=lambda3
ssFilterTrue = ssFilter[ssFilter$new=="True",]

hist(ssFilter$age,100,col=rgb(0,0,1,0.25),main="Recent Txs pre-filter, Redundant & New P2P Transactions, Lambda 3", xlab="Transaction Age")
hist(ssFilterTrue$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))



ssFilter=lambda10
ssFilterTrue = ssFilter[ssFilter$new=="True",]

hist(ssFilter$age,100,col=rgb(0,0,1,0.25),main="Recent Txs pre-filter, Redundant & New P2P Transactions, Lambda 10", xlab="Transaction Age")
hist(ssFilterTrue$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))


##Get all novel rates
sum(lambda1$new=="TRUE")/nrow(lambda1)
sum(lambda2$new=="True")/nrow(lambda2)
sum(lambda3$new=="True")/nrow(lambda3)
sum(lambda4$new=="True")/nrow(lambda4)
sum(lambda5$new=="True")/nrow(lambda5)
sum(lambda10$new=="True")/nrow(lambda10)

##hist of numTxs in filter
l1unique=unique(lambda1[c("time","numTxs","recipientAgent")])
l2unique=unique(lambda2[c("time","numTxs","recipientAgent")])
l3unique=unique(lambda3[c("time","numTxs","recipientAgent")])
l4unique=unique(lambda4[c("time","numTxs","recipientAgent")])
l5unique=unique(lambda5[c("time","numTxs","recipientAgent")])
l10unique=unique(lambda10[c("time","numTxs","recipientAgent")])
hist(l1unique$numTxs,100, main = "Txs per Second:  1", xlab="# of Txs To Reconcile")
hist(l2unique$numTxs,100, main = "Txs per Second:  0.5", xlab="# of Txs To Reconcile")
hist(l3unique$numTxs,100, main="Txs per Second: 0.33", xlab="# of Txs To Reconcile")
hist(l4unique$numTxs,100, main = "Txs per Second:  0.25", xlab="# of Txs To Reconcile")
hist(l5unique$numTxs,100, main = "Txs per Second:  0.2", xlab="# of Txs To Reconcile")
hist(l10unique$numTxs,100, main="Txs per Second: 0.10", xlab="# of Txs To Reconcile")



##plot all density functions
lines1=density(l1unique$numTxs)
lines2=density(l2unique$numTxs)
lines3=density(l3unique$numTxs)
lines4=density(l4unique$numTxs)
lines5=density(l5unique$numTxs)
lines10=density(l10unique$numTxs)



plot(lines1,col="red", lwd="2", xlim=c(0,2300),ylim=c(0,0.02), main="P2P Tx Filter Sizes", xlab="# of Txs in Filter to Reconcile")
lines(lines2,col="blue", lwd="2")
lines(lines3,col="green", lwd="2")
lines(lines4,col="purple", lwd="2")
lines(lines5,col="orange", lwd="2")
lines(lines10,col="brown", lwd="2")
legend("topright",title="Txs/Sec", c("1","0.5","0.33","0.25","0.2","0.1"), fill=c("red","blue","green","purple","orange","brown"))

##Time for P2P Duration & LoRaWAN Novel Reconcilliation
hist(interactionsUrban$p2pTime,6000,xlim=c(0,200), main="Duration of P2P Gossiping, Highway", xlab="P2P Time Duration",col=rgb(1,0,0,0.5))
#abline(v=33.9,col="red", lwd=6)

hist(urbanp2p$dTxsTime,400,xlim=c(0,200),col=rgb(0,0,1,0.5),add=T)

##get cumsum of urbanp2p$dTxsTime
h=hist(urbanp2p$dTxsTime,800,xlim=c(0,200),col=rgb(0,0,1,0.5))
h$cumsum=h$counts
totSum=sum(h$counts)
tempSum=0
for (i in 1:length(h$counts)){
  tempSum=tempSum+h$counts[i]
  h$cumsum[i]=tempSum/totSum
  print(tempSum)
}

hist(interactionsUrban$p2pTime,6000,xlim=c(0,200), main="Duration of P2P Gossiping, Highway", xlab="P2P Time Duration",col=rgb(1,0,0,0.5))

lines(h$cumsum*3000,lwd=3,add=T)
legend("right",c("CDF of Set Reconcilliation", "Duration of Gossip"), fill=c(rgb(0,0,0,1),rgb(1,0,0,0.8), lwd=10))


## Plot difference in txFilter size with differing netsize
n25=d
n25Unique=unique(n25[c("time","numTxs","recipientAgent")])


hist(n25Unique$numTxs,100, main = "Txs per Second:  1", xlab="# of Txs To Reconcile")


ssFilter150=n150
ssFilterTrue150 = ssFilter[ssFilter$new=="True",]
n150Unique=unique(n150[c("time","numTxs","recipientAgent")])
hist(ssFilter150$age,100,col=rgb(0,0,1,0.25),main="Recent Txs pre-filter, Redundant & New P2P Transactions, 150 Agents", xlab="Transaction Age")
hist(ssFilterTrue150$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))


ssFilter125=n125
ssFilterTrue125 = ssFilter[ssFilter$new=="True",]
n125Unique=unique(n125[c("time","numTxs","recipientAgent")])
hist(ssFilter125$age,100,col=rgb(0,0,1,0.25),main="Recent Txs pre-filter, Redundant & New P2P Transactions, 125 Agents", xlab="Transaction Age")
hist(ssFilterTrue125$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))


ssFilter100=n100
ssFilterTrue100 = ssFilter[ssFilter$new=="True",]
n100Unique=unique(n100[c("time","numTxs","recipientAgent")])
hist(ssFilter100$age,100,col=rgb(0,0,1,0.25),main="Recent Txs pre-filter, Redundant & New P2P Transactions, 100 Agents", xlab="Transaction Age")
hist(ssFilterTrue100$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

ssFilter50=n50
ssFilterTrue50 = ssFilter[ssFilter$new=="True",]
n50Unique=unique(n50[c("time","numTxs","recipientAgent")])
hist(ssFilter50$age,100,col=rgb(0,0,1,0.25),main="Recent Txs pre-filter, Redundant & New P2P Transactions, 50 Agents", xlab="Transaction Age")
hist(ssFilterTrue50$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))


ssFilter25=n25
ssFilterTrue25 = ssFilter[ssFilter$new=="True",]
n25Unique=unique(n25[c("time","numTxs","recipientAgent")])

hist(ssFilter25$age,100,col=rgb(0,0,1,0.25),main="Recent Txs pre-filter, Redundant & New P2P Transactions, 25 Agents", xlab="Transaction Age")
hist(ssFilterTrue25$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))


##different agent size numTxs ID:
hist(n25Unique$numTxs, 100,main="25")
hist(n50Unique$numTxs, 100,main="50")
hist(n100Unique$numTxs, 100,main="100")
hist(n125Unique$numTxs, 100,main="125")
hist(n150Unique$numTxs, 100,main="150")



##prefilter
f <- file.choose()
##pf7000 <- read.csv(f)

##8000
pf8000True = pf8000[pf8000$new=="True",]
pf8000Unique=unique(pf8000[c("time","numTxs","recipientAgent")])

hist(pf8000$age,100,col=rgb(0,0,1,0.25),main="Prefilter 8000", xlab="Transaction Age")
hist(pf8000True$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

hist(pf8000Unique$numTxs, 100, main="PreFilter 8000")

#7000
pf7000True = pf7000[pf7000$new=="True",]
pf7000Unique=unique(pf7000[c("time","numTxs","recipientAgent")])

hist(pf7000$age,100,col=rgb(0,0,1,0.25),main="Prefilter 7000", xlab="Transaction Age")
hist(pf7000True$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

hist(pf7000Unique$numTxs, 100, main="PreFilter 7000")

#6000
pf6000True = pf6000[pf6000$new=="True",]
pf6000Unique=unique(pf6000[c("time","numTxs","recipientAgent")])

hist(pf6000$age,100,col=rgb(0,0,1,0.25),main="Prefilter 6000", xlab="Transaction Age")
hist(pf6000True$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

hist(pf6000Unique$numTxs, 100, main="PreFilter 6000")


#5000
pf5000True = pf5000[pf5000$new=="True",]
pf5000Unique=unique(pf5000[c("time","numTxs","recipientAgent")])

hist(pf5000$age,100,col=rgb(0,0,1,0.25),main="Prefilter 5000", xlab="Transaction Age")
hist(pf5000True$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

hist(pf5000Unique$numTxs, 100, main="PreFilter 5000")

#4000
pf4000True = pf4000[pf4000$new=="True",]
pf4000Unique=unique(pf4000[c("time","numTxs","recipientAgent")])

hist(pf4000$age,100,col=rgb(0,0,1,0.25),main="Prefilter 4000", xlab="Transaction Age")
hist(pf4000True$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

hist(pf4000Unique$numTxs, 100, main="PreFilter 4000")

#3000
pf3000True = pf3000[pf3000$new=="True",]
pf3000Unique=unique(pf3000[c("time","numTxs","recipientAgent")])

hist(pf3000$age,100,col=rgb(0,0,1,0.25),main="Prefilter 3000", xlab="Transaction Age")
hist(pf3000True$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

hist(pf3000Unique$numTxs, 100, main="PreFilter 3000")

#2000
pf2000True = pf2000[pf2000$new=="True",]
pf2000Unique=unique(pf2000[c("time","numTxs","recipientAgent")])

hist(pf2000$age,100,col=rgb(0,0,1,0.25),main="Prefilter 2000", xlab="Transaction Age")
hist(pf2000True$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

hist(pf2000Unique$numTxs, 100, main="PreFilter 2000")

#1500
pf1500True = pf1500[pf1500$new=="True",]
pf1500Unique=unique(pf1500[c("time","numTxs","recipientAgent")])

hist(pf1500$age,100,col=rgb(0,0,1,0.25),main="Prefilter 1500", xlab="Transaction Age")
hist(pf1500True$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

hist(pf1500Unique$numTxs, 100, main="PreFilter 1500")

#1000
pf1000True = pf1000[pf1000$new=="True",]
pf1000Unique=unique(pf1000[c("time","numTxs","recipientAgent")])

hist(pf1000$age,100,col=rgb(0,0,1,0.25),main="Prefilter 1000", xlab="Transaction Age")
hist(pf1000True$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

hist(pf1000Unique$numTxs, 100, main="PreFilter 1000")


#500
pf500True = pf500[pf500$new=="True",]
pf500Unique=unique(pf500[c("time","numTxs","recipientAgent")])

hist(pf500$age,100,col=rgb(0,0,1,0.25),main="Prefilter 500", xlab="Transaction Age")
hist(pf500True$age,100,col=rgb(1,0,0,0.5),add=T)
legend("topleft",c("Redundant Txs", "New Txs"), fill=c(rgb(0,0,1,0.25),rgb(1,0,0,0.6), lwd=10))

hist(pf500Unique$numTxs, 100, main="PreFilter 500")




# Section xConfirmation Time of preFilterSize ------


file_list = list.files(pattern="*.csv")
blockFiles  = c()
p2pFiles = c()
for (i in seq_along(file_list)) {
  #print(file_list[i])
  if (grepl("INTERACTION", file_list[i],fixed = TRUE)){
  
  }
  else if (grepl("P2P", file_list[i],fixed = TRUE)){}
  else if (grepl("TXFILTER", file_list[i],fixed = TRUE)){
    p2pFiles = c(p2pFiles, file_list[i])
  }  
  else {blockFiles = c(blockFiles, file_list[i])}
}

#data_list <- vector("list", "length" = length(file_list))
preFilterDF <- data.frame(matrix(ncol = 10, nrow = length(blockFiles)))
colnames(preFilterDF) <- c("PreFilter", "totnumConTxs", "meanConTime","uniqueTxs","expectedConfirmation", "orphanageRate", "BlockFilter","fp","latency", "totLatency")

#loop through all files
for (i in seq_along(blockFiles)) {
  print(i/length(blockFiles))
  filename = blockFiles[i]
  #print(filename)
  
  ## Read data in
  #simuData <- read.csv(filename, header = TRUE)
  simuData <- read.csv(filename)
  simuData$confirmedBlock[1]="True"

  txConfirmationTime =  c()
  ##add to listblockConfirmationTime-txCreationTime
  #confirmedBlocks = simuData[simuData$confirmedBlock=="True",] #Sometimes "TRUE"
  confirmedBlocks = simuData[simuData$confirmedBlock=="True",] #Sometimes "TRUE"
  confirmedBlocks=confirmedBlocks[1:(nrow(confirmedBlocks)/2),]
  for (j in 1:nrow(confirmedBlocks)){
    #print(confirmedBlocks$ID[j])
    tx_creation_time = strsplit(confirmedBlocks$transaction_creation_time[j], ",")[[1]] #returns 1, 2 3
    #print(tx_creation_time)
    ###tx_creation_time is list of strings
    for (tct in tx_creation_time){
      tct = gsub("[^0-9.-]", "", tct)
      #print(strtoi(tct))
      txConfirmationTime = append( txConfirmationTime, strtoi(confirmedBlocks$confirmationTime[j]) - strtoi(tct) )
      #txConfirmationTime = c( txConfirmationTime, confirmedBlocks$confirmationTime - strtoi(tct) )
    } 
  }
    ##get unique txs
  uniquetxs=c()
  for (j in 1:nrow(confirmedBlocks)){
    txs = strsplit(confirmedBlocks$block_transactions[j], ",")[[1]] #returns 1, 2 3

    for (t in txs){
      tct = gsub("[^0-9.-]", "", t)
      if (!(t %in% uniquetxs)){ ##if not in uniquetxs
        uniquetxs = c(uniquetxs,tct)
      }
    }
  }
  preFilterDF$uniqueTxs[i] = length(unique(uniquetxs))
  
  preFilterDF$meanConTime[i] = mean(txConfirmationTime,na.rm=TRUE)
  #preFilter=strsplit(gsub("_", " ",filename), " +")[[1]]
  preFilter=strsplit(gsub("_", " ",filename), " +")[[1]][2]
  
  #if (grepl(".csv",preFilter,fixed="True")){
  #  preFilterDF$PreFilter[i] = strtoi(substr(preFilter,1,nchar(preFilter)-4))
  #}
  #else{
    #preFilterDF$PreFilter[i] = strtoi(preFilter)
  #}
  preFilterDF$PreFilter[i] = strtoi(preFilter)
  bt =strsplit(gsub("_", " ",filename), " +")[[1]][4]
  
  preFilterDF$BlockFilter[i] = strtoi(substr(bt,1,nchar(bt)-2))
  
  ##for false positive rate!!
  pfDec=strsplit(gsub("_", " ",filename), " +")[[1]][6]
  pfDec=substr(pfDec,1,nchar(pfDec)-4)
  preFilterDF$fp[i] = as.numeric(pfDec)
  
  
  ##get the latency
  agentSeen = grepl("agent_", names(simuData)) #index for 
  agentSeendf = data.frame(simuData[agentSeen])[2:200,]
  sTimes = apply(agentSeendf, 1, function(x){x-min(x, na.rm=TRUE)})
  preFilterDF$latency[i]=(sum(sTimes,na.rm=TRUE) + sum(is.na(sTimes))*max(sTimes,na.rm=TRUE))/(199*100)
  
  ##Expected Confirmation Time
  
  #Get Expected Confirmation Time
  reSubmitTime = mean(txConfirmationTime, na.rm=TRUE) + sd(txConfirmationTime, na.rm=TRUE)*2
  preFilterDF$expectedConfirmation[i] = 0
  meanTime = mean(txConfirmationTime, na.rm=TRUE)
  orphanRate = 1-preFilterDF$uniqueTxs[i]/(40000/2) #for 10000 txs
  ##orphanRate = sum(simuData$confirmedBlock=="False")/nrow(simuData)
  preFilterDF$orphanageRate[i]=orphanRate
  for (n in 1:30){
    temp= sum( (meanTime+ reSubmitTime*(n-1)) * (1-orphanRate)*(orphanRate**(n-1) ))
    #print(temp)
    preFilterDF$expectedConfirmation[i] = preFilterDF$expectedConfirmation[i] + temp
  }
  
  preFilterDF$totLatency[i] = preFilterDF$expectedConfirmation[i] + preFilterDF$latency[i]
  
  
 
  

##plot h200
#h<-hist(txConfirmationTime, breaks=100)
#plot(h, main=preFilterDF$PreFilter[i])
#d <- density(txConfirmationTime, na.rm=TRUE)

#lines(x=d$x,y=d$y*length(txConfirmationTime)*(h$breaks[2] - h$breaks[1]), col="blue", lwd=2)


#preFilterDF$ConfirmationDistribution[i]= list(d)
#preFilterDF$latencyDistribution[i] = list(sTimes)
preFilterDF$totnumConTxs[i] = length(txConfirmationTime) ##count unique later


}

##PLot all lines
plot(1, type="n", xlab="", ylab="", xlim=c(0, 4000), ylim=c(0, 100))
colorLists=c("blue",'red',"green","orange","yellow","cyan","black","darkgreen","darkorchid","darkred","coral2","dimgray","gold3","navyblue","peru","pink","sienna3","purple3","rosybrown")
for (i in seq_along(preFilterDF)){
  lines(preFilterDF$ConfirmationDistribution[[i]]$x,preFilterDF$ConfirmationDistribution[[i]]$y*preFilterDF$totnumConTxs[i],col=colorLists[i])
}

library(ggplot2)
f <- y ~ poly(x,5, raw=TRUE)
ggplot(preFilterDF, aes(x=fp, y=expectedConfirmation))+
  geom_point(position="dodge", stat="identity", size=4) + 
  geom_smooth(lwd=3, se=FALSE) +
  ggtitle("Highway, 100 Agents, Bloom Filter False Postive Rate ~ ECT") +
  xlab("Bloom Filter False Positive Rate") + ylab("Expected Confirmation Time") + 
  theme(plot.title = element_text(size=22))
 # stat_regline_equation(label.x = 0.1, label.y = 5000)


library(ggplot2)
ggplot(preFilterDF, aes(x=fp, y=expectedConfirmation/min(expectedConfirmation)))+
  geom_point(position="dodge", stat="identity", size=4) + 
  geom_smooth(lwd=3) +
  ggtitle("Highway, 100 Agents, Bloom Filter False Postive Rate ~ ECT") +
  xlab("Bloom Filter False Positive Rate") + ylab("Expected Confirmation Time") + 
  theme(plot.title = element_text(size=22))

pf = preFilterDF[preFilterDF$PreFilter<5000,]
plot(pf$PreFilter,pf$expectedConfirmation,lwd=10,main="Filter Cutoff & Expected Confirmation Time")
plot(pf$PreFilter,pf$expectedConfirmation,lwd=10,main="Filter Cutoff & Expected Confirmation Time")

plot(pf$PreFilter,pf$expectedConfirmation,type="l", lwd=4,main="Filter Cutoff & Expected Confirmation Time, Blocks Only", ylab="Expected Confirmation Time", xlab="Filter Cutoff (Sec)")
#plot(pf$PreFilter,pf$totnumConTxs)
#plot(pf$PreFilter,pf$uniqueTxs)
#plot(pf$PreFilter,pf$orphanageRate)


plot(1, type="n", xlab="", ylab="", xlim=c(0, 5000), ylim=c(0, 40))
colorLists=c("blue",'red',"green","orange","darkorchid","black")

lines(pf$ConfirmationDistribution[[26]]$x, pf$ConfirmationDistribution[[26]]$y*pf$uniqueTxs[26],col="blue", lwd=3) #25 
#lines(pf$ConfirmationDistribution[[28]]$x, pf$ConfirmationDistribution[[28]]$y*pf$uniqueTxs[28],col="green") #75
lines(pf$ConfirmationDistribution[[27]]$x, pf$ConfirmationDistribution[[8]]$y*pf$uniqueTxs[8],col="red", lwd=3) #100
#lines(pf$ConfirmationDistribution[[9]]$x, pf$ConfirmationDistribution[[9]]$y*pf$uniqueTxs[9],col="green") #200
lines(pf$ConfirmationDistribution[[1]]$x, pf$ConfirmationDistribution[[1]]$y*pf$uniqueTxs[1],col="orange", lwd=3) #500

lines(pf$ConfirmationDistribution[[16]]$x, pf$ConfirmationDistribution[[16]]$y*pf$uniqueTxs[16],col="darkorchid", lwd=3) #1000


#lines(pf$ConfirmationDistribution[[3]]$x, pf$ConfirmationDistribution[[3]]$y*pf$uniqueTxs[3],col="black") #2000

lines(pf$ConfirmationDistribution[[6]]$x, pf$ConfirmationDistribution[[6]]$y*pf$uniqueTxs[6],col="darkgreen", lwd=3) #4000



### ANALYSIS OF tBlocks in p2p
simuData <- p2p600

blockTime =  c()
##add to listblockConfirmationTime-txCreationTime
#confirmedBlocks = simuData[simuData$confirmedBlock=="True",] #Sometimes "TRUE"
#confirmedBlocks = simuData[simuData$confirmedBlock=="True",] #Sometimes "TRUE"
for (j in 1:nrow(simuData)){
  #print(confirmedBlocks$ID[i])
  blockT = strsplit(simuData$tBlocks[j], ",")[[1]] #returns 1, 2 3
  #print(tx_creation_time)
  ###tx_creation_time is list of strings
  for (b in blockT){
    b = gsub("[^0-9.-]", "", b)
    #print(strtoi(tct))
    blockTime = append( blockTime, strtoi(strtoi(b)) )
    #txConfirmationTime = c( txConfirmationTime, confirmedBlocks$confirmationTime - strtoi(tct) )
  } 
}


for (i in seq_along(blockFiles)){ 
  preFilter=strsplit(gsub("_", " ",blockFiles[i]), " +")[[1]][2]
  
  if (grepl(".csv",preFilter,fixed="True")){
    preFilterDF$PreFilter[i] = strtoi(substr(preFilter,1,nchar(preFilter)-4))
  }
    else{
      preFilterDF$PreFilter[i] = strtoi(preFilter)
    }
}

for (i in seq_along(blockFiles)){ 
  #print(blockFiles[i])
  blockFilter=strsplit(gsub("_", " ",blockFiles[i]), " +")[[1]][4]
  if (grepl(".csv",blockFilter,fixed="True")){
    preFilterDF$BlockFilter[i] = strtoi(substr(blockFilter,1,nchar(blockFilter)-4))
    }
  
}



pf = preFilterDF[preFilterDF$PreFilter<=2000,]
pf = pf[pf$BlockFilter<=2000,]

freq <- matrix( c(52.55773339,52.4810681,52.31722167,52.10666244,51.85152692,51.55303103,51.24675264,50.95101158,50.68197668,50.43064738,50.22846931,50.08543927,49.994372,49.93623365,49.93623365,49.95474748,50.020008,50.10406391,50.19520387,50.2870507,11.52907226,11.51214938,11.47624542,11.43321299,11.37467086,11.31150225,11.24373738,11.17828919,11.11804254,11.06485559,11.01959922,10.98659913,10.96551607,10.95785573,10.95472114,10.96099211,10.97318713,10.99220547,11.01079815,11.02951726,4.90118465,4.89277852,4.87879018,4.85866683,4.83424424,4.8066515,4.77798155,4.75203635,4.72511158,4.70191937,4.68361504,4.67039555,4.66098397,4.65570661,4.65570661,4.65882092,4.66314904,4.67240082,4.6797336,4.68703179,2.73969745,2.73613684,2.72762931,2.7166304,2.7027496,2.6875514,2.67207306,2.65649384,2.64181,2.62924142,2.61895192,2.61137649,2.60655494,2.60347547,2.60347547,2.60535233,2.60781839,2.61234295,2.61707225,2.62180777,1.78840879,1.78489564,1.7797031,1.77276685,1.76342337,1.75402455,1.74399484,1.73336661,1.72447279,1.71564231,1.70939685,1.7038622,1.70073726,1.6988028,1.6988028,1.69961249,1.70173829,1.7047733,1.70781914,1.71071479,1.28985419,1.28747029,1.28368926,1.27891973,1.27255435,1.26528502,1.25795304,1.25044504,1.24373112,1.2379787,1.23266801,1.22947598,1.22694364,1.22564332,1.22564332,1.22629314,1.22767628,1.22947598,1.2319294,1.23390357,0.99890991,0.9969349,0.99403376,0.98990311,0.98518794,0.97966981,0.97400602,0.96817904,0.9629888,0.95835985,0.95454662,0.95190337,0.94983488,0.94908041,0.94876476,0.94961552,0.95056832,0.95224781,0.95385465,0.95538447,0.81525917,0.81386686,0.81139444,0.80823488,0.80404819,0.79951033,0.79493013,0.79021474,0.78622136,0.78225906,0.77937373,0.77680709,0.77545595,0.77456974,0.77434183,0.77489849,0.77594474,0.77721495,0.77849226,0.77980458,0.69323956,0.69189474,0.69001949,0.68717958,0.68365041,0.67987454,0.67579296,0.67196405,0.66836977,0.66510667,0.66258759,0.66043484,0.6591803,0.65849162,0.65849162,0.65885797,0.65963519,0.66075872,0.66190098,0.6630472,0.6086962,0.60762395,0.60600669,0.60344773,0.60050042,0.59712626,0.59348405,0.59019735,0.58684333,0.58408302,0.58172941,0.58005317,0.57895527,0.57842395,0.57817575,0.57867237,0.57927387,0.58033743,0.58128325,0.58219843),nrow=10,ncol=20)
freq2 <- matrix(c(0.54851987,0.54766937,0.5459702,0.54375649,0.54093532,0.53802293,0.53494464,0.53187436,0.52896294,0.52646198,0.52435606,0.52281311,0.52157284,0.52106858,0.52106858,0.52132058,0.52202189,0.52281311,0.52379486,0.52475492,0.5048295,0.5039379,0.50229659,0.50027297,0.497864,0.49496823,0.49219442,0.48927935,0.4866184,0.4843179,0.482411,0.48109686,0.48008502,0.47961592,0.47942176,0.47971962,0.48045098,0.48109686,0.48198218,0.48289125,0.47235861,0.47148872,0.47012833,0.46822716,0.46585552,0.46325103,0.46058238,0.45785409,0.45538011,0.45325877,0.45146018,0.45008397,0.4492363,0.44876516,0.44876516,0.44897784,0.44949506,0.4502979,0.45100349,0.45180931,0.44806656,0.4471529,0.44586017,0.44404929,0.44188451,0.43937562,0.4368033,0.4343326,0.4320046,0.42986732,0.42814069,0.42689383,0.42599487,0.42557269,0.42548668,0.42586023,0.42638349,0.42710527,0.42784126,0.42859559,0.42939911,0.42873197,0.42740367,0.42572568,0.42347464,0.42121565,0.41864654,0.41623261,0.41404033,0.41201794,0.4103554,0.40921444,0.40829659,0.40798516,0.40786166,0.40820489,0.40873244,0.40933876,0.41007412,0.4108327,0.41480007,0.4142238,0.41298141,0.41125155,0.40921444,0.40691806,0.40454606,0.40220968,0.40008444,0.39806768,0.39654096,0.39537183,0.39452055,0.39417174,0.39403881,0.39435632,0.39480955,0.3955169,0.39624941,0.39679568,0.40320251,0.40248992,0.40137764,0.3996568,0.39774416,0.3955169,0.39316686,0.39083263,0.38869389,0.38694127,0.38545323,0.38426319,0.38350541,0.38307775,0.38307775,0.38321394,0.38380617,0.38440022,0.38507073,0.38566871,0.39342197,0.39271922,0.39159611,0.3899818,0.38800827,0.38588444,0.38359275,0.38133106,0.37924175,0.37751086,0.37590355,0.37487529,0.37412251,0.37379817,0.37361391,0.37396027,0.37441129,0.37494205,0.37555358,0.37625418,0.38453735,0.38401981,0.38283588,0.38133106,0.37940861,0.37724461,0.37510515,0.37282519,0.37089542,0.36914938,0.36761235,0.36651541,0.36575363,0.36537528,0.36537528,0.36559857,0.36608557,0.36669274,0.36723014,0.36794767,0.37635928,0.37573976,0.37465246,0.37317009,0.37124711,0.36914938,0.36700487,0.36484329,0.36291902,0.36109526,0.35975516,0.35869104,0.35795307,0.35758199,0.35758199,0.35773389,0.3582407,0.35869104,0.35947532,0.36002217), nrow=10, ncol=20)
freqTotal=rbind(freq,freq2)


freqTest=rbind(c(52.55773339,52.4810681,52.31722167,52.10666244,51.85152692,51.55303103
                 ,51.24675264,50.95101158,50.68197668,50.43064738,50.22846931,50.08543927
                 ,49.994372,49.93623365,49.93623365,49.95474748,50.020008,50.10406391
                 ,50.19520387,50.2870507))

freqTest=rbind(freqTest,c(11.52907226,11.51214938,11.47624542,11.43321299,11.37467086,11.31150225
                          ,11.24373738,11.17828919,11.11804254,11.06485559,11.01959922,10.98659913
                          ,10.96551607,10.95785573,10.95472114,10.96099211,10.97318713,10.99220547
                          ,11.01079815,11.02951726))

freqTest=rbind(freqTest,c(4.90118465,4.89277852,4.87879018,4.85866683,4.83424424,4.8066515
                          ,4.77798155,4.75203635,4.72511158,4.70191937,4.68361504,4.67039555
                          ,4.66098397,4.65570661,4.65570661,4.65882092,4.66314904,4.67240082
                          ,4.6797336,4.68703179))

freqTest=rbind(freqTest,c(2.73969745,2.73613684,2.72762931,2.7166304,2.7027496,2.6875514
                          ,2.67207306,2.65649384,2.64181,2.62924142,2.61895192,2.61137649
                          ,2.60655494,2.60347547,2.60347547,2.60535233,2.60781839,2.61234295
                          ,2.61707225,2.62180777))
freqTest=rbind(freqTest,c(1.78840879,1.78489564,1.7797031,1.77276685,1.76342337,1.75402455
                          ,1.74399484,1.73336661,1.72447279,1.71564231,1.70939685,1.7038622
                          ,1.70073726,1.6988028,1.6988028,1.69961249,1.70173829,1.7047733
                          ,1.70781914,1.71071479))
freqTest=rbind(freqTest,c(1.28985419,1.28747029,1.28368926,1.27891973,1.27255435,1.26528502
                          ,1.25795304,1.25044504,1.24373112,1.2379787,1.23266801,1.22947598
                          ,1.22694364,1.22564332,1.22564332,1.22629314,1.22767628,1.22947598
                          ,1.2319294,1.23390357))
freqTest=rbind(freqTest,c(0.99890991,0.9969349,0.99403376,0.98990311,0.98518794,0.97966981
                          ,0.97400602,0.96817904,0.9629888,0.95835985,0.95454662,0.95190337
                          ,0.94983488,0.94908041,0.94876476,0.94961552,0.95056832,0.95224781
                          ,0.95385465,0.95538447))
freqTest=rbind(freqTest,c(0.81525917,0.81386686,0.81139444,0.80823488,0.80404819,0.79951033
                          ,0.79493013,0.79021474,0.78622136,0.78225906,0.77937373,0.77680709
                          ,0.77545595,0.77456974,0.77434183,0.77489849,0.77594474,0.77721495
                          ,0.77849226,0.77980458))
freqTest=rbind(freqTest,c(0.69323956,0.69189474,0.69001949,0.68717958,0.68365041,0.67987454
                          ,0.67579296,0.67196405,0.66836977,0.66510667,0.66258759,0.66043484
                          ,0.6591803,0.65849162,0.65849162,0.65885797,0.65963519,0.66075872
                          ,0.66190098,0.6630472))
freqTest=rbind(freqTest,c(0.6086962,0.60762395,0.60600669,0.60344773,0.60050042,0.59712626
                          ,0.59348405,0.59019735,0.58684333,0.58408302,0.58172941,0.58005317
                          ,0.57895527,0.57842395,0.57817575,0.57867237,0.57927387,0.58033743
                          ,0.58128325,0.58219843))

freqTest=rbind(freqTest,c(0.54851987,0.54766937,0.5459702,0.54375649,0.54093532,0.53802293
                          ,0.53494464,0.53187436,0.52896294,0.52646198,0.52435606,0.52281311
                          ,0.52157284,0.52106858,0.52106858,0.52132058,0.52202189,0.52281311
                          ,0.52379486,0.52475492))
freqTest=rbind(freqTest,c(0.5048295,0.5039379,0.50229659,0.50027297,0.497864,0.49496823
                          ,0.49219442,0.48927935,0.4866184,0.4843179,0.482411,0.48109686
                          ,0.48008502,0.47961592,0.47942176,0.47971962,0.48045098,0.48109686
                          ,0.48198218,0.48289125))
freqTest=rbind(freqTest,c(0.47235861,0.47148872,0.47012833,0.46822716,0.46585552,0.46325103
                          ,0.46058238,0.45785409,0.45538011,0.45325877,0.45146018,0.45008397
                          ,0.4492363,0.44876516,0.44876516,0.44897784,0.44949506,0.4502979
                          ,0.45100349,0.45180931))
freqTest=rbind(freqTest,c(0.44806656,0.4471529,0.44586017,0.44404929,0.44188451,0.43937562
                          ,0.4368033,0.4343326,0.4320046,0.42986732,0.42814069,0.42689383
                          ,0.42599487,0.42557269,0.42548668,0.42586023,0.42638349,0.42710527
                          ,0.42784126,0.42859559))
freqTest=rbind(freqTest,c(0.42939911,0.42873197,0.42740367,0.42572568,0.42347464,0.42121565
                          ,0.41864654,0.41623261,0.41404033,0.41201794,0.4103554,0.40921444
                          ,0.40829659,0.40798516,0.40786166,0.40820489,0.40873244,0.40933876
                          ,0.41007412,0.4108327))
freqTest=rbind(freqTest,c(0.41480007,0.4142238,0.41298141,0.41125155,0.40921444,0.40691806
                 ,0.40454606,0.40220968,0.40008444,0.39806768,0.39654096,0.39537183
                 ,0.39452055,0.39417174,0.39403881,0.39435632,0.39480955,0.3955169
                 ,0.39624941,0.39679568))

freqTest=rbind(freqTest,c(0.40320251,0.40248992,0.40137764,0.3996568,0.39774416,0.3955169
                 ,0.39316686,0.39083263,0.38869389,0.38694127,0.38545323,0.38426319
                 ,0.38350541,0.38307775,0.38307775,0.38321394,0.38380617,0.38440022
                 ,0.38507073,0.38566871))
freqTest=rbind(freqTest,c(0.39342197,0.39271922,0.39159611,0.3899818,0.38800827,0.38588444
                 ,0.38359275,0.38133106,0.37924175,0.37751086,0.37590355,0.37487529
                 ,0.37412251,0.37379817,0.37361391,0.37396027,0.37441129,0.37494205
                 ,0.37555358,0.37625418))
freqTest=rbind(freqTest,c(0.38453735,0.38401981,0.38283588,0.38133106,0.37940861,0.37724461
                 ,0.37510515,0.37282519,0.37089542,0.36914938,0.36761235,0.36651541
                 ,0.36575363,0.36537528,0.36537528,0.36559857,0.36608557,0.36669274
                 ,0.36723014,0.36794767))
freqTest=rbind(freqTest,c(0.37635928,0.37573976,0.37465246,0.37317009,0.37124711,0.36914938
                 ,0.36700487,0.36484329,0.36291902,0.36109526,0.35975516,0.35869104
                 ,0.35795307,0.35758199,0.35758199,0.35773389,0.3582407,0.35869104
                 ,0.35947532,0.36002217))

##turn into long dataframe
freqDF <- data.frame(matrix(ncol = 3, nrow = 200))
colnames(freqDF) <- c("PreFilter", "BlockFilter", "lt")
for (i in 1:20){
  for (j in 1:20){
    freqDF[(i-1)*20 + j,] = c(i*100,j*100, freqTest[i,j])
  }
}


library(ggplot2)
library(ggpubr)
ggplot(freqDF, aes(x=BlockFilter, y=as.numeric(PreFilter), fill=lt)) + 
  geom_tile() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Highway 100 Agents, Transaction Throughput ~ Cutoffs") +
  xlab("Block Filter Cutoff") + ylab("Transaction Filter Cutoff") +
  labs(fill="Transactions/Sec") + 
  theme(plot.title = element_text(size=26))


library(ggplot2)
library(ggpubr)
ggplot(pf, aes(x=BlockFilter, y=as.numeric(PreFilter), fill=expectedConfirmation)) + 
  geom_tile() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Downtown 100 Agents, Combine Filter Results ~ ECT") +
  xlab("Block Filter Cutoff") + ylab("Transaction Filter Cutoff") +
  labs(fill="ECT") + 
   theme(plot.title = element_text(size=26))

##plot tCutoff/PreFilter VS. false positive rate (P)
ggplot(preFilterDF, aes(x=as.numeric(PreFilter), y=fp, fill=fp)) + 
  geom_tile() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Highway, 100 Agents, Filter Results ~ False Postive Rate") +
  xlab("Bloom Filter False Positive Rate") + ylab("Transaction Filter Cutoff") +
  labs(fill="ECT") + 
  theme(plot.title = element_text(size=26))

ggplot(test, aes(x=PreFilter, y=expectedConfirmation, group=fp, color=fp)) + 
  geom_line() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Highway, 100 Agents, Filter Results ~ False Postive Rate") +
  xlab("Transaction Filter Cutoff") + ylab("Expected Confirmation Time") +
  labs(fill="ECT") + 
  theme(plot.title = element_text(size=26))

ggplot(test, aes(x=fp, y=expectedConfirmation, group=PreFilter, color=PreFilter)) + 
  geom_line() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Highway, 100 Agents, Filter Results ~ False Postive Rate") +
  xlab("Transaction Filter Cutoff") + ylab("Expected Confirmation Time") +
  labs(fill="ECT") + 
  theme(plot.title = element_text(size=26))

preFilterDF$fp2 =as.integer(preFilterDF$fp*100)

  ##add MaximumTxsHwy100 to each row of preFilterDF (20x50)
for (i in 1:nrow(preFilterDF)){ ##160 obs of 9 variables
  print(i)
  #print(preFilterDF$PreFilter[i]/100)
  #print(preFilterDF$fp[i]/0.01)
  preFilterDF$throughput[i] = MaximumTxsHwy100[preFilterDF$PreFilter[i]/100,  preFilterDF$fp2[i]][[1]]
  preFilterDF$throughputETC[i] = preFilterDF$expectedConfirmation[i]*preFilterDF$throughput[i][[1]]
  preFilterDF$throughputtotLat[i] = preFilterDF$totLatency[i]*preFilterDF$throughput[i][[1]]
}

ggplot(preFilterDF, aes(x=fp, y=throughputETC, group=PreFilter, color=PreFilter)) + 
  geom_line() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Highway, 100 Agents, Filter Results ~ Maximum Confirmation Throughput") +
  xlab("Transaction Filter Cutoff") + ylab("throughputETC") +
  labs(fill="ECT") + 
  theme(plot.title = element_text(size=26))


ggplot(preFilterDF, aes(x=PreFilter, y=throughputETC, group=fp, color=fp)) + 
  geom_line() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Highway, 100 Agents, Filter Results ~ Maximum Confirmation Throughput") +
  xlab("Transaction Filter Cutoff") + ylab("throughputETC") +
  labs(fill="ECT") + 
  theme(plot.title = element_text(size=26))

ggplot(preFilterDF, aes(x=PreFilter, y=as.numeric(latency), group=fp, color=fp)) + 
  geom_line() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Highway, 100 Agents, Filter Results ~ False Postive Rate") +
  xlab("Transaction Filter Cutoff") + ylab("Network Latency") +
  labs(fill="ECT") + 
  theme(plot.title = element_text(size=26))

ggplot(preFilterDF, aes(x=fp, y=as.numeric(latency), group=PreFilter, color=PreFilter)) + 
  geom_line() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Highway, 100 Agents, Filter Results ~ False Postive Rate") +
  xlab("Transaction Filter Cutoff") + ylab("Network Latency") +
  labs(fill="ECT") + 
  theme(plot.title = element_text(size=26))


ggplot(preFilterDF, aes(x=PreFilter, y=as.numeric(throughputtotLat), group=fp, color=fp)) + 
  geom_line() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Highway, 100 Agents, Filter Results ~ False Postive Rate") +
  xlab("Transaction Filter Cutoff") + ylab("Tx Confirmation Latency") +
  labs(fill="ECT") + 
  theme(plot.title = element_text(size=26))

ggplot(preFilterDF, aes(x=fp, y=as.numeric(throughputtotLat), group=PreFilter, color=PreFilter)) + 
  geom_line() + 
  scale_fill_gradient(low="blue",high="orange") +
  ggtitle("Highway, 100 Agents, Filter Results ~ False Postive Rate") +
  xlab("Transaction Filter Cutoff") + ylab("Tx Confirmation Latency") +
  labs(fill="ECT") + 
  theme(plot.title = element_text(size=26))



ggplot(refDataDownTown2refs, aes( y=expectedConfirmation, x=numAgents, group=group, col=group, label=group), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  geom_label_repel(aes(label = group),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')+
  ggtitle(paste("Downtown 2 Refs, Minting Time ~ ")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  scale_color_gradient(low="blue", high="red") + 
  ylim(0,200)+
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.8)


## get filter size and success rate
file_list = list.files(pattern="*.csv")
blockFiles  = c()
p2pFiles = c()
for (i in seq_along(file_list)) {
  #print(file_list[i])
  if (grepl("INTERACTION", file_list[i],fixed = TRUE)){
    
  }
  else if (grepl("P2P", file_list[i],fixed = TRUE)){}
  else if (grepl("TXFILTER", file_list[i],fixed = TRUE)){
    p2pFiles = c(p2pFiles, file_list[i])
  }  
  else {blockFiles = c(blockFiles, file_list[i])}
}

#data_list <- vector("list", "length" = length(file_list))
p2pDF <- data.frame(matrix(ncol = 4, nrow = length(p2pFiles)))
colnames(p2pDF) <- c("PreFilter", "BlockFilter", "BlockRate", "TxRate")


for (i in seq_along(p2pFiles)){ 
  
  
  filename = p2pFiles[i]
  #print(filename)
  print(i/length(p2pFiles)) ##what percantage are we?
  ## Read data in
  #simuData <- read.csv(filename, header = TRUE)
  simuData <- read.csv(filename)

  #print(blockFiles[i])
  blockFilter=strsplit(gsub("_", " ",filename), " +")[[1]][4]
  if (grepl(".csv",blockFilter,fixed="True")){
    p2pDF$BlockFilter[i] = strtoi(substr(blockFilter,1,nchar(blockFilter)-4))
  }else{
    p2pDF$BlockFilter[i] =strtoi(blockFilter)
  }
  p2pDF$PreFilter[i] =  strtoi(strsplit(gsub("_", " ",filename), " +")[[1]][2])
  
  simuDataTx = simuData[simuData$type=="tx",]
  simuDataBlock = simuData[simuData$type=="block",]
  
  p2pDF$BlockRate[i] = sum(simuDataBlock$new=="True")/nrow(simuDataBlock)
  p2pDF$TxRate[i] = sum(simuDataTx$new=="True")/nrow(simuDataTx)
  
}

bf <- unique(p2pDF$BlockFilter)
blockLM <- lm(p2pDF$BlockRate ~ poly(p2pDF$BlockFilter, 3))
p <- predict(blockLM)

plot(p2pDF$BlockFilter, p2pDF$BlockRate)
ix <- sort(x,index.return=T)$ix
lines(bf, p, col=2, lwd=2)

fit <- lm(p2pDF$BlockRate ~ poly(p2pDF$BlockFilter, 2)) ##manually create model


#f <- BlockRate ~ poly(BlockFilter,7,raw=TRUE)
#f <- BlockRate ~ log
f <- BlockRate ~ poly(1/BlockFilter,3,raw=TRUE)
model <- lm(f,p2) ##p2 is just unique(p2pDF[c(blockFilter,BlockRate)])
bFilter=seq(25,1500,25)
bRate <- predict(model, data.frame(BlockFilter = bFilter))
plot(p2$BlockFilter,p2$BlockRate,lwd=5,main="Inverse poly order 3")
lines(bFilter, bRate, col="red",lwd=3)


f = y ~ poly(1/x,3,raw=TRUE)
ggplot(p2, aes(x=BlockFilter, y= BlockRate)) + 
  geom_point(size=4) + 
  ggtitle("Downtown, 100 Agents, Block Gossip Statistics") + xlab("Block Filter Cutoff") + ylab("Block Novelty Rate") +
  theme(plot.title = element_text(size=22)) + 
  stat_smooth(method = "lm", formula = f, se=FALSE,lwd=3) +
  stat_regline_equation(label.x=500, aes(label =  paste(..eq.label.., sep = "~~~~")),formula = f) 


f = y ~ poly(1/x,3,raw=TRUE)
ggplot(p2pDF, aes(x=PreFilter, y= TxRate)) + 
  geom_point(size=4) + 
  ggtitle("Downtown, 100 Agents, Transaction Gossip Statistics") + xlab("Transaction Filter Cutoff") + ylab("Transaction Novelty Rate") +
  theme(plot.title = element_text(size=22)) + 
  stat_smooth(method = "lm", formula = f, se=FALSE,lwd=3) +
  stat_regline_equation(label.x=500, aes(label =  paste(..eq.label.., sep = "~~~~")),formula = f) 


library(ggplot2)
library(ggpubr)
f = y ~ poly(x,6,raw=TRUE)
#f = y ~ ns(x,5)
#f = y ~ bs(x, df=4, raw=TRUE)
ggplot(p2pDF, aes(x=BlockFilter, y= BlockRate)) + 
  geom_point(size=4) + 
  ggtitle("Downtown, 100 Agents, Block Gossip Statistics") + xlab("Block Filter Cutoff") + ylab("Block Novelty Rate") +
  theme(plot.title = element_text(size=22)) + 
  stat_smooth(method = "lm", formula = f, se=FALSE,lwd=3) +
  stat_regline_equation(label.x=250, aes(label =  paste(..eq.label.., sep = "~~~~")),formula = f) 



ggplot(p2pDF, aes(x=BlockFilter, y= BlockRate)) + 
  geom_point(size=4) + 
  ggtitle("Downtown, 100 Agents, Block Gossip Statistics") + xlab("Block Filter Cutoff") + ylab("Block Novelty Rate") +
  theme(plot.title = element_text(size=22)) + 
  geom_smooth(method = "lm", formula = f, se=FALSE,lwd=3) +
  stat_poly_eq(parse=T, formula=f)

#ggscatter(p2pDF, x="BlockFilter", y="BlockRate", add="reg.line")


f = y ~ poly(x, 3, raw=TRUE)
ggplot(p2pDF, aes(x=PreFilter, y= TxRate)) + 
  geom_point(size=4) + 
  ggtitle("Highway, 100 Agents, Transaction Gossip Statistics") + xlab("Transaction Filter Cutoff") + ylab("Transaction Novelty Rate") +
  theme(plot.title = element_text(size=22)) + 
  stat_smooth(method = "lm", formula = f, se=FALSE,lwd=3) +
  stat_regline_equation(label.x=1000,
                        aes(label =  paste(..eq.label.., sep = "~~~~")),
                        formula = f
  ) 

##plot shareTimes with CDF
par(mar = c(5,5,2,5))
h <-hist(test2Min,100, main="Highway, 100 Agents, Novel Items Proliferation Latency", xlab="Item Latency")
par(new = T)
##plot shareTimes w/ CDF
plot(x = h$mids, y=ec(h$mids)*max(h$counts), col = rgb(0,0,0,alpha=0), axes=F, xlab=NA, ylab=NA)
lines(x = h$mids, y=ec(h$mids)*max(h$counts), col ='red', lwd=5)
axis(4, at=seq(from = 0, to = max(h$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'red', col.axis = 'red')
mtext(side = 4, line = 3, 'Cumulative Density', col = 'red')

f = y ~ poly(x,3, raw=TRUE)

ggplot( aes(x=300/(1:length(my.ecdf)), y= my.ecdf)) + 
  geom_point(size=4) + 
  ggtitle("Highway, 100 Agents, Transaction Gossip Statistics") + xlab("Transaction Filter Cutoff") + ylab("Transaction Novelty Rate") +
  theme(plot.title = element_text(size=22)) + 
  stat_regline_equation(label.x = 20, label.y = 0.5,formula = f) + 
  geom_smooth(method = "lm", formula =f, lwd=3, se=FALSE)) + 
  geom_point(size=4) + 
  ggtitle("Highway, 100 Agents, Transaction Gossip Statistics") + xlab("Transaction Filter Cutoff") + ylab("Transaction Novelty Rate") +
  theme(plot.title = element_text(size=22)) + 
  stat_regline_equation(label.x = 20, label.y = 0.5,formula = f) + 
  geom_smooth(method = "lm", formula =f, lwd=3, se=FALSE)

cdfEquation <- data.frame(matrix(ncol = 2, nrow = 3000))
colnames(cdfEquation) <- c("x", "y")
cdfEquation$x <- x
cdfEquation$y <- ec(x)

#f = y ~ bs(x,df=5)
f = y ~ bs(x,df=5)
f = y ~ poly(x,4,raw=TRUE)
ggplot(cdfEquation, aes(x=x, y= y)) + 
  geom_point(size=5) + 
  ggtitle("Highway, 100 Agents, Item Novelty Penetration Distribution") + xlab("Time Latency") + ylab("Network Penetration %") +
  theme(plot.title = element_text(size=22)) + 
  stat_smooth( formula = f, se=FALSE,lwd=3, method = "lm") +
  stat_regline_equation(label.x=1000, label.y=0.25, aes(label =  paste(..eq.label.., sep = "~~~~")), formula = f) 




for (i in seq_along(blockFiles)){ 
  
  
  filename = p2pFiles[i]
  #print(filename)
  print(i/length(p2pFiles)) ##what percantage are we?
  ## Read data in
  #simuData <- read.csv(filename, header = TRUE)
  #simuData <- read.csv(filename)
  
  #print(blockFiles[i])
  blockFilter=strsplit(gsub("_", " ",filename), " +")[[1]][4]
  if (grepl(".csv",blockFilter,fixed="True")){
    preFilterDF$BlockFilter[i] = strtoi(substr(blockFilter,1,nchar(blockFilter)-4))
  }
  else{
    preFilterDF$BlockFilter[i] =strtoi(blockFilter)
  }
 
  
}



###just get blockID


for (i in seq_along(blockFiles)){ 
  
  
  filename = p2pFiles[i]
  #print(filename)
  print(i/length(p2pFiles)) ##what percantage are we?
  ## Read data in
  #simuData <- read.csv(filename, header = TRUE)
  #simuData <- read.csv(filename)
  
  #print(blockFiles[i])
  blockFilter=strsplit(gsub("_", " ",filename), " +")[[1]][4]
  if (grepl(".csv",blockFilter,fixed="True")){
    preFilterDF$BlockFilter[i] = strtoi(substr(blockFilter,1,nchar(blockFilter)-4))
  }
  else{
    preFilterDF$BlockFilter[i] =strtoi(blockFilter)
  }
  
  
}
