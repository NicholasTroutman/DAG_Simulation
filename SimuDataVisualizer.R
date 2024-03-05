##Nicholas Troutman
##Visualization of CSV SimuData

##\\wsl$\Ubuntu\home\ntroutm\git\DAG_Simulation\SimuData

library(ggplot2)
library(reshape2)


##upload csv as simuData
simuData=d50
#simuData=dhtTest



# Section PreProcesssing -------------
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
shareTimesMedian = apply(agentSeendf, 1, FUN = median)  - apply(agentSeendf, 1, FUN = min)

simuData$maxShareTime = c(0,shareTimes)
d <- density(simuData$maxShareTime, na.rm=TRUE)
dmedian <- density(shareTimesMedian, na.rm=TRUE)
dx <- diff(d$x)[1]

h<-hist(shareTimes, breaks=30)

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
confirmedBlocks = simuData[simuData$confirmedBlock=="TRUE",]
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
  df = head(df,-20)
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
  ggtitle(paste("Downtown 2 Refs, Minting Time ~ ETC")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  scale_color_gradient(low="blue", high="red") + 
  ylim(0,200)+
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.8)

ggplot(refDataDownTown2refs, aes( y=expectedConfirmation, x=numAgents, group=group, col=group, label=group), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  geom_label_repel(aes(label = group),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')+
  ggtitle(paste("Downtown 2 Refs, Minting Time ~ ETC")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Number of Agents") + ylab("Expected Transaction Confirmation Time") +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  scale_color_gradient(low="blue", high="red") + 
  ylim(0,200)



#INDIVIDUAL WORKS
ggplot(refDataDownTown2refs, aes( y=expectedConfirmation, x=numAgents, group=group, col=group, label=group), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste("Downtown 2 Refs, Minting Time ~ ETC")) +
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
  ggtitle(paste("Downtown 3 Refs, Minting Time ~ ETC")) +
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
  ggtitle(paste("Downtown DAG PoL ~ ETC")) +
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
  ggtitle(paste("Downtown 3 Refs, Minting Time ~ ETC")) +
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
  ggtitle(paste("Downtown DAG 200 Agents, Minting Time ~ ETC")) +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.2) +
  scale_color_manual(name="Number of Refs",values=c('darkred','darkblue'), labels=c("3","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime)))) + 
  ylim(0,1000)


##Plot 2/3/4 attachment time

ggplot(refDataHwy, aes( y=expectedConfirmation, x=blockTime, group=refs, col = ifelse(refs == 2, ifelse(refs == 3, 'green','blue'),'red')), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle(paste("Downtown DAG 200 Agents, Minting Time ~ ETC")) +
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
  geom_line(data = X30v100,  aes(x = time, y = numTxs, color = "30Tx")) +
  geom_line(data = X45V100,  aes(x = time, y = numTxs, color = "45Tx")) +
  geom_line(data = X130v100,  aes(x = time, y = numTxs, color = "130Tx")) +
  #geom_line(data = X130v100,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125, color = "130Block")) 
  #geom_line(data = X30v100,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125, color = "30Block")) +
  #geom_line(data = X130v100,  aes(x = time, y = (blockTxStorage+blockStorage)*0.000125, color = "130Block")) +
  #geom_line(data = X30v100,  aes(x = time, y = (txStorage)*0.000125, color = "30Tx")) +
  #geom_line(data = X130v100,  aes(x = time, y = (txStorage)*0.000125, color = "130Tx")) +
  xlab('Time') +
  ylab('Storage in KiloBytes') + 
  ggtitle("Block Storage w/ Different Block Tx Limits") + 
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

maxTxsStats <- function(){

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
for (nAgents in 1:nrow(file_lists)){
  

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
  df = head(df,-20)
  
  agentSeen = grepl("agent_", names(df)) #index for 
  agentSeendf = data.frame(df[agentSeen])[-1,]
  shareTimes = apply(agentSeendf, 1, FUN = max)  - apply(agentSeendf, 1, FUN = min)
  confirmedBlocks = df[df$confirmedBlock=="True",]
  unconfirmedBlocks = df[df$confirmedBlock=="False",]
  uniquetxs=c()
  
  
  ##Get tx Confirmation Time and Count
  for (j in 2:nrow(confirmedBlocks)){
    tx_creation_time = strsplit(confirmedBlocks$transaction_creation_time[j], ",")[[1]] #returns 1, 2 3
    txs = strsplit(confirmedBlocks$block_transactions[j], ",")[[1]] #returns tx ids, 1,2,3,4....
    #print(tx_creation_time)
    for (tx in txs){
      t = strtoi(gsub("[^0-9.-]", "", tx))
      #print(t)
      uniquetxs = append(uniquetxs, t)
    }
    uniquetxs = unique(uniquetxs)
    
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
  for (j in 2:nrow(unconfirmedBlocks)){
    txs = strsplit(unconfirmedBlocks$block_transactions[j], ",")[[1]] #returns tx ids, 1,2,3,4....
    
    for (tx in txs){
      t = strtoi(gsub("[^0-9.-]", "", tx))
      #print(t)
      unconfirmedTxs = append(unconfirmedTxs, t)
    }
  unconfirmedTxs=unique(unconfirmedTxs)
    
  }
  
  
  totTxs = length(unique(c(unconfirmedTxs,uniquetxs)))
  confirmedTxsRate = length(uniquetxs)/totTxs
  
  ##save data
  #refData$blockTime[i] = as.numeric(strsplit(gsub("[^0-9.-]", " ", file_list[[i]]), " +")[[1]][5])
  ##"dlt", "maps","maxTxs", "numAgents","NumTxs","TxsPerBlock","txConfirmationTime", "cTime")
  refData$txConfirmationRate[i] = confirmedTxsRate
  refData$unconfirmedUniqueTxs[i] = list(unconfirmedTxs)
  refData$unconfirmedNumUnique[i] = length(unconfirmedTxs)
  refData$uniqueTxs[i] = list(uniquetxs)
  refData$numUnique[i] = length(uniquetxs)
  refData$cTime[i] = list(na.omit(txConfirmationTime))
  refData$dlt[i] =  strsplit(gsub("_", " ",filename), " +")[[1]][2]
  refData$maps[i] = strsplit(gsub("_", " ",filename), " +")[[1]][13]
  #refData$numTxs[i] =  strsplit(gsub("_", " ",filename), " +")[[1]][5]
  refData$NumTxs[i] = length(txConfirmationTime)
  refData$TxsPerBlock[i] = length(txConfirmationTime)/(nrow(df)-1)
  refData$maxTxs[i] = strtoi(gsub("\\..*", "",strsplit(gsub("_", " ",filename), " +")[[1]][27]))
  refData$txConfirmationTime[i] = mean(refData$cTime[i][[1]], na.rm=TRUE)
  refData$numAgents[i]=strsplit(gsub("_", " ",filename), " +")[[1]][9]
  
  
  orphanRate = sum(df$confirmedBlock=="False")/nrow(df)
  refData$orphanageRate[i]=orphanRate
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
  blockSize=8+32+32+32+8+192*2
  #txSize = (32+4+3*36)+  (4*16) + (256) #tx data + tag
  txSize = (32+4+3*36)+  (4*16) + (192) #tx data + tag #truncate to 192 bits, or 24 bytes
  
  #df <- read.csv(filename, header = TRUE)
  data=read.csv(currentVFiles[i], header = TRUE)
  
  #Preprocessing
  data$blockStorage=data$numBlocks*blockSize
  data$txStorage=data$numTxs*txSize
  data$blockTxStorage=data$blockTxs*txSize
  data$totStorage=(data$blockTxStorage+data$blockStorage)*0.000125+data$txStorage*0.000125
  
  refData$maxStorage[i]=max(data$totStorage)
  refData$meanStorage[i]=mean(data$totStorage)
  
}

volumePreProcessing <- function(data){
  
  #Preprocessing
  data$blockStorage=data$numBlocks*blockSize
  data$txStorage=data$numTxs*txSize
  data$blockTxStorage=data$blockTxs*txSize
  data$totStorage=(data$blockTxStorage+data$blockStorage)*0.000125+data$txStorage*0.000125
  return(data)
}



ratio=floor(max(refData$expectedConfirmation)/max(refData$maxStorage))
p = ggplot() + 
  #geom_line(data = refData, size=2, aes(x = maxTxs, y = TxsPerBlock*40, color = "Txs/Block")) +
  geom_line(data = refData, size=2, aes(x = maxTxs, y = expectedConfirmation, color = "Confirmation Time")) +
  geom_line(data = refData, size=2, aes(x = maxTxs, y = maxStorage*ratio, color = "Maximum Storage")) +
  geom_line(data = refData, size=2, aes(x = maxTxs, y = meanStorage*ratio, color = "Mean Storage")) +
  xlab('Max Txs/Block') +
  ylab('') + 
  ggtitle(paste("Linear PoW Balance, # Agents: ",refData$numAgents[1])) + 
  labs(colour="Metrics")+
  scale_color_manual(values=c("blue","red", "green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,2), legend.position=c(0.9,0.9)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Expected Confirmation Time",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./ratio, name="Total Storage (KB)")
  )
print(p)

} ##end of file_lists loop


}

library(ggplot2)
#tx/Block
p = ggplot() + 
  geom_line(data = refData, size=2, aes(x = maxTxs, y = TxsPerBlock*40, color = "Txs/Block")) +
  geom_line(data = refData, size=2, aes(x = maxTxs, y = expectedConfirmation, color = "Confirmation Time")) +
  geom_line(data = refData, size=2, aes(x = maxTxs, y = totStorage, color = "totStorage")) +
  xlab('Max Txs/Block') +
  ylab('') + 
  ggtitle("Linear DLT, Block Size Statistics") + 
  labs(colour="Max Txs / Block")+
  scale_color_manual(values=c("blue","red","green"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Expected Confirmation Time",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./40, name="Txs/Block")
  )
print(p)


p = ggplot() + 
  geom_line(data = refData, size=2, aes(x = maxTxs, y = TxsPerBlock*40, color = "Txs/Block")) +
  #geom_line(data = refData, size=2, aes(x = maxTxs, y = expectedConfirmation, color = "Confirmation Time")) +
  geom_line(data = refData, size=2, aes(x = maxTxs, y = totStorage, color = "totStorage")) +
  xlab('Max Txs/Block') +
  ylab('') + 
  ggtitle("Linear DLT, Block Size Statistics") + 
  labs(colour="Max Txs / Block")+
  scale_color_manual(values=c("blue","red"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "DLT Storage in KB",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./40, name="Txs/Block")
  )
print(p)


ratio=floor(max(refData$expectedConfirmation)/max(refData$totStorage))
p = ggplot() + 
  #geom_line(data = refData, size=2, aes(x = maxTxs, y = TxsPerBlock*40, color = "Txs/Block")) +
  geom_line(data = refData, size=2, aes(x = maxTxs, y = expectedConfirmation, color = "Confirmation Time")) +
  geom_line(data = refData, size=2, aes(x = maxTxs, y = totStorage*ratio, color = "totStorage")) +
  xlab('Max Txs/Block') +
  ylab('') + 
  ggtitle(paste("Linear PoW Accumulating, # Agents: ",refData$numAgents[1])) + 
  labs(colour="Max Txs / Block")+
  scale_color_manual(values=c("blue","red"))+
  theme(plot.title = element_text(size=30),legend.justification=c(1,2), legend.position=c(0.9,0.9)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total Storage",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./ratio, name="Txs/Block")
  )
print(p)

