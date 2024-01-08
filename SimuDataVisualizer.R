##Nicholas Troutman
##Visualization of CSV SimuData

##\\wsl$\Ubuntu\home\ntroutm\git\DAG_Simulation\SimuData

library(ggplot2)
library(reshape2)


##upload csv as simuData
simuData=testtest
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
confirmedBlocks = simuData[simuData$confirmedBlock=="True",]
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

h<-hist(txConfirmationTime, breaks=40)


d <- density(txConfirmationTime, na.rm=TRUE)
lines(x=d$x,y=d$y*length(txConfirmationTime)*(h$breaks[2] - h$breaks[1]))











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





# Section Get Penetration Distributionsfrom folder ----------


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
  refData$blockTime[i] = as.numeric(strsplit(gsub("[^0-9.-]", " ", file_list[[i]]), " +")[[1]][5])
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
  refData$expectedConfirmation[i] = 0
  for (n in 1:30){
    temp= sum( (meanTime+ reSubmitTime*(n-1)) * (1-refData$orphanRate[i])*(refData$orphanRate[i]**(n-1) ))
    econfList = append(econfList,temp)
    refData$expectedConfirmation[i] = refData$expectedConfirmation[i] + temp
  }
  
  #plot(seq_along(econfList), econfList, type="l", main=paste("BlockTime: ",refData$blockTime[i], " orphanRate: ",refData$orphanRate[i]))
  
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
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime))))


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
  ggtitle("50 AGents Minting Time ~ Expected Transaction Confirmation Time") +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.5) +
  scale_color_manual(name="Number of Agents",values=c('darkred','darkgreen','darkblue'), labels=c("3","4","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime))))



ggplot(refDataDownTown100, aes( y=expectedConfirmation, x=blockTime,group=refs, col = ifelse(refs == 2,'red',ifelse(refs==3,'blue','green'))), pch = 19) + 
  geom_point(position="dodge", stat="identity") +
  ggtitle("100 AGents Minting Time ~ Expected Transaction Confirmation Time") +
  theme(plot.title = element_text(size=30),legend.justification=c(1,1), legend.position=c(0.9,0.9)) + 
  xlab("Block Minting Time") + ylab("Expected Transaction Confirmation Time") +
  geom_smooth(se=FALSE,  formula=y ~ poly(x, 1, raw=TRUE), span=0.5) +
  scale_color_manual(name="Number of Agents",values=c('darkred','darkgreen','darkblue'), labels=c("3","4","2")) +
  scale_x_continuous(breaks = pretty(refData$blockTime, n = length(unique(refData$blockTime))))
