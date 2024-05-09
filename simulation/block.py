#classes of Blocks

import sys
from operator import attrgetter
import math
import networkx as nx
import copy
import statistics
import traceback

class LightBlock:
    def __init__(self,_version, _time, _minters, _prevBlock, _nonce, _consensus, _merkle, _txs):
        self.version=_version #uint8
        self.time=_time #uint32
        self.minters=_minters #uint32[maxMinters]
        self.prevBlock=_prevBlock #PHOTON hash 32 bytes, 256 bits
        self.nonce=_nonce  #uint24
        self.consensus=_consensus#Difficulty/GeoHash 8/40
        self.merkle=_merkle #256 bits, 32 bytes
        self.txs=_txs
        #total size: 648 bit header

    def getData(self):
        #use int.tobytes(bits/8,'big'/'little')
        values = [self.version, self.time ,self.minters, self.prevBlock, self.nonce, self.consensus, self.merkle, self.txs]
        #print("\n\nVALUES:\n",values)
        #print("\n\nBytes:\n",repr(bytes([self.version])))
        return values


class BaseBlock:
    def __init__(self, txs, agents, creation_time, blockCounter, numAgents, _numRSU, _maxTxs, _minTxs): #list of txs and agents
        self.maxTxs=_maxTxs
        self.minTxs=_minTxs
        self.blockTransactions = txs
        self.id = blockCounter

        if self.id!=0:
            if len(self.blockTransactions)>self.maxTxs or len(self.blockTransactions)<self.minTxs:
                sys.exit("\n\nTOO MANY/FEW TXS per BLOCK")
        self.numTxs = len(txs)
        self.creators = agents
        self.creation_time = creation_time
        self.confirmationTime = ""
        #self.blockLinks  = [blockLinks] #move to implemented classes
        self.seen=[""]*(numAgents+_numRSU)
        #print("\n\nBlockID: ",self.id,"\tNumRSU",_numRSU,"\n\n")
        #traceback.print_stack()
        #for agent in agents:
            #self.seen[agent.id]=creation_time #assign creation time
            #print("\nblock: ",self.id," - agent: ",agent," - seen @ ",self.seen[agent.id])
            #print("\n\twhole seen:",self.seen)

        self.confirmed = False #False
        self.confirmedBlocks = []

    def __str__(self):
        return str(self.id)

    def __repr__(self):
        return str(self.id)



##block with only 1 possible link (linear)
class LinearBlock(BaseBlock):
    def __init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents, __blockLinks,  _numRSU, _maxTxs, _minTxs):
        BaseBlock.__init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents, _numRSU,_maxTxs, _minTxs) #list of txs and agents
        #print("\t\tBLOCK: ",__blockCounter, " --? ",__txs, " ~ ",self.blockTransactions)
        if __blockLinks == None:
            self.chainNum = 0
            self.blockLinks = []
        else:
            if len(__blockLinks) > 1:
                sys.exit("ERROR: creating LinearBlock with too many blockLinks:\t"+str(len(__blockLinks)) )
            else: #proper number of __blockLinks
                self.blockLinks = __blockLinks
                self.chainNum = self.blockLinks[0].chainNum + 1 #increase chainNum


##block with multiple potential links
class DAGBlock(BaseBlock):
    ##init with BlockLinks
    def __init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents, __blockLinks, __references, _numRSU,_maxTxs, _minTxs):
        BaseBlock.__init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents, _numRSU,_maxTxs, _minTxs) #list of txs and agents
        self.maxLinks = __references
        if __blockLinks == None:
            self.chainNum = 0
            self.blockLinks = []
        else:
            if len(__blockLinks) > maxLinks:
                sys.exit("ERROR: creating DAGBlock with too many blockLinks:\t"+str(len(__blockLinks)) )
            else: #proper number of __blockLinks
                self.blockLinks = [__blockLinks]
                self.chainNum = max(self.blockLinks, key= attrgetter('chainNum'))+1




##hashgraph nodes:
#timestamp, txs, self-latest-hash, gossipers-latest-hash
#1 hash is self, 2nd is gossiper's
class HashGraphBlock:
    def __init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents, __blockLinks, _numRSU,_maxTxs, _minTxs):

        BaseBlock.__init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents, _numRSU,_maxTxs, _minTxs) #list of txs and agents

        self.witness = False #first of new round = chainNum
        self.famous = False #popular
        self.timeFamous = ""

        self.confirmed=False

        self.witnessesSeen = []
        self.witnessesStronglySeen = []


        self.orderTime = ""
        self.orderAssignTime = ""

        if __blockLinks == None:
            self.chainNum = 0
            self.blockLinks = []

        else:
            if len(__blockLinks) <1 or len(__blockLinks) >2:
                sys.exit("ERROR: creating hashGraphBlock with too many blockLinks:\t"+str(len(__blockLinks)) )
            else: #proper number of __blockLinks
                self.blockLinks = __blockLinks
                self.chainNum = 0
                #TODO place divideRounds here saved to chainNUm


    def __str__(self):
        return str(self.id)

    def __repr__(self):
        return str(self.id)

    ##DEFINING EQ BRICKS SET HASHING??
    #def __eq__(self, other):
    #    return (self.id == other.id)



##Strongly See nodes
def stronglySee( source, destination, DG, numAgents):
    #print("\nStronglySee: ",source," - ",destination)
    superMajority = math.ceil(2/3*numAgents) #get superMajority
    agentsSeen = set() #make it a set,
    #agentsSeen.add(source.creators[0]) #add source

    #for shared ownership blocks
    for c in source.creators:
        agentsSeen.add(c)

    #print("\n\t\tSTRONGLY SEE: ",source," created by ",source.creators)

    for b in source.blockLinks:
        #print("\n\n",b," Strongly Sees: ",destination,"??")
        #subFound, tempAgentsSeen, tempVisitedBlocks, tempDestBlocks = _dfs(b, destination, DG, agentsSeen.copy(), superMajority, [],[])
        subFound, tempAgentsSeen, tempVisitedBlocks, tempDestBlocks = _dfs(b, destination, DG, agentsSeen.copy(), superMajority, set(),set())

        #print("subfound: ",subFound)
        #print("tempAgentSeen: ",tempAgentsSeen)
        if subFound:
            agentsSeen.update(tempAgentsSeen)
            if (len(agentsSeen)>=superMajority):
                source.witnessesStronglySeen.append(destination) #should be WITNESS NOT AGENT
                source.witnessesSeen.append(destination)#SHOULD BE WITNESS NOT AGENT
                return True

    return False





#agentsSeen is set
#visitedBlocks are blocks that dfs has already covered
#destBlocks are blocks that are visited and lead to destination
def _dfs(source, destination, DG, agentsSeen, superMajority, visitedBlocks, destBlocks): #returns Found(t/f), agentsSeen
    found=False
    subFound = False
    #visitedBlocks.append(source) ##add to visited
    visitedBlocks.add(source) ##add to visited block

    #print("\t\t\t_dfs: ",source,"/",source.creators[0]," - ",destination)
    #agentsSeen.add(source.creators[0]) #add intermediary
    for c in source.creators:
        if c not in agentsSeen:
            agentsSeen.add(c)

    ##recursively check blocks and append new agentsSeen to agentsSeen
    for block in source.blockLinks: #for each blockLink

        #print("\t\t\t\t intermediary: ",block,"\t",agentsSeen)
        if block.id == destination.id: #if match, return
            #destBlocks.append(source)
            destBlocks.add(source)
            found=True #found destination/target block
            #agentsSeen.add(block.creators[0]) #add agent
            ##for multiple owners
            for c in block.creators:
                if c not in agentsSeen:
                    agentsSeen.add(c)

            #print("\t\t\t\tDESTINATION FOUND!!!\t",agentsSeen)
            if len(agentsSeen)>=superMajority: #if supermajority, return and be done
                return True, agentsSeen, visitedBlocks, destBlocks #SUCCESS

        elif block.id > destination.id: #overshot, end: #recursion -- block.id>destination.id
            #print("\t\t\t\t\t",source," PRE _DFS: ",agentsSeen)



            if block in visitedBlocks:
                if block in destBlocks:
                    found=True
                    if c not in agentsSeen:
                        for c in block.creators:
                                agentsSeen.add(c)
                    if len(agentsSeen)>=superMajority: #if supermajority, return and be done
                        return True, agentsSeen, visitedBlocks, destBlocks #SUCCESS
            else: ##NOT IN VISITEDBLOCKS
                subFound, tempAgentsSeen, visitedBlocks, destBlocks =  _dfs(block, destination, DG, agentsSeen.copy(), superMajority, visitedBlocks.copy(), destBlocks.copy()) #add new agents

                #print("\t\t\t\t\t",source," Rec _DFS: ",subFound," - ",tempAgentsSeen, " -->",agentsSeen)
                if subFound == True: #success
                    #destBlocks.append(source)
                    destBlocks.add(source) ##This is a destBlock
                    #print("SUBFOUND==TRUE")
                    found=True
                    agentsSeen.update(tempAgentsSeen) #add to self
                    if len(agentsSeen)>=superMajority: #
                        return True, agentsSeen, visitedBlocks, destBlocks

    return found, agentsSeen, visitedBlocks, destBlocks

##Strongly See nodes
def see( source, destination, DG):
    #superMajority = math.ceil(2/3*numAgents) #get superMajority
    #agentsSeen = set() #make it a set,
    #agentsSeen.add(source.creators[0]) #add source
    #print("\n\t\tSTRONGLY SEE: ",source," created by ",source.creators)

    for b in source.blockLinks:
        subFound, tempVisitedBlocks = _dfsSeen(b, destination, DG, []) #tempVisitedBlocks not used
        if subFound:
            return True
    #print("\t\tfunction StronglySee FAILED --> agentsSeen:\t",agentsSeen)
    return False

def _dfsSeen(source, destination, DG, visitedBlocks): #returns Found(t/f), agentsSeen
    ##recursively check blocks and append new agentsSeen to agentsSeen
    visitedBlocks.append(source)
    for block in source.blockLinks: #for each blockLink
        #print("\t\t\t\t intermediary: ",block,"\t",agentsSeen)
        if block.id == destination.id: #if match, return
            return True, visitedBlocks

        elif block.id > destination.id and block not in visitedBlocks: #overshot, end: #recursion -- block.id>destination.id
            #print("\t\t\t\t\t",source," PRE _DFS: ",agentsSeen)
            subFound, visitedBlocks =  _dfsSeen(block, destination, DG, visitedBlocks.copy()) #add new agents
            if subFound:
                return True, visitedBlocks
        #elif block.id<destination.id, then ignore

    return False, visitedBlocks



#BreadthFirst Search
def shortestDestinationPredecessorTime(source, destination, DG):
    maxLinks = source.id-destination.id
    visitedBlocks = set()

    orderTimes = []

    branch =source.blockLinks

    if destination in branch:
        #print("\tDESTINATION: ",destination, " -- ", source.creation_time, " from ",source)
        return source.creation_time #done

    for b in branch:
        visitedBlocks.add(b) #add to visitedBlocks set

    ancestorTimes = []
    #print("\nShortestDest: ",source," ~ ",destination)
    ##NEXT GENERATION
    for i in range(0,maxLinks): #must be less than 0 maxLinks in
        #print(branch)
        if len(branch)==0:
            if len(orderTimes)==0:
                return -1
            else:
                return min(orderTimes)
        #branch is list of blocks
        nextBranches = [] #nextBranch for next generation of for loop
        ##FOR EACH BLOCK IN CURRENT GENERATION
        for tip in branch: #for each block in branch
            tipLinks=tip.blockLinks #save blockLinks to tipLinks

            ##FOR EACH TIP IN BLOCK of CURRENT GENERATION
            for tipLink in tipLinks: #for each next gen tip/block
                if tipLink == destination: #IF match to dstination, save to orderTimes and continue
                    #print("\tDESTINATION: ",destination, " -- ", tip.creation_time, " from ",tip)
                    orderTimes.append(tip.creation_time) #return destination's ancestor's creation time

                    if len(orderTimes)==2: #if both orderTimes found, then do this
                        return min(orderTimes)

                elif tipLink.id > destination.id: #still room, traverse
                    if tipLink not in visitedBlocks: #Is it visited
                        visitedBlocks.add(tipLink)
                        nextBranches.append(tipLink)

                    #else: visited and do nothing

        branch=nextBranches #move to next generation

    return -1

def intersection(lst1, lst2):
    lst3 = [value for value in lst1 if value in lst2]
    return lst3

##assign r value to source block, must strongly see superMajority of witnesses
#witnesses  = {}, key = round#, value is list of witnesses
def divideRounds(source, witnesses, DG,  numAgents, largestR):
    #print("\n\nSTART DIVIDE ROUNDS")
    superMajority = math.ceil(2/3*numAgents) #get superMajority #

    #lastRoundsMax is dictionary {Agent.ID : maximumValue of agent's linkedBlocks' chainNum}
    #EXAMPLE: newBlock -->[block 1, block2], chainNum's 1 and 2 respectively, both are created by agent 0, then lastRoundsMax[0]=2
    lastRoundsMax = {source.creators[0]: 0, source.creators[1]: 0}
    minRound = 0
    for b in source.blockLinks: #for each linked Block 2
        minRound = max(b.chainNum, minRound)
        for bc in b.creators: #for each creator in linked block 2*2
            if bc in source.creators: #if creator in source's creators
                lastRoundsMax[bc] = max(lastRoundsMax[bc], b.chainNum) #add to dictionary of lastRoundsMax if larger than existing

    #now lastRounds is list of all own created blockLinks'

    #print("\n\ndivideRounds Start: ",source.id)
    #print("minround: ",minRound, " largestR+1",largestR+1 )
    ##Check if newBlock's chainNum is higher than linked blocks' chainNum
    for r in reversed(range(minRound,largestR+1)): #for R->0
        #print("\n\tr: ",r," witness: ",witnesses[r])
        agentWitnesses = set()
        for blockWitness in witnesses[r]:
            for aWitness in blockWitness.creators:
                agentWitnesses.add(aWitness)
        #if len(witnesses[r])>=superMajority:
        if len(agentWitnesses)>=superMajority: #CHANGE TO SUPERMAJORITY OF AGENTS
            #print("\n\tRound: ",r)
            #rscore = 0
            rscore = set()

            for w in witnesses[r]: #loop through witnesses individually
                #source.witnessesSeen=[]
                #print("\n\tWitness ? ",w)
                if stronglySee(source, w, DG, numAgents):
                    #rscore += 1
                    for creatorAgent in w.creators:
                        rscore.add(creatorAgent)
                    #print("\tStrongly Sees: ",w," - ",rscore)
                    if len(rscore) >= superMajority: #Found chainNum
                        source.chainNum = w.chainNum + 1 #assign chainNum
                        #print("\tFound ChainNum: ",source.chainNum)

                        #check if witness' last block was same R
                        for b in source.blockLinks: #for every linked block
                            #if b.creators == source.creators: #if same creator
                            if intersection(b.creators, source.creators):
                                if b.chainNum < source.chainNum: #if smaller chainNum, then witness!
                                    source.witness = True #source block  witness
                                    #print("\tIS WITNESS: ",source.witness)

                                    ##Does it see all witnesses?
                                    #print("\n\n\n\nSeen Witnesses: ",source.witnessesSeen)
                                    #print(set(witnesses[r]), " - ",source.witnessesSeen)
                                    for wr in set(witnesses[r]):
                                        seen = False
                                        for ws in source.witnessesSeen:
                                            if wr.id ==ws.id: #
                                                seen = True
                                        if seen==False:
                                            #print("ADDING: ",wr)
                                            if see(source, wr, DG):
                                                source.witnessesSeen.append(wr)

                                    for ws in source.witnessesSeen:
                                        if ws.chainNum == source.chainNum:
                                            source.witnessesSeen.remove(ws)
                                    for wss in source.witnessesStronglySeen:
                                        if wss.chainNum == source.chainNum:
                                            source.witnessesStronglySeen.remove(wss)


                                    #print("After SeeStrongly: ",source.witnessesStronglySeen)
                                    #print("After Sees: ",source.witnessesSeen, "\n\n\n")

                        return #done

    #have not found chainnum, same as previous blockLink's chainNum
    source.chainNum = minRound
    #print("\tLastRoundMax: ",lastRoundsMax)
    for lrm in lastRoundsMax: #for each lastRoundsMax (belonging to different agents)
        #print("\tlrm: ",lastRoundsMax[lrm])
        if minRound != lastRoundsMax[lrm]: #If minRound of blockLinks != lastRoundmax of any creator, then the newBlock is a witness!!
            source.witness = True
            #assign sees/stronglySees
            for b in source.blockLinks:
                if b.chainNum==source.chainNum: #also a witness
                    source.witnessesSeen += b.witnessesSeen
                    source.witnessesStronglySeen += b.witnessesStronglySeen

            #get unique witnesses only
            source.witnessesSeen=list(set(source.witnessesSeen))
            source.witnessesStronglySeen = list(set(source.witnessesStronglySeen))

            #fill in rest from witnesses
            #print("\n\n")
            #print(source.chainNum-1)
            #print(witnesses)
            #print(lastRoundsMax)
            for w in set(witnesses[source.chainNum-1]):
                stronglySeen = False
                for wss in source.witnessesStronglySeen:
                    if w.id ==wss.id: #
                        stronglySeen = True
                if stronglySeen==False:
                    #print("ADDING: ",w)
                    if stronglySee(source, w, DG, numAgents):
                        source.witnessesStronglySeen.append(w)
                        if w not in source.witnessesSeen:
                            source.witnessesSeen.append(w)
                    else:
                        if w not in source.witnessesSeen:
                            if see(source, w, DG):
                                source.witnessesSeen.append(w)
            #get unique witnesses only
            source.witnessesSeen=list(set(source.witnessesSeen))
            source.witnessesStronglySeen = list(set(source.witnessesStronglySeen))

            for ws in source.witnessesSeen:
                if ws.chainNum == source.chainNum:
                    source.witnessesSeen.remove(ws)
            for wss in source.witnessesStronglySeen:
                if wss.chainNum == source.chainNum:
                    source.witnessesStronglySeen.remove(wss)

            break #only need to call newBlock witness once

        #print("\n\tINHERITED WITNESS: ",source.witnessesStronglySeen, " - ",source.witnessesSeen)

##determine if target witness is famous by beeing SEEN BY SUPERMAJORITY OF later witnesses
def isFamous(target, witnesses, DG, numAgents, time):
    if target.famous==True:
        return
    else:
        #sys.exit("ISFAMOUS DEBUG")
        #print("\nisFamous: ", target," - ",witnesses)
        superMajority = math.ceil(2/3*numAgents) #get superMajority
        #numSeen=0
        numSeen = set()
        for w in witnesses: #for each witness
            #print("\tWitness: ",w," Target: ",target," - ",w.witnessesSeen)
            if target in w.witnessesSeen:
                #numSeen+=1 #increment
                for creatorAgent in w.creators:
                    numSeen.add(creatorAgent)
                #print("\tSeen: ",numSeen)
                if len(numSeen) >= superMajority: #Does it meet supermajority?
                    target.famous= True #FAMOUS WITNESS
                    #print("\t\t\t", target," is now famous: ",time)
                    target.timeFamous = time
                    #print("\n\n\n\nFOUND FAMOUS: ",target)
        #return True


##Order events before famous witnesses
def orderEvent(famousWitnesses, target, DG, numAgents, time):
    #print("\n\tTarget Confirmed: ",target.confirmed)
    if (target.confirmed == False): #needs ordering
        #superMajority = math.ceil(2/3*numAgents) #get superMajority
        predecessorTimes = []
        for fw in famousWitnesses:
            predecessorTime = shortestDestinationPredecessorTime(fw, target, DG)
            if predecessorTime>0: #Valid
                predecessorTimes.append(predecessorTime) #save it

        #print("\n\tPredecessor Times:\t",predecessorTimes)
        if (len(predecessorTimes)>= math.ceil(1/3*numAgents)): #enough for ordering, breaks hashgraph specification
            #print("\n\nTarget: ",target, " predecessorTimes: ",predecessorTimes, "  median = ",statistics.median(predecessorTimes))
            target.orderTime = statistics.median(predecessorTimes)
            target.orderAssignTime = time
            target.confirmationTime= time
            target.confirmed = True
            return True
        else: #Target is unconfirmed & not enough to give order
            return False
    else:
        return True #target is already confirmed
    sys.exit("ERROR ORDER EVENTS FAILED")


##Order entire round-1 + stragglers compared to famousWitnesses
##Order all events previous to generation
def orderRange(famousWitnesses, stragglers, DG, witnesses, numAgents, time, blocks):#, generation):

    #print("\n\nStart orderRange: ",time, " stragglers: ",stragglers, " famousWitnesses: ",famousWitnesses)
    #all stragglers:
    newStragglers=[]
    for straggler in stragglers:
        if orderEvent(famousWitnesses, straggler, DG, numAgents, time) == False: #if false, add to stragglers
            newStragglers.append(straggler)

    #all txs between currentFamousWitnesses-lastWitnesses
    lastRound = famousWitnesses[0].chainNum-3 #all events that we want to check (PREVIOUS GENERATION)
    #print("lastRound: ",lastRound)
    #print("witnesses: ",witnesses)
    if lastRound<1:
        lastRound=0
    #print(lastRound)
    #minBlock = min(witnesses[lastRound-1], key= attrgetter('id')).
    minBlock = min(witnesses[lastRound], key= attrgetter('id')).id
    maxBlock = max(famousWitnesses, key= attrgetter('id')).id -1  #smaller than biggest famous witness


    #print("\nRANGE: ",minBlock," - ",maxBlock)
    for i in range(minBlock,maxBlock):
        #print("\torder block ? ",blocks[i].id)
        if orderEvent(famousWitnesses, blocks[i], DG, numAgents, time ) == False: #if false, add to stragglers
            newStragglers.append(blocks[i])
            #print("\t\tNot Ordered, straggler -",blocks[i])

    newStragglers = list(set(newStragglers)) #remove redundancy
    return newStragglers


##Function to confirm blocks N links behind block
def confirmBlocks(block, confirmationNumber):
    #print("\n\nconfirm block: ",block)
    #print("\tSTART confirmBlocks: ", confirmationNumber)

    targetBlocks = [block]
    for i in range(0,confirmationNumber):
        #print("\t",i," ",targetBlocks)
        tempBlocks = []
        for targetBlock in targetBlocks:
            #print("\ttargetBlock: ",targetBlock," linked --> ",targetBlock.blockLinks)
            if (targetBlock.blockLinks != None and bool(targetBlock.blockLinks) == True):
                #print("\t\tappend")
                for bl in targetBlock.blockLinks:
                    tempBlocks.append(bl)


        #print("confirmed Blocks: ",tempBlocks)

        targetBlocks = list(set(tempBlocks))
        #print("CONFIRMATION Block links:\t",targetBlocks)
    #set blocks to confirmed
    #print("\tConfirming Blocks: ",targetBlocks)
    for cblock in targetBlocks:
        if cblock.confirmed != True:
            cblock.confirmed=True
            cblock.confirmationTime = block.creation_time

    block.confirmedBlocks = targetBlocks ##confirmed blocks


    return targetBlocks



#BreadthFirst Search
def BFS(source, destination, DG):
    #print("\nStart BFS: ",source, " ~ ",destination)
    maxLinks = source.id-destination.id
    visitedBlocks = set()
    orderTimes = []
    branch =source.blockLinks

    if destination in branch:
        #print("\n\tLINKED: ",source, " links: ",branch," FOUND ",destination)
        return True #done

    for b in branch:
        visitedBlocks.add(b) #add to visitedBlocks set

    ##NEXT GENERATION
    for i in range(0,maxLinks): #must be less than 0 maxLinks in
        #print(branch)
        if len(branch)==0:
            return False
        #branch is list of blocks
        nextBranches = [] #nextBranch for next generation of for loop
        ##FOR EACH BLOCK IN CURRENT GENERATION
        for tip in branch: #for each block in branch
            tipLinks=tip.blockLinks #save blockLinks to tipLinks

            ##FOR EACH TIP IN BLOCK of CURRENT GENERATION
            for tipLink in tipLinks: #for each next gen tip/block
                if tipLink == destination: #IF match to dstination, save to orderTimes and continue
                    #print("\tDESTINATION: ",source, " -- ", tipLink, " -- ", destination)
                    return True #FOUND IT

                elif tipLink.id > destination.id: #still room, traverse
                    if tipLink not in visitedBlocks: #Is it
                        nextBranches.append(tipLink)
                        visitedBlocks.add(tipLink)

                    #else: visited and do nothing

        branch=nextBranches #move to next generation

    return False
