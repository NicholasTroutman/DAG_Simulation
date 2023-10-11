#classes of Blocks

import sys
from operator import attrgetter

class BaseBlock:
    def __init__(self, txs, agents, creation_time, blockCounter, numAgents): #list of txs and agents
        self.blockTransactions = txs
        self.creators = agents
        self.creation_time = creation_time
        self.confirmationTime = ""
        self.id = blockCounter
        #self.blockLinks  = [blockLinks] #move to implemented classes
        self.seen = [""]*numAgents
        self.confirmed = False
        self.confirmedBlocks = []

    def __str__(self):
        return str(self.id)

    def __repr__(self):
        return str(self.id)



##block with only 1 possible link (linear)
class LinearBlock(BaseBlock):
    def __init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents, __blockLinks):

        BaseBlock.__init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents) #list of txs and agents

        if __blockLinks == None:
            self.chainNum = 0
            self.blockLinks = []
        else:
            if len(__blockLinks) > 1:
                sys.exit("ERROR: creating LinearBlock with too many blockLinks:\t"+str(len(__blockLinks)) )
            else: #proper number of __blockLinks
                self.blockLinks = [__blockLinks]
                self.chainNum = self.blockLinks[0].chainNum + 1 #increase chainNum


##block with multiple potential links
class DAGBlock(BaseBlock):
    ##init with BlockLinks
    def __init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents, __blockLinks):
        BaseBlock.__init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents) #list of txs and agents
        self.maxLinks = 3
        if __blockLinks == None:
            self.chainNum = 0
            self.blockLinks = []
        else:
            if len(__blockLinks) > maxLinks:
                sys.exit("ERROR: creating DAGBlock with too many blockLinks:\t"+str(len(__blockLinks)) )
            else: #proper number of __blockLinks
                self.blockLinks = [__blockLinks]
                self.chainNum = max(self.blockLinks, key= attrgetter('chainNum')+1)




class BaseStationBlock:
    def __init__(self, txs, creationTime, blockCounter, numAgents, prevBlock): #list of txs and agents
        self.blockTransactions = txs
        #self.creators = agents
        self.creation_time = creation_time
        self.id = blockCounter
        #self.blockLinks  = []
        self.seen = [""]*numAgents
        self.blockLinks = [prevBlock] #will realistically be only 1 link but this is the standardized format

    def __str__(self):
        return str(self.id)

    def __repr__(self):
        return str(self.id)



##Function to confirm blocks N links behind block
def confirmBlocks(block):
    #print("\n\nconfirm block: ",block)
    #print("\nSTART confirmBlocks:")
    #print("CONFIRMING BLOCK")
    confirmationNumber = 3

    targetBlocks = [block]
    for i in range(0,confirmationNumber):
        #print(i," ",targetBlocks)
        tempBlocks = []
        for targetBlock in targetBlocks:
            #print("\ttargetBlock: ",targetBlock," linked --> ",targetBlock.blockLinks)
            if (targetBlock.blockLinks != None and bool(targetBlock.blockLinks) == True):
                #print("\t\tappend")
                for block in targetBlock.blockLinks:
                    tempBlocks.append(block)


        #print("confirmed Blocks: ",tempBlocks)

        targetBlocks = list(set(tempBlocks))
        #print("CONFIRMATION Block links:\t",targetBlocks)
    #set blocks to confirmed
    for cblock in targetBlocks:
        cblock.confirmed=True
        cblock.confirmationTime = block.creation_time

    block.confirmedBlocks = targetBlocks ##confirmed blocks
