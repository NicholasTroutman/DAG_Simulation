#classes of Blocks

import sys

class BaseBlock:
    def __init__(self, txs, agents, creation_time, blockCounter, numAgents): #list of txs and agents
        self.blockTransactions = txs
        self.creators = agents
        self.creation_time = creation_time
        self.id = blockCounter
        #self.blockLinks  = [blockLinks] #move to implemented classes
        self.seen = [""]*numAgents

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
        self.maxLinks = 8
        if __blockLinks == None:
            self.blockLinks = []
        else:
            if len(__blockLinks) > maxLinks:
                sys.exit("ERROR: creating DAGBlock with too many blockLinks:\t"+str(len(__blockLinks)) )
            else: #proper number of __blockLinks
                self.blockLinks = [__blockLinks]




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
