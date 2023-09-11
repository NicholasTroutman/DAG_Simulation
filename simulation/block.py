#classes of Blocks

class Block:
    def __init__(self, txs, agents, creation_time, blockCounter, numAgents): #list of txs and agents
        self.blockTransactions = txs
        self.creators = agents
        self.creation_time = creation_time
        self.id = blockCounter
        self.blockLinks  = []
        self.seen = [""]*numAgents

    def __init__(self, txs, agents, creation_time, blockCounter, numAgents, blockLinks): #list of txs and agents
        self.blockTransactions = txs
        self.creators = agents
        self.creation_time = creation_time
        self.id = blockCounter
        self.blockLinks  = [blockLinks]
        self.seen = [""]*numAgents
        
    def __str__(self):
        return str(self.id)

    def __repr__(self):
        return str(self.id)
   

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