import hashlib
import sys
from simulation.node import Node


class Agent(Node):
    def __init__(self, counter):
        Node.__init__(self, counter)
        ##These variables are mobile agent specific
        self.coordinates= [] #list of double x and y coordinates in double [x,y]
        self.past_coordinates=[]
        self.destination=[]
        self.prev_dest=[]
        self.vector=[]
        #self.speed=40 #testing purpose
        #self.speed=7.74 #NICK HARDCODED THIS for Houston
        self.speed=0.541*60 #NICK HARDCODED THIS for HoustonHwy
        #self.distance = 0 #REMOVE ME LATER
        #self.estimatedDistance = 0 #remove me later
        #self.destinationTime = [] #DEBUG PURPOSES
        #self.estimatedTimes = []

        #self.hash =int(hashlib.sha256(str(self.id).encode('utf-8')).hexdigest(), 16) % 10**8
        #print(self.id, "  ",self.hash)


##Don't know if these need to be specified
    #def __str__(self):
     #   return str(self.id)

    #def __repr__(self):
      #  return str(self.id)


class DHTAgent(Agent):
    def __init__(self,counter):
        Agent.__init__(self, counter)

        self.hash =int(hashlib.sha256(str(self.id).encode('utf-8')).hexdigest(), 16) % 10**8
        #print(self.id, "  ",self.hash)
        self.lastBlock = None
        self.lastTx = None


    ##DHT functions
    def signTxs(self,time):
        #collect validated Txs
        signedTxs = []
        for tx in self._visible_transactions:
            #print("\ntx: ",tx.id)
            #print("\tOutTx: ",tx.outTx)
            #print("\tOutTx - Agent: ",tx.outTx.agent)
            if tx.outTx==None: #
                if tx.verifier == self:
                    #print("\nStart Tx: ",tx.id," - Verifier: ",tx.verifier)
                    signedTxs.append(tx)
            elif tx.outTx.agent ==self:
                signedTxs.append(tx)

        #move signedTxs to confirmed_txs
        self.add_submitted_transactions(signedTxs,time)
        #print("\nSubmitted txs: ")
        #print(self._submitted_transactions)

    ##DHT functions
    def validateTxs(self,time):
        #collect validated Txs
        validatedTxs = []
        #print("\nValidating: ",self.id)
        for tx in self._submitted_transactions:
            #print("\tassigendValidator: ",tx.verifier)
            if tx.verifier ==self:
                #sys.exit("VERIFIED + DEBUG")
                validatedTxs.append(tx)

        #print("\tValidatedTxs: ",validatedTxs)
        return validatedTxs
