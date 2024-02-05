import hashlib
import sys
from simulation.node import Node


class Agent(Node):
    def __init__(self, counter, _map):
        Node.__init__(self, counter, _map)
        ##These variables are mobile agent specific
        #self.coordinates= [] #list of double x and y coordinates in double [x,y]
        self.past_coordinates=[]
        self.destination=[]
        self.prev_dest=[]
        self.vector=[]

        self.lastMintedBlock = 0
        #self.speed=40 #testing purpose
        #self.speed=7.74 #NICK HARDCODED THIS for Houston
        speedDic = {"Houstonredblue.jpg": 7.744, "HoustonHwyredblue.jpg": 0.541*60} #0.541*60=32
        ##Houston Downtown Distance: 500ft/88 pixels = 5.681818 ft/
        ##Houston Downtown Speed: 30mph =  44ft/sec --> (44 ft/sec)/ (5.6818 ft/pixel) = 7.74400000248 pixels/second

        ##Houstonhwy Distance: 2*5280ft/60 pixels = 176 ft/
        ##HoustonHwy Speed: 65mph =  95.3333ft/sec --> (95.3333 ft/sec)/ (176 ft/pixel) = 0.54166477272 pixels/second

        self.speed=speedDic[_map] #NICK HARDCODED THIS for HoustonHwy

        #print(self.speed)
        #sys.exit("DEBUG AGENT")
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
    def __init__(self,counter, _map):
        Agent.__init__(self, counter, _map)

        self.hash =int(hashlib.sha256(str(self.id).encode('utf-8')).hexdigest(), 16) % 10**8
        #print(self.id, "  ",self.hash)
        self.lastBlock = None
        self.lastTx = None


    ##DHT functions
    def signTxs(self,time):
        #print("\nSIgn Txs STart!")
        #collect validated Txs
        signedTxs = []
        for tx in self._visible_transactions:
            #print("\ntx: ",tx.id)
            #print("\tOutTx: ",tx.outTx)
            #print("\tOutTx - Agent: ",tx.outTx.agent)
            if tx.outTx==None: #
                if tx.verifier == self:
                    #print("\nStart Tx: ",tx.id," - Verifier: ",tx.verifier," Creator: "")\tx.signed = True
                    tx.signedTime=time
                    signedTxs.append(tx)
            elif tx.outTx.agent ==self:
                #print("\nSigning TX: ",tx,"\t",tx.outTx.agent," == ",self.id,"\tVerifier: ",tx.verifier, "Creator: ",tx.agent)
                tx.signed = True
                tx.signedTime=time
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
                #sys.exit("VERIFIED + DEBUG
                validatedTxs.append(tx)

        #print("\tValidatedTxs: ",validatedTxs)
        return validatedTxs

    ##DHT functions
    def validateTxsSimple(self,time):
        #collect validated Txs
        validatedTxs = []
        #print("\nValidating: ",self.id)
        for tx in self._submitted_transactions:
            #print("\tassigendValidator: ",tx.verifier)
            if tx.verifier ==self:
                #sys.exit("VERIFIED + DEBUG")
                validatedTxs.append(tx)

        for tx in self._visible_transactions:
            if tx.verifier ==self:
                #sys.exit("VERIFIED + DEBUG")
                validatedTxs.append(tx)

        #print("\tValidatedTxs: ",validatedTxs)
        return validatedTxs
