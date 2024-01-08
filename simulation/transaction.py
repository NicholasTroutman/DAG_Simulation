from collections import defaultdict
import hashlib

class Transaction:
    def __init__(self, _arrival_time, _counter, _numAgents): #initialize with numAgents for seen variable
            self.arrival_time = _arrival_time
            self.id = _counter
            self.agent = None
            self.seen=[""]*_numAgents#list where index=agent and value=time seen, for latency statistics
            #self.assignedValidator = -1
            #For tip selection and calculating confirmation_confidence
            #self.cum_weight = 1
            #self.cum_weight_multiple_agents  = defaultdict(lambda: 1)
            #self.exit_probability = 0
            #self.exit_probability_multiple_agents  = defaultdict(lambda: 0)
            #self.confirmation_confidence = 0
            #self.confirmation_confidence_multiple_agents = defaultdict(lambda: 0)

            #For performance statistics
            self.weight_update_time = 0
            self.tip_selection_time = 0



    def __str__(self):
        return str(self.id)

    def __repr__(self):
        return str(self.id)

    def __gt__(self,other):
        return(self.id > other.id)

    def __lt__(self,other):
        return(self.id < other.id)

    #Convert self.id --->hash, then lookup consensuCode[hash%agents] to get validator id
    #def assignValidator(self,consensusCode):
    #    self.assignedValidator = consensusCode[(int(hashlib.sha256(str(self.id).encode('utf-8')).hexdigest(), 16) % 10**8)%len(consensusCode)]

    def set_weight_update_time(self, weight_update_time):
        self.weight_update_time = weight_update_time

    def set_tip_selection_time(self, tip_selection_time):
        self.tip_selection_time = tip_selection_time



class DHTTransaction(Transaction):
    def __init__(self, _arrival_time, _counter, _numAgents, _outTx, _consensusCode): #initialize with numAgents for seen variable
        Transaction.__init__(self, _arrival_time, _counter, _numAgents)
        self.outTx = _outTx
        self.hash =int(hashlib.sha256(str(self.id).encode('utf-8')).hexdigest(), 16) % 10**8
        self.verifier = _consensusCode[self.hash%len(_consensusCode)] #assign verifier
        self.used = False
        self.signed =False
        self.signedTime = -1
