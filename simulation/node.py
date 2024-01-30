import traceback
from simulation.block import DAGBlock, LinearBlock
import sys
from cuckoo.filter import BCuckooFilter
import math

#Node is a base class that mobile agents and immobile base stations inherit common functions from
class Node:
    def __init__(self, _counter, _map):
        self.id = _counter
        #self._visible_transactions = [] ##visible blocks instead in this block setup
        #self.coordinates= [] #list of double x and y coordinates in double [x,y] #agent specific
        #self.past_coordinates=[]#agent specific
        #self.destination=[]#agent specific
        #self.vector=[]#agent specific
        #self.prev_dest=[]#agent specific

        #self.speed=15 #agent specific
        self.tradeTime = []
        self.witness = False
        self.coordinates= [] #list of double x and y coordinates in double [x,y]
        #self.radius=18.63 #hardcoded radius of p2p connectivity

        radiusDic = {"Houstonredblue.jpg": 44, "HoustonHwyredblue.jpg": 27.96}
        ##Houston Downtown Distance: 500ft/88 pixels = 5.681818 ft/pixel
        ##Wifi Downtown Distance = 150-300 ft, call it 250ft --> 250/5.68 = 44.01

        ##Houstonhwy Distance: 2*5280ft/60 pixels = 176 ft/pixel
        ##Wifi Distance = 150-300 ft, call it 300ft --> 300/176 = 1.7
        ##LPWAN Distance = 2km, call it 1.5km ~ 4921ft -->  4921/176 = 27.96

        self.radius=radiusDic[_map] #hardcoded radius of p2p connectivity
        #print(self.radius)
        #sys.exit("DEBUG")

        #transaction variables
        self._visible_transactions=[]
        self._submitted_transactions = []
        self._confirmed_transactions=[]

        #block variables
        self._visible_blocks = []
        #self._confirmed_blocks = []
        self._linked_blocks = []
        #For analysis
        self.agent_average_confirmation_confidence = 0
        self.tips = []
        self.record_tips = []

        ##Don't resubmit txs
        #self.resubmitTxs = []


        ##Cuckoo filters
        #Txs
        self.cuckooVisible = BCuckooFilter(capacity=math.floor(1000/0.84), error_rate=0.001, bucket_size=2) #fingerprint is 9
        self.cuckooSubmitted = BCuckooFilter(capacity=math.floor(1000/0.84), error_rate=0.001, bucket_size=2) #fingerprint is 9
        self.cuckooConfirmed = BCuckooFilter(capacity=math.floor(1500/0.84), error_rate=0.001, bucket_size=2) #fingerprint is 9

        #blocks
        self.cuckooVisibleBlocks = BCuckooFilter(capacity=math.floor(500/0.84), error_rate=0.001, bucket_size=2) #fingerprint is 9
        self.cuckooLinkedBlocks = BCuckooFilter(capacity=math.floor(500/0.84), error_rate=0.001, bucket_size=2) #fingerprint is 9
        self.cuckooConfirmedBlocks = BCuckooFilter(capacity=math.floor(1000/0.84), error_rate=0.001, bucket_size=2) #fingerprint is 9


    ##ADD Functions with CuckooFilter
    def add_visible_transactions(self, new_txs, time):  #no return

        change = False
        #print("newest! :",newest_txs)
        for tx in new_txs:
            #print(tx," ",tx.seen[self.id])
            if tx.seen[self.id] == "": #remove me later
                #print("\nUNSEEN: ", tx,"\n")
                tx.seen[self.id] = time

                ##check where it is
                if self.cuckooVisible.contains(str(tx))==False: #if alrdy in visible:
                    if self.cuckooSubmitted.contains(str(tx))==False: #if alrdy in submitted:
                        if self.cuckooConfirmed.contains(str(tx))==False: #if alrdy in confirmed:
                            self._visible_transactions.append(tx) #only add if we've never seen before
                            change = True  #change of visBlocks
                            self.cuckooVisible.insert(str(tx)) #add to cuckooFilter
                            #sys.exit("WORKED THIS TIME")
        return change


    def add_submitted_transactions(self, new_submitted_txs, currentTime):
        for tx in new_submitted_txs:
            if tx.seen[self.id] == "": #if unseen
                tx.seen[self.id] = currentTime

            ##check where it is
            if self.cuckooSubmitted.contains(str(tx))==True: #if in submitted alrdy:
                pass #do nothing
            else:
                if self.cuckooVisible.contains(str(tx))==True: #if in vis alrdy:
                    #remove from vis, and add to submitted
                    self.cuckooVisible.delete(str(tx))
                    self._visible_transactions.remove(tx)

                    self.cuckooSubmitted.insert(str(tx))
                    self._submitted_transactions.append(tx)



    def add_confirmed_transactions(self, new_confirmed_txs, time):

        for tx in new_confirmed_txs:
            if tx.seen[self.id] == "": #if unseen
                tx.seen[self.id] = time

            if self.cuckooConfirmed.contains(str(tx))==True: #if in confirmed alrdy:
                #print("ALRDY CONFIRMED")
                pass
            else:
                if self.cuckooSubmitted.contains(str(tx))==True: #if in submitted alrdy:
                    #remove from submitted and add to confirmed
                    self.cuckooSubmitted.delete(str(tx))
                    self._submitted_transactions.remove(tx)

                    self.cuckooConfirmed.insert(str(tx))
                    self._confirmed_transactions.append(tx)

                else: #must be in visible now
                    if self.cuckooVisible.contains(str(tx))==True: #if in submitted alrdy:
                        #remove from submitted and add to confirmed
                        try:
                            self._visible_transactions.remove(tx)
                            self.cuckooVisible.delete(str(tx))
                        except:
                            print("\nWhoopsy, false postive ;):\t",tx,"\n")

                        self.cuckooConfirmed.insert(str(tx))
                        self._confirmed_transactions.append(tx)


    ##Block functions
    def add_visible_blocks(self, new_blocks, time): #no return

        changeMade = False
        #print("newest! :",newest_blocks)
        for block in new_blocks:
            #print(block,":  ",block.seen[self.id])
            if block.seen[self.id] == "":
                #print("\nUNSEEN: ", block,"\n")
                block.seen[self.id] = time
                self._visible_blocks.append(block)
                changeMade = True
                #print("appended to vis_txs: ",self._visible_blocks)

            #if block in self._visible_blocks and block in self._confirmed_blocks: #remove from visible if in confirmed
            if block in self._visible_blocks and block in self._linked_blocks: #remove from visible if in confirmed
                self._visible_blocks.remove(block)

            #move confirmed txs for linear/DAG blockchain
            if isinstance(block, (DAGBlock, LinearBlock)):
                self.confirmTxs(block.confirmedBlocks, time)

        return changeMade



    ##add functions (list indexing)

   #
   # def add_visible_transactions(self, new_txs, time):  #no return
   #     #print("\nadd_vis_trans begin: ", time)
   #     #print("new: ",new_txs)
   #     #print("old: ",self._visible_transactions)
   #     new_txscopy = new_txs.copy()
   #     newest_txs = list(set(new_txscopy) - set(self._visible_transactions))
   #     newest_txs = list(set(newest_txs) - set(self._submitted_transactions))        #remove confirmed transactions from newest_txs
   #     newest_txs = list(set(newest_txs) - set(self._confirmed_transactions))
   #
   #     change = False
   #     #print("newest! :",newest_txs)
   #     for tx in newest_txs:
   #         #print(tx," ",tx.seen[self.id])
   #         if tx.seen[self.id] == "":
   #             #print("\nUNSEEN: ", tx,"\n")
   #             tx.seen[self.id] = time
   #             self._visible_transactions.append(tx) #only add if we've never seen before
   #             change = True
   #     return change


    # def add_submitted_transactions(self, new_submitted_txs, currentTime):
    #
    #     new_submitted_txs2=new_submitted_txs.copy()
    #
    #     #print(self.id," submitted Txs:",new_submitted_txs2)
    #     newest_submitted_txs = list(set(new_submitted_txs2) - set(self._submitted_transactions))
    #     newest_submitted_txs = list(set(newest_submitted_txs) - set(self._confirmed_transactions))
    #
    #
    #     for tx in newest_submitted_txs:
    #         if tx.seen[self.id] == "": #if unseen
    #             tx.seen[self.id] = currentTime
    #
    #         self._submitted_transactions.append(tx)
    #
    #         if tx in self._visible_transactions: #remove submitted transaction from _visible_transcation list
    #             self._visible_transactions.remove(tx)
    #             #print(self.id," removing from _visible_transcation:\t", tx)
    #


    # def add_confirmed_transactions(self, new_confirmed_txs, time):
    #
    #     ##Slower after testing
    #     #s = set(self._confirmed_transactions)
    #     #newest_confirmed_txs = [x for x in new_confirmed_txs if x not in s]
    #
    #
    #
    #     newest_confirmed_txs = list(set(new_confirmed_txs) - set(self._confirmed_transactions))
    #
    #
    #
    #
    #     for tx in newest_confirmed_txs:
    #         if tx.seen[self.id] == "": #if unseen
    #             tx.seen[self.id] = time
    #         self._confirmed_transactions.append(tx)
    #
    #
    #         if tx in self._visible_transactions: #remove confirmed transaction from _visible_transcation list
    #             self._visible_transactions.remove(tx)
    #
    #         if tx in self._submitted_transactions: #remove confirmed transaction from _visible_transcation list
    #             self._submitted_transactions.remove(tx)
    #


    # ##Block functions
    # def add_visible_blocks(self, new_blocks, time): #no return
    #     #print("\nadd_vis_trans begin: ", time)
    #     #print("new: ",new_blocks)
    #     #print("old: ",self._visible_blocks)
    #
    #     changeMade = False
    #
    #     newest_blocks = list(set(new_blocks) - set(self._visible_blocks))
    #     #print("newest! :",newest_blocks)
    #     for block in newest_blocks:
    #         #print(block,":  ",block.seen[self.id])
    #         if block.seen[self.id] == "":
    #             #print("\nUNSEEN: ", block,"\n")
    #             block.seen[self.id] = time
    #             self._visible_blocks.append(block)
    #             changeMade = True
    #             #print("appended to vis_txs: ",self._visible_blocks)
    #
    #         #if block in self._visible_blocks and block in self._confirmed_blocks: #remove from visible if in confirmed
    #         if block in self._visible_blocks and block in self._linked_blocks: #remove from visible if in confirmed
    #             self._visible_blocks.remove(block)
    #
    #         #move confirmed txs for linear/DAG blockchain
    #         if isinstance(block, (DAGBlock, LinearBlock)):
    #             self.confirmTxs(block.confirmedBlocks, time)
    #
    #     return changeMade



    #def add_confirmed_blocks(self, new_blocks, time): #no return
    def add_linked_blocks(self, new_blocks, time): #no return
        #print("\nadd_confirm_block begin: ", time)
        #print("new: ",new_blocks)
        #print("old: ",self._confirmed_blocks)

        #newest_blocks = list(set(new_blocks) - set(self._confirmed_blocks))
        newest_blocks = list(set(new_blocks) - set(self._linked_blocks))
        #print("newest! :",newest_blocks)
        for block in newest_blocks:
            #print(block," ",block.seen[self.id])
            if block.seen[self.id] == "":
                #print("\nUNSEEN: ", block,"\n")
                block.seen[self.id] = time
                self._linked_blocks.append(block)
                #print("appended to confirmed_blocks: ",self._confirmed_blocks)

            ##Removing: remove from visible_blocks because confirmed_blocks will be used as LINKED blocks from now on
            if block in self._visible_blocks:
                self._visible_blocks.remove(block)
                if block not in self._linked_blocks: #remove from visible if in
                    self._linked_blocks.append(block)
            #move confirmed txs for linear/DAG blockchain
            if isinstance(block, (DAGBlock, LinearBlock)):
                self.confirmTxs(block.confirmedBlocks, time)

    def confirm_all_vis_blocks(self, time):
        self._linked_blocks = self._linked_blocks + self._visible_blocks
        self._visible_blocks = [] #empty it out

#Confirm TXS

##Function to move confirmed blocks' txs to confirmed_txs
    def confirmTxs(self, confirmedBlocks, time):

        if len(confirmedBlocks)> 0:
            newConfirmedTxs = []
            for confirmedBlock in confirmedBlocks:
                if len(confirmedBlock.blockTransactions)>0:
                    newConfirmedTxs = newConfirmedTxs + confirmedBlock.blockTransactions

            #get rid of redundancy
            #print(newConfirmedTxs)
            newConfirmedtxs = list(set(newConfirmedTxs))

            if len(newConfirmedTxs)>0:
                self.add_confirmed_transactions(newConfirmedTxs, time)

##Get Functions
    def get_visible_blocks(self): #return vis blocks
        return self._visible_blocks

    #def get_confirmed_blocks(self):
        #return self._confirmed_blocks

    def get_linked_blocks(self):
        return self._linked_blocks

    def get_visible_transactions(self): #return vis txs
        return self._visible_transactions

    def get_submitted_transactions(self): #return submitted txs
        return self._submitted_transactions

    def get_confirmed_transactions(self): #return vis txs
        return self._confirmed_transactions

    def __str__(self):
        return str(self.id)

    def __repr__(self):
        return str(self.id)
