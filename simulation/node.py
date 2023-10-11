import traceback
#Node is a base class that mobile agents and immobile base stations inherit common functions from
class Node:
    def __init__(self, _counter):
        self.id = _counter
        #self._visible_transactions = [] ##visible blocks instead in this block setup
        #self.coordinates= [] #list of double x and y coordinates in double [x,y] #agent specific
        #self.past_coordinates=[]#agent specific
        #self.destination=[]#agent specific
        #self.vector=[]#agent specific
        #self.prev_dest=[]#agent specific

        #self.speed=15 #agent specific


        self.coordinates= [] #list of double x and y coordinates in double [x,y]
        self.radius=60 #hardcoded radius of p2p connectivity

        #transaction variables
        self._visible_transactions=[]
        self._submitted_transactions = []
        self._confirmed_transactions=[]

        #block variables
        self._visible_blocks = []
        self._confirmed_blocks = []

        #For analysis
        self.agent_average_confirmation_confidence = 0
        self.tips = []
        self.record_tips = []

        self.resubmitTxs = []



    ##add functions

    def add_visible_transactions(self, new_txs, time):  #no return
        #print("\nadd_vis_trans begin: ", time)
        #print("new: ",new_txs)
        #print("old: ",self._visible_transactions)
        new_txscopy = new_txs.copy()
        reSubmitTxs = []
        for tx in new_txscopy:
            if (tx.resubmit==True and tx not in self._visible_transactions):
            #if (tx.resubmit==True):
                #print("RESUBMITTED TX FOUND")
                reSubmitTxs.append(tx)
                new_txscopy.remove(tx)

        newest_txs = list(set(new_txscopy) - set(self._visible_transactions))
        newest_txs = list(set(newest_txs) - set(self._submitted_transactions))        #remove confirmed transactions from newest_txs
        newest_txs = list(set(newest_txs) - set(self._confirmed_transactions))

        #print("newest! :",newest_txs)
        for tx in newest_txs:
            #print(tx," ",tx.seen[self.id])
            if tx.seen[self.id] == "":
                #print("\nUNSEEN: ", tx,"\n")
                tx.seen[self.id] = time
                self._visible_transactions.append(tx) #only add if we've never seen before
                #print("appended to vis_txs: ",self._visible_transactions)
            #if  tx in self._confirmed_transactions: #remove confirmed transaction from _visible_transcation list
                #self._visible_transactions.remove(tx)

        for tx in reSubmitTxs:
            #print("agent ",self.id," resubmit tx:\t",tx)

            #check if seen
            if tx.seen[self.id] == "":
                #print("\nUNSEEN: ", tx,"\n")
                tx.seen[self.id] = time

            #add to vis_txs
            self._visible_transactions.append(tx)

            if tx in self._submitted_transactions: #remove confirmed transaction from _visible_transcation list
                self._submitted_transactions.remove(tx) #ONLY TIME WE GO FROM submitted-->visible
                #print("\tREMOVING tx from submited_txs:\t",tx)
            #still remove if in _confirmed_transactions
            if tx in self._confirmed_transactions: #remove confirmed transaction from _visible_transcation list
                self._visible_transactions.remove(tx)
                #print("\tREMOVING tx from vis_txs cuz its confirmed:\t",tx)
        #if self.id == 0:
            #print("\n\nend add_vis_txs Agent 0 visible_txs:\t",sorted(self.get_visible_transactions(), key=lambda x: x.id))



    def add_submitted_transactions(self, new_submitted_txs, currentTime):

        new_submitted_txs2=new_submitted_txs.copy()

        #print(self.id," submitted Txs:",new_submitted_txs2)
        newest_submitted_txs = list(set(new_submitted_txs2) - set(self._submitted_transactions))
        newest_submitted_txs = list(set(newest_submitted_txs) - set(self._confirmed_transactions))


        for tx in newest_submitted_txs:
            if tx.seen[self.id] == "": #if unseen
                tx.seen[self.id] = currentTime

            self._submitted_transactions.append(tx)

            if tx in self._visible_transactions: #remove submitted transaction from _visible_transcation list
                self._visible_transactions.remove(tx)
                #print(self.id," removing from _visible_transcation:\t", tx)

            #add tx self.resubmitTxs
            if tx.agent == self:
                #print("add_submitted_transactions -> submitted tx's agent == self, add to resubmitTxs[]")
                #print("TIME:\t",currentTime)
                self.resubmitTxs.append([tx, currentTime]) #submit [tx, time], when time+100 happens, resubmit




    def add_confirmed_transactions(self, new_confirmed_txs, time):
        newest_confirmed_txs = list(set(new_confirmed_txs) - set(self._confirmed_transactions))

        for tx in newest_confirmed_txs:
            if tx.seen[self.id] == "": #if unseen
                tx.seen[self.id] = time
            self._confirmed_transactions.append(tx)

            if tx in self._visible_transactions: #remove confirmed transaction from _visible_transcation list
                self._visible_transactions.remove(tx)

            if tx in self._submitted_transactions: #remove confirmed transaction from _visible_transcation list
                self._submitted_transactions.remove(tx)


            ##remove from resubmitTxs list
            for [temptx, temptime] in self.resubmitTxs: #TODO: CHANGE TO DICTIONARY?
                if tx == temptx:
                    #match, remove from resubmitTxs
                     self.resubmitTxs.remove([temptx,temptime])




    def reSubmitTxs(self, currentTime):
        #print("Starting reSubmitTxs")
    ##MAGIC NUMBER
        #print(self.resubmitTxs)
        elapsedTime=120 #from simulations this is ~98-99%
        cutoff=currentTime-elapsedTime
        reSubmittedTxs = []
        for tx, txTime in self.resubmitTxs:
            if txTime<cutoff:
                #print("\n\n\t",self.id," - RESUBMIT ",tx,":\t",currentTime, " ",txTime,"\n\n")
                #resubmit
                reSubmittedTxs.append(tx)
                self.resubmitTxs.remove([tx,txTime]) #remove
                if tx in self._submitted_transactions:
                    self._submitted_transactions.remove(tx)
                    tx.resubmit = True #resubmit T/F (boolean)

        self.add_visible_transactions(reSubmittedTxs, currentTime)


    ##Block functions
    def add_visible_blocks(self, new_blocks, time): #no return
        #print("\nadd_vis_trans begin: ", time)
        #print("new: ",new_blocks)
        #print("old: ",self._visible_blocks)

        newest_blocks = list(set(new_blocks) - set(self._visible_blocks))
        #print("newest! :",newest_txs)
        for block in newest_blocks:
            #print(block," ",block.seen[self.id])
            if block.seen[self.id] == "":
                #print("\nUNSEEN: ", block,"\n")
                block.seen[self.id] = time
                self._visible_blocks.append(block)
                #print("appended to vis_txs: ",self._visible_blocks)

            if block in self._visible_blocks and block in self._confirmed_blocks: #remove from visible if in confirmed
                self._visible_blocks.remove(block)

            #move confirmed txs
            self.confirmTxs(block.confirmedBlocks, time)



    def add_confirmed_blocks(self, new_blocks, time): #no return
    #print("\nadd_vis_trans begin: ", time)
    #print("new: ",new_blocks)
    #print("old: ",self._visible_transactions)

        newest_blocks = list(set(new_blocks) - set(self._confirmed_blocks))
        #print("newest! :",newest_txs)
        for block in newest_blocks:
            #print(block," ",block.seen[self.id])
            if block.seen[self.id] == "":
                #print("\nUNSEEN: ", block,"\n")
                block.seen[self.id] = time
                self._confirmed_blocks.append(block)
                #print("appended to vis_txs: ",self._visible_blocks)

            if block in self._visible_blocks and block in self._confirmed_blocks: #remove from visible if in confirmed
                    self._visible_blocks.remove(block)

            #move confirmed txs
            self.confirmTxs(block.confirmedBlocks, time)


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

    def get_confirmed_blocks(self):
        return self._confirmed_blocks


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
