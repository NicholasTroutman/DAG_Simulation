#Base Station Nodes Immobile and only nodes with wider-internet networking
from simulation.node import Node
from itertools import chain
from simulation.block import DAGBlock, LinearBlock

class BaseStation(Node):
    def __init__(self, _counter, _map, _coordinates, _numAgents):
        Node.__init__(self, _counter, _map)
        self.coordinates = _coordinates
        self.lookupId = _counter + _numAgents


    ##add functions

    def add_visible_transactions(self, new_txs, time):  #no return
        #print("\nadd_vis_trans begin: ", time)
        #print("new: ",new_txs)
        #print("old: ",self._visible_transactions)
        new_txscopy = new_txs.copy()
        newest_txs = list(set(new_txscopy) - set(self._visible_transactions))
        newest_txs = list(set(newest_txs) - set(self._submitted_transactions))        #remove confirmed transactions from newest_txs
        newest_txs = list(set(newest_txs) - set(self._confirmed_transactions))

        change = False
        #print("newest! :",newest_txs)
        for tx in newest_txs:
            #print(tx," ",tx.seen[self.id])
            if tx.seen[self.lookupId] == "":
                #print("\nUNSEEN: ", tx,"\n")
                tx.seen[self.lookupId] = time
                self.numTxs=self.numTxs+1
                self.maxNumTxs=self.maxNumTxs+1
                self._visible_transactions.append(tx) #only add if we've never seen before
                change = True
        return change


    def add_submitted_transactions(self, new_submitted_txs, currentTime):

        new_submitted_txs2=new_submitted_txs.copy()

        #print(self.id," submitted Txs:",new_submitted_txs2)
        newest_submitted_txs = list(set(new_submitted_txs2) - set(self._submitted_transactions))
        newest_submitted_txs = list(set(newest_submitted_txs) - set(self._confirmed_transactions))


        for tx in newest_submitted_txs:
            if tx.seen[self.lookupId] == "": #if unseen
                tx.seen[self.lookupId] = currentTime
                self.numTxs=self.numTxs+1
                self.maxNumTxs = self.maxNumTxs+1
            self._submitted_transactions.append(tx)

            if tx in self._visible_transactions: #remove submitted transaction from _visible_transcation list
                self._visible_transactions.remove(tx)
                #print(self.id," removing from _visible_transcation:\t", tx)



    def add_confirmed_transactions(self, new_confirmed_txs, time):

        ##Slower after testing
        #s = set(self._confirmed_transactions)
        #newest_confirmed_txs = [x for x in new_confirmed_txs if x not in s]



        newest_confirmed_txs = list(set(new_confirmed_txs) - set(self._confirmed_transactions))




        for tx in newest_confirmed_txs:
            if tx.seen[self.lookupId] == "": #if unseen
                tx.seen[self.lookupId] = time
                self.numTxs=self.numTxs+1
                self.maxNumTxs=self.maxNumTxs+1
            self._confirmed_transactions.append(tx)


            if tx in self._visible_transactions: #remove confirmed transaction from _visible_transcation list
                self._visible_transactions.remove(tx)

            if tx in self._submitted_transactions: #remove confirmed transaction from _visible_transcation list
                self._submitted_transactions.remove(tx)



    ##Block functions
    def add_visible_blocks(self, new_blocks, time): #no return
        #if self.id==2:
            #print("\n\nadd_vis_trans begin: ",self.id,"\t", time,"\tnewVis: ",new_blocks,"\toldVis: ",self.get_visible_blocks())
            #traceback.print_stack()
            #print(type(self))
        #print("new: ",new_blocks)
        #print("old: ",self._visible_blocks)

        changeMade = False

        newest_blocks = list(set(new_blocks) - set(self._visible_blocks))
        #print("newest! :",newest_blocks)
        for block in newest_blocks:
                #print("\tBlock id: ",block,":  ",block.seen[self.id])
            #print("\nSeen matrix: ",block.seen)
            if block.seen[self.lookupId] == "":
                self.numBlocks=self.numBlocks+1
                self.maxNumBlocks=self.maxNumBlocks+1
                #print("\nUNSEEN: ", block,"\n")
                block.seen[self.lookupId] = time
                self._visible_blocks.append(block)
                changeMade = True
                #print("appended to vis_txs: ",self._visible_blocks)

            #if block in self._visible_blocks and block in self._confirmed_blocks: #remove from visible if in confirmed
            if block in self._visible_blocks and block in self._linked_blocks: #remove from visible if in confirmed
                self._visible_blocks.remove(block)

            #move confirmed txs for linear/DAG blockchain
            if isinstance(block, (DAGBlock, LinearBlock)):
                self.confirmTxs(block.confirmedBlocks, time)

            #print("\tAgent: ",self.id, " visBlocks:\t",self.get_visible_blocks())
        return changeMade



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
            if block.seen[self.lookupId] == "":
                #print("\nUNSEEN: ", block,"\n")
                block.seen[self.lookupId] = time
                self.numBlocks=self.numBlocks+1
                self.maxNumBlocks=self.maxNumBlocks+1
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
