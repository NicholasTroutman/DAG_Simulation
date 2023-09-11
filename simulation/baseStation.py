#Base Station Nodes Immobile and only nodes with wider-internet networking
from simulation.node import Node
from simulation.block import BaseStationBlock #basestation block
from itertools import chain

class BaseStation(Node):
    def __init__(self, _counter, _coordinates):
        Node.__init__(self, _counter)
        self.coordinates = _coordinates


        #self.coordinates= [] #list of double x and y coordinates in double [x,y]
        #self.radius=60 #hardcoded radius of p2p connectivity

        ##block variables
        #self._visible_transactions=[]
        #self.confirmed_transactions=[]
        
        ##transaction variables
        #self._visible_blocks = []
        #self.confirmed_blocks = []
        
        ##For analysis
        #self.agent_average_confirmation_confidence = 0
        #self.tips = []
        #self.record_tips = []



    #trade uncomfired and confirmed txs and find union for each baseStation
    def tradeTxs(baseStations):
        totVisTxs=[]
        for bs in baseStations:
            totVisTxs.append(bs._visible_transactions)
        totVisTxs=list(set().union(*totVisTxs)) #Union of unique items

        ##replace visTxs with the correct set
        for bs in BaseStations:
            bs._visible_transactions=totVisTxs


    ##move all txs to confirmed and copy _confirmed_blocks from current_bs to everyone else
    def confirmTxsAndBlock(self, basestations, newBlock,time):
        for bs in basestations:
            bs.add_confirmed_transactions(bs._visible_transactions, time)
            bs._confirmed_blocks.append(newBlock) #should be identical to  self._confirmed_blocks

    #creates block (assumes BaseStation has all uncomfired transactions [visible_txs]])
    def createBaseStationBlock(self, baseStations, creationTime, no_of_agents):
        self.tradeTxs(baseStations) #trade visTxs
        newBlock= BaseStationBlock(self._visible_transactions,  creationTime, len(self._confirmed_blocks), no_of_agents, self._confirmed_blocks[-1]) #createNewBlock
        self._confirmed_blocks.append(newBlock) #append newBlock
        confirmTxsAndBlock(self, baseStations, newBlock, creationTime)#pass confirmedTxs and blocks to every baseStation





	##Transaction functions
    
    #def get_visible_transactions(self): #return vis txs
    #    return self._visible_transactions
        
    #def add_visible_transactions(self, new_txs, time):  #no return
        #print("\nadd_vis_trans begin: ", time)
        #print("new: ",new_txs)
        #print("old: ",self._visible_transactions)
        
     #   newest_txs = list(set(new_txs) - set(self._visible_transactions))
        #print("newest! :",newest_txs)
    #    for tx in newest_txs:
            #print(tx," ",tx.seen[self.id])
    #        if tx.seen[self.id] == "":
    #            #print("\nUNSEEN: ", tx,"\n")
   #             tx.seen[self.id] = time
   #             self._visible_transactions.append(tx) 
                #print("appended to vis_txs: ",self._visible_transactions)


    ##Block functions
   # def add_visible_blocks(self, new_blocks, time): #no return
   # #print("\nadd_vis_trans begin: ", time)
    #print("new: ",new_blocks)
    #print("old: ",self._visible_transactions)
    
      #  newest_blocks = list(set(new_blocks) - set(self._visible_blocks))
        #print("newest! :",newest_txs)
     #   for block in newest_blocks:
            #print(block," ",block.seen[self.id])
     #       if block.seen[self.id] == "":
     #           #print("\nUNSEEN: ", block,"\n")
     #           block.seen[self.id] = time
     #           self._visible_blocks.append(block) 
                #print("appended to vis_txs: ",self._visible_blocks)
    
    #def get_visible_blocks(self): #return vis blocks
      #  return self._visible_blocks
        
        

   # def __str__(self):
    #    return str(self.id)

    #def __repr__(self):
       # return str(self.id)