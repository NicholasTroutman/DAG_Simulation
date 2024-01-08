import sys
import timeit
import random
import time
import math
from collections import Counter
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
from numpy.random import rand
from operator import add, attrgetter
import os


from simulation.helpers import update_progress, create_distance_matrix, \
common_elements, clamp, load_file, create_coordinates, create_coordinates_nodes, routes_export, routes_importer, confirmationLayer_importer
from simulation.mapMaker import Distance, DistanceToVector,  FindEdges, IdentfiyBlueEdgeIntersection,  LoadImageIntoGraph, isBetween #FindCenter
from simulation.plotting import print_info, print_graph, print_graph_temp, print_coordinates, print_coordinates_img, print_tips_over_time, print_gif, print_tips_over_time_multiple_agents, print_tips_over_time_multiple_agents_with_tangle, print_attachment_probabilities_alone,print_attachment_probabilities_all_agents
from simulation.agent import Agent, DHTAgent
from simulation.transaction import Transaction,DHTTransaction
from simulation.block import DAGBlock, LinearBlock, HashGraphBlock,  confirmBlocks, stronglySee, divideRounds, isFamous, orderRange, BFS


from simulation.DLTFuncs import create_block_near, create_block_individual

class Multi_Agent_Simulation:
    def __init__(self, _no_of_transactions, _lambda, _no_of_agents, \
                 _alpha, _distance, _tip_selection_algo, _latency = 1, \
                 _agent_choice = None, _printing = False, _lambda_m = 1/60,  \
                 _seed = 1, _DLTMode = "linear", _consensus = "individual",  \
                 _importMap = "Error", _blockTime = 40, _references = 1, _group = 1):

        ##define variables
        self.consensus = _consensus #individual (PoW), near (PoS)
        #self.consensus_selection() ##assign self.create_block to either individual or near

        self.DLTMode = _DLTMode #dag, linear, dht, or hashgraph
        self.group = _group
        if self.DLTMode == "hashgraph":
            self.witnesses = {}
            self.stragglers = []
        elif self.DLTMode == "dht":
            self.consensusCode= {} #Partition of hashSpace into agents
        #elif self.DLTMode == "dag":
            #self.references = _references
        self.references = _references

        self.blockTime = _blockTime
        self.importMap = str(_importMap)+str(".jpg")
        self.currentBlock = 0
        self.seed = _seed
        self.no_of_transactions = _no_of_transactions
        self.lam = _lambda
        self.lam_m = _lambda_m
        self.no_of_agents = _no_of_agents
        self.alpha = _alpha
        self.latency = _latency
        self.total_num_tx = _no_of_transactions
        if (type(_distance) is float or type(_distance) is int):
            self.distances = create_distance_matrix(self.no_of_agents, _distance)
        else:
            self.distances = _distance
        self.tip_selection_algo = _tip_selection_algo
        if _agent_choice is None:
            _agent_choice = list(np.ones(self.no_of_agents)/self.no_of_agents)
        self.agent_choice = _agent_choice
        self.printing = _printing

        #Basic parameter checks
        if (round(sum(self.agent_choice), 3) != 1.0):
            print("Agent choice not summing to 1.0: {}".format(sum(self.agent_choice)))
            sys.exit(1)
        if (len(self.agent_choice) != self.no_of_agents):
            print("Agent choice not matching no_of_agents: {}".format(len(self.agent_choice)))
            sys.exit(1)
        if (self.no_of_agents == 1):
            print("ERROR:  Use a Single_Agent_Simulation()")
            sys.exit()

        self.transactions = []
        self.agents = []
        self.arrival_times = []
        self.not_visible_transactions = []




        ##block variables
        self.blocks=[]
        if self.consensus == "individual": #PoW
            self.block_arrival_times = []


        #For analysis only
        self.record_tips = []
        self.record_attachment_probabilities = []
        self.nearbyCounter=0
        self.ETC=0
        self.ETCcount=0


        #For max. four agents always the same colors in prints
        self.agent_colors = ['#a8d6ff', '#ff9494', '#dcc0dd', '#e0ff80']
        self.agent_tip_colors = ['#f5faff', '#ffe0e0', '#f8f2f8', '#f9ffe6']

        #For more than four agents random colors and lighter tip colors
        for i in range(self.no_of_agents-4):
            r = lambda: random.randint(0,255)
            color = '#{:02x}{:02x}{:02x}'.format(r(), r(), r())
            self.agent_colors.append(color)
            self.agent_tip_colors.append(color)


        ## Get confirmationLayer
        print("!!!! FIX CONFIRMATION NUMBER")
        if self.consensus=="individual":
            self.confirmationNumber = 1
        else:
            self.confirmationNumber = confirmationLayer_importer(self, "confirmationLayer.csv")
        print("\nConfirmation Number: ", self.confirmationNumber)

    #############################################################################
    # SIMULATION: SETUP
    #############################################################################




    def setup(self):

        np.random.seed(self.seed)
        #Create agents
        agent_counter = 0
        for agent in range(self.no_of_agents):
            if self.DLTMode =="dht":
                self.agents.append(DHTAgent(agent_counter, self.importMap))
            else:
                self.agents.append(Agent(agent_counter,  self.importMap))
            agent_counter += 1

        #Create directed graph object
        self.DG = nx.DiGraph()
        #self.traffic, self.backgroundImg= LoadImageIntoGraph('DCredblue.png') ##THIS WORKS RIGHT NOW
        #self.traffic, self.backgroundImg= LoadImageIntoGraph('NYredblue.png') #TODO COMMAND LINE INPUT NT newyork new york
        ##Houston Distance: 500ft/88 pixels = 5.681818 ft/pixel
        ##Houston Speed: 30mph =  44ft/sec --> (44 ft/sec)/ (5.6818 ft/pixel) = 7.74400000248 pixels/second
        #self.traffic, self.backgroundImg= LoadImageIntoGraph('Houstonredblue.jpg') #TODO COMMAND LINE INPUT Houston H htown home, maps, map mapping input

        ##Houstonhwy Distance: 2*5280ft/60 pixels = 176 ft/pixel
        ##HoustonHwy Speed: 65mph =  95.3333ft/sec --> (95.3333 ft/sec)/ (176 ft/pixel) = 0.54166477272 pixels/second
        self.traffic, self.backgroundImg= LoadImageIntoGraph(self.importMap) #TODO COMMAND LINE INPUT Houston H htown home, maps, map mapping input
        #self.traffic, self.backgroundImg= LoadImageIntoGraph('HoustonHwyredblue.jpg') #TODO COMMAND LINE INPUT Houston H htown home, maps, map mapping input

        #print("\n\n\n\n\nTraffic: ",self.backgroundImg.shape)
        #print("\t",self.traffic.edges)
        #print("\t",self.traffic.edges.data)

        #create travelling sales person (TSPTSP)
        self.tsp = nx.approximation.traveling_salesman_problem
        #Create agent coordinates & destination
        #print(self.traffic)
        create_coordinates_nodes(self.agents, self.traffic, self.tsp)

        #Create random arrival times Txs
        np.random.seed(self.seed)
        inter_arrival_times = np.random.exponential(1 / self.lam, self.no_of_transactions)
        self.arrival_times = list(np.cumsum(inter_arrival_times))

        maxArrivalTime = max(self.arrival_times)

        #print("Transactions Created")

        #Createa random arrival times for self blocks in predetermined modes (indivdual + Blockchain)
        if self.consensus == "individual" and (self.DLTMode == "linear" or self.DLTMode == "dag"): #PoW
            np.random.seed(self.seed)
            #inter_arrival_times = np.random.exponential(40 / self.lam, int(self.no_of_transactions/40) ) #5x tx rate HARDCODED NT TODO: MAKE DYNAMIC NICK
            inter_arrival_times = np.random.exponential(self.blockTime / self.lam, int(self.no_of_transactions/self.blockTime) )
            #print("\n")
            #print(inter_arrival_times)
            #print("\n")
            #print(sum(inter_arrival_times)/len(inter_arrival_times))
            self.block_arrival_times = list(np.cumsum(inter_arrival_times))
            self.block_arrival_times.sort()
            #print(self.block_arrival_times)

            #create owners list
            np.random.seed(self.seed)
            self.block_owners = list(np.random.randint(0,self.no_of_agents, len(self.block_arrival_times))) ##assign block owners

            #print("Blocks Created")

        ##if milestone issue rate is not zero, calculate number of milestones
        #if self.lam_m != 0:
        #    num_of_milestones = int((self.no_of_transactions / self.lam) * self.lam_m)
        #self.total_num_tx += num_of_milestones
        #    for i in range(num_of_milestones):
        #        self.arrival_times.append((1/self.lam_m)*(i+1))

        self.arrival_times.sort()
        transaction_counter = 0

        if self.DLTMode == "dag" or self.DLTMode == "linear":
            #Create genesis transaction object, store in list and add to graph object
            self.transactions.append(Transaction(0, transaction_counter, self.no_of_agents))
            #transaction_counter += 1
            self.blocks.append(self.createBlock([], [self.agents[0]], 0, 0, self.no_of_agents, None)) #genesis block

            #set seen for everyone
            for s in self.blocks[0].seen:
                s=0 #set seen for everyone at 0

            for agent in self.agents: #add to vis blocks for everyone
                agent.add_visible_blocks([self.blocks[0]], 0)



            self.DG.add_node(self.blocks[0], pos=(self.blocks[0].creation_time, \
                    self.blocks[0].creators[0].id*2), \
                    node_color=self.agent_colors[self.blocks[0].creators[0].id])



        #create a "joining block" for each agent
        elif self.DLTMode == "hashgraph":
            self.witnesses[0] = [] #initialize witnesses[0] as empty list
            for agent in self.agents:
                self.blocks.append(self.createBlock([], [agent], 0, len(self.blocks), self.no_of_agents, None)) #genesis block (s))
                #set seen for minting agent_tip_colors
                #self.blocks[-1].seen[agent.id] = 0
                #add to vis list for minting agent
                #print("\nWitness block: ",self.blocks[-1])
                agent.add_visible_blocks([self.blocks[-1]], 0)
                agent.lastMintedBlock=self.blocks[-1] #most recent block
                #add to networkx DG
                self.DG.add_node(self.blocks[-1], pos=(self.blocks[-1].creation_time, \
                        self.blocks[-1].creators[0].id*2), \
                        node_color=self.agent_colors[self.blocks[-1].creators[0].id])

                #add to witnesses list
                self.witnesses[0].append(self.blocks[-1])
                self.blocks[-1].witness = True


            #print("\n0 Witnesses = ",self.witnesses[0])
            #sys.exit("DEBUG")

            ##create tradeTime for each agent
            for agent in self.agents:
                agent.tradeTime = [-5]*self.no_of_agents
                #print("\nagent vis_blocks:\t",agent.get_visible_blocks())


        elif self.DLTMode == "dht":

            ##create consensus code
            hashAgents = []
            for a in self.agents:
                hashAgents.append([a.hash, a])

            hashAgents = sorted(hashAgents) #[ [smallestKWM, agent id], ..., [largest KWM, agent id]]
            for i in range(0,self.no_of_agents):
                self.consensusCode[i]=hashAgents[i][1] #consensusCode[order]=agentId

            #print("\n\tConsensus Code:")
            #print(self.consensusCode)

            #create genesis block
            #Create genesis transaction object, store in list and add to graph object
            self.transactions.append(DHTTransaction(0, transaction_counter, self.no_of_agents, None, self.consensusCode))
            transaction_counter += 1
            self.blocks.append(self.createBlock([], [self.agents[0]], 0, 0, self.no_of_agents, None)) #genesis block

            #set seen for everyone
            for s in self.blocks[0].seen:
                s=0 #set seen for everyone at 0

            for agent in self.agents: #add to vis blocks for everyone
                agent.add_visible_blocks([self.blocks[0]], 0)



            self.DG.add_node(self.blocks[0], pos=(self.blocks[0].creation_time, \
                    self.blocks[0].creators[0].id*2), \
                    node_color=self.agent_colors[self.blocks[0].creators[0].id])

            #sys.exit("DEBUG")
        #Create other transaction objects and store in list, transaction
        transaction_counter = 0
        if self.DLTMode == "dht":
            for i in range(len(self.arrival_times)):
                self.transactions.append(DHTTransaction(self.arrival_times[i], transaction_counter, self.no_of_agents, None, self.consensusCode)) #create transactions + seen list
                transaction_counter += 1
        else:
            for i in range(len(self.arrival_times)):
                self.transactions.append(Transaction(self.arrival_times[i], transaction_counter, self.no_of_agents)) #create transactions + seen list
                transaction_counter += 1


        dir_name = './routes/'
        suffix = '.csv'
        base_name = self.importMap+"_txs"+str(self.no_of_transactions)+"_agents"+str(self.no_of_agents) + "_seed"+ str(self.seed)

        routesFileName = os.path.join(dir_name, base_name + suffix)
        print(routesFileName)



        #CHECK IF PRNG ROUTS EXIST
        if os.path.isfile(routesFileName): #
            routes_importer(self, routesFileName)
            #for agent in self.agents:
                #print(agent)
                #print(agent.destination)
            #sys.exit("FILE EXISTS TODO LOAD")
        else:
            #print("routesFile DNE ~ Creating" )
            #number of routes/sim 1/50*2 1/25 -->txs/4 is destinations
            numDest = self.no_of_transactions#/15 #NYredblue is 227/1000, uses 130/thousand
            np.random.seed(self.seed+1)
            #print("Creating PRNG Routes: ")
            for agent in self.agents:

                update_progress(agent.id/self.no_of_agents, agent.id)
                for i in range(0,int(numDest)): #for each destination
                    #print("\nSTART Agent: ",agent.id," Destination ",i,": ",agent.destination,"\n")
                    newDest=np.random.choice([x for x in self.traffic.nodes if x!=agent.destination[-1]])
                    #set new TSP path
                    #print("TSP RESULT: ",self.tsp(self.traffic, nodes=[newDest, agent.destination[-1]], cycle=False)[1:])
                    agent.destination.extend(self.tsp(self.traffic, nodes=[newDest, agent.destination[-1]], cycle=False)[1:])


                    #print("Agent: ",agent.id," Destination ",i,": ",agent.destination)
                    #set slope and vector
                    if i==0:
                        streetSlope=[ self.traffic.nodes[agent.destination[0]]['pos'][0]- self.traffic.nodes[agent.prev_dest]['pos'][0],  self.traffic.nodes[agent.destination[0]]['pos'][1] - self.traffic.nodes[agent.prev_dest]['pos'][1]  ]

                        agent.vector=streetSlope/np.linalg.norm(streetSlope)
                    elif i%10==0:
                        tempTime = 0
                        for i in range(1,len(agent.destination)):
                            tempTime += self.traffic.edges[agent.destination[i],agent.destination[i-1]]['weight']/agent.speed
                            #if agent.id==3:
                                #print(agent.destination[i]," --> ",tempTime)
                        #print("\n",tempTime, "  - ", (maxArrivalTime+2))
                        if tempTime > (maxArrivalTime+2):
                            #print("\tENOUGH Destinations: ",i, " / ",int(numDest))
                            break
                        #print("\n\nEDGE WEIGHT:")
                        #print(self.traffic.nodes)
                        #print(self.traffic.edges[0,5])
                        #print(self.traffic.edges[0,5]['weight'])
                        #print(self.traffic.edges[5,0]['weight'])

                        #sys.exit("DEBUG END
                #sys.exit("DEBUG END")
            print("  PRNG Routes Created: ",len(self.agents[0].destination),"\n\n\n")
            #for agent in self.agents:
                #print(agent)
                #pint(agent.destination)
                #print(type(agent.destination[0]))
            ##Save routes as csv file
            routes_export(routesFileName, self.agents)




    #Move all agents
    def moveAgents(self, arrival_time, prevTime):
        for agent in self.agents:
            if self.printing:
                agent.past_coordinates.append(agent.coordinates)
            prevCoordinates = agent.coordinates #DEBUGGING REMOVE ME
            ##old system random walk
            #agent.coordinates=agent.coordinates + np.random.normal(0,6,2)*(transaction.arrival_time-prevTime)

            #new system directed TSP
            #Speed/second
            speed = agent.speed
            scalar=(arrival_time-prevTime)*speed

            vector = [v*scalar for v in agent.vector]
            newCoord = list( map(add, agent.coordinates, vector) )
            #print(prevTime, " -> ",arrival_time," = ",(arrival_time-prevTime)," ~~~ Scalar: ",speed,"\tscalar: ",scalar, "   *** ",vector)



            if (isBetween(newCoord, self.traffic.nodes[agent.destination[0]]['pos'], agent.coordinates)==False): #epsilon=0.5, if true, then not between
                agent.coordinates=newCoord
            else: #overshot, new coordinates are agent.destination[0]
                agent.coordinates=self.traffic.nodes[agent.destination[0]]['pos'] #new coordinates are center of intersection
                agent.prev_dest=agent.destination[0] #arrived at destination
                if len(agent.destination)>1: #more places to go
                    #new destination
                    agent.destination=agent.destination[1:]
                    #set slope and vector
                    streetSlope=[ self.traffic.nodes[agent.destination[0]]['pos'][0]- self.traffic.nodes[agent.prev_dest]['pos'][0],  self.traffic.nodes[agent.destination[0]]['pos'][1] - self.traffic.nodes[agent.prev_dest]['pos'][1]  ]

                    agent.vector=streetSlope/np.linalg.norm(streetSlope)



                else: #reached terminus, make new destinations
                    #print("NEW DESTINATION")
                    newDest=np.random.choice([x for x in self.traffic.nodes if x!=agent.prev_dest])
                    #print("\n ",agent.id,"  DISTANCE: ",agent.distance,"\tEstimatedDistance: ",agent.estimatedDistance)


                    sys.exit("USED ALL PATHS, ALLOCATE MORE")
                    #set new TSP path
                    agent.destination=self.tsp(self.traffic, nodes=[newDest, agent.prev_dest], cycle=False)[1:]
                    #set slope and vector
                    streetSlope=[ self.traffic.nodes[agent.destination[0]]['pos'][0]- self.traffic.nodes[agent.prev_dest]['pos'][0],  self.traffic.nodes[agent.destination[0]]['pos'][1] - self.traffic.nodes[agent.prev_dest]['pos'][1]  ]

                    agent.vector=streetSlope/np.linalg.norm(streetSlope)


            #If coordinates are outside boundaries [0,backgroundImg.shape[0]] bounce them back in, 101-->99, -1-->1. bounce around boundary, not pacman
            if agent.coordinates[0]>self.backgroundImg.shape[1] or agent.coordinates[0]<0:
                sys.exit("ERROR TOTALLY LOST 0:\t",self.backgroundImg.shape[0]," ",self.backgroundImg.shape[1],"\tVS",agent.coordinates[0]," ",agent.coordinates[1])
                #agent.coordinates[0]=-1*agent.coordinates[0]%self.backgroundImg.shape[0] #100 MAGIC NUMBER
            if agent.coordinates[1]>self.backgroundImg.shape[0] or agent.coordinates[1]<0:
                sys.exit("ERROR TOTALLY LOST 0:\t",self.backgroundImg.shape[0]," ",self.backgroundImg.shape[1],"\tVS",agent.coordinates[0]," ",agent.coordinates[1])
                #agent.coordinates[1]=-1*agent.coordinates[1]%self.backgroundImg.shape[1] #MAGIC NUMBER




    def cleanOldTxsAndBlocks(self, transaction):
        lowerBound = 1500
        if self.consensus == "near":
            lowerBound=2500
        if self.DLTMode == "hashgraph":
            lowerbound = 2200
        currentTime = transaction.arrival_time

        if (transaction.id >= 0 and transaction.id % lowerBound == 0):
            #print("CLEANING")
            ##remove old txs
            for agent in self.agents:


                #remove old confirmed txs
                #numStayingTxs = 0
                keepTxs = []
                for tx in agent.get_confirmed_transactions():
                    #print("Tx_arrival_time: ",transaction.arrival_time, " DIFF: ",transaction.arrival_time - tx.arrival_time)
                    if currentTime - tx.arrival_time < lowerBound: ##MAGIC NUMBER
                        keepTxs.append(tx)
                        #numStayingTxs +=1

                agent._confirmed_transactions = keepTxs

                ###Seed 1
                ##No remove oldSubmitted Txs = 294.596
                ##Remove oldSubmitted Txs = 72.866

                ###Seed 2
                ##No remove Txs = 295.596
                ##Remove oldSubmitted Txs = 72.952

                ##Remove old submitted Txs
                #numStayingTxs = 0
                keepTxs = []
                for tx in agent.get_submitted_transactions():
                    #print("Tx_arrival_time: ",transaction.arrival_time, " DIFF: ",transaction.arrival_time - tx.arrival_time)
                    if currentTime - tx.arrival_time < lowerBound: ##MAGIC NUMBER
                        keepTxs.append(tx)
                        #numStayingTxs +=1
                agent._submitted_transactions = keepTxs


                #remove old linked_blocks
                keepBlocks=[]
                for block in agent.get_linked_blocks():
                    if currentTime - block.creation_time < lowerBound: ##MAGIC NUMBER 800
                        keepBlocks.append(block)
                #agent._confirmed_blocks = keepBlocks
                agent._linked_blocks = keepBlocks

                keepBlocks=[]
                #remove old visible blocks
                #for block in agent.get_visible_blocks():
                    #print("\n",currentTime, " - ", block.id, ": ", block.creation_time)

                    #if currentTime - block.creation_time < lowerBound: ##MAGIC NUMBER
                        #keepBlocks.append(block)
                #agent._visible_blocks = keepBlocks







##Block(txs, agents, time, len(self.blocks), self.no_of_agents, None) #None for no new blockLinks (yet)
    #wrapper to select which block we are creating
    def createBlock(self, txs, agents, creation_time, blockCounter, numAgents, blockLinks):
        if self.DLTMode == "linear":
            return LinearBlock(txs, agents, creation_time, blockCounter, numAgents, blockLinks)
        elif self.DLTMode == "dag":
            return DAGBlock(txs, agents, creation_time, blockCounter, numAgents, blockLinks, self.references)
        elif self.DLTMode == "hashgraph":
            return HashGraphBlock(txs, agents, creation_time, blockCounter, numAgents, blockLinks)
        elif self.DLTMode == "dht":
            return LinearBlock(txs, agents, creation_time, blockCounter, numAgents, blockLinks)

        else:
            sys.exit("ERROR: DLTMODE invalid:\t",str(self.DLTMODE))





    def tip_selection(self, block):

        if(self.tip_selection_algo == "random"):
            self.random_selection(block)
        elif (self.tip_selection_algo == "unweighted"):
            self.unweighted_MCMC(block)
        elif (self.tip_selection_algo == "weighted-genesis"):
            self.weighted_genesis_MCMC(block)
        elif (self.tip_selection_algo == "weighted-entry-point"):
            self.weighted_entry_point_MCMC(block)
        elif (self.tip_selection_algo == "longest"):
            #self.longest_selection(block) #actually block
            self.longest_selection_fast(block) #actually block
        else:
            print("ERROR:  Valid tip selection algorithms are 'random', 'weighted-genesis', 'weighted-entry-point', "
                  "'unweighted'")
            sys.exit()



    #############################################################################
    # SIMULATION: MAIN LOOP
    #############################################################################


    def run(self):

        start_time = timeit.default_timer()

        if self.printing:
            print_info(self)

        #Create dictionary with simulation parameter changes when provided
        #if(len(sys.argv) == -1):
        #    dic = {x[0]: x[1:] for x in self.config[1:]}
        prevTime=0


        ##save coordinates in figure
        print_coordinates_img(self,self.agents,0, self.backgroundImg)


        for s in self.transactions[0].seen:
            s=0 #set seen for everyone at 0



        ##Start loop
        #Start with first transaction (NOT genesis)
        #for transaction in self.transactions[1:]: ##Loop for each tx

        ##Loop with 1 second between
        endTime = math.ceil(self.transactions[-1].arrival_time)
        currentTx = 1
        #currentBlock = 0

        ###main run loop for each second
        for i in range(1,endTime):
            mintedTxs=[]
            mintedBlocks = []

            ##get new minted Txs
            while self.transactions[currentTx].arrival_time < i: #has this tx been minted
                mintedTxs.append(self.transactions[currentTx]) #if so add to mintedTxs
                currentTx +=1 #check next Tx




            ##Move agents
            self.moveAgents(i, prevTime)



            #mint Txs & Do something every 400 to clean visible_transactions
            for transaction in mintedTxs: #for each minted tx since last time increment
                transaction.agent = np.random.choice(self.agents, p=self.agent_choice) #choose agent
                transaction.agent.add_visible_transactions([transaction],  transaction.arrival_time) #add tips to minted tx
                self.cleanOldTxsAndBlocks(transaction) ##Do something every 400 to clean visible_transactions
                ##Find outTx: last tx from same agent
                if self.DLTMode == "dht":
                    transaction.outTx = transaction.agent.lastTx
                    #print("\n",transaction.id)
                    #print("\toutTx: ",transaction.outTx)
                    #print("\tagent: ",transaction.agent)
                    #print("\tagent.lastTx: ",transaction.agent.lastTx)
                    #print("\tverifier: ",transaction.verifier)
                    transaction.agent.lastTx = transaction #upaate lastTx

            #sys.exit("DEBUG")
            if self.DLTMode == "linear" or self.DLTMode == "dag":
                ##exchange transactions
                self.transfer_txs_and_blocks(self.agents,  i)
                ##PoW minting
                if self.consensus == "individual": #individual
                    #print("Current Block:\t",currentBlock)
                    #print("block_arrival_times:\t",self.block_arrival_times)
                    if self.currentBlock < len(self.block_arrival_times):
                        while self.block_arrival_times[self.currentBlock] < i:
                            mintingAgent = self.agents[self.block_owners[self.currentBlock]]

                            #print("\n\nminting Agent vis_txs b4 minting:\t",sorted(mintingAgent.get_visible_transactions(), key=lambda x: x.id))
                            #print("minting Agent sub_txs b4 minting:\t",    sorted(mintingAgent.get_submitted_transactions(), key=lambda x: x.id))

                            txs = list(set(mintingAgent.get_visible_transactions()))
                            #print("\tminting txs:",sorted(txs, key=lambda x: x.id))
                            #createBlock(self, txs, agents, creation_time, blockCounter, numAgents, blockLinks):
                            newBlock = self.createBlock(txs, [mintingAgent], i, len(self.blocks), self.no_of_agents, None)
                            self.blocks.append(newBlock)

                            #print("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\nblock ID",newBlock,"-",newBlock.creators,", ",newBlock.blockLinks,":\t",sorted(newBlock.blockTransactions, key=lambda x: x.id))


                            #use up txs
                            mintingAgent.add_submitted_transactions(txs, i)
                            #print("minting Agent vis_txs after minting:\t",sorted(mintingAgent.get_visible_transactions(), key=lambda x: x.id))
                            #print("minting Agent sub_txs after minting:\t",sorted(mintingAgent.get_submitted_transactions(), key=lambda x: x.id),"\n\n")



                            #add block to DG
                            self.DG.add_node(newBlock, pos=(newBlock.creation_time, \
                                    newBlock.creators[0].id*2), \
                                    node_color=self.agent_colors[newBlock.creators[0].id])


                            #choose tsa
                            #maxLink = 1
                            #if self.DLTMode == "dag":
                                #maxLink = self.blocks[0].maxLinks
                                #print("\n",self.DLTMode, " maxLink: ",maxLink)
                            #for j in range(0,maxLink): #TODO CHANGE TO 1 CALL
                                #self.tip_selection(newBlock) #add maxLink blocks
                            self.tip_selection(newBlock)
                            #print("\n\t",newBlock,": numLinks = ",newBlock.blockLinks)

                            if len(newBlock.blockLinks)==0:
                                sys.exit("INCORRECT BLOCK LINKS")
                            #print("newBlock links: ",newBlock.blockLinks)
                            confirmedBlocks = confirmBlocks(newBlock, self.confirmationNumber) #TODO: TRANSFER LINKED BLOCKS from vis


                            mintingAgent.add_visible_blocks([newBlock], i)
                            #mintingAgent.add_confirmed_blocks(confirmedBlocks, i)
                            #mintingAgent.add_linked_blocks(confirmedBlocks, i)


                            #advance currentBlock
                            self.currentBlock+=1

                            ##transfer with new block, redundant but ez to code
                            self.transfer_txs_and_blocks(self.agents,  i)

                            if self.currentBlock <=len(self.block_arrival_times):
                                break #done with this
            elif self.DLTMode == "hashgraph": ##createBlock and transfer as hashgraph
                #trade blocks, if new blocks arrive, create block/event
                self.transfer_txs_and_blocks(self.agents,  i)
                #sys.exit("\ntrade then mint event/block")
            elif self.DLTMode == "dht":
                self.transfer_txs_and_blocks(self.agents, i)#trade and mint newBlock

            start_selection = time.time() #timing for analysis



            update_progress(i/endTime, i)
            #prevTime=transaction.arrival_time ##old system whereby tx.arrival_time was increment
            prevTime=i #last second

            ##DEBUG NICK DELETE ME
            #if i%100==0:
        #        print("\n\n",i)
        #        print("\tConfirmed Blocks: ",len(self.agents[0].get_confirmed_blocks()))
        #        print("\tVis Blocks: ",len(self.agents[0].get_visible_blocks()))

        #        print("\tVis Txs: ", len(self.agents[0].get_visible_transactions()))
        #        print("\tSub Txs: ", len(self.agents[0].get_submitted_transactions()))
        #        print("\tCon Txs: ",len(self.agents[0].get_confirmed_transactions()))



            ##save coordinates in frame for gif making later
            if self.printing:
                print_coordinates_img(self,self.agents, i , self.backgroundImg)

            ##print temporary graph of the system before it is done
           # if (transaction.id==30):
           #     print_graph_temp(self) #temp graph

        if self.printing:
            print("Simulation time: " + str(np.round(timeit.default_timer() - start_time, 3)) + " seconds\n")
            #print_coordinates(self, self.agents,
            print("TX_ID: ",transaction.id)

        #For measuring partitioning
        start_time2 = timeit.default_timer()
        # self.calc_exit_probabilities_multiple_agents(transaction)
        # self.calc_attachment_probabilities(transaction)
        # self.calc_confirmation_confidence_multiple_agents(transaction)



        #paths = list( nx.all_simple_paths(self.DG, source = self.blocks[42], target = self.blocks[35], cutoff = 42-35+1) )
        #print("\n\nPaths from 42-->35: ",paths)
        #flattened = list(set([node for sublist in paths for node in sublist]))
        #print("\nflattened = ",flattened)

        #print("Make DAG")
        #print_graph(self)
        #print("ETC: ",self.ETC)
        #print("ETC AVERAGE: ",self.ETC/self.ETCcount)

        if self.printing:
            print_graph(self)
            print("Calculation time further measures: " + str(np.round(timeit.default_timer() - start_time2, 3)) + " seconds\n")
            #print("\nGraph information:\n" + nx.info(self.DG))




            print("PRINT GIF")
            print_gif(self, self.transactions) ##create gif
            print("DONE PRINTING")
            #print_coordinates(self,self.agents)






    def check_parameters_changes(self, transaction, parameters):

        #If change event for a transaction is provided
        if transaction.id in parameters:
            #If change of distance is provided
            if parameters[transaction.id][0] != False:
                self.distances = parameters[transaction.id][0]
            #If change of agent probabilities is provided
            if parameters[transaction.id][1] != False:
                self.agent_choice = parameters[transaction.id][1]


    #############################################################################
    # SIMULATION: HELPERS
    #############################################################################


    def get_tips(self):

        tips = []
        for transaction in self.DG.nodes:
            if (len(list(self.DG.predecessors(transaction))) == 0):
                tips.append(transaction)

        return tips


    def get_visible_transactions(self, incoming_transaction_time, incoming_transaction_agent):

        #Initialize empty lists (for each transaction these are populated again)
        self.not_visible_transactions = []
        for agent in self.agents:
            agent.visible_transactions = []

        #Loop through all transactions in DAG
        for transaction in self.DG.nodes:
            #get transaction.agent coordinates
            #print(transaction.agent)
            #print(self.agents[transaction.agent.id].coordinates[0])
            if transaction.agent!=None:
                tx_x = self.agents[transaction.agent.id].coordinates[0]
                tx_y = self.agents[transaction.agent.id].coordinates[1]

            #For EACH agent record the currently visible and not visible transactions
            for agent in self.agents:

                #Genesis always visible
                if (transaction.arrival_time == 0):

                    agent.visible_transactions.append(transaction)

                else:
                    #Get distance from agent to agent of transaction from distance matrix
                    #distance = self.distances[agent.id][transaction.agent.id] #old distance 2d matrix

                    distance = math.hypot(agent.coordinates[0] - tx_x, agent.coordinates[1] - tx_y)
                    #Determine if the transaction is visible (incoming_transaction.arrival_time determines current time)
                    if (transaction.arrival_time + self.latency + distance <= incoming_transaction_time):

                        agent.visible_transactions.append(transaction)

                    #Record not visible transactions for 'current agent' only (reduces overhead)
                    elif(incoming_transaction_agent == agent):
                        self.not_visible_transactions.append(transaction)


 ##Create block between N agents within close proximity
    def create_block_nearby(self, agents, time): #radius=distance for tx transfer,
        print("\nCREATING BLOCK\n")


        print("Agents: ",agents)
        ##get all txs
        txs=[]
        for agent in agents:
            print("AGENT TXS: ",agent.get_visible_transactions())
            txs = list(set(txs) | set(agent.get_visible_transactions())) ##combine all freeTxs

        print("TX Unions: ",txs)
        if txs==[]:
            print("No Txs for block")
            return

        ##get links
        visBlocks=[]
        for agent in agents:
            visBlocks = list(set(visBlocks) | set(agent.get_visible_blocks()))

        print("Block #: ", len(self.blocks))
        print("NumBlocks #: ",self.DG.number_of_nodes())
        #newBlock = Block(txs, agents, time, len(self.blocks), self.no_of_agents, None) #None for no new blockLinks (yet)
        #$newBlock = createBlock(self, txs, agents, creation_time, blockCounter, numAgents, blockLinks):

        ##CHANGE THIS OVER
        newBlock = self.createBlock(txs, agents, time, len(self.blocks), self.no_of_agents, None) #create block with nearby nodes
        sys.exit("BROKEN FIX LATER")
        self.blocks.append(newBlock)
        for agent in agents:
            agent.usedTxs=txs
            agent.freeTxs=[]

        self.DG.add_node(newBlock, pos=(newBlock.creation_time, \
                newBlock.creators[0].id*2), \
                node_color=self.agent_colors[newBlock.creators[0].id])


        #choose tsa
        self.tip_selection(newBlock)
        for agent in agents:
            agent.add_visible_blocks([newBlock], time)








    ##TODO: transfer transactions and blocks within radius
    def transfer_txs_and_blocks(self,agents, time): #radius=distance for tx transfer,
        #print("\ntransfering")
        radius=agents[0].radius #save radius in agent variables
        #if agents within radius, transfer txs
        lastAgent=len(agents)-1
        neighbors=[]
        #loop through all agents and append txs

        if self.DLTMode in  ["linear", "dag"]:
            for index, agent in enumerate(agents): #Number of comparison (n^2-n)/2
                #print("\n\n\n\nINDEX: ",index,"\t\tlastAgent: ",lastAgent)

                if (index!=lastAgent): #end condition
                    neighbors=[agents[index]]
                    neighborsCount=0 #number of neighbors, if meets threshold, then create block

                    for i in range(index,len(agents)): #check distance between all agents
                        #print("\tCompare: ",i," - ",index)
                        if (i != index):
                            distance=math.hypot(agents[index].coordinates[0] - agents[i].coordinates[0], agents[index].coordinates[1] - agents[i].coordinates[1])

                            if distance<radius:  # if true then neighbors
                                neighborsCount += 1
                                neighbors.append(agents[i])

                                ##trade blocks
                                #get blocks
                                indexVisBlocks = agents[index].get_visible_blocks()
                                iVisBlocks = agents[i].get_visible_blocks()

                                #indexConfirmedBlocks =  agents[index].get_confirmed_blocks()
                                #iConfirmedBlocks =  agents[i].get_confirmed_blocks()
                                indexLinkedBlocks =  agents[index].get_linked_blocks()
                                iLinkedBlocks =  agents[i].get_linked_blocks()

                                #Trade blocks
                                agents[index].add_visible_blocks(iVisBlocks, time)
                                agents[i].add_visible_blocks(indexVisBlocks, time)

                                #agents[index].add_confirmed_blocks(iConfirmedBlocks, time)
                                #agents[i].add_confirmed_blocks(indexConfirmedBlocks, time)
                                agents[index].add_linked_blocks(iLinkedBlocks, time)
                                agents[i].add_linked_blocks(indexLinkedBlocks, time)

                                ##Trade txs
                                #get txs
                                indexVisibleTxs = agents[index].get_visible_transactions()
                                iVisibleTxs = agents[i].get_visible_transactions()

                                indexSubmittedTxs = agents[index].get_submitted_transactions()
                                iSubmittedTxs = agents[i].get_submitted_transactions()

                                indexConfirmedTxs = agents[index].get_confirmed_transactions()
                                iConfirmedTxs = agents[i].get_confirmed_transactions()

                                #trade txs
                                agents[index].add_visible_transactions(iVisibleTxs, time)

                                agents[i].add_visible_transactions(indexVisibleTxs, time)
                                #CHECK FOR agent 0 visible_tx
                                minIndex= min(i,index)


                                agents[index].add_submitted_transactions(iSubmittedTxs, time)
                                agents[i].add_submitted_transactions(indexSubmittedTxs, time)

                                agents[index].add_confirmed_transactions(iConfirmedTxs, time)
                                agents[i].add_confirmed_transactions(indexConfirmedTxs, time)


                ##localBlock necessity
                if self.consensus == "near":
                    if neighborsCount >= (self.group-1): #need >self.group

                        #newBlock = self.createBlock(txs, [agents[index], agents[i]], time, len(self.blocks), self.no_of_agents, visBlocks)
                        txs = list(set(agents[index].get_visible_transactions()))
                        if len(txs)<1: #not enough txs
                            pass
                            #print("\nNOT ENOUGH TXS")
                            #break #Don't make a new Block
                        else:
                            self.nearbyCounter += 1
                            #print("\nNEAR CONSENSUS: ",neighborsCount," ~ ",neighbors,"   TOT: ",self.nearbyCounter)
                            #print("\tagent: ",agents[index], "   txs: ",txs)
                            #def __init__(self, __txs, __agents, __creation_time, __blockCounter, __numAgents, __blockLinks):

                            newBlock = self.createBlock(txs, neighbors, time, len(self.blocks), self.no_of_agents, None) #create block with nearby nodes
                            #print("\n",newBlock, "'s Time: ",newBlock.creation_time)
                            self.blocks.append(newBlock)
                            #use up txs
                            for n in neighbors:
                                n.add_submitted_transactions(txs, time)

                            #add block to DG
                            self.DG.add_node(newBlock, pos=(newBlock.creation_time, \
                                        newBlock.creators[0].id*2), \
                                        node_color=self.agent_colors[newBlock.creators[0].id])

                            #choose tsa
                            #maxLink = 1
                            #if self.DLTMode == "dag":
                                #maxLink = self.blocks[0].maxLinks
                                #print("\n",self.DLTMode, " maxLink: ",maxLink)
                            #for j in range(0,maxLink):
                                #self.tip_selection(newBlock) #add maxLink blocks
                            self.tip_selection(newBlock)

                            confirmedBlocks = confirmBlocks(newBlock, self.confirmationNumber)

                            for n in neighbors:
                                n.add_visible_blocks([newBlock], time)
                                #n.add_confirmed_blocks(confirmedBlocks, time)
                                #n.add_linked_blocks(confirmedBlocks, time)




                            #advance current block
                            self.currentBlock+=1
                            #print("currentBlock: ",self.currentBlock)

        elif self.DLTMode == "hashgraph":
            ##trade txs like regular, if difference made, if new vis_block created
            for index, agent in enumerate(agents): #Number of comparison (n^2-n)/2

                if (index!=lastAgent): #end condition
                    for i in range(index,len(agents)): #check distance between all agents

                        if (i != index):
                            distance=math.hypot(agents[index].coordinates[0] - agents[i].coordinates[0], agents[index].coordinates[1] - agents[i].coordinates[1])

                            #print("\nTrade Time t/f: ", (agents[index].tradeTime[i]< time-4))
                            #if distance<radius and (agents[index].tradeTime[i]< time-10):  # if true then neighbors #TIME IS FIXED TODO
                            if distance<radius:  # if true then neighbors #TIME IS FIXED TODO

                                ##trade blocks
                                #get blocks
                                indexVisBlocks = agents[index].get_visible_blocks()
                                iVisBlocks = agents[i].get_visible_blocks()

                                #print("viSblocks BEFORE:\t",indexVisBlocks," - ",iVisBlocks)

                                ##NEVER PASS CONFIRMED BLOCKS
                                #indexConfirmedBlocks =  agents[index].get_confirmed_blocks()
                                #iConfirmedBlocks =  agents[i].get_confirmed_blocks()
                                indexLinkedBlocks =  agents[index].get_linked_blocks()
                                iLinkedBlocks =  agents[i].get_linked_blocks()

                                #Trade blocks
                                indexChanged = agents[index].add_visible_blocks(iVisBlocks, time)
                                iChanged = agents[i].add_visible_blocks(indexVisBlocks, time)

                                #print("\nchanged:\t",indexChanged," - ",iChanged)
                                #print("viSblocks AFTER:\t",agents[index].get_visible_blocks()," - ",agents[i].get_visible_blocks())

                                #not passing confirmed blocks
                                #agents[index].add_confirmed_blocks(iConfirmedBlocks, time)
                                #agents[i].add_confirmed_blocks(indexConfirmedBlocks, time)
                                agents[index].add_linked_blocks(iLinkedBlocks, time)
                                agents[i].add_linked_blocks(indexLinkedBlocks, time)

                                ##Trade txs
                                ## HASHGRAPH DOESNT TRADE VIS_TXS - UNLESS SHARING BLOCK OWNERSHIP
                                indexVisibleTxs = agents[index].get_visible_transactions()
                                iVisibleTxs = agents[i].get_visible_transactions()

                                indexSubmittedTxs = agents[index].get_submitted_transactions()
                                iSubmittedTxs = agents[i].get_submitted_transactions()

                                indexConfirmedTxs = agents[index].get_confirmed_transactions()
                                iConfirmedTxs = agents[i].get_confirmed_transactions()

                                #add Txs
                                ##HASHGRAPH DOESNT ADD VIS TXS
                                visTxsIndex = agents[index].add_visible_transactions(iVisibleTxs, time)
                                visTxsI = agents[i].add_visible_transactions(indexVisibleTxs, time)

                                agents[index].add_submitted_transactions(iSubmittedTxs, time)
                                agents[i].add_submitted_transactions(indexSubmittedTxs, time)

                                agents[index].add_confirmed_transactions(iConfirmedTxs, time)
                                agents[i].add_confirmed_transactions(indexConfirmedTxs, time)

                                #mintingAgents = []

                                #MINT BLOCK IF new VisTx and tradeTime has enough elapsed time
                                if (visTxsIndex or visTxsI) and agents[index].tradeTime[i]< time-10: #If new txs, mint new Block
                                    #print("\nMAKE BLOCK")
                                    agents[index].tradeTime[i] = time
                                    agents[i].tradeTime[index] = time

                                    #get txs
                                    txs = agents[index].get_visible_transactions()
                                    #txs += agents[i].get_visible_transactions()
                                    txs = list(set(txs))

                                    ##get blocks to link SHOULD BE ONLY 1 element in submited_blocks
                                    #print("\n\nAgent ",mintingAgent, " Visible Blocks:\t", mintingAgent.get_visible_blocks())
                                    visBlocks = [agents[index].lastMintedBlock]
                                    if agents[i].lastMintedBlock not in visBlocks:
                                        visBlocks.append(agents[i].lastMintedBlock)



                                    #print(visBlocks)
                                    #if len(visBlocks) <1 or len(visBlocks)>2:
                                        #exitMessage = "\n\n"+str(index)+"+"+str(i) + " Wrong vis_blocks: "+ str(visBlocks)
                                        #sys.exit(exitMessage)
                                    #print("visBlocks: ",visBlocks)
                                    #newBlock = self.createBlock(txs, [agents[index], agents[i]], time, len(self.blocks), self.no_of_agents, visBlocks)
                                    newBlock = self.createBlock(txs, [agents[index], agents[i]], time, len(self.blocks), self.no_of_agents, visBlocks)

                                    self.blocks.append(newBlock)

                                    agents[index].add_submitted_transactions(txs,time)
                                    agents[i].add_submitted_transactions(txs,time)

                                    #add block to DG
                                    self.DG.add_node(newBlock, pos=(newBlock.creation_time, \
                                            newBlock.creators[0].id*2), \
                                            node_color=self.agent_colors[newBlock.creators[0].id])

                                    for b in newBlock.blockLinks:
                                        #print("b - ",b)
                                        self.DG.add_edge(newBlock, b) #add tip
                                        #mintingAgent.add_confirmed_blocks(b, time)
                                    agents[i].confirm_all_vis_blocks(time)
                                    agents[i].add_visible_blocks([newBlock], time)

                                    agents[index].confirm_all_vis_blocks(time)
                                    agents[index].add_visible_blocks([newBlock], time)

                                    if agents[index].lastMintedBlock.chainNum!=agents[i].lastMintedBlock.chainNum:
                                        newBlock.witness = True

                                    agents[index].lastMintedBlock=newBlock
                                    agents[i].lastMintedBlock=newBlock

                                    ##check divideRounds

                                    #print("\nPRE-DivideRounds  isWitness(",newBlock.id,") = ",newBlock.witness,"\tAgents:",newBlock.creators)
                                    divideRounds(newBlock, self.witnesses, self.DG, self.no_of_agents, max(self.witnesses.keys()) )

                                    if newBlock.witness==True: #witness add to list
                                        #print("\n\tWITNESS ID: ",newBlock.id)
                                        if newBlock.chainNum in self.witnesses.keys():
                                            #add to existing witness for started round1
                                            #print("ADDING TO EXISTING WITNESSES")
                                            self.witnesses[newBlock.chainNum].append(newBlock)
                                        else:
                                            #new round and new witness
                                            #print("Creating new Witness ROund")
                                            self.witnesses[newBlock.chainNum] = [newBlock]

                                        #check for famous
                                        #if max(self.witnesses.keys())>=3:
                                        #print("\n\tWITNESSES: ",self.witnesses)
                                        #print(max(self.witnesses.keys()))
                                        #print(math.ceil(2/3*self.no_of_agents))
                                        if max(self.witnesses.keys())>=3:
                                            agentWitnesses = set()
                                            for blockWitness in self.witnesses[newBlock.chainNum]:
                                                for aWitness in blockWitness.creators:
                                                    agentWitnesses.add(aWitness)
                                            if len(agentWitnesses) == math.ceil(2/3*self.no_of_agents): #SUPERMAJORITY FOR GIVEN generation

                                            #if len(self.witnesses[newBlock.chainNum]) == math.ceil(2/3*self.no_of_agents): #SUPERMAJORITY FOR GIVEN generation
                                                #1. for each strongly seen witnesses[Maxround-1] by newBlock [maxround
                                                    #2. does it See witnesses[maxround-2]
                                                    #3. if supermajority of witnesses[maxround-1] see target, it is famous
                                                famousWitnesses = []
                                                for w2 in self.witnesses[newBlock.chainNum-2]: #look one back
                                                    #print("\nisFamous(",w2,")")
                                                    #assign famous blocks
                                                    isFamous(w2, newBlock.witnessesStronglySeen, self.DG, self.no_of_agents, time)
                                                    #collect famous
                                                    if w2.famous==True:
                                                        famousWitnesses.append(w2)

                                                #order event
                                                self.stragglers = orderRange(famousWitnesses,  self.stragglers, self.DG, self.witnesses,  self.no_of_agents, time, self.blocks)#, newBlock.chainNum-1)
                                                #print("\nBlock: ",newBlock, " round: ", newBlock.chainNum," -- stragglers", self.stragglers)
        elif self.DLTMode == "dht": #tree-chain
            ##trade txs like regular, if difference made, if new vis_block created
            for index, agent in enumerate(agents): #Number of comparison (n^2-n)/2

                if (index!=lastAgent): #end condition
                    neighbors=[agents[index]]
                    neighborsCount=0 #number of neighbors, if meets threshold, then create block

                    for i in range(index,len(agents)): #check distance between all agents

                        if (i != index):
                            distance=math.hypot(agents[index].coordinates[0] - agents[i].coordinates[0], agents[index].coordinates[1] - agents[i].coordinates[1])

                            #print("\nTrade Time t/f: ", (agents[index].tradeTime[i]< time-4))
                            if distance<radius:  # if true then neighbors #TIME IS FIXED TODO
                                #print("\nclose and time T")
                                neighborsCount += 1
                                neighbors.append(agents[i])

                                ##Trade visTxs
                                indexVisibleTxs = agents[index].get_visible_transactions()
                                iVisibleTxs = agents[i].get_visible_transactions()

                                #add vis_txs
                                indexChanged = agents[index].add_visible_transactions(iVisibleTxs, time)
                                iChanged = agents[i].add_visible_transactions(indexVisibleTxs, time)


                                ##Sign txs who
                                if (indexChanged): #Check if we need to mint a newBlock
                                    #print("\n",agents[index], " - vis b4: ",agents[index].get_visible_transactions())
                                    #print(agents[index], " - submitted b4: ",agents[index].get_submitted_transactions())
                                    agents[index].signTxs(time)
                                    #print(agents[index], " - vis after: ",agents[index].get_visible_transactions())
                                    #print(agents[index], " - submitted b4: ",agents[index].get_submitted_transactions())
                                if (iChanged): #Check if we need to mint a newBlock
                                    agents[i].signTxs(time)


                                ##trade submitted Txs + confirmed Tx
                                indexSubmittedTxs = agents[index].get_submitted_transactions()
                                iSubmittedTxs = agents[i].get_submitted_transactions()

                                #add Txs
                                agents[index].add_submitted_transactions(iSubmittedTxs, time)
                                agents[i].add_submitted_transactions(indexSubmittedTxs, time)



                                ##mint new Block
                                for a in [agents[index], agents[i]]:
                                    #print("agents, ", [agents[index], agents[i]])
                                    #sys.exit("Debug")
                                    ##get validated Txs
                                    validatedTxs=[]
                                    if self.consensus=="simple":
                                        #print("SIMPLE ")
                                        validatedTxs = a.validateTxsSimple(time) #simple way with no signage
                                    else:
                                        validatedTxs = a.validateTxs(time) ##Proper way with tx in->out
                                    #validatedTxs = a.validateTxs(time) ##Proper way with tx in->out
                                    #validatedTxs = a.validateTxsSimple(time) #simple way with no signage
                                    #print("\n",a, " - Validated Txs: ",validatedTxs)
                                    #newBlock
                                    if len(validatedTxs)>0:
                                        for vtx in validatedTxs:
                                            if vtx.outTx!=None:
                                                self.ETC+= time-math.ceil(vtx.arrival_time)
                                                self.ETCcount += 1
                                                #print("\n\nTime: ",time," Validated Tx: ",vtx," SignedTime: ",vtx.signedTime, " DIFF:",time-vtx.signedTime,"\tVerifier: ",vtx.verifier, " signer: ",vtx.outTx.agent,"\tCreator", vtx.agent," Arrival: ",math.ceil(vtx.arrival_time), " ETC: ",self.ETC)

                                        if a.lastBlock != None:
                                            newBlock = self.createBlock(validatedTxs, [a], time, len(self.blocks), self.no_of_agents, [a.lastBlock])
                                        else:
                                            newBlock = self.createBlock(validatedTxs, [a], time, len(self.blocks), self.no_of_agents, None)
                                        a.lastBlock = newBlock #replace lastBlock
                                        self.blocks.append(newBlock)

                                        a.add_confirmed_transactions(validatedTxs,time)
                                        #agents[i].add_submitted_transactions(txs,time)

                                        #add block to DG
                                        self.DG.add_node(newBlock, pos=(newBlock.creation_time, \
                                                newBlock.creators[0].id*2), \
                                                node_color=self.agent_colors[newBlock.creators[0].id])

                                        for b in newBlock.blockLinks:
                                            #print("b - ",b)
                                            self.DG.add_edge(newBlock, b) #add tip
                                            #mintingAgent.add_confirmed_blocks(b, time)
                                        ##Add to confirmedBlocks
                                        #a.add_confirmed_blocks([newBlock], time)
                                        a.add_linked_blocks([newBlock], time)
                                        newBlock.confirmed = True #all of these are confirmed
                                        newBlock.confirmationTime = time

                                #confirmed Txs
                                indexConfirmedTxs = agents[index].get_confirmed_transactions()
                                iConfirmedTxs = agents[i].get_confirmed_transactions()

                                agents[index].add_confirmed_transactions(iConfirmedTxs, time)
                                agents[i].add_confirmed_transactions(indexConfirmedTxs, time)

                                ##trade blocks - ONLY CONFIRMED/LINKED BLOCKS
                                ##get confirmedBlocks
                                #indexConfirmedBlocks =  agents[index].get_confirmed_blocks()
                                #iConfirmedBlocks =  agents[i].get_confirmed_blocks()

                                ##add_confirmed_blocks
                                #agents[index].add_confirmed_blocks(iConfirmedBlocks, time)
                                #agents[i].add_confirmed_blocks(indexConfirmedBlocks, time)
                                #get confirmedBlocks
                                indexLinkedBlocks =  agents[index].get_linked_blocks()
                                iLinkedBlocks =  agents[i].get_linked_blocks()

                                #add_confirmed_blocks
                                agents[index].add_linked_blocks(iLinkedBlocks, time)
                                agents[i].add_linked_blocks(indexLinkedBlocks, time)


        #print("\nEXITING TRANSFER")

    #returns valid tips for a given agent
    def get_valid_tips_multiple_agents(self, agent):

        valid_tips = []

        if len(agent.get_visible_blocks())>0: #check if there exists vis_blocks
            for b in agent.get_visible_blocks(): #
                if(len(list(self.DG.predecessors(b))) == 0):
                    valid_tips.append(b)

                #add to valid tips if no approvers are visible yet
                elif(len(set(list(self.DG.predecessors(b))).intersection(set(agent.get_visible_blocks()))) == 0): #if no predecssors of tx are in visible transaction
                    #print("\n\n!!!!!!! HASN'T SEEN NEW LINK YET !!!!\n\n")
                    valid_tips.append(b)

        return valid_tips


    def all_approvers_not_visible(self, transaction):
        return set(list(self.DG.predecessors(transaction))).issubset(set(self.not_visible_transactions))


    def calc_transition_probabilities_multiple_agents(self, approvers, agent):

        weights = [approver.cum_weight_multiple_agents[agent] for approver in approvers]
        normalized_weights = [weight - max(weights) for weight in weights]

        denominator_transition_probabilities = sum([math.exp(self.alpha * weight) \
        for weight in normalized_weights])

        return [math.exp(self.alpha * (approver.cum_weight_multiple_agents[agent] \
                - max(weights))) / denominator_transition_probabilities \
                for approver in approvers]


    #############################################################################
    # TIP-SELECTION: RANDOM
    #############################################################################


    #currently adds 1 blockLink to block
    #1. get valid_tips from agent 0 (EXTREMELY SLOW REORDER)
    #2. Get longestTip from valid_tips
    #3. Assign to block
    def longest_selection(self, block):
        #print("\nCreator: ",block.creators)
        #print("\nVis_blocks: ",block.creators[0].get_visible_blocks())
        valid_tips = self.get_valid_tips_multiple_agents(block.creators[0])
        block.creators[0].record_tips.append(valid_tips)

        #self.record_tips.append(valid_tips)


        longestBlockCount = -1 #chainNum
        selectedTip = None

        #print("Valid Tips: ",valid_tips)
        for tip in valid_tips:
            if tip not in block.blockLinks:
                if (tip.chainNum >longestBlockCount): #longer chain
                    longestBlockCount = tip.chainNum
                    selectedTip = tip
                elif (tip.chainNum ==longestBlockCount): #if tie, pick largest # tips
                    if len(tip.blockTransactions) > len(selectedTip.blockTransactions):
                        selectedTip = tip


        if selectedTip != None:
            self.DG.add_edge(block, selectedTip) #add tip
            block.blockLinks.append(selectedTip)
            block.chainNum = max(longestBlockCount + 1, block.chainNum)



    ##NEW SYSTEM
    #1. Order visible_blocks by chainNum
    #2. Add biggest chainnum to block
    #3. For rest (refs-1):
        #3a. check if tip is in visible_blocks
        #3b. add if so
    def longest_selection_fast(self, block): ##TODO TEST OUT
        #print("\nStart longest_selection_fast: ")
        #1. order visible blocks by chainNum
        sortedVisBlocks = sorted(block.creators[0].get_visible_blocks(), key=lambda x: x.chainNum, reverse=True )


        #print("\nSorted Visible Blocks: ")
        #for svb in sortedVisBlocks:
        #    print("\t", svb.chainNum,"\tconfirmed: ",svb.confirmed)


        #2. auto add largest sortedVisBlocks
        #self.DG.add_edge(block, sortedVisBlocks[0]) #add tip
        #block.blockLinks.append(sortedVisBlocks[0])

        block.chainNum = max(sortedVisBlocks[0].chainNum + 1, block.chainNum)

        #3. For rest
        linkedBlocks = [sortedVisBlocks[0]]
        alreadyLinkedBlocks = []
        longestIndex= 1
        #print("\n\n\n\n\n\n\n\n\n\n\n\nMax # of refs: ",self.references,"\n\n\n\n\n\n\n\n\n")
        for i in range(1,self.references): #for number of refs
            #print("\tcurrent Reference #: ",i,"\t\t",linkedBlocks)
            findTip=True
            while findTip and longestIndex<len(sortedVisBlocks): #find tip
                #print("\t\tWHILE LOOP")\
                #print("\t\tPotential Tip: ",sortedVisBlocks[longestIndex]," Chain: ",sortedVisBlocks[longestIndex].chainNum, "\tnewBlock chainNum: ",block.chainNum)
                for lb in linkedBlocks:
                    #print("\t\t",lb)
                    seen =  BFS(lb, sortedVisBlocks[longestIndex], self.DG)
                    #print("\t\t\tSeen: ",seen)
                    if seen==False: #add to linkedBlocks
                        findTip=False #break while loop
                        linkedBlocks.append(sortedVisBlocks[longestIndex]) #add to linkedBlocks list
                        break #breaks out of for loop and stays in while loop
                    else: #seen==True, then move to confirmed_blocks instead
                        alreadyLinkedBlocks.append(sortedVisBlocks[longestIndex])
                longestIndex = longestIndex + 1 #increment

        #print("Final linkedBlocks: ",linkedBlocks)
        #self.DG.add_edge(block, linkedBlocks) #add tips
        #block.blockLinks.append(linkedBlocks)
        for lb in linkedBlocks:
            block.blockLinks.append(lb)
            self.DG.add_edge(block, lb)


        #print("\nALB: ",alreadyLinkedBlocks)
        ##deal with alreadyLinkedBlocks
        for c in block.creators:
            #c.add_confirmed_blocks(alreadyLinkedBlocks, block.creation_time
            c.add_linked_blocks(alreadyLinkedBlocks + linkedBlocks, block.creation_time)

        #print("VIS BLOCKS: ",block.creators[0].get_visible_blocks())
        #print("LINKED BLOCKS: ",block.creators[0].get_linked_blocks())


    def random_selection(self, block):

        #Needed for plotting number of tips over time for ALL agents
        #for agent in self.agents:
        #    if(agent != transaction.agent):
        #        self.get_visible_transactions(transaction.arrival_time, agent)
        #        valid_tips = self.get_valid_tips_multiple_agents(agent)
        #        agent.record_tips.append(valid_tips)

        #Get visible transactions and valid tips (and record these)
        #self.get_visible_transactions(transaction.arrival_time, transaction.agent)
        valid_tips = self.get_valid_tips_multiple_agents(block.creators[0])
        block.creators[0].record_tips.append(valid_tips)
        self.record_tips.append(valid_tips)


        ##error check print all transactions
        #print("\nAvailable Txs")
        #for tx in transaction.agent.visible_transactions:
        #    print(tx) #seen node_color
        #    print(list(self.DG.predecessors(tx)))
        #    print(len(list(self.DG.predecessors(tx))))

        #Reference 2-8 random tips
        valid_tips2 = valid_tips.copy() #create copy to edit
        #print("Valid Tips: ",valid_tips2)
        for count, tip in enumerate(valid_tips):
            if count>7: #no more than 8 tips
                break
            tempTip = np.random.choice(valid_tips2) #get randomTip
            self.DG.add_edge(block, tempTip) #add tip
            valid_tips2.remove(tempTip) #remove tempTip from working valid_tips2
            block.blockLinks.append(tempTip)

    #############################################################################
    # Find an Entry_Point randomly from 100lambda~200lambda txs ago for TIP-SELECTION walk
    #############################################################################

    def find_entry_point(self, transaction):
        tx_idx = 0
        if transaction.id > 100 * self.lam:
            low = max(0, transaction.id- 200*self.lam)
            high = transaction.id - 100*self.lam
            tx_idx = np.random.randint(low=low, high=high)
        return self.transactions[tx_idx]


    #############################################################################
    # TIP-SELECTION: UNWEIGHTED
    #############################################################################

    def unweighted_MCMC(self, transaction):

        #Needed for plotting number of tips over time for ALL agents
        #for agent in self.agents:
        #    if(agent != transaction.agent):
        #        self.get_visible_transactions(transaction.arrival_time, agent)
        #        valid_tips = self.get_valid_tips_multiple_agents(agent)
        #        agent.record_tips.append(valid_tips)

        valid_tips = self.get_valid_tips_multiple_agents(transaction.agent)
        transaction.agent.record_tips.append(valid_tips)
        self.record_tips.append(valid_tips)

        #Get visible transactions and valid tips (and record these)
        #self.get_visible_transactions(transaction.arrival_time, transaction.agent)
        #valid_tips = self.get_valid_tips_multiple_agents(transaction.agent)
        transaction.agent.record_tips.append(valid_tips)
        self.record_tips.append(valid_tips)


        valid_tips2=valid_tips.copy() #create copy to edit

        for count, tip in enumerate(valid_tips):
            if count>7: #no more than 8 tips
                break
            #tempTip = np.random.choice(valid_tips2) #get randomTip
            tempTip = self.unweighted_random_walk(transaction, valid_tips2)
            self.DG.add_edge(transaction, tempTip) #add tip
            valid_tips2.remove(tempTip) #remove tempTip from working valid_tips2


        #Walk to two tips
        #tip1 = self.unweighted_random_walk(transaction, valid_tips)
        #tip2 = self.unweighted_random_walk(transaction, valid_tips)

        #Add tips to graph (only once)
        #self.DG.add_edge(transaction,tip1)
        #if(tip1 != tip2):
        #    self.DG.add_edge(transaction,tip2)


    def unweighted_random_walk(self, transaction, valid_tips):
        print("Start Unweighted Random Walk")
        # Start walk at genesis
        walker_on = self.transactions[0]

        #If only genesis a valid tip, approve genesis
        if (valid_tips == [walker_on]):
            return walker_on

        while (walker_on not in valid_tips):

            approvers = list(self.DG.predecessors(walker_on))
            print("Approvers: ", list(self.DG.predecessors(walker_on)))
            visible_approvers = common_elements(approvers, transaction.agent.get_visible_transactions())
            print("Visible_approvers: ", visible_approvers)
            walker_on = np.random.choice(visible_approvers)

        return walker_on


    #############################################################################
    # TIP-SELECTION: WEIGHTED
    #############################################################################


    def weighted_genesis_MCMC(self, transaction):

        ##Needed for plotting number of tips over time for ALL agents
        #for agent in self.agents:
        #    if(agent != transaction.agent):
        #        self.get_visible_transactions(transaction.arrival_time, agent)
        #        valid_tips = self.get_valid_tips_multiple_agents(agent)
        #        agent.record_tips.append(valid_tips)

        ##Get visible transactions and valid tips (and record these)
        #self.get_visible_transactions(transaction.arrival_time, transaction.agent)
        #valid_tips = self.get_valid_tips_multiple_agents(transaction.agent)
        #transaction.agent.record_tips.append(valid_tips)
        #self.record_tips.append(valid_tips)


        #Collect valid tips and record them
        valid_tips = self.get_valid_tips_multiple_agents(transaction.agent)
        transaction.agent.record_tips.append(valid_tips)
        self.record_tips.append(valid_tips)

        #Walk to two tips
        tip1 = self.weighted_random_walk(transaction, valid_tips, self.transactions[0])
        tip2 = self.weighted_random_walk(transaction, valid_tips, self.transactions[0])

        #Add tips to graph (only once)
        self.DG.add_edge(transaction, tip1)
        if (tip1 != tip2):
            self.DG.add_edge(transaction, tip2)


    def weighted_entry_point_MCMC(self, transaction):
        ## Needed for plotting number of tips over time for ALL agents
        #for agent in self.agents:
        #    if (agent != transaction.agent):
        #        self.get_visible_transactions(transaction.arrival_time, agent)
        #        valid_tips = self.get_valid_tips_multiple_agents(agent)
        #        agent.record_tips.append(valid_tips)

        ## Get visible transactions and valid tips (and record these)
        #self.get_visible_transactions(transaction.arrival_time, transaction.agent)
        #valid_tips = self.get_valid_tips_multiple_agents(transaction.agent)
        transaction.agent.record_tips.append(valid_tips)
        self.record_tips.append(valid_tips)

        # Walk to two tips
        tip1 = self.weighted_random_walk(transaction, valid_tips, self.find_entry_point(transaction))
        tip2 = self.weighted_random_walk(transaction, valid_tips, self.find_entry_point(transaction))

        self.DG.add_edge(transaction, tip1)
        if (tip1 != tip2):
            self.DG.add_edge(transaction, tip2)


    def weighted_random_walk(self, transaction, valid_tips, initial_walker_on):

        #Start walk at genesis
        walker_on = initial_walker_on

        # #Start walk at genesis
        # walker_on = self.transactions[0]

        #If only genesis a valid tip, approve genesis
        if (valid_tips == [walker_on]):
            return walker_on

        while (walker_on not in valid_tips):

            approvers = list(self.DG.predecessors(walker_on))
            visible_approvers = common_elements(approvers, transaction.agent.visible_transactions)
            transition_probabilities = self.calc_transition_probabilities_multiple_agents(visible_approvers, transaction.agent)

            #Choose with transition probabilities
            walker_on = np.random.choice(visible_approvers, p=transition_probabilities)

        return walker_on


    #############################################################################
    # CONFIRMATION CONFIDENCE: MULTI AGENT
    #############################################################################


    def update_weights_multiple_agents(self, incoming_transaction):

        entrypoint = self.find_entry_point(incoming_transaction)

        # Update all descendants of incoming_transaction only (cum_weight += 1)
        for transaction in nx.descendants(self.DG, incoming_transaction):

            if transaction.arrival_time >= entrypoint.arrival_time:
                # Update for each agent separately
                for agent in self.agents:
                    if transaction in agent.visible_transactions:
                        transaction.cum_weight_multiple_agents[agent] += 1


    def calc_exit_probabilities_multiple_agents(self, incoming_transaction):

        for agent in self.agents:

            #Reset exit probability of all transactions to 0%, just needed when run multiple times throughout simulation
            for transaction in self.DG.nodes:
                transaction.exit_probability_multiple_agents[agent] = 0

            #Set genesis to 100%
            self.transactions[0].exit_probability_multiple_agents[agent] = 1

            #Determine visible transaction for t + 1, so that all transactions (h = 1) are included
            #self.get_visible_transactions(incoming_transaction.arrival_time + self.latency, agent)

        #Start at genesis, tips in the end
        sorted = list(reversed(list(nx.topological_sort(self.DG))))

        #Calculate exit probabilities
        for transaction in sorted:

            for agent in self.agents:

                if (transaction in agent.visible_transactions):

                    #Get visible direct approvers and transition probabilities to walk to them
                    approvers = list(self.DG.predecessors(transaction))
                    visible_approvers = common_elements(approvers, agent.visible_transactions)
                    transition_probabilities = self.calc_transition_probabilities_multiple_agents(visible_approvers, agent)

                    #For every visible direct approver update the exit probability by adding the exit probability
                    #of the current transaction times the transition probabilitiy of walking to the approver
                    for (approver, transition_probability) in zip(visible_approvers, transition_probabilities):
                        approver.exit_probability_multiple_agents[agent] += (
                                    transaction.exit_probability_multiple_agents[agent] * transition_probability)


    def calc_confirmation_confidence_multiple_agents(self, incoming_transaction):

        #Loop over agents and get visible transactions and valid tips
        for agent in self.agents:
            #self.get_visible_transactions(incoming_transaction.arrival_time + self.latency, agent)
            agent.tips = self.get_valid_tips_multiple_agents(agent)

            #Loop over visible transactions
            for transaction in agent.visible_transactions:
                #Reset confirmation confidence to 0%, just needed when function called multiple times during simulation
                # transaction.confirmation_confidence_multiple_agents[agent] = 0

                #Loop over valid tips
                for tip in agent.tips:

                    if(nx.has_path(self.DG,tip,transaction) and tip != transaction):

                        transaction.confirmation_confidence_multiple_agents[agent] += tip.exit_probability_multiple_agents[agent]

                    #Tips have 0 confirmation confidence by default
                    tip.confirmation_confidence_multiple_agents[agent] = 0


    #Uses exit probabilities to caluclate attachment probabilities
    def calc_attachment_probabilities(self, incoming_transaction):

        attachment_probabilities_without_main = []
        attachment_probabilities_all = []

        self.calc_exit_probabilities_multiple_agents(incoming_transaction)

        for agent in self.agents:
            #self.get_visible_transactions(incoming_transaction.arrival_time + self.latency, agent)
            agent.tips = self.get_valid_tips_multiple_agents(agent)

        for agent in self.agents:

            sum_ = 0
            sum_ = sum(tip.exit_probability_multiple_agents[agent] for tip in agent.tips if tip.agent == agent)

            for other_agent in self.agents:
                if(other_agent != agent):
                    sum_ += sum(tip.exit_probability_multiple_agents[other_agent] for tip in other_agent.tips if tip.agent == agent)

            attachment_probabilities_all.append(sum_/self.no_of_agents)

            if(agent != self.agents[0]):
                attachment_probabilities_without_main.append(sum_/self.no_of_agents)

        # print(attachment_probabilities_without_main)
        # print(attachment_probabilities_all)
        return attachment_probabilities_all
