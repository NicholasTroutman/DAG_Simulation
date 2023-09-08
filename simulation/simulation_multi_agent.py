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
from operator import add

from simulation.helpers import update_progress, create_distance_matrix, \
common_elements, clamp, load_file, create_coordinates, create_coordinates_nodes
from simulation.mapMaker import Distance, DistanceToVector,  FindEdges, IdentfiyBlueEdgeIntersection,  LoadImageIntoGraph, isBetween
from simulation.plotting import print_info, print_graph, print_graph_temp, print_coordinates, print_coordinates_img, print_tips_over_time, print_gif, print_tips_over_time_multiple_agents, print_tips_over_time_multiple_agents_with_tangle, print_attachment_probabilities_alone,print_attachment_probabilities_all_agents, print_coordinates_img_including_baseStations
from simulation.agent import Agent
from simulation.transaction import Transaction
from simulation.block import Block, BaseStationBlock

from simulation.baseStation import BaseStation

class Multi_Agent_Simulation:
    def __init__(self, _no_of_transactions, _lambda, _no_of_agents, \
                 _alpha, _distance, _tip_selection_algo, _latency = 1, \
                 _agent_choice=None, _printing=False, _lambda_m=1/60, _seed=10, _map="DCRedBlue.png"):

        #Use configuration file when provided
        if(len(sys.argv) == -1): #nick changed to make obsolete config File
            self.config = load_file(sys.argv[1])
            self.no_of_transactions = self.config[0][0]
            self.lam = self.config[0][1]
            self.no_of_agents = self.config[0][2]
            self.alpha = self.config[0][3]
            self.latency = self.config[0][4]
            self.distances = self.config[0][5]
            self.tip_selection_algo = self.config[0][6]
            self.agent_choice = self.config[0][7]
            self.printing = self.config[0][8]
            self.total_num_tx = self.config[0][0]
        #Otherwise use the provided parameters
        else:
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
            self.map = _map

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

        self.baseStations= [] #stationary base Stations
        
        ##block variables
        self.blocks=[]

        #For analysis only
        self.record_tips = []
        self.record_attachment_probabilities = []

        #For max. four agents always the same colors in prints
        self.agent_colors = ['#a8d6ff', '#ff9494', '#dcc0dd', '#e0ff80']
        self.agent_tip_colors = ['#f5faff', '#ffe0e0', '#f8f2f8', '#f9ffe6']

        #For more than four agents random colors and lighter tip colors
        for i in range(self.no_of_agents-4):
            r = lambda: random.randint(0,255)
            color = '#{:02x}{:02x}{:02x}'.format(r(), r(), r())
            self.agent_colors.append(color)
            self.agent_tip_colors.append(color)


    #############################################################################
    # SIMULATION: SETUP
    #############################################################################




    def setup(self):

        np.random.seed(self.seed) #TODO: CHANGE ME TO BE COMMANDLINE BASED ##MAGIC NUMBER
        #Create agents
        agent_counter = 0
        for agent in range(self.no_of_agents):
            self.agents.append(Agent(agent_counter))
            agent_counter += 1

        #Create directed graph object
        self.DG = nx.DiGraph()
        self.traffic, self.backgroundImg= LoadImageIntoGraph(self.map) #hardcoded image, graph and image saved
        
        print("Traffic: ",self.backgroundImg.shape)
        
        #create travelling sales person (TSPTSP)
        self.tsp = nx.approximation.traveling_salesman_problem
        #Create agent coordinates & destination
        create_coordinates_nodes(self.agents, self.traffic, self.tsp)
    
        #Create random arrival times
        inter_arrival_times = np.random.exponential(1 / self.lam, self.no_of_transactions)
        self.arrival_times = list(np.cumsum(inter_arrival_times))
        
        #if milestone issue rate is not zero, calculate number of milestones
        if self.lam_m != 0:
            num_of_milestones = int((self.no_of_transactions / self.lam) * self.lam_m)
            self.total_num_tx += num_of_milestones
            for i in range(num_of_milestones):
                self.arrival_times.append((1/self.lam_m)*(i+1))
        
        self.arrival_times.sort()

        #Create genesis transaction object, store in list and add to graph object
        transaction_counter = 0
        self.transactions.append(Transaction(0, transaction_counter, self.no_of_agents))
        #print("genesis: ",self.transactions[0].seen)
        
        #for count, seenTime in enumerate(self.transactions[0].seen):
        #    self.transactions[0].seen[count]=0
            
        #self.DG.add_node(self.transactions[0], pos=(0, 0), no=transaction_counter, node_color='#99ffff')

        transaction_counter += 1

        #Create other transaction objects and store in list
        for i in range(len(self.arrival_times)):
            self.transactions.append(Transaction(self.arrival_times[i], transaction_counter, self.no_of_agents)) #create transactions + seen list
            transaction_counter += 1
            
            
            
            
        ##set up PRNG routes
        #number of routes/sim 1/50*2 1/25 -->txs/4 is destinations
        numDest = self.no_of_transactions/4
        for agent in self.agents:
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
    
        ##create BaseStations
        for i in range(0,2): #hardcodes for coordinate position=node[id]
            
            coordinates=self.traffic.nodes[i]['pos']#get permenant coordinates for static baseStation
            #print(i)
            #print(coordinates)
            self.baseStations.append(BaseStation(i,coordinates)) #BaseStation(id,permenantPosition)




        #Move all agents
    def moveAgents(self, arrival_time, prevTime):
        for agent in self.agents:
           #print("Moving Agent:\t",agent)
            agent.past_coordinates.append(agent.coordinates)
            #print("\t",agent.past_coordinates)
            ##old system random walk
            #agent.coordinates=agent.coordinates + np.random.normal(0,6,2)*(transaction.arrival_time-prevTime)
            
            #new system directed TSP
            #Speed/second
            speed = agent.speed
            #print("\tSpeed: ",speed)
            scalar=(arrival_time-prevTime)*speed
            #print("\tScalar: ",scalar)
            vector = [v*scalar for v in agent.vector]
            #print("\tVector: ",vector)
            newCoord = list( map(add, agent.coordinates, vector))
            #print("\tNewCoord: ",newCoord)
           
            
            
            if (isBetween(newCoord, self.traffic.nodes[agent.destination[0]]['pos'], agent.coordinates)==False): #epsilon=0.5, if true, then not between
                #print("\tNot at intersection yet")
                agent.coordinates=newCoord
            else: #overshot, new coordinates are agent.destination[0]
                #print("\tOvershot")
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
                    #set new TSP path
                    agent.destination=self.tsp(self.traffic, nodes=[newDest, agent.prev_dest], cycle=False)[1:]
                    #set slope and vector
                    streetSlope=[ self.traffic.nodes[agent.destination[0]]['pos'][0]- self.traffic.nodes[agent.prev_dest]['pos'][0],  self.traffic.nodes[agent.destination[0]]['pos'][1] - self.traffic.nodes[agent.prev_dest]['pos'][1]  ] 
                    
                    agent.vector=streetSlope/np.linalg.norm(streetSlope)
                    
            
            #If coordinates are outside boundaries [0,backgroundImg.shape[0]] bounce them back in, 101-->99, -1-->1. bounce around boundary, not pacman

            if agent.coordinates[0]>self.backgroundImg.shape[1] or agent.coordinates[0]<0:
                print("\tAgent coordinates: ",agent.coordinates[0],"  - ",self.backgroundImg.shape[0])
                print("ERROR:\tPACMAN BOUNCE?!")
                agent.coordinates[0]=-1*agent.coordinates[0]%self.backgroundImg.shape[0] #100 MAGIC NUMBER
            if agent.coordinates[1]>self.backgroundImg.shape[0] or agent.coordinates[1]<0:
                                agent.coordinates[1]=-1*agent.coordinates[1]%self.backgroundImg.shape[1] #MAGIC NUMBER
           

    def cleanOldTxsAndBlocks(self, transaction):
        if (transaction.id >= 0 and transaction.id % 400 == 0):
                
                ##remove old txs
                for agent in self.agents:
                    saveTxs=[]
                    #vis_txs=agent.get_visible_transactions()
                    #print("DEBUG Vis Txs:\t")
                    #for vtxs in vis_txs:
                     #   print(vtxs.__class__.__name__ , end =", "))
                       
                    #print("\n")
                    for count, tx in enumerate(agent.get_visible_transactions()):
                        #print("Tx_arrival_time: ",transaction.arrival_time, " DIFF: ",transaction.arrival_time - tx.arrival_time)
                        if transaction.arrival_time - tx.arrival_time < 400: ##MAGIC NUMBER 400
                            saveTxs.append(tx)
                            
                    agent._visible_transactions=saveTxs #TODO: using _visible_transactions is iffy, maybe turn into function?
                
                ##remove old blocks
                for agent in self.agents:
                    saveBlocks=[]
                    for count, b in enumerate(agent.get_visible_blocks()):
                        if transaction.arrival_time - b.creation_time < 400: ##MAGIC NUMBER 400
                            saveBlocks.append(b)
                    agent._visible_blocks=saveBlocks #TODO: using _visible_blocks is iffy, maybe turn into function?



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
        #print_coordinates_img(self,self.agents,0, self.backgroundImg)
        print_coordinates_img_including_baseStations(self,self.agents, self.baseStations, 0 , self.backgroundImg)
            
        
        #append genesis transaction block to users
        #for agent in self.agents:
            #agent.add_visible_transactions([self.transactions[0]], 0)
            #visible_transactions.append(self.transactions[0]) 
        
        for s in self.transactions[0].seen:
            s=0 #set seen for everyone at 0
                
        ##Start loop
        #Start with first transaction (NOT genesis)
        #for transaction in self.transactions[1:]: ##Loop for each tx

        ##Loop with 1 second between
        endTime = math.ceil(self.transactions[-1].arrival_time)
        currentTx = 1
        for i in range(1,endTime):
            mintedTxs=[]
            while self.transactions[currentTx].arrival_time < i: #has this tx been minted
                mintedTxs.append(self.transactions[currentTx]) #if so add to mintedTxs
                currentTx +=1 #check next Tx
            




            ##Move agents
            self.moveAgents(i, prevTime)
               
                            
            ##Do something every 400 to clean visible_transactions
            

            
            #mint Txs
            for transaction in mintedTxs: #for each minted tx since last time increment:
                transaction.agent = np.random.choice(self.agents, p=self.agent_choice) #choose agent
                transaction.agent.add_visible_transactions([transaction],  transaction.arrival_time) #add tips to minted tx
                self.cleanOldTxsAndBlocks(transaction) ##Do something every 400 to clean visible_transactions

            ##exchange transactions           
            self.transfer_txs_and_blocks(self.agents,  i) #time is i instead of arrival_time
            


            #Add transaction to directed graph object (with random y coordinate for plotting the graph)
            #self.DG.add_node(transaction, pos=(transaction.arrival_time, \
                #np.random.uniform(0, 1)+transaction.agent.id*2), \
                #node_color=self.agent_colors[transaction.agent.id])

            #start_selection = time.time() #timing for analysis
           
           #Select tips
            #self.tip_selection(transaction)
            
            #append transacion to visible transactions
            #transaction.agent.add_visible_transactions([transaction], transaction.arrival_time)
            
            #ts_time = np.round(time.time() - start_selection, 5)
            #transaction.set_tip_selection_time(ts_time)

            #start_update_weight = time.time()
            
            
            #Update weights (of transactions referenced by the current transaction)
            #if("weighted-" in self.tip_selection_algo):
            #    self.update_weights_multiple_agents(transaction)
           # weight_update_time = np.round(time.time() - start_update_weight, 5)
            #transaction.set_weight_update_time(weight_update_time)



            #update_progress(transaction.id/self.total_num_tx, transaction) ##transaction increment system

            #Progress bar update
            #if self.printing:
            #    update_progress(transaction.id/self.total_num_tx, transaction)
            
            update_progress(i/endTime, i)
            #prevTime=transaction.arrival_time ##old system whereby tx.arrival_time was increment
            prevTime=i-1 #last second


            ##save coordinates in frame for gif making later
            if self.printing:
                #print_coordinates_img(self,self.agents, i , self.backgroundImg)
                print_coordinates_img_including_baseStations(self,self.agents, self.baseStations, i , self.backgroundImg)
            ##print temporary graph of the system before it is done
           # if (transaction.id==30):
           #     print_graph_temp(self) #temp graph

        if self.printing:
            print("Simulation time: " + str(np.round(timeit.default_timer() - start_time, 3)) + " seconds\n")
            #print_coordinates(self, self.agents,
            print("TX_ID: ",transaction.id)

        #For measuring partitioning
        #start_time2 = timeit.default_timer()
        # self.calc_exit_probabilities_multiple_agents(transaction)
        # self.calc_attachment_probabilities(transaction)
        # self.calc_confirmation_confidence_multiple_agents(transaction)


        print("Show DAG")
        print_graph(self)
        if self.printing:
            #print("Calculation time further measures: " + str(np.round(timeit.default_timer() - start_time2, 3)) + " seconds\n")
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


    ##IS THIS BEING USED? Seems redundant with node.get_visible_transactions and trade
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


            
            
            
            
            
     
    ##TODO: transfer transactions and blocks within radius
    def transfer_txs_and_blocks(self, agents, time): #radius=distance for tx transfer,
    #def transfer_transactions(self,agents, time): #radius=distance for tx transfer,
        radius=agents[0].radius #save radius in agent variables
        #if agents within radius, transfer txs
        numAgents=len(agents)-1
        neighbors=[]
        #loop through all agents and append txs
        for index, agent in enumerate(agents):

            if (index!=numAgents): #end condition
                neighbors=[agents[index]]
                neighborsCount=0 #number of neighbors, if meets threshold, then create block

                for i in range(index,len(agents)): #check distance between all agents

                    if (i != index):
                        distance=math.hypot(agents[index].coordinates[0] - agents[i].coordinates[0], agents[index].coordinates[1] - agents[i].coordinates[1])

                        if distance<radius:  #neighbors
                            neighborsCount += 1
                            neighbors.append(agents[i])
                        
                            ##trade blocks
                            #agents[index].add_visible_transactions(agents[i].get_visible_blocks())
                            #agents[i].add_visible_transactions(agents[index].get_visible_blocks())

                            indexTxs = agents[index].get_visible_blocks()
                            iTxs = agents[i].get_visible_blocks()
                            
                            agents[index].add_visible_blocks(iTxs, time)
                            agents[i].add_visible_blocks(indexTxs, time)
                            
                            ##trade txs
                            indexVisibleTxs = agents[index].get_visible_transactions()
                            iVisibleTxs = agents[i].get_visible_transactions()
                            
                            agents[index].add_visible_transactions(iVisibleTxs, time)
                            agents[i].add_visible_transactions(indexVisibleTxs, time)
                            

            ##localBlock necessity
            #if neighborsCount > 2: #2
               # self.create_block_nearby(neighbors,  time)


##TODO: transfer transactions and blocks within radius
    def transfer_txs_and_blocks_including_baseStations(self, agents, time): #radius=distance for tx transfer,
    #def transfer_transactions(self,agents, time): #radius=distance for tx transfer,
        radius=agents[0].radius #save radius in agent variables
        #if agents within radius, transfer txs
        numAgents=len(agents)-1
        neighbors=[]
        #loop through all agents and append txs
        for index, agent in enumerate(agents):

            if (index!=numAgents): #end condition
                neighbors=[agents[index]]
                neighborsCount=0 #number of neighbors, if meets threshold, then create block

                for i in range(index,len(agents)): #check distance between all agents

                    if (i != index):
                        distance=math.hypot(agents[index].coordinates[0] - agents[i].coordinates[0], agents[index].coordinates[1] - agents[i].coordinates[1])

                        if distance<radius:  #neighbors
                            neighborsCount += 1
                            neighbors.append(agents[i])
                        
                            ##trade blocks
                            agents[index].add_visible_blocks(agents[i].get_visible_blocks(), time)
                            agents[i].add_visible_blocks(agents[index].get_visible_blocks(), time)
                            
                            ##trade txs

                            #confirmed_txs
                            agent[index].add_confirmed_transactions(agents[i]._confirmed_transactions, time)
                            agent[i].add_confirmed_transactions(agents[index]._confirmed_transactions, time)

                            #visible_txs
                            agents[index].add_visible_transactions(agents[i].get_visible_transactions(), time)
                            agents[i].add_visible_transactions(agents[index].get_visible_transactions(), time)

                            
                            
            ##localBlock necessity
            #if neighborsCount > 2: #2
               # self.create_block_nearby(neighbors,  time)

       #base station transfer transactions
        for index, agent in enumerate(agents):
            for bs in baseStations:
                distance=math.hypot(agent.coordinates[0] - bs.coordinates[0], agent.coordinates[1] - bs.coordinates[1])
                if bs.radius < radius: #neigbors
                    ##Trade Txs
                    #trade txs to baseStation
                    bs.add_confirmed_transactions(agent._confirmed_transactions, time)
                    bs.add_visible_transactions(agent._visible_transactions, time) 
                    #trade txs to agents
                    agent.add_confirmed_transactions(bs._confirmed_transactions, time)
                    agent.add_visible_transactions(bs._visible_transactions, time)

                    ##Trade Blocks
                    #trade blocks to baseStations
                    bs.add_confirmed_blocks(agent._confirmed_blocks, time)
                    bs.add_visible_blocks(agent._visible_blocks, time) 
                    #trade blocks to agents
                    agent.add_confirmed_blocks(bs._confirmed_blocks, time)
                    agent.add_visible_blocks(bs._visible_blocks, time)
      

                

