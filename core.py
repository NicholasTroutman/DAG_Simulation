import timeit, time, os
import numpy as np
import scipy.stats as st
import networkx as nx
import matplotlib.pyplot as plt
import sys, getopt
#from simulation.block import Block
from simulation.helpers import update_progress, csv_export, create_random_graph_distances, volume_export
from simulation.plotting import print_graph, print_tips_over_time, \
print_tips_over_time_multiple_agents, print_tips_over_time_multiple_agents_with_tangle, \
print_attachment_probabilities_alone, print_attachment_probabilities_all_agents
from simulation.simulation import Single_Agent_Simulation
from simulation.simulation_multi_agent import Multi_Agent_Simulation


########
# GET COmmnad Line Operations
######

tsa =  "longest" #"weighted-entry-point" # weighted-entry-point, weighted-genesis, unweighted, random, longest
netsize = 10
lam_m = 1/40 #milestone rate

alpha = 0.01
txs = 400
printing=True
seed=1
DLTMode = "linear" #linear, dag, dht
consensus = "individual" #individual, nearby
inputMap = "Houstonredblue"
blockTime = 40
references = 1
group = 1
volume =0
basestations=0

##Commands --alpha/-a, --txs/-t, --netsize/-n, --lambda/-l
commands=["alpha =", "txs =", "netsize =", "lambda =", "printing =", "seed =", "dltmode =", "consensus =", "map =", "blocktime =", "references =", "group =", "volume =", "rsu ="]
opts, args = getopt.getopt(sys.argv[1:], "atnlpsdcmbrgv:r:", commands)
for opt, arg in opts:
    print(opt," = ",arg)
    if opt in ('-a', '--alpha '):
        alpha= float(arg)
        #print(arg)
    elif opt in ('-t', '--txs '):
        txs=int(arg)
        #print(arg)
    elif opt in ('-n', '--netsize '):
        #print("Netsize FOUND: ",netsize)
        netsize=int(arg)
    elif opt in ('-l', '--lambda '):
        #print("Lambda Found")
        lam_m=1/int(arg)
    elif opt in ('-p', '--printing '):
        #print("Lambda Found")
        p = str(arg).lower()
        print("\t\tPRINTING TEST:\t",arg)
        if arg=="0" or arg=="False" or arg=="FALSE" or arg=="false":
            printing=False
        else:
            printing=True

        #print("PRINTING FOUND AND WILL BE CHANGED!!!", arg," ",bool(arg))
        #printing= bool(arg)
    elif opt in ('-s', '--seed '):
        #print("Seed Found: ", arg)
        #print("Lambda Found")
        seed=int(arg)
    elif opt in ('-d', '--dltmode '):
        DLTMode = str(arg).lower()
        if (DLTMode not in  ["linear", "dag", "dht", "hashgraph"]   ):
            DLTMode = "linear"
        #print("DLTMode FOund: ",DLTMode)
    elif opt in ('-c', '--consensus '):
        consensus = str(arg).lower()
        if (consensus not in  ["individual", "near", "simple"]   ):
            consensus = "individual"
        #print("Consensus Found: ",consensus)
    elif opt in ('-m', '--map '):
        inputMap = str(arg)
        if inputMap not in ["Houstonredblue", "HoustonHwyredblue"]:
            inputMap = "Houstonredblue"
        #print("Map Found: ",inputMap)
    elif opt in ('b', '--blocktime '):
        blockTime = int(arg)
        if blockTime<1:
            sys.exit("INCORRECT BLOCKTIME")
    elif opt in ('r', '--references '):
        references =int(arg)
        if references<1:
            sys.exit("Incorrect references")
    elif opt in ('g', '--group '):
        group =int(arg)
        if group<2:
            sys.exit("Incorrect Group Size")
    elif opt in ('v', '--volume '):
        if arg=="0" or arg=="False" or arg=="FALSE" or arg=="false":
            volume=False
        else:
            volume=True
    elif opt in ('r', '--rsu '):
        if arg=="0" or arg=="False" or arg=="FALSE" or arg=="false":
            basestations=False
        else:
            basestations=True
        #print("??", arg,"   ",bool(arg))
        #print("Volume: ",volume)




#print("\nAlpha: ", alpha)
print("Txs: ", txs)
print("Netize: ", netsize)
#print("Lambda: ", lam_m)
print("Printing: ", printing)
print("Seed: ", seed)
print("DLTMode: ",DLTMode)
print("Consensus: ",consensus)
print("Map: ",inputMap)
print("BlockTime: ", blockTime)
print("References: ",references)
print("Group: ", group)
print("Volume: ",volume)
print("Road Side Unit: ",basestations)
#sys.exit()
#############################################################################
# SIMULATION: SINGLE AGENT
#############################################################################

#Parameters: no_of_transactions, lambda, no_of_agents, alpha, latency (h), tip_selection_algo
#Tip selection algorithms: Choose among "random", "weighted", "unweighted" as input

# simu = Single_Agent_Simulation(100, 50, 1, 0.005, 1, "weighted")
# simu.setup()
# simu.run()
# print_tips_over_time(simu)

#############################################################################
# SIMULATION: MULTI AGENT
#############################################################################

#Parameters: no_of_transactions, lambda, no_of_agents, alpha, distance, tip_selection_algo
# latency (default value 1), agent_choice (default vlaue uniform distribution, printing)
#Tip selection algorithms: Choose among "random", "weighted", "unweighted" as input




start_time = timeit.default_timer()
# my_lambda = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
my_lambda = [1]
# To make sure each running has 20 milestones issued so that enough confirmed txs can be obtained to cal mean()
# total_tx_nums = [x * 1200 for x in my_lambda]
#tsa =  "weighted-genesis" #"weighted-entry-point" # weighted-entry-point, weighted-genesis, unweighted, random
#netsize = 10
#lam_m = 1/40 #milestone rate

#alpha = 0.01
#txs = 800

dir_name = './SimuData/'
suffix = '.csv'


for lam in my_lambda:
    timestr = time.strftime("%Y%m%d-%H%M")
    base_name = '{}_{}_{}_txs_{}_tsa_{}_size_{}_seed_{}_map_{}_blockTime_{}_refs_{}_group_{}' \
                .format(timestr, DLTMode, consensus, txs, tsa, netsize, seed, inputMap, blockTime, references, group)
    simu2 = Multi_Agent_Simulation(_no_of_transactions = txs, _lambda = lam,
                                _no_of_agents = netsize, _alpha = alpha,
                                _distance = 1, _tip_selection_algo = tsa,
                                _latency=1, _agent_choice=None,
                                _printing=printing, _lambda_m=lam_m, _seed=seed,
                                 _DLTMode=DLTMode, _consensus=consensus, _importMap=inputMap, _blockTime=blockTime,
                                  _references=references, _group=group, _volume = volume, _basestations = basestations)
    simu2.setup()
    simu2.run()
    if volume == 0: ##ETC run
        file_name = os.path.join(dir_name, base_name + suffix)
        csv_export(simu2, file_name)
    else: #volume test
        print("\nTODO: Storage\n")
        #simu2.getVolume() ##gets data from everyone
        file_name = os.path.join(dir_name, base_name + "_VOLUME" + suffix)
        volume_export(simu2, file_name)
        #print(simu2.agents[0].storageData)
print("TOTAL simulation time: " + str(np.round(timeit.default_timer() - start_time, 3)) + " seconds\n")

#############################################################################
# PLOTTING
#############################################################################

# print_graph(simu2)
# print_tips_over_time(simu2)
# print_tips_over_time_multiple_agents(simu2, simu2.no_of_transactions)
# print_tips_over_time_multiple_agents_with_tangle(simu2, simu2.no_of_transactions)
# print_attachment_probabilities_all_agents(simu2)
