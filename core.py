import timeit, time, os
import numpy as np
import scipy.stats as st
import networkx as nx
import matplotlib.pyplot as plt
import sys, getopt
#from simulation.block import Block
from simulation.helpers import update_progress, csv_export, create_random_graph_distances
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
seed=10
DLTMode = "linear" #linear, dag, dht
consensus = "individual" #individual, nearby


##Commands --alpha/-a, --txs/-t, --netsize/-n, --lambda/-l
commands=["alpha =", "txs =", "netsize =", "lambda =", "printing =", "seed =", "dltmode =", "consensus ="]
opts, args = getopt.getopt(sys.argv[1:], "atnlpsd:c:", commands)
for opt, arg in opts:
    if opt in ('-a', '--alpha '):
        alpha= float(arg)
        #print(arg)
    elif opt in ('-t', '--txs '):
        txs=int(arg)
        #print(arg)
    elif opt in ('-n', '--netsize '):
        #print("Netsize FOUND")
        netsize=int(arg)
    elif opt in ('-l', '--lambda '):
        #print("Lambda Found")
        lam_m=1/int(arg)
    elif opt in ('-p', '--printing '):
        #print("PRINTING FOUND AND WILL BE CHANGED!!!", arg," ",bool(arg))
        #print("Lambda Found")
        p = str(arg).lower()
        if arg=="0" or arg=="False" or arg=="FALSE" or arg=="false":
            printing=False
        else:
            printing=True
        #printing= bool(arg)
    elif opt in ('-s', '--seed '):
        #print("PRINTING FOUND AND WILL BE CHANGED!!!", arg," ",bool(arg))
        #print("Lambda Found")
        seed=int(arg)

    elif opt in ('-d', '--dltmode '):
        DLTMode = str(arg).lower()
        if (DLTMode not in  ["linear", "dag", "dht"]   ):
            DLTMode = "linear"
    elif opt in ('-c', '--consensus '):
        consensus = str(arg).lower()
        if (consensus not in  ["individual", "near"]   ):
            consensus = "individual"

print("Alpha: ", alpha)
print("Txs: ", txs)
print("Netize: ", netsize)
print("Lambda: ", lam_m)
print("Printing: ", printing)
print("Seed: ", seed)
print("DLTMode: ",DLTMode)
print("Consensus: ",consensus)
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
    base_name = '{}alpha_{}lam_{}_txs_{}_tsa_{}_size_{}' \
                .format(timestr, alpha, lam, txs, tsa, netsize)
    simu2 = Multi_Agent_Simulation(_no_of_transactions = txs, _lambda = lam,
                                _no_of_agents = netsize, _alpha = alpha,
                                _distance = 1, _tip_selection_algo = tsa,
                                _latency=1, _agent_choice=None,
                                _printing=printing, _lambda_m=lam_m, _seed=seed, _DLTMode=DLTMode, _consensus=consensus)
    simu2.setup()
    simu2.run()
    file_name = os.path.join(dir_name, base_name + suffix)
    csv_export(simu2, file_name)

print("TOTAL simulation time: " + str(np.round(timeit.default_timer() - start_time, 3)) + " seconds\n")

#############################################################################
# PLOTTING
#############################################################################

# print_graph(simu2)
# print_tips_over_time(simu2)
# print_tips_over_time_multiple_agents(simu2, simu2.no_of_transactions)
# print_tips_over_time_multiple_agents_with_tangle(simu2, simu2.no_of_transactions)
# print_attachment_probabilities_all_agents(simu2)
