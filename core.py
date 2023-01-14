import timeit, time, os
import numpy as np
import scipy.stats as st
import networkx as nx
import matplotlib.pyplot as plt

from simulation.helpers import update_progress, csv_export, create_random_graph_distances
from simulation.plotting import print_graph, print_tips_over_time, \
print_tips_over_time_multiple_agents, print_tips_over_time_multiple_agents_with_tangle, \
print_attachment_probabilities_alone, print_attachment_probabilities_all_agents
from simulation.simulation import Single_Agent_Simulation
from simulation.simulation_multi_agent import Multi_Agent_Simulation

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
tsa = "weighted-entry-point" # weighted-entry-point, weighted-genesis, unweighted, random
netsize = 50
lam_m = 0
alpha = 0.001
txs = 100

dir_name = './SimuData/'
suffix = '.csv'
for lam in my_lambda:
    timestr = time.strftime("%Y%m%d-%H%M")
    base_name = '{}lam_{}_txs_{}_tsa_{}_size_{}' \
                .format(timestr, lam, txs, tsa, netsize)
    simu2 = Multi_Agent_Simulation(_no_of_transactions = txs, _lambda = lam,
                                _no_of_agents = netsize,_alpha = alpha,
                                _distance = 1, _tip_selection_algo = tsa,
                                _latency=1, _agent_choice=None, 
                                _printing=True, _lambda_m=lam_m)
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
