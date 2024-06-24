import timeit, time, os
import numpy as np
import scipy.stats as st
import networkx as nx
import matplotlib.pyplot as plt
import sys, getopt
#from simulation.block import Block
from simulation.helpers import update_progress, csv_export, create_random_graph_distances, volume_export, p2p_export, tx_export
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
lam = 1 ##frequency of tx minting
lam_m = 1/40 #milestone rate
prefilterTime = 6000 ##preFilter cutoff time, all sent txs must be newer than this
blockfilterTime = 6000
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
volume =0 #false, don't print storage information for each agent
basestations=0
pruning=0
balance= 0
maxTxs=130
minTxs=0
keep=200
p2p=0 #false, don't print p2p information
falsePositive = 0
ceiling = 0

commands=["alpha =", "txs =", "netsize =", "lambda =", "printing =", "seed =", "dltmode =", "consensus =", "map =", "blocktime =", "references =", "group =", "volume =", "rsu =", "pruning =", "balance =", "maxTxs =", "minTxs =", "keep =", "p2p =", "prefilterTime =", "blockfilterTime =", "falsePositive =", "ceiling ="]
opts, args = getopt.getopt(sys.argv[1:], "atnlpsdcmbrgvrpbmmk:p:", commands)
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
        #lam_m=1/int(arg) ##Not being used
        lam=float(arg)
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
        if DLTMode=="linear" and references>1:
            sys.exit("LINEAR CANNOT HAVE MORE THAN 1 REFERENCE: ",references)
    elif opt in ('g', '--group '):
        group =int(arg)
        if group<1:
            sys.exit("Incorrect Group Size")
    elif opt in ('v', '--volume '):
        if arg=="0" or arg=="False" or arg=="FALSE" or arg=="false":
            volume=False
        else:
            volume=True
    elif opt in ('r', '--rsu '):
        if int(arg)<0:
            sys.exit("BaseStation NEGATIVE, ERROR")
        else:
            basestations=int(arg)
    elif opt in ('p', '--pruning '):
        if  arg=="0" or arg=="False" or arg=="FALSE" or arg=="false":
            pruning=0
        else:
            pruning=1
    elif opt in ('b', '--balance '):
        if  arg=="0" or arg=="False" or arg=="FALSE" or arg=="false":
            balance=0
        else:
            balance=1
    elif opt in ('-m', '--maxTxs '):
        if int(arg)>0:
            maxTxs=int(arg)
        else:
            sys.exit("maxTxs INVALID")
    elif opt in ('-m', '--minTxs '):
        if int(arg)>=0:
            minTxs=int(arg)
        else:
            sys.exit("minTxs INVALID")
    elif opt in ('-k', '--keep '):
        if int(arg)>=0:
            keep=int(arg)
        else:
            sys.exit("Keep INVALID")
    elif opt in ('-p', '--p2p '):
        if int(arg)==1 or arg==True or arg=="True":
            p2p=1
        elif int(arg)==0 or arg==False or arg=="False":
            p2p=0
        else:
            sys.exit("P2P INVALID")
    elif opt in ('-p', '--prefilterTime '):
        if float(arg)>0:
            prefilterTime=int(arg)
            blockfilterTime = prefilterTime*1.25 #hardcode for me after testing this is a solid value
            #print(int(arg)*1.25)
        else:
            sys.exit("prefilterTime INVALID")
    elif opt in ('-b', '--blockfilterTime '):
        print("\nBLOCKFILTER TIME IS BEING USED\n")
        if float(arg)>0:
            blockfilterTime=int(arg)
        else:
            sys.exit("blockfilterTime INVALID")
    elif opt in ('-f', '--falsePositive '):
        if float(arg)>=0 and float(arg)<=1:
            falsePositive=float(arg)
        else:
            sys.exit("falsePositive INVALID")
    elif opt in ('-c', '--ceiling '):
        if float(arg)>=0:
            ceiling = float(arg)
        else:
            sys.exit("ceiling invalid")




#print("\nAlpha: ", alpha)
print("Txs: ", txs)
print("Netize: ", netsize)
print("Lambda: ", lam)
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
print("Pruning: ",pruning)
print("Balance: ",balance)
print("MaxTxs: ",maxTxs)
print("MinTxs: ",minTxs)
print("KeepTime: ",keep)
print("P2P: ",p2p)
print("prefilterTime: ",prefilterTime)
print("blockfilterTime: ",blockfilterTime)
print("falsePositive: ",falsePositive)
print("ceiling: ",ceiling)

##check for improper Group with near
if consensus=="near" and group<2:
    sys.exit("Consensus = NEAR, and Group < 2, ERROR")
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
#my_lambda = [1]
# To make sure each running has 20 milestones issued so that enough confirmed txs can be obtained to cal mean()
# total_tx_nums = [x * 1200 for x in my_lambda]
#tsa =  "weighted-genesis" #"weighted-entry-point" # weighted-entry-point, weighted-genesis, unweighted, random
#netsize = 10
#lam_m = 1/40 #milestone rate

#alpha = 0.01
#txs = 800

dir_name = './SimuData/'
suffix = '.csv'


#for lam in my_lambda:
timestr = time.strftime("%Y%m%d-%H%M")
#base_name = '{}_{}_{}_txs_{}_tsa_{}_size_{}_seed_{}_map_{}_blockTime_{}_refs_{}_group_{}_rsu_{}_pruning_{}_balance_{}_maxTxs_{}_minTxs_{}_keep_{}_p2p_{}_lambda_{}_tCutoff_{}_bCutoff_{}_fp_{}' \
#            .format(timestr, DLTMode, consensus, txs, tsa, netsize, seed, inputMap, blockTime, references, group, basestations, pruning, balance, maxTxs, minTxs, keep, p2p, lam, prefilterTime, blockfilterTime, falsePositive)
base_name = '{}_{}_{}_txs_{}_size_{}_map_{}_blockTime_{}_refs_{}_group_{}_rsu_{}_pruning_{}_balance_{}_minTxs_{}_keep_{}_p2p_{}_lambda_{}_tCutoff_{}_bCutoff_{}_fp_{}_ceil_{}' \
            .format(timestr, DLTMode, consensus, txs, netsize,  inputMap, blockTime, references, group, basestations, pruning, balance, minTxs, keep, p2p, lam, prefilterTime, blockfilterTime, falsePositive, ceiling)

simu2 = Multi_Agent_Simulation(_no_of_transactions = txs, _lambda = lam,
                            _no_of_agents = netsize, _alpha = alpha,
                            _distance = 1, _tip_selection_algo = tsa,
                            _latency=1, _agent_choice=None,
                            _printing=printing, _lambda_m=lam_m, _seed=seed,
                             _DLTMode=DLTMode, _consensus=consensus, _importMap=inputMap, _blockTime=blockTime,
                              _references=references, _group=group, _volume = volume, _basestations = basestations,
                               _pruning = pruning, _balance=balance, _maxTxs=maxTxs, _minTxs=minTxs,_keep=keep, _p2p=p2p,
                                _prefilterTime=prefilterTime, _blockfilterTime=blockfilterTime, _falsePositive= falsePositive,
                                _ceiling = ceiling)
simu2.setup()
simu2.run()

##csv export block data
file_name = os.path.join(dir_name, base_name + suffix)
csv_export(simu2, file_name)
if volume==1 or volume==True: #volume
    file_name = os.path.join(dir_name, base_name + "_VOLUME" + suffix)
    volume_export(simu2, file_name)
    #print(simu2.agents[0].storageData)
if p2p==1 or p2p==True:
    file_name1 = os.path.join(dir_name, base_name + "_P2P" + suffix)
    file_name2 = os.path.join(dir_name, base_name + "_INTERACTION" + suffix)
    file_name3 = os.path.join(dir_name, base_name + "_TXFILTER" + suffix)
    p2p_export(simu2, file_name1, file_name2) ##Don't need right now, enable later
    tx_export(simu2, file_name3) ##Don't need right now, enable later
print("TOTAL simulation time: " + str(np.round(timeit.default_timer() - start_time, 3)) + " seconds\n")

#############################################################################
# PLOTTING
#############################################################################

# print_graph(simu2)
# print_tips_over_time(simu2)
# print_tips_over_time_multiple_agents(simu2, simu2.no_of_transactions)
# print_tips_over_time_multiple_agents_with_tangle(simu2, simu2.no_of_transactions)
# print_attachment_probabilities_all_agents(simu2)
