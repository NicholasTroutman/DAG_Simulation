#Consensus Algorithms
from simulation.block import Block, BaseStationBlock

##################################
##      Tip Selection		   ###
##################################

##Where is this used?
def all_approvers_not_visible(Multi_Agent_Simulation, transaction):
    return set(list(Multi_Agent_Simulation.DG.predecessors(transaction))).issubset(set(Multi_Agent_Simulation.not_visible_transactions))



#returns valid tips for a given agent
def get_valid_tips_multiple_agents(Multi_Agent_Simulation, agent):

    valid_tips = []
    print("ALL txs: ",agent.get_visible_blocks())
    #print("class: ",agent.get_visible_transactions()[0].__class__)
    if len(agent.get_visible_blocks())>0  :
        for b in agent.get_visible_blocks():

            #NDT DEBUG
            #print("TX CHECKED: ",transaction.id)
            #print("Seen: ", agent.get_visible_transactions())
            #print(list(self.DG.predecessors(transaction)))
            #print("block: ",transaction)
            #Add to valid tips if transaction has no approvers at all
            #print(len(list(self.DG.predecessors(transaction))))
            if(len(list(Multi_Agent_Simulation.DG.predecessors(b))) == 0):
                valid_tips.append(b)

            #Add to valid tips if all approvers not visible yet
            #elif(self.all_approvers_not_visible(transaction)):

             #   valid_tips.append(transaction)

            #add to valid tips if no approvers are visible yet
            elif(len(set(list(Multi_Agent_Simulation.DG.predecessors(b))).intersection(set(agent.get_visible_blocks()))) == 0): #if no predecssors of tx are in visible transaction
                #print("\n\n!!!!!!! HASN'T SEEN NEW LINK YET !!!!\n\n")
                valid_tips.append(b)

    return valid_tips



##Calculate transition probabilities for multiple agents
def calc_transition_probabilities_multiple_agents(Multi_Agent_Simulation, approvers, agent):

    weights = [approver.cum_weight_multiple_agents[agent] for approver in approvers]
    normalized_weights = [weight - max(weights) for weight in weights]

    denominator_transition_probabilities = sum([math.exp(Multi_Agent_Simulation.alpha * weight) \
    for weight in normalized_weights])

    return [math.exp(Multi_Agent_Simulation.alpha * (approver.cum_weight_multiple_agents[agent] \
            - max(weights))) / denominator_transition_probabilities \
            for approver in approvers]



#############################################################################
# TIP-SELECTION: RANDOM
#############################################################################


def random_selection(Multi_Agent_Simulation, block):

    #Needed for plotting number of tips over time for ALL agents
    #for agent in self.agents:
    #    if(agent != transaction.agent):
    #        self.get_visible_transactions(transaction.arrival_time, agent)
    #        valid_tips = self.get_valid_tips_multiple_agents(agent)
    #        agent.record_tips.append(valid_tips)

    #Get visible transactions and valid tips (and record these)
    #self.get_visible_transactions(transaction.arrival_time, transaction.agent)
    valid_tips = Multi_Agent_Simulation.get_valid_tips_multiple_agents(block.creators[0])
    block.creators[0].record_tips.append(valid_tips)
    Multi_Agent_Simulation.record_tips.append(valid_tips)


    ##error check print all transactions
    #print("\nAvailable Txs")
    #for tx in transaction.agent.visible_transactions:
    #    print(tx) #seen node_color
    #    print(list(self.DG.predecessors(tx)))
    #    print(len(list(self.DG.predecessors(tx))))

    #Reference 2-8 random tips
    valid_tips2 = valid_tips.copy() #create copy to edit
    print("Valid Tips: ",valid_tips2)
    for count, tip in enumerate(valid_tips):
        if count>7: #no more than 8 tips
            break
        tempTip = np.random.choice(valid_tips2) #get randomTip
        Multi_Agent_Simulation.DG.add_edge(block, tempTip) #add tip
        valid_tips2.remove(tempTip) #remove tempTip from working valid_tips2
        block.blockLinks.append(tempTip)

#############################################################################
# Find an Entry_Point randomly from 100lambda~200lambda txs ago for TIP-SELECTION walk
#############################################################################

def find_entry_point(Multi_Agent_Simulation, transaction):
    tx_idx = 0
    if transaction.id > 100 * Multi_Agent_Simulation.lam:
        low = max(0, transaction.id- 200*Multi_Agent_Simulation.lam)
        high = transaction.id - 100*Multi_Agent_Simulation.lam
        tx_idx = np.random.randint(low=low, high=high)
    return Multi_Agent_Simulation.transactions[tx_idx]


#############################################################################
# TIP-SELECTION: UNWEIGHTED
#############################################################################

def unweighted_MCMC(Multi_Agent_Simulation, transaction):

    #Needed for plotting number of tips over time for ALL agents
    #for agent in self.agents:
    #    if(agent != transaction.agent):
    #        self.get_visible_transactions(transaction.arrival_time, agent)
    #        valid_tips = self.get_valid_tips_multiple_agents(agent)
    #        agent.record_tips.append(valid_tips)

    valid_tips = Multi_Agent_Simulation.get_valid_tips_multiple_agents(transaction.agent)
    transaction.agent.record_tips.append(valid_tips)
    Multi_Agent_Simulation.record_tips.append(valid_tips)

    #Get visible transactions and valid tips (and record these)
    #self.get_visible_transactions(transaction.arrival_time, transaction.agent)
    #valid_tips = self.get_valid_tips_multiple_agents(transaction.agent)
    transaction.agent.record_tips.append(valid_tips)
    Multi_Agent_Simulation.record_tips.append(valid_tips)


    valid_tips2=valid_tips.copy() #create copy to edit

    for count, tip in enumerate(valid_tips):
        if count>7: #no more than 8 tips
            break
        #tempTip = np.random.choice(valid_tips2) #get randomTip
        tempTip = Multi_Agent_Simulation.unweighted_random_walk(transaction, valid_tips2)
        Multi_Agent_Simulation.DG.add_edge(transaction, tempTip) #add tip
        valid_tips2.remove(tempTip) #remove tempTip from working valid_tips2


    #Walk to two tips
    #tip1 = self.unweighted_random_walk(transaction, valid_tips)
    #tip2 = self.unweighted_random_walk(transaction, valid_tips)

    #Add tips to graph (only once)
    #self.DG.add_edge(transaction,tip1)
    #if(tip1 != tip2):
    #    self.DG.add_edge(transaction,tip2)


def unweighted_random_walk(Multi_Agent_Simulation, transaction, valid_tips):
    print("Start Unweighted Random Walk")
    # Start walk at genesis
    walker_on = Multi_Agent_Simulation.transactions[0]

    #If only genesis a valid tip, approve genesis
    if (valid_tips == [walker_on]):
        return walker_on

    while (walker_on not in valid_tips):

        approvers = list(Multi_Agent_Simulation.DG.predecessors(walker_on))
        print("Approvers: ", list(Multi_Agent_Simulation.DG.predecessors(walker_on)))
        visible_approvers = common_elements(approvers, transaction.agent.get_visible_transactions())
        print("Visible_approvers: ", visible_approvers)
        walker_on = np.random.choice(visible_approvers)

    return walker_on


#############################################################################
# TIP-SELECTION: WEIGHTED
#############################################################################


def weighted_genesis_MCMC(Multi_Agent_Simulation, transaction):

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
    valid_tips = Multi_Agent_Simulation.get_valid_tips_multiple_agents(transaction.agent)
    transaction.agent.record_tips.append(valid_tips)
    Multi_Agent_Simulation.record_tips.append(valid_tips)

    #Walk to two tips
    tip1 = Multi_Agent_Simulation.weighted_random_walk(transaction, valid_tips, Multi_Agent_Simulation.transactions[0])
    tip2 = Multi_Agent_Simulation.weighted_random_walk(transaction, valid_tips, Multi_Agent_Simulation.transactions[0])

    #Add tips to graph (only once)
    Multi_Agent_Simulation.DG.add_edge(transaction, tip1)
    if (tip1 != tip2):
        Multi_Agent_Simulation.DG.add_edge(transaction, tip2)


def weighted_entry_point_MCMC(Multi_Agent_Simulation, transaction):
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
    Multi_Agent_Simulation.record_tips.append(valid_tips)

    # Walk to two tips
    tip1 = Multi_Agent_Simulation.weighted_random_walk(transaction, valid_tips, Multi_Agent_Simulation.find_entry_point(transaction))
    tip2 = Multi_Agent_Simulation.weighted_random_walk(transaction, valid_tips, Multi_Agent_Simulation.find_entry_point(transaction))

    Multi_Agent_Simulation.DG.add_edge(transaction, tip1)
    if (tip1 != tip2):
        Multi_Agent_Simulation.DG.add_edge(transaction, tip2)


def weighted_random_walk(Multi_Agent_Simulation, transaction, valid_tips, initial_walker_on):

    #Start walk at genesis
    walker_on = initial_walker_on

    # #Start walk at genesis
    # walker_on = self.transactions[0]

    #If only genesis a valid tip, approve genesis
    if (valid_tips == [walker_on]):
        return walker_on

    while (walker_on not in valid_tips):

        approvers = list(Multi_Agent_Simulation.DG.predecessors(walker_on))
        visible_approvers = common_elements(approvers, transaction.agent.visible_transactions)
        transition_probabilities = Multi_Agent_Simulation.calc_transition_probabilities_multiple_agents(visible_approvers, transaction.agent)

        #Choose with transition probabilities
        walker_on = np.random.choice(visible_approvers, p=transition_probabilities)

    return walker_on


#############################################################################
# CONFIRMATION CONFIDENCE: MULTI AGENT
#############################################################################


def update_weights_multiple_agents(Multi_Agent_Simulation, incoming_transaction):

    entrypoint = Multi_Agent_Simulation.find_entry_point(incoming_transaction)

    # Update all descendants of incoming_transaction only (cum_weight += 1)
    for transaction in nx.descendants(Multi_Agent_Simulation.DG, incoming_transaction):

        if transaction.arrival_time >= entrypoint.arrival_time:
            # Update for each agent separately
            for agent in Multi_Agent_Simulation.agents:
                if transaction in agent.visible_transactions:
                    transaction.cum_weight_multiple_agents[agent] += 1


def calc_exit_probabilities_multiple_agents(Multi_Agent_Simulation, incoming_transaction):

    for agent in Multi_Agent_Simulation.agents:

        #Reset exit probability of all transactions to 0%, just needed when run multiple times throughout simulation
        for transaction in Multi_Agent_Simulation.DG.nodes:
            transaction.exit_probability_multiple_agents[agent] = 0

        #Set genesis to 100%
        Multi_Agent_Simulation.transactions[0].exit_probability_multiple_agents[agent] = 1

        #Determine visible transaction for t + 1, so that all transactions (h = 1) are included
        #self.get_visible_transactions(incoming_transaction.arrival_time + self.latency, agent)

    #Start at genesis, tips in the end
    sorted = list(reversed(list(nx.topological_sort(Multi_Agent_Simulation.DG))))

    #Calculate exit probabilities
    for transaction in sorted:

        for agent in Multi_Agent_Simulation.agents:

            if (transaction in agent.visible_transactions):

                #Get visible direct approvers and transition probabilities to walk to them
                approvers = list(Multi_Agent_Simulation.DG.predecessors(transaction))
                visible_approvers = common_elements(approvers, agent.visible_transactions)
                transition_probabilities = Multi_Agent_Simulation.calc_transition_probabilities_multiple_agents(visible_approvers, agent)

                #For every visible direct approver update the exit probability by adding the exit probability
                #of the current transaction times the transition probabilitiy of walking to the approver
                for (approver, transition_probability) in zip(visible_approvers, transition_probabilities):
                    approver.exit_probability_multiple_agents[agent] += (
                                transaction.exit_probability_multiple_agents[agent] * transition_probability)


def calc_confirmation_confidence_multiple_agents(Multi_Agent_Simulation, incoming_transaction):

    #Loop over agents and get visible transactions and valid tips
    for agent in Multi_Agent_Simulation.agents:
        #self.get_visible_transactions(incoming_transaction.arrival_time + self.latency, agent)
        agent.tips = Multi_Agent_Simulation.get_valid_tips_multiple_agents(agent)

        #Loop over visible transactions
        for transaction in agent.visible_transactions:
            #Reset confirmation confidence to 0%, just needed when function called multiple times during simulation
            # transaction.confirmation_confidence_multiple_agents[agent] = 0

            #Loop over valid tips
            for tip in agent.tips:

                if(nx.has_path(Multi_Agent_Simulation.DG,tip,transaction) and tip != transaction):

                    transaction.confirmation_confidence_multiple_agents[agent] += tip.exit_probability_multiple_agents[agent]

                #Tips have 0 confirmation confidence by default
                tip.confirmation_confidence_multiple_agents[agent] = 0


#Uses exit probabilities to caluclate attachment probabilities
def calc_attachment_probabilities(Multi_Agent_Simulation, incoming_transaction):

    attachment_probabilities_without_main = []
    attachment_probabilities_all = []

    Multi_Agent_Simulation.calc_exit_probabilities_multiple_agents(incoming_transaction)

    for agent in Multi_Agent_Simulation.agents:
        #self.get_visible_transactions(incoming_transaction.arrival_time + self.latency, agent)
        agent.tips = Multi_Agent_Simulation.get_valid_tips_multiple_agents(agent)

    for agent in Multi_Agent_Simulation.agents:

        sum_ = 0
        sum_ = sum(tip.exit_probability_multiple_agents[agent] for tip in agent.tips if tip.agent == agent)

        for other_agent in Multi_Agent_Simulation.agents:
            if(other_agent != agent):
                sum_ += sum(tip.exit_probability_multiple_agents[other_agent] for tip in other_agent.tips if tip.agent == agent)

        attachment_probabilities_all.append(sum_/Multi_Agent_Simulation.no_of_agents)

        if(agent != Multi_Agent_Simulation.agents[0]):
            attachment_probabilities_without_main.append(sum_/Multi_Agent_Simulation.no_of_agents)

    # print(attachment_probabilities_without_main)
    # print(attachment_probabilities_all)
    return attachment_probabilities_all


##TIP SELECTION Decider
def tip_selection(Multi_Agent_Simulation, transaction):

        if(Multi_Agent_Simulation.tip_selection_algo == "random"):
            random_selection(transaction)
        elif (Multi_Agent_Simulation.tip_selection_algo == "unweighted"):
            unweighted_MCMC(transaction)
        elif (Multi_Agent_Simulation.tip_selection_algo == "weighted-genesis"):
            weighted_genesis_MCMC(transaction)
        elif (Multi_Agent_Simulation.tip_selection_algo == "weighted-entry-point"):
            weighted_entry_point_MCMC(transaction)
        else:
            print("ERROR:  Valid tip selection algorithms are 'random', 'weighted-genesis', 'weighted-entry-point', "
                  "'unweighted'")
            sys.exit()





##################################
##      CONSENSUS ALGS		   ###
##################################


##Create block between N agents within close proximity
def create_block_nearby(Multi_Agent_Simulation, agents, time): #radius=distance for tx transfer,
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

    print("Block #: ", len(Multi_Agent_Simulation.blocks))
    print("NumBlocks #: ",Multi_Agent_Simulation.DG.number_of_nodes())
    newBlock = Block(txs, agents, time, len(Multi_Agent_Simulation.blocks), Multi_Agent_Simulation.no_of_agents)
    Multi_Agent_Simulation.blocks.append(newBlock)
    for agent in agents:
        agent.usedTxs=txs
        agent.freeTxs=[]

    Multi_Agent_Simulation.DG.add_node(newBlock, pos=(newBlock.creation_time, \
            np.random.uniform(0, 1)+newBlock.creators[0].id*2), \
            node_color=Multi_Agent_Simulation.agent_colors[newBlock.creators[0].id])


    #choose tsa
    tip_selection(newBlock)
    for agent in agents:
        agent.add_visible_blocks([newBlock], time)
