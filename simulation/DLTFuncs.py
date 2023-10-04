#Nick Troutman
#DLT Functions


##Create block between N agents within close proximity
def create_block_near(self, agents, time): #radius=distance for tx transfer, agents = neighbors
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
    newBlock = Block(txs, agents, time, len(self.blocks), self.no_of_agents, None) #None for no new blockLinks (yet)
    self.blocks.append(newBlock)
    for agent in agents:
        agent.usedTxs=txs
        agent.freeTxs=[]

    self.DG.add_node(newBlock, pos=(newBlock.creation_time, \
         np.random.uniform(0, 1)+newBlock.creators[0].id*2), \
         node_color=self.agent_colors[newBlock.creators[0].id])


    #choose tsa
    self.tip_selection(newBlock)
    for agent in agents:
     agent.add_visible_blocks([newBlock], time)



##Create block with 1 agent
def create_block_individual(self, agent, time): #radius=distance for tx transfer,
    print("\nCREATING BLOCK\n")

    print("Agents: ",agents)
    ##get all txs
    txs=list(set(agent.get_visible_transactions()))

    print("TX Unions: ",txs)
    if txs==[]:
        print("No Txs for block")
        return

    ##get links
    visBlocks= list(set(agent.get_visible_blocks()))


    print("Block #: ", len(self.blocks))
    print("NumBlocks #: ",self.DG.number_of_nodes())
    newBlock = Block(txs, agent, time, len(self.blocks), self.no_of_agents, None) #None for no new blockLinks (yet)
    self.blocks.append(newBlock)

    #clear out usedTxs and freeTxs
    agent.usedTxs=txs
    agent.freeTxs=[]

    self.DG.add_node(newBlock, pos=(newBlock.creation_time, \
    np.random.uniform(0, 1)+newBlock.creators[0].id*2), \
    node_color=self.agent_colors[newBlock.creators[0].id])


    #choose tsa
    self.tip_selection(newBlock)
    for agent in agents:
        agent.add_visible_blocks([newBlock], time)
