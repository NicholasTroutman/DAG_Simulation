import pickle
import numpy as np
import networkx as nx #dag graphs
import matplotlib.pyplot as plt
import imageio #gif making
import matplotlib.cm as cm #coloring
import sys
import math
from simulation.block import DAGBlock, LinearBlock, HashGraphBlock

#############################################################################
# PRINTING AND PLOTTING
#############################################################################

def print_info(self):
    text = "\nParameters:  Transactions = " + str(self.no_of_transactions) + \
            ",  Tip-Selection = " + str(self.tip_selection_algo).upper() + \
            ",  Lambda = " + str(self.lam)
    if(self.tip_selection_algo == "weighted-genesis" or "weighted-entry-point"):
        text += ",  Alpha = " + str(self.alpha)
    #if(self.no_of_agents != 1):
    #    text += ",  Distances = " + str(self.distances)
    text += " | Simulation started...\n"
    print(text)

def print_graph_temp(self):

    #Positioning and text of labels
    pos = nx.get_node_attributes(self.DG, 'pos')
    lower_pos = {key: (x, y - 0.07) for key, (x, y) in pos.items()} #For label offset (0.1)

    #Create labels with the confirmation confidence of every transaction (of the issueing agent)
    labels = {
        # transaction: str(str(np.round(transaction.exit_probability_multiple_agents[transaction.agent], 2)) + "  " +
        #                  str(np.round(transaction.confirmation_confidence_multiple_agents[transaction.agent], 2)))
        transaction : str(np.round(transaction.exit_probability_multiple_agents[transaction.agent],2))
        for transaction in self.DG.nodes if transaction.agent != None
    }
    #For genesis take agent 0 as default (always same value)
    labels[self.transactions[0]] = str(np.round(self.transactions[0].exit_probability_multiple_agents[self.agents[0]],2))

    #col = [['r','b'][int(np.round(transaction.confirmation_confidence,1))] for transaction in self.DG.nodes()] #Color change for 100% confidence

    #Coloring of nodes
    tips = self.get_tips()
    for tip in tips:
        # self.DG.node[tip]["node_color"] = '#ffdbb8'
        self.DG.nodes[tip]["node_color"] = self.agent_tip_colors[int(str(tip.agent))]

    # col = list(nx.get_node_attributes(self.DG, 'node_color').values()) #Didn't work on Linux
    col = []
    for transaction in self.DG:
        if transaction.arrival_time > 0 and transaction.arrival_time % (1/self.lam_m) == 0:

            linked=list(nx.descendants(self.DG, transaction))
            print('tx_ID: ', transaction.id, 'tx_arr_time: ', transaction.arrival_time, 'Descendants: ', len(linked), ' Orphan Rate: ', 1-len(linked)/transaction.id  )
            col.append('maroon')
        else:
            col.append(self.DG.nodes[transaction]["node_color"])


    #Creating figure
    plt.figure(1,figsize=(14, 7))
    nx.draw_networkx(self.DG, pos, with_labels=True, node_size = 100, font_size=5.5, node_color = col)
    # nx.draw_networkx_labels(self.DG, lower_pos, labels=labels, font_size=6)

    #Print title
    title = "Transactions = " + str(self.no_of_transactions) +\
            ",  " + r'$\lambda$' + " = " + str(self.lam) +\
            ",  " + r'$d$' + " = " + str(self.distances[1][0])
    if(self.tip_selection_algo == "weighted-genesis" or "weighted-entry-point"):
        title += ",  " + r'$\alpha$' + " = " + str(self.alpha)
    plt.xlabel("Time (s)")
    plt.yticks([])
    plt.title(title)
    #Save the graph
    plt.savefig('graphTemp.png')
    #plt.show() ##Hangs here??


def print_graph(self):

    #Positioning and text of labels
    pos = nx.get_node_attributes(self.DG, 'pos')
    lower_pos = {key: (x, y - 0.07) for key, (x, y) in pos.items()} #For label offset (0.1)

    #Create labels with the confirmation confidence of every transaction (of the issueing agent)
    #labels = {
        # transaction: str(str(np.round(transaction.exit_probability_multiple_agents[transaction.agent], 2)) + "  " +
        #                  str(np.round(transaction.confirmation_confidence_multiple_agents[transaction.agent], 2)))
    #    transaction : str(np.round(transaction.exit_probability_multiple_agents[transaction.agent],2))
    #    for transaction in self.DG.nodes if transaction.creators != None ##for transaction in self.DG.nodes if transaction.agent != None
    #}
    #For genesis take agent 0 as default (always same value)
    #labels[self.transactions[0]] = str(np.round(self.transactions[0].exit_probability_multiple_agents[self.agents[0]],2))

    #col = [['r','b'][int(np.round(transaction.confirmation_confidence,1))] for transaction in self.DG.nodes()] #Color change for 100% confidence

    #Coloring of nodes
    tips = self.get_tips()
    for tip in tips:
        # self.DG.node[tip]["node_color"] = '#ffdbb8'
        self.DG.nodes[tip]["node_color"] = self.agent_tip_colors[int(str(tip.creators[0]))]



    # col = list(nx.get_node_attributes(self.DG, 'node_color').values()) #Didn't work on Linux
    col = []
    for transaction in self.DG:
        #for hashgraph
        if isinstance(transaction, (HashGraphBlock)):
            if transaction.witness==True:
                if transaction.chainNum%3==0:
                    col.append('red')
                elif transaction.chainNum%3==1:
                    col.append('blue')
                else:
                    col.append('green')

            elif transaction.creation_time > 0 and transaction.creation_time % (1/self.lam_m) == 0:
                linked=list(nx.descendants(self.DG, transaction))

                #print('tx_ID: ', transaction.id, 'tx_arr_time: ', transaction.creation_time, 'Descendants: ', len(linked), ' Orphan Rate: ', 1-len(linked)/transaction.id  )
                col.append('maroon')
            else:
                col.append(self.DG.nodes[transaction]["node_color"])
        else:
            if transaction.creation_time > 0 and transaction.creation_time % (1/self.lam_m) == 0:
                linked=list(nx.descendants(self.DG, transaction))

                #print('tx_ID: ', transaction.id, 'tx_arr_time: ', transaction.creation_time, 'Descendants: ', len(linked), ' Orphan Rate: ', 1-len(linked)/transaction.id  )
                col.append('maroon')
            else:
                col.append(self.DG.nodes[transaction]["node_color"])



    hashgraph=False
    witnessSize=[]
    witnessShape=[]
    for block in self.DG:
        if isinstance(block, (DAGBlock, LinearBlock)):
            #print("linear/Dag")
            pass
        else:
            hashgraph = True
            #print("hashgraph")
            if block.witness==True:
                if block.famous==True:
                    witnessSize.append(700)
                else:
                    witnessSize.append(250)
                witnessShape.append('o')
            else:
                witnessSize.append(50)
                witnessShape.append('o')


    #Creating figure
    plt.figure(1,figsize=(14, 7))
    if hashgraph:
        nx.draw_networkx(self.DG, pos, with_labels=True,  font_size=5.5, node_color = col, node_size = witnessSize)
    else: #not hashgraph
        nx.draw_networkx(self.DG, pos, with_labels=True, node_size = 100, font_size=5.5, node_color = col)
    # nx.draw_networkx_labels(self.DG, lower_pos, labels=labels, font_size=6)

    #Print title
    title = "Transactions = " + str(self.no_of_transactions) +\
            ",  " + r'$\lambda$' + " = " + str(self.lam) +\
            ",  " + r'$d$' + " = " + str(self.distances[1][0])
    if(self.tip_selection_algo == "weighted-genesis" or "weighted-entry-point"):
        title += ",  " + r'$\alpha$' + " = " + str(self.alpha)
    plt.xlabel("Time (s)")
    plt.yticks([])
    plt.title(title)
    #Save the graph
    plt.savefig(f'./img/graph.png')
    plt.show()


##NT: print_position of agents
def print_coordinates(self, agents, time):

    colors = cm.rainbow(np.linspace(0, 1, len(agents)))

    ##NT: get agents coordinates into posx and posy
    posx=[]
    posy=[]

    #Creating figure
    plt.figure(2,figsize=(14, 7))
    plt.cla()
    ax = plt.gca()

    for index, agent in enumerate(agents):
        linex=[]
        liney=[]

        posx.append(agent.coordinates[0])
        posy.append(agent.coordinates[1])


        #print(index,": curr Pos: ",str(agent.coordinates))
        #get linegraph
        for coordinates in agent.past_coordinates:
            linex.append(coordinates[0])
            liney.append(coordinates[1])

            #print(coordinates)

        plt.plot(linex,liney,color=colors[index]) #past positions

        #plot radius
        circle=plt.Circle((agent.coordinates[0],agent.coordinates[1]),10, color='black', fill=False)
        ax.add_artist(circle)

        if time>0:
            curX = [linex[-1],agent.coordinates[0]]
            curY = [liney[-1],agent.coordinates[1]]
            plt.plot(curX, curY, color=colors[index]) #current pos


    ##Get agent labels:
    labels=[]
    for i in range(0,len(agents)):
        labels.append("Agent "+str(i+1))

    #create scatterplot
    plt.scatter(posx,posy,color=colors) #original
    ##reverse yx for image matching
    #plt.scatter(posy,posx,color=colors)
    #Print title
    title = "Transactions = " + str(self.no_of_transactions) + ", Time: " + str(time)
    plt.xlabel("X axis Coordinates")
    plt.ylabel("Y axis Coordinates")
    plt.title(title)

    for i, label in enumerate(labels):
        plt.annotate(label,[posx[i],posy[i]])
    plt.xlim(0,100)
    plt.ylim(0,100)
    #Save the graph
    plt.savefig(f'./img/CG_{time}.png')
    #plt.close()
    #plt.show()



##NT: print_position of agents overtop backgroundimg

##NT: print_position of agents
def print_coordinates_img(self, agents, time, backgroundImg):

    colors = cm.rainbow(np.linspace(0, 1, len(agents)))

    ##NT: get agents coordinates into posx and posy
    posx=[]
    posy=[]

    #Creating figure
    plt.figure(2,figsize=(14, 7))
    plt.cla()
    ax = plt.gca()
    plt.imshow(backgroundImg)

    for index, agent in enumerate(agents):
        linex=[]
        liney=[]

        posx.append(agent.coordinates[0])
        posy.append(agent.coordinates[1])

        #print(index,": curr Pos: ",str(agent.coordinates))
        #get linegraph
        for coordinates in agent.past_coordinates:
            linex.append(coordinates[0])
            liney.append(coordinates[1])
            #print(coordinates)

        plt.plot(linex,liney,color=colors[index]) #past positions

        #plot radius
        circle=plt.Circle((agent.coordinates[0],agent.coordinates[1]),agent.radius, color='black', fill=False)
        ax.add_artist(circle)

        if time>0:
            curX = [linex[-1],agent.coordinates[0]]
            curY = [liney[-1],agent.coordinates[1]]
            plt.plot(curX, curY, color=colors[index]) #current pos


    ##Get agent labels:
    labels=[]
    for i in range(0,len(agents)):
        labels.append("Agent "+str(i+1))


    #create scatterplot
    #for i in range(0,len(agents)): #check
    #    print("Agent:  ",i," ",agents[i].coordinates[0],", ",agents[i].coordinates[1])
    #    print(posx[i],", ",posy[i])


    plt.scatter(posx,posy,color=colors, s=250)
    #Print title
    title = "Transactions = " + str(self.no_of_transactions) + ", Time: " + str(time)
    plt.xlabel("X axis Coordinates")
    plt.ylabel("Y axis Coordinates")
    plt.title(title)

    for i, label in enumerate(labels):
        plt.annotate(label,[posx[i],posy[i]])
    #plt.xlim(0,backgroundImg.shape[1])
    #plt.ylim(0,backgroundImg.shape[0])
    #Save the graph
    plt.savefig(f'./img/CG_{time}.png')
    #plt.close()
    #plt.show()

    ##end program
    #sys.exit()


def print_gif(self, transactions):

    time=[]
    frames=[]
   # #get all tx.arrival_times
    #for tx in transactions:
     #   time.append(tx.arrival_time)
      ##Load Times
    endTime = math.ceil(self.transactions[-1].arrival_time)
    for i in range(1,endTime):
        time.append(i)
    #load all frames
    image=[]

    for t in time:
        image = imageio.imread(f'./img/CG_{t}.png')
        frames.append(image)

    #for i in range(0,5):
    #    frames.append(image)

    #create gif
    #imageio.mimsave('./CG_gif.gif',  frames,  fps = 5)  #fps is no longer supported, use duration
    imageio.mimsave('./CG_gif.gif',  frames, duration=2)



def print_tips_over_time(self):

    plt.figure(figsize=(14, 7))

    #Get no of tips per time
    no_tips = []
    for i in self.record_tips:
        no_tips.append(len(i))

    plt.plot(self.arrival_times, no_tips, label="Tips")

    #Cut off first 250 transactions for mean and best fit
    if(self.no_of_transactions >= 250):
        cut_off = 250
    else:
        cut_off = 0

    #Plot mean
    x_mean = [self.arrival_times[cut_off], self.arrival_times[-1]]
    y_mean = [np.mean(no_tips[cut_off:]), np.mean(no_tips[cut_off:])]
    plt.plot(x_mean, y_mean, label="Average Tips", linestyle='--')

    #Plot best fitted line
    plt.plot(np.unique(self.arrival_times[cut_off:]), \
    np.poly1d(np.polyfit(self.arrival_times[cut_off:], no_tips[cut_off:], 1))\
    (np.unique(self.arrival_times[cut_off:])), label="Best Fit Line", linestyle='--')

    #Print title
    title = "Transactions = " + str(self.no_of_transactions) + \
            ",  " + r'$\lambda$' + " = " + str(self.lam)
    if (self.tip_selection_algo == "weighted"):
        title += ",  " + r'$\alpha$' + " = " + str(self.alpha)
    plt.xlabel("Time (s)")
    plt.ylabel("Number of tips")
    plt.legend(loc='upper left')
    plt.title(title)
    plt.show()

def print_tips_over_time_multiple_agents_with_tangle(self, no_current_transactions):

    plt.figure(figsize=(14, 7))
    plt.subplot(2, 1, 1)

    #Get no of tips per time
    for agent in self.agents:
        no_tips = [0]
        for i in agent.record_tips:
            no_tips.append(len(i))
        label = "Tips Agent " + str(agent)
        #plt.subplot(2, 1, int(str(agent))+1)
        plt.plot(self.arrival_times[:no_current_transactions], no_tips[:no_current_transactions], label=label, color=self.agent_colors[int(str(agent))])

        #Cut off first 60% of transactions
        if(no_current_transactions >= 500):
            cut_off = int(no_current_transactions * 0.2)
        else:
            cut_off = 0

        #Plot mean
        # label = "Average Tips Agent " + str(agent)
        # x_mean = [self.arrival_times[cut_off], self.arrival_times[no_current_transactions-1]]
        # y_mean = [np.mean(no_tips[cut_off:no_current_transactions-1]), np.mean(no_tips[cut_off:no_current_transactions-1])]
        # plt.plot(x_mean, y_mean, label=label, linestyle='--')

        #Plot best fitted line
        # plt.plot(np.unique(self.arrival_times[cut_off:no_current_transactions-1]), \
        # np.poly1d(np.polyfit(self.arrival_times[cut_off:no_current_transactions-1], no_tips[cut_off:no_current_transactions-1], 1))\
        # (np.unique(self.arrival_times[cut_off:no_current_transactions-1])), label="Best Fit Line", linestyle='--')

    #Print title
    title = "Transactions = " + str(self.no_of_transactions) + \
            ",  " + r'$\lambda$' + " = " + str(self.lam) + \
            ",  " + r'$d$' + " = " + str(self.distances[1][0])
    if (self.tip_selection_algo == "weighted"):
        title += ",  " + r'$\alpha$' + " = " + str(self.alpha)
    plt.xlabel("Time (s)")
    plt.ylabel("Number of tips")
    plt.legend(loc='upper left')
    plt.title(title)

    plt.subplot(2, 1, 2)

    #Positioning and text of labels
    pos = nx.get_node_attributes(self.DG, 'pos')
    lower_pos = {key: (x, y - 0.1) for key, (x, y) in pos.items()} #For label offset (0.1)

    #Create labels with the confirmation confidence of every transaction (of the issueing agent)
    labels = {
        transaction: str(str(np.round(transaction.exit_probability_multiple_agents[transaction.agent], 2)) + "  " +
                         str(np.round(transaction.confirmation_confidence_multiple_agents[transaction.agent], 2)))
        for transaction in self.DG.nodes if transaction.agent != None
    }
    #For genesis take agent 0 as default (always same value)
    labels[self.transactions[0]] = str(np.round(self.transactions[0].exit_probability_multiple_agents[self.agents[0]],2))

    #col = [['r','b'][int(np.round(transaction.confirmation_confidence,1))] for transaction in self.DG.nodes()] #Color change for 100% confidence

    #Coloring of tips
    tips = self.get_tips()
    for tip in tips:
        # self.DG.node[tip]["node_color"] = '#ffdbb8'
        self.DG.nodes[tip]["node_color"] = self.agent_tip_colors[int(str(tip.agent))]

    #Didn't work on Linux
    # col = list(nx.get_node_attributes(self.DG, 'node_color').values())
    col = []
    for transaction in self.DG:
        col.append(self.DG.nodes[transaction]["node_color"])

    #Creating figure
    #plt.figure(figsize=(12, 6))
    nx.draw_networkx(self.DG, pos, with_labels=True, node_size = 100, font_size=5.5, node_color = col)
    #nx.draw_networkx_labels(self.DG, lower_pos, labels=labels, font_size=6)

    plt.xlabel("Time (s)")
    plt.yticks([])
    plt.show()


def print_tips_over_time_multiple_agents(self, no_current_transactions):

    plt.figure(figsize=(14, 7))

    #Get no of tips per time
    for agent in self.agents:
        no_tips = [0]
        for i in agent.record_tips:
            no_tips.append(len(i))
        label = "Tips Agent " + str(agent)
        #plt.subplot(2, 1, int(str(agent))+1)
        plt.plot(self.arrival_times[:no_current_transactions], no_tips[:no_current_transactions], label=label)#, color=self.agent_colors[int(str(agent))])

        #Cut off first 60% of transactions
        if(no_current_transactions >= 500):
            cut_off = int(no_current_transactions * 0.2)
        else:
            cut_off = 0

        #Plot mean
        label = "Average Tips Agent " + str(agent)
        x_mean = [self.arrival_times[cut_off], self.arrival_times[no_current_transactions-1]]
        y_mean = [np.mean(no_tips[cut_off:no_current_transactions-1]), np.mean(no_tips[cut_off:no_current_transactions-1])]
        plt.plot(x_mean, y_mean, label=label, linestyle='--')
        print(np.mean(no_tips))

        #Plot best fitted line
        # plt.plot(np.unique(self.arrival_times[cut_off:no_current_transactions-1]), \
        # np.poly1d(np.polyfit(self.arrival_times[cut_off:no_current_transactions-1], no_tips[cut_off:no_current_transactions-1], 1))\
        # (np.unique(self.arrival_times[cut_off:no_current_transactions-1])), label="Best Fit Line", linestyle='--')

    #Print title
    title = "Transactions = " + str(self.no_of_transactions) + \
            ",  " + r'$\lambda$' + " = " + str(self.lam) + \
            ",  " + r'$d$' + " = " + str(self.distances[1][0])
    if (self.tip_selection_algo == "weighted"):
        title += ",  " + r'$\alpha$' + " = " + str(self.alpha)
    plt.xlabel("Time (s)")
    plt.ylabel("Number of tips")
    plt.legend(loc='upper left')
    plt.title(title)
    plt.show()


def print_attachment_probabilities_alone(self):

    title = "Transactions = " + str(self.no_of_transactions) + \
            ",  " + r'$\lambda$' + " = " + str(self.lam) + \
            ",  " + r'$d$' + " = " + str(self.distances[1][0])
    if (self.tip_selection_algo == "weighted"):
        title += ",  " + r'$\alpha$' + " = " + str(self.alpha)

    # with open('graph' +  str(title) + '_3' + '.pkl', 'wb') as handle:
    with open('subtangle_attach_prob.pkl', 'wb') as handle:
        pickle.dump(self.record_attachment_probabilities, handle, protocol=pickle.HIGHEST_PROTOCOL)

    plt.figure(figsize=(14, 7))

    x = np.squeeze([i[0] for i in self.record_attachment_probabilities])
    y = np.squeeze([i[1] for i in self.record_attachment_probabilities])

    plt.plot(x,y, label="Attachment probability sub-Tangle branch")
    plt.ylim(0, 0.7)

    plt.plot(np.unique(x), np.poly1d(np.polyfit(x, y, 1))(np.unique(x)),\
    label="Best Fit", linestyle='--')

    x_mean = [i for i in x]
    y_mean = [np.mean(y) for i in y]
    print(np.mean(y))
    print(np.std(y))
    plt.plot(x_mean, y_mean,\
    label="Average", linestyle='-')

    plt.xlabel("Transactions")
    plt.ylabel("Probability to attach to sub-Tangle branch")
    plt.legend(loc='upper left')
    plt.title(title)
    plt.tight_layout()
    # plt.show()
    # plt.savefig('graph' +  str(title) + '_3' + '.png')


def print_attachment_probabilities_all_agents(self):

    title = "Transactions = " + str(self.no_of_transactions) + \
            ",  " + r'$\lambda$' + " = " + str(self.lam) + \
            ",  " + r'$d$' + " = " + str(self.distances[1][0])
    if (self.tip_selection_algo == "weighted"):
        title += ",  " + r'$\alpha$' + " = " + str(self.alpha)

    plt.figure(figsize=(20, 10))

    #Attachment probabilities
    plt.subplot(1, 2, 1)

    x = np.squeeze([i[0] for i in self.record_attachment_probabilities])
    y = np.squeeze([i[1] for i in self.record_attachment_probabilities])

    labels = ["Agent " + str(i) for i in range(len(y))]

    #For more than 10 agents
    # ax = plt.axes()
    # ax.set_color_cycle([plt.cm.tab20c(i) for i in np.linspace(0, 1, len(y))])
    plt.plot(x, y)
    plt.xlabel("Transactions")
    plt.ylabel("Probability to attach to sub-Tangle branch")
    plt.legend(labels, loc="upper right", ncol=2)

    #Boxplot
    plt.subplot(1, 2, 2)

    data = []

    for agent in range(self.no_of_agents):
        agent_data = [i[1][agent] for i in self.record_attachment_probabilities]
        data.append(agent_data)

    plt.boxplot(data, 0, '+')
    plt.xlabel("Agents")
    plt.xticks(np.arange(1, self.no_of_agents+1), np.arange(0, self.no_of_agents))
    plt.suptitle(title)
    plt.tight_layout()
    plt.subplots_adjust(top=0.94)
    plt.show()
    # plt.savefig(str(no) + '.png')
