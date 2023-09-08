import sys
import csv
import configparser
import ast
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt


#create agent 

#NT: Create random coordinates for each agent
def create_coordinates(agents, maxDistance,):
    for agent in agents:
        agent.coordinates=np.random.uniform(0,maxDistance,2).tolist()

#create random coordinates in [x,y] shape, of trafficImage
def create_coordinates_nodes(agents, graph, tsp):
    for agent in agents:
        begin, dest=np.random.choice(graph.nodes, size=2, replace=False)
        #print(agent,": ",origin)
        agent.coordinates = graph.nodes[begin]['pos']
        agent.prev_dest = begin
        agent.destination = tsp(graph, nodes=[dest,begin], cycle=False)[1:] #set 
        
        streetSlope=[ graph.nodes[agent.destination[0]]['pos'][0]- graph.nodes[begin]['pos'][0],  graph.nodes[agent.destination[0]]['pos'][1] - graph.nodes[begin]['pos'][1]  ]   
        
        agent.vector=streetSlope/np.linalg.norm(streetSlope)
        #print(agent.vector)


def update_progress(progress, time):

    bar_length = 50
    status = ""
    if isinstance(progress, int):
        progress = float(progress)
    if not isinstance(progress, float):
        progress = 0
    if progress < 0:
        progress = 0
    if progress >= 1:
        progress = 1
        status = "| Simulation completed...\r\n"
    block = int(round(bar_length*progress))
    text = "\rPercent:  [{0}] {1}% | Number:  {2} {3}".\
        format( "#"*block + "-"*(bar_length-block), np.round((progress*100),1), time, status)
    sys.stdout.write(text)
    sys.stdout.flush()


def create_distance_matrix(no_of_agents, distance):

    m = [[distance] * no_of_agents for i in range(no_of_agents)]
    for i in range(no_of_agents):
        m[i][i] = 0
    return m


def create_random_graph_distances(no_of_agents):

    n = no_of_agents  #number nodes
    m = n  #number edges

    G = nx.gnm_random_graph(n, m)

    while not nx.is_connected(G):
        G = nx.gnm_random_graph(n, m)

    distances = (nx.floyd_warshall_numpy(G)).tolist()

    print("Closeness centrality per agent:  " + str(nx.closeness_centrality(G)))

    # print the random graph
    nx.draw(G, with_labels=True)
    # plt.savefig('agent_graph.png')

    return distances


def common_elements(a, b):

    a_set = set(a)
    b_set = set(b)

    if len(a_set.intersection(b_set)) > 0:
        return list((a_set.intersection(b_set)))
    else:
        return []


def clamp(val, minimum=0, maximum=255):
    if val < minimum:
        return minimum
    if val > maximum:
        return maximum
    return val


def load_file(filename):

    try:
        config = configparser.ConfigParser()
        config.read(filename)
        data = []
        simulation_config_parameters = config['PARAMETERS']

        #Check if all simulation parameters provided
        if not all(key in simulation_config_parameters.keys() for key in ['no_of_transactions','lambda','no_of_agents',\
                                                                      'alpha','latency','distance','tip_selection_algo',\
                                                                      'agent_choice','printing']):

            print("Parameter error! Please provide 'no_of_transactions','lambda','no_of_agents','alpha','latency',"
            "'distance','tip_selection_algo','agent_choice','printing'!")
            sys.exit(1)

        #Load simulation parameters
        _no_of_transactions = int(simulation_config_parameters['no_of_transactions'])
        _lambda = float(simulation_config_parameters['lambda'])
        _no_of_agents = int(simulation_config_parameters['no_of_agents'])
        _alpha = float(simulation_config_parameters['alpha'])
        _latency = int(simulation_config_parameters['latency'])

        if (type(ast.literal_eval(simulation_config_parameters['distance'])) is list):
            _distance = ast.literal_eval(simulation_config_parameters['distance'])
        else:
            _distance = create_distance_matrix(_no_of_agents,float(simulation_config_parameters['distance']))

        _tip_selection_algo = simulation_config_parameters['tip_selection_algo']

        if (simulation_config_parameters['agent_choice'] == 'None'):
            _agent_choice = list(np.ones(_no_of_agents) / _no_of_agents)
        else:
            _agent_choice = ast.literal_eval(simulation_config_parameters['agent_choice'])

        _printing = config.getboolean('PARAMETERS','printing')

        data.append((_no_of_transactions, _lambda, _no_of_agents, \
        _alpha, _latency, _distance, _tip_selection_algo, _agent_choice, _printing))

        #Load change parameters
        for key in config:
            if(key != 'PARAMETERS' and key != 'DEFAULT'):

                event_change_parameters = config[key]

                if 'step' not in event_change_parameters:
                    print("Please provide a 'step' for the parameter change!")
                    sys.exit(1)
                step = int(event_change_parameters['step'])

                if 'distance' not in event_change_parameters:
                    _distance = False
                elif (type(ast.literal_eval(event_change_parameters['distance'])) is list):
                    _distance = ast.literal_eval(event_change_parameters['distance'])
                else:
                    _distance = create_distance_matrix(_no_of_agents,float(event_change_parameters['distance']))

                if 'agent_choice' not in event_change_parameters:
                    _agent_choice = False
                elif (event_change_parameters['agent_choice'] == 'None'):
                    _agent_choice = list(np.ones(_no_of_agents) / _no_of_agents)
                else:
                    _agent_choice = ast.literal_eval(event_change_parameters['agent_choice'])
                    if (round(sum(_agent_choice), 3) != 1.0):
                        print("Agent choice not summing to 1.0: {}".format(sum(_agent_choice)))
                        sys.exit(1)
                    if (len(_agent_choice) != _no_of_agents):
                        print("Agent choice not matching no_of_agents: {}".format(len(_agent_choice)))
                        sys.exit(1)

                data.append((step, _distance, _agent_choice))

    except Exception as e:
        print(e)

    return data


def csv_export(self, file_name):

    with open(file_name, 'w', newline='') as file:
        writer = csv.writer(file, dialect='excel')
        #Write csv file header
        
        header=['txID', 'tips', 'arrival_time', 'agent',  'adoption_rate', 'block_transactions']
        #print(self.DG.nodes[0].id)
        #print(self.DG.nodes[0].seen)
        for transaction in self.DG.nodes:
            #print("seenList: ",transaction.seen)
            for agentId, agentSeen in enumerate(transaction.seen):
                header.append(str("agent_"+str(agentId+1)))
            break
        print(header)
        writer.writerow(header) #add confirmation time + 
        #Write genesis
        #writer.writerow([0,[],0, '', 0, 0])
        for transaction in self.DG.nodes:
            #Write all other transactions
            if(transaction.creation_time != 0):
                line = []
                line.append(transaction.id) #txid
                line.append(list(self.DG.successors(transaction))) #tips
                line.append(transaction.creation_time) #arrival_time
                line.append(transaction.creators) ##int(transaction.creators[0].id)+1) #agent
                
                
                #line.append(0) ##line.append(transaction.tip_selection_time
                #line.append(0) ## line.append(transaction.weight_update_time)
                line.append(len(list(nx.descendants(self.DG, transaction)))/(transaction.id+0.001)) #adoption_rate
                line.append(transaction.blockTransactions)
                for agentSeen in transaction.seen:
                    line.append(agentSeen)
                writer.writerow(line)

