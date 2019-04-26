import pandas as pd
import matplotlib.pyplot as plt
import csv
import numpy as np
import time

# new_indirect_ref_list = np.zeros((7000, 1))
new_indirect_ref_list = []
index = 0
lambdas = [1, 2]
# simulation_num = [i for i in range(1, 43)]
average_start = 1
layer = {}
layer_num = 1
mean_confirmed = []
mean_confirmed_in_each_simulation = []


def find_indirect_refs(loc, refs):
    """
    This function finds all the indirect references for all of our transactions.
    :param loc: Current transaction
    :param refs: References
    """
    global index
    global new_indirect_ref_list
    if loc == 0:
        return
    elif loc in indirect_references.keys():
        new_indirect_ref_list = list(set((new_indirect_ref_list + indirect_references[loc])))
        return
    else:
        num_of_refs = len(refs[loc])
        if refs[loc][0] not in new_indirect_ref_list:
            new_indirect_ref_list.append(refs[loc][0])
        find_indirect_refs(refs[loc][0], refs)
        if num_of_refs == 2:
            if refs[loc][1] not in new_indirect_ref_list:
                new_indirect_ref_list.append(refs[loc][1])
            find_indirect_refs(refs[loc][1], refs)


def find_layers_refs(loc, refs, confirmed_by_milestone):
    global layer_num
    if loc not in confirmed_by_milestone or loc == 0:
        return
    else:
        num_of_refs = len(refs[loc])
        if refs[loc][0] in list(layer.keys()):
            layer[refs[loc][0]] = min(layer[refs[loc][0]], layer_num)
        else:
            layer[refs[loc][0]] = layer_num
        layer_num += 1
        find_layers_refs(refs[loc][0], refs, confirmed_by_milestone)
        layer_num -= 1
        if num_of_refs == 2:
            if refs[loc][1] in list(layer.keys()):
                layer[refs[loc][1]] = min(layer[refs[loc][1]], layer_num)
            else:
                layer[refs[loc][1]] = layer_num
            layer_num += 1
            find_layers_refs(refs[loc][1], refs, confirmed_by_milestone)
            layer_num -= 1
    return layer


def store_indirect_refs_csv(indirect_refs, filename):
    """
    Storing all indirect references in a csv file
    :param indirect_refs: the dictionary containing the indirect references
    :param filename: The name of the output file.
    """
    begin_time = time.time()
    with open(filename, 'w', newline='') as file:
        writer = csv.writer(file, dialect='excel')
        writer.writerow(['index', 'indirect_references'])
        writer.writerow([0, []])
        for transaction in indirect_refs.keys():
            if transaction != 0:
                line = []
                line.append(transaction)
                line.append(indirect_refs[transaction])
                writer.writerow(line)
    print('write to indirect references to file took: ', time.time() - begin_time)


def milestone_conf_by_layer(title, milestone_range=(4, 9), font_size=13):
    font_size = font_size
    fig, ax = plt.subplots()
    ax.set_title(title, fontsize=font_size)
    ax.set_xlabel('Layer Number', fontsize=font_size)
    ax.set_ylabel('Number of TXs confirmed in the layer', fontsize=font_size)

    for i in range(milestone_range[0], milestone_range[1]):
        print('Starting', milestone_indexes[i])

        begin_time = time.time()
        layer_num = 1
        layer = find_layers_refs(milestone_indexes[i], references,
                                 conf_by_milestone[milestone_indexes[i]] + [milestone_indexes[i]])
        print('second recursive took: ', time.time() - begin_time)

        divided_by_layer = {}
        all_layers = list(set(list(layer.values())))
        conf_in_layer = []
        for k in all_layers:
            divided_by_layer[k] = [l for l, j in layer.items() if j == k]
            conf_in_layer.append(len(divided_by_layer[k]))
        x = np.array(all_layers)
        y = np.array(conf_in_layer)

        ax.set_xticks(np.arange(0, 20, step=1))
        ax.set_yticks(np.arange(0, 50, step=1))
        ax.plot(x, y, label='Milestone {}'.format(i), marker='o')
        plt.rcParams.update({'font.size': font_size})
        print('done with {} conf in layer is: {}'.format(i, conf_in_layer))

    legend = ax.legend(loc='upper right')
    plt.show()


def tx_num_conf_by_each_milestone(num_conf_by_milestone, current_lambda):
    x = np.array(list(num_conf_by_milestone.keys()))
    y = np.array(list(num_conf_by_milestone.values()))
    mean_confirmed.append(np.mean(y) / 60)
    y_mean = [np.mean(y)] * len(x)
    y_std = [np.std(y)] * len(x)
    fig, ax = plt.subplots()
    ax.set_title('No of transactions = 6000, lambda = {}, No of nodes = 20, Alpha = 0.001, Distance = 1, '
                 'Tip selection alg = weighted, Latency = 1'.format(current_lambda), fontsize=18)
    ax.set_xlabel('milestone number', fontsize=18)
    ax.set_ylabel('transcations confirmed by this milestone', fontsize=18)
    ax.set_xticks(np.arange(0, max(x) + 1, step=5))
    ax.bar(x, y)
    mean_line = ax.plot(x, y_mean, label='Mean = {:.2f}'.format(y_mean[0]), linestyle='--', color='red')
    ax.text(max(x) - 5, max(y) - 5, 'std = {:.2f}'.format(y_std[0]), horizontalalignment='center',
            verticalalignment='center', fontsize=18)

    plt.rcParams.update({'font.size': 22})
    legend = ax.legend(loc='upper right')
    # plt.figure(dpi=300)
    plt.show()


def average_conf_time_by_lambda():
    # Plot average confirmation time based on lambda
    fig, ax = plt.subplots()
    x = np.array(lambdas)
    y = np.array(mean_confirmed)
    ax.set_xlabel('lambda', fontsize=18)
    ax.set_ylabel('Average number of transactions confirmed (TX/S)', fontsize=18)
    ax.set_xticks(np.arange(0, max(x) + 1, step=1))
    ax.plot(x, y)
    plt.show()


for current_lambda in lambdas:
    file_name = "lambda_{}_tansactions_{}.csv".format(current_lambda, current_lambda * 1200)
    begin_time = time.time()
    # Reading data from simulation file
    data = pd.read_csv(file_name)
    data = data.sort_index()
    print('Reading main csv file took: ', time.time() - begin_time)

    references = data['references'].apply(eval).to_dict()
    # Finding all the indirect references
    begin_time = time.time()
    indirect_references = {}
    for j in references:
        new_indirect_ref_list = []
        find_indirect_refs(j, references)
        indirect_references[j] = new_indirect_ref_list
    print('recursive function: ', time.time() - begin_time)

    # # Writing indirect references into a file.
    # # Warning: This file would be large and is ignored in git.
    # indirect_ref_file = 'indirect_refs.csv'
    # store_indirect_refs_csv(indirect_references, indirect_ref_file)

    # Finding all milestones
    begin_time = time.time()
    milestone_times = [i for i in range(int(max(data['time']))) if i % 60 == 0]
    milestone_indexes = []
    for i in milestone_times:
        try:
            milestone_indexes.append((data[data['time'] == i].index[0]))
        except:
            pass
    print('Find milestones from data frame: ', time.time() - begin_time)

    # Reading from the indirect references file
    begin_time = time.time()
    indirect_keys = list(indirect_references.keys())
    indirect_values = list(indirect_references.values())
    df_dict = {'index': indirect_keys, 'indirect_references': indirect_values}
    indirect_refs_data = pd.DataFrame(data=df_dict)
    print(indirect_refs_data.head())
    milestone_indirect = [indirect_refs_data.loc[i][1] for i in milestone_indexes]

    # Find all the transactions that are confirmed by each milestone and their number.
    conf_by_milestone = {}
    num_conf_by_milestone = {}
    for trans in range(len(milestone_indirect)):
        already_confirmed = []
        for ms in range(0, trans):
            already_confirmed = list(set(already_confirmed + indirect_references[milestone_indexes[ms]]))

        if trans == 0:
            num_conf_by_milestone[trans] = len(indirect_references[milestone_indexes[trans]])
            conf_by_milestone[milestone_indexes[trans]] = indirect_references[milestone_indexes[trans]]
        else:
            conf_by_milestone[milestone_indexes[trans]] = []
            for ref in indirect_references[milestone_indexes[trans]]:
                if ref not in already_confirmed:
                    conf_by_milestone[milestone_indexes[trans]].append(ref)
            num_conf_by_milestone[trans] = len(conf_by_milestone[milestone_indexes[trans]])
    # print(num_conf_by_milestone)
    # print(conf_by_milestone)

    title = 'Num of TX = 6000, lambda = {}, Num of nodes = 20, Alpha = 0.001, \n Distance = 1, '
    ' Tip selection alg = weighted, Latency = 1'.format(current_lambda)

    # Plot transactions confirmed by each milestone on each layer
    milestone_conf_by_layer(title, milestone_range=(5, 8))
    # Plot the number of transactions confirmed by each milestone
    tx_num_conf_by_each_milestone(num_conf_by_milestone, current_lambda)
    # # # # #

# Plot average confirmation time based on lambda
average_conf_time_by_lambda()