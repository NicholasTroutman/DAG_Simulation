import pandas as pd
import matplotlib.pyplot as plt
import csv
import numpy as np
import time

new_indirect_ref_list = []
index = 0
lambdas = [8, 9, 10]
# simulation_num = [i for i in range(1, 43)]
average_start = 1
layer = {}
layer_num = 1


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


mean_confirmed = []
mean_confirmed_in_each_simulation = []

# Calculate all lambdas corresponding layered confirmed tx and store into csv files
for current_lambda in lambdas:
    begin_time = time.time()
    # Reading data from simulation file
    data = pd.read_csv("SimuData/layered_conf_num_20_Milestones/lambda_{}_tansactions_{}.csv".format(current_lambda,
                                                                                                     current_lambda * 1200))

    data = data.sort_index()
    print('read main csv file: ', time.time() - begin_time)
    references = data['references'].apply(eval).to_dict()

    # Finding all the indirect references
    begin_time = time.time()
    indirect_references = {}
    for j in references:
        new_indirect_ref_list = []
        find_indirect_refs(j, references)
        indirect_references[j] = new_indirect_ref_list
    print('recursive function: ', time.time() - begin_time)

    # Storing all indirect references in a csv file
    begin_time = time.time()
    with open('indirect_refs.csv', 'w', newline='') as file:
        writer = csv.writer(file, dialect='excel')
        writer.writerow(['index', 'indirect_references'])
        writer.writerow([0, []])
        for transaction in indirect_references.keys():
            if transaction != 0:
                line = []
                line.append(transaction)
                line.append(indirect_references[transaction])
                writer.writerow(line)
    print('write to indirect_refs.csv: ', time.time() - begin_time)

    # Finding all milestones
    begin_time = time.time()
    milestone_times = [i for i in range(int(max(data['time']))) if i % 60 == 0]
    print(milestone_times)
    milestone_indexes = []
    for i in milestone_times:
        try:
            milestone_indexes.append((data[data['time'] == i].index[0]))
        except:
            pass
    print(milestone_indexes)
    print('find milestones from data frame: ', time.time() - begin_time)

    # Reading from the indirect references file
    begin_time = time.time()
    indirect_refs_data = pd.read_csv("indirect_refs.csv")
    milestone_indirect = [indirect_refs_data.loc[i][1] for i in milestone_indexes]
    print('read indirect_refs.csv and find milestones indirect from data frame: ', time.time() - begin_time)

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
    print(num_conf_by_milestone)
    print(conf_by_milestone)

    for i in range(5, 16):
        print('Starting', milestone_indexes[i])

        begin_time = time.time()
        layer_num = 1
        layer = {}
        find_layers_refs(milestone_indexes[i], references,
                         conf_by_milestone[milestone_indexes[i]] + [milestone_indexes[i]])
        print('second recursive took: ', time.time() - begin_time)

        divided_by_layer = {}
        all_layers = list(set(list(layer.values())))
        conf_in_layer = []
        for k in all_layers:
            divided_by_layer[k] = [l for l, j in layer.items() if j == k]
            print(divided_by_layer[k])
            conf_in_layer.append(len(divided_by_layer[k]))
        x = np.array(all_layers)
        y = np.array(conf_in_layer)

        # # # # Store transactions confirmed by each milestone on each layer into csv files
        with open('SimuData/layered_conf_num_20_Milestones/layered_conf_num/lambda_{}_layer_conf_{}.csv'.format(current_lambda, i), 'w',
                  newline='') as file2:
            writer = csv.writer(file2, dialect='excel')
            writer.writerow(['layer_index', 'conf_txs_num'])
            for layer in x:
                line = []
                line.append(layer)
                line.append(y[layer - 1])
                writer.writerow(line)

        # ax.set_xticks(np.arange(0, 20, step=1))
        # ax.set_yticks(np.arange(0, y.max(), step=1))
        # ax.plot(x, y, label='Milestone {}'.format(i), marker='o')
        # plt.rcParams.update({'font.size': 22})
        print('done with {} conf in layer is: {}'.format(i, conf_in_layer))

    # legend = ax.legend(loc='upper right')
    # plt.show()
