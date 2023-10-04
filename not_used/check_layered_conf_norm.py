import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from scipy import stats
import random

lambdas = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# lambdas = [6]
milestone_start = 5
milestone_end = 9

UpperBound = []

# Given lambda, return a array of the mean values for specific range of milestones
def cal_mean_layered_conf(curr_lambda):
    mlstns_dict = {}
    for milestone in range(milestone_start, milestone_end + 1):
        data = pd.read_csv('SimuData/layered_conf_num/lambda_{}_layer_conf_{}.csv'.format(curr_lambda, milestone))
        mlstns_dict.__setitem__(milestone, data['conf_txs_num'])
    mlstns_df = pd.DataFrame(mlstns_dict)
    mlstns_mean_series = mlstns_df.mean(axis=1)
    # print(type(mlstns_mean_array))
    mlstns_mean_array = pd.Series(mlstns_mean_series).values
    print('Lambda={} Mlstns Conf Dict: '.format(curr_lambda))
    print(mlstns_df)
    # print('Lambda={} sum by rows: '.format(curr_lambda))
    # print(mlstns_df.sum(axis=1, skipna=True))
    print('Lambda={} sum by columns: '.format(curr_lambda))
    print(mlstns_df.sum(axis=1, skipna=True))
    # print('Layer length for each mlstn: '.format(mlstns_df.count(), type(mlstns_df.count())))
    # print(mlstns_df.count())
    # mean_upper_bound = np.round(np.mean(mlstns_df.count()), 3)
    # print('Mean Upper Bound: {}'.format(mean_upper_bound))
    # UpperBound.append(mean_upper_bound)
    # print('Mean vals of Mlstns: ', mlstns_mean_array)
    return mlstns_mean_array, mlstns_df.count(), mlstns_df


def generate_exam_arr(mlstns_layred_confs_df):
    exam_arr = []
    confs_sum_arr = mlstns_layred_confs_df.sum(axis=1, skipna=True)
    # print('conf_num_sum: ', confs_sum_arr)
    # print(type(confs_sum_arr))
    for layer in range(len(confs_sum_arr)):
        for freq in range(int(confs_sum_arr[layer])):
            exam_arr.append(float(layer+1))
    return exam_arr


def shapiro_check_layered_conf(examined_arr):
    # data = pd.read_csv('SimuData/layered_conf_num/lambdas-5-9-averaged.csv')
    # for i in range(5, 10):
    #     lambda_dict = data['lambda_{}'.format(i)].to_dict()
    #     val = np.fromiter(lambda_dict.values(), dtype=float)
    #     nan_free_val = val[~np.isnan(val)]
    #     print(nan_free_val)
    #     print(stats.shapiro(nan_free_val))
    # avg_lambda_dict = data['lambda_avg'].to_dict()
    # avg_val = np.fromiter(avg_lambda_dict.values(), dtype=float)
    # print(examined_arr)
    print('Shapiro test:')
    print(stats.shapiro(np.array(examined_arr)))


def normaltest_check_layered_conf(examined_arr):
    # data = pd.read_csv('SimuData/layered_conf_num/lambdas-5-9-averaged.csv')
    # for i in range(5, 10):
    #     lambda_dict = data['lambda_{}'.format(i)].to_dict()
    #     val = np.fromiter(lambda_dict.values(), dtype=float)
    #     nan_free_val = val[~np.isnan(val)]
    #     print(nan_free_val)
    #     print(stats.normaltest(nan_free_val))
    # avg_lambda_dict = data['lambda_avg'].to_dict()
    # avg_val = np.fromiter(avg_lambda_dict.values(), dtype=float)
    # print(examined_arr)
    print('Normal test:')
    print(stats.normaltest(np.array(examined_arr)))


def plot_norm_simu_comparisons(avg_val, curr_lambda):
    fig, ax = plt.subplots()
    ax.set_title('Simu Layered Conf v.s. Fitted Gaussian Model: {} = {}'.format(r'$\lambda$', curr_lambda))
    ax.set_xlabel('Layer Index Number')
    ax.set_ylabel('Number of Conf_TXs Located in Layers')

    # ax.set_title('Simu Layered Conf Distr v.s. Layer Num: Lambda = {}'.format(curr_lambda))
    # ax.set_xlabel('Layer Number')
    # ax.set_ylabel('Number of transactions confirmed in this layer')

    x = np.arange(1, len(avg_val)+1)
    y = avg_val / [sum(avg_val)]   # Normalized
    # plt.plot(x, y, label='Avg_Simu pdf')

    # # calculate mu and sigma^2
    mean_val = np.sum(x*y)
    var = np.sum(((x-mean_val)*(x-mean_val))*y)
    std_val = np.sqrt(var)

    # # Plot bar_conf_num_v.s._layers
    plt.bar(x, avg_val, label='Simu_Conf_bar')
    plt.plot(x, avg_val, label='Simu_Conf_num')

    # # Plot normal pdf
    print('mu: {}, sigma: {}'.format(mean_val, std_val))
    plt.plot(x, stats.norm.pdf(x, mean_val, std_val) * sum(avg_val), linestyle='--', label='Fitted_Gauss_Model')

    # plt.plot(x, stats.norm.pdf(x, mean_val, std_val), label='Normal pdf')  # Normalized


    # # Plot CDF
    # dx = 1
    # CY_simu = np.cumsum(y * dx)
    # plt.plot(x, CY_simu, label='Avg_Simu CDF', ls='--')
    # plt.plot(x, stats.norm.cdf(x, mean_val, std_val), label='Norm CDF', ls='--')
    # plt.xticks(x)
    ax.set_xticks(np.arange(0, max(x)+1, step=1))
    plt.legend(loc='upper left')
    plt.show()
    return mean_val, std_val


def plot_mu_sigma_layer(mu_arr, sigma_arr):
    fig, ax = plt.subplots()
    ax.set_title('Relationship between {} and Normal Distr Parameters'.format(r'$\lambda$'))
    ax.set_xlabel('{} Values'.format(r'$\lambda$'))
    ax.set_ylabel('Normal Distribution Properties Values')

    x = np.asarray(lambdas)
    plt.plot(x, mu_arr, label='Normal {}'.format(r'$\mu$'))
    plt.plot(x, sigma_arr, label='Normal {}^2'.format(r'$\sigma$'))
    plt.legend(loc='upper left')
    # plt.show()


def plot_layer_num_lambdas(mean_layer_num, mu_arr):
    fig, ax = plt.subplots()
    ax.set_title('Mean Layers Num v.s. Lambdas')
    ax.set_xlabel('Lambda Value')
    ax.set_ylabel('Number of Mean Layers')

    plt.plot(lambdas, mean_layer_num, label='Mean Layers for Each Lambda')
    # plt.plot(lambdas, np.multiply(np.array(mean_layer_num), np.array(mu_arr)), label='Mean Layers for Each Lambda')

    plt.legend(loc='upper left')
    # plt.show()


if __name__ == '__main__':
    # shapiro_check_layered_conf()
    # normaltest_check_layered_conf()

    mus = []
    sigmas = []
    layers_num_mean = []
    for current_lambda in lambdas:
        test_array, layers_num_array, mlstns_confs_df = cal_mean_layered_conf(current_lambda)
        exam_arr = generate_exam_arr(mlstns_confs_df)
        # print('exam_arr: ', '\n', exam_arr)
        print('mean_conf: ', test_array)
        for layer in range(len(test_array)):
            print(layer+1, ' ', float(test_array[layer]))
        shapiro_check_layered_conf(exam_arr)
        normaltest_check_layered_conf(exam_arr)
        print('Shapiro_test for lambda={}: '.format(current_lambda), stats.shapiro(test_array))
        print('Normal_test for lambda={}: '.format(current_lambda), stats.normaltest(test_array))
        mu, sigma = plot_norm_simu_comparisons(test_array, current_lambda)
        mus.append(np.round(mu, 3))
        sigmas.append(np.round(sigma, 3))
        layers_num_mean.append(layers_num_array.mean())
    print('mus: ', mus, 'sigmas:', sigmas)
    CI_upper_bound = np.array([np.round(1.96 * sig + m, 2) for sig, m in zip(sigmas, mus)])
    print('CI Upper Bound: {}'.format(CI_upper_bound))
    print('Actual Mean Upper Bounds: {}'.format(np.array(UpperBound)))
    plot_mu_sigma_layer(np.asarray(mus), np.asarray(sigmas))
    plot_layer_num_lambdas(layers_num_mean, mus)
