import math


def regression(real_data, computed_data):
    mae = 0
    rmse = 0
    for real, computed in zip(real_data, computed_data):
        mae += sum(abs(r - p) for r, p in zip(real, computed)) / len(real)
        rmse += math.sqrt(sum((r - p) ** 2 for r, p in zip(real, computed)) / len(real))
    # plot_regression(real_data_list, computed_data_list)
    # print(real_data)
    return mae / len(real_data), rmse / len(real_data)


def mean_square_error(real_data, computed_data):
    mse = 0
    for real, computed in zip(real_data, computed_data):
        mse += sum((r - p) ** 2 for r, p in zip(real, computed)) / len(real)
    return mse / len(real_data)

