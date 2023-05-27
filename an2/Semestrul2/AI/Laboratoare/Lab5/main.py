from regression import *
from classification import *
import csv
import sklearn.metrics as sk_metrics


def read_from_csv(file_name):
    with open(file_name, 'r') as f:
        csv_reader = csv.reader(f)
        data = []
        for line in csv_reader:
            data += [line]
        return data

def parse_regression_data(data):
    real_data = []
    computed_data = []
    for d in data:
        for i in range(3):
            real_data += [int(d[i])]
        for j in range(3, 6):
            computed_data += [int(d[j])]

    real_data = [real_data[n:n + 3] for n in range(0, len(real_data), 3)]
    computed_data = [computed_data[n:n + 3] for n in range(0, len(computed_data), 3)]

    return real_data, computed_data

def parse_classification_data(data):
    real_data = []
    computed_data = []
    for line in data:
        real_data += [line[0]]
        computed_data += [line[1]]
    return real_data, computed_data


def prediction_error_regression():
    sports_data = read_from_csv('data/sport.csv')
    sports_data.pop(0)
    real_data, computed_data = parse_regression_data(sports_data)
    mae, rmse = regression(real_data, computed_data)
    print('Mean Absolute Error(L1): ', mae)
    print('Root Mean Square Error(L2): ', rmse)
    print('\n')


def multiclass_classification_performance_measures():
    flowers_data = read_from_csv('data/flowers.csv')
    real_data, computed_data = parse_classification_data(flowers_data)
    matrix, acc, precision, recall = classification_using_libs(real_data, computed_data, list(set(real_data)))
    print('Confusion Matrix: ', matrix)
    print('Accuracy: ', acc)
    print('Precision: ', precision)
    print('Recall: ', recall)
    print('\n')
    acc, precision, recall = my_classification(real_data, computed_data, list(set(real_data)))
    print('Accuracy: ', acc)
    print('Precision: ', precision)
    print('Recall: ', recall)
    print('\n')


def run_regression_loss():
    sports_data = read_from_csv('data/sport.csv')
    sports_data.pop(0)
    real, computed = parse_regression_data(sports_data)
    mse = mean_square_error(real, computed)
    print('Loss for regression problem: ', mse)


def run_binary_classification_loss():
    real_values = ['spam', 'spam', 'ham', 'ham', 'spam', 'ham']
    computed_outputs = [[0.7, 0.3], [0.2, 0.8], [0.4, 0.6], [0.9, 0.1], [0.7, 0.3], [0.4, 0.6]]
    log_loss = binary_classification_loss(real_values, computed_outputs, 'spam')
    print('Loss for binary classification: ', log_loss)


def run_multiclass_classification_loss():
    loss = multiclass_classification_loss([1, 0, 0, 0, 0], [0.8, 0.17, 0.01, 0.01, 0.1])
    print('Loss for multiclass classification: ', loss)


def run_multilabel_classification_loss():
    loss = multilabel_classification_loss([0, 1, 0, 1, 0], [0.7, 0.9, 0.1, 0.8, 0.3])
    print('Loss for multilabel classification: ', loss)

if __name__ == "__main__":
    prediction_error_regression()
    multiclass_classification_performance_measures()
    run_regression_loss()
    run_binary_classification_loss()
    run_multiclass_classification_loss()
    run_multilabel_classification_loss()
