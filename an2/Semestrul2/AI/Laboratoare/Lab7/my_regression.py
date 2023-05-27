import numpy as np
import pandas as pd
import random
from sklearn.metrics import mean_squared_error as mse

class MySGDRegression:
    def __init__(self):
        self.intercept_ = None
        self.coef_ = None

    # simple stochastic GD
    def fit(self, x, y, df_val, df_out,  learningRate = 0.001, noEpochs = 1000):
        #self.coef_ = np.zeros(x.shape[1])
        self.coef_ = [random.random() for _ in range(x.shape[1])]    #beta or w coefficients 
        #self.intercept_ = random.random()
        self.intercept_ = 0.0

        results = np.array([])
        for _ in range(noEpochs):
            # shuffle the training examples in order to prevent cycles
            permutation = np.random.permutation(x.shape[0])
            x = x.iloc[permutation]
            y = y.iloc[permutation]

            for i in range(x.shape[0]): # for each sample from the training data
                ycomputed = self.eval(list(x.iloc[i]))     # estimate the output
                crtError = ycomputed - y.iloc[i]     # compute the error for the current sample
                self.coef_ -= learningRate * crtError * x.iloc[i]
                self.intercept_ -= learningRate * crtError * 1
            #print(self.intercept_)
            predicted = np.array(self.predict(df_val))

            results = np.append(results, mse(df_out, predicted))
        
        return results

    def eval(self, xi):
        yi = self.intercept_
        for j in range(len(xi)):
            yi += self.coef_[j] * xi[j]
        return yi 

    def predict(self, x):
        yComputed = [self.eval(xi.values) for xi in x.iloc]
        return yComputed
    


class MyNonLinearGDRegression:
    def __init__(self, kernel=None, gamma=1, degree=2):
        self.intercept_ = None
        self.coef_ = None
        self.kernel = kernel
        self.gamma = gamma
        self.degree = degree

    def fit(self, x, y, learningRate=0.001, noEpochs=1000):
        # apply kernel function to input features
        if self.kernel is not None:
            x = self.kernel(x, self.gamma, self.degree)

        self.coef_ = np.zeros(x.shape[1])
        self.intercept_ = 0.0

        for _ in range(noEpochs):
            permutation = np.random.permutation(x.shape[0])
            x = x.iloc[permutation]
            y = y.iloc[permutation]

            for i in range(x.shape[0]):
                ycomputed = self.eval(list(x.iloc[i]))
                crtError = ycomputed - y.iloc[i]
                self.coef_ -= learningRate * crtError * x.iloc[i]
                self.intercept_ -= learningRate * crtError

    def eval(self, xi):
        yi = self.intercept_
        for j in range(len(xi)):
            yi += self.coef_[j] * xi[j]
        return yi 

    def predict(self, x):
        if self.kernel is not None:
            x = self.kernel(x, self.gamma, self.degree)

        yComputed = [self.eval(xi.values) for xi in x.iloc]
        return yComputed

    # nonlinear kernel functions
    @staticmethod
    def polynomial_kernel(x, gamma, degree):
        n_samples = x.shape[0]
        kernel_matrix = np.zeros((n_samples, n_samples))

        for i in range(n_samples):
            for j in range(i, n_samples):
                kernel_value = (gamma * np.dot(x.iloc[i], x.iloc[j]) + 1) ** degree
                kernel_matrix[i, j] = kernel_value
                kernel_matrix[j, i] = kernel_value

        return pd.DataFrame(kernel_matrix)

    @staticmethod
    def rbf_kernel(x, gamma, degree):
        gram = np.zeros((x.shape[0], x.shape[0]))
        for i in range(x.shape[0]):
            for j in range(x.shape[0]):
                dist = np.linalg.norm(x[i] - x[j])
                gram[i, j] = np.exp(-gamma * dist ** 2)
        return gram