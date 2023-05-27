from math import exp
from numpy.linalg import inv
import numpy as np
from sklearn.metrics import mean_squared_error as mse
from sklearn.metrics import accuracy_score

def sigmoid(x):
    return 1 / (1 + exp(-x))

def softmax(z):
    return np.exp(z) / np.sum(np.exp(z), keepdims=True)  

class MyLogisticRegression:
    def __init__(self):
        self.intercept_ = [0.0]
        self.coef_ = []
        self.classes_ = [1, 2, 3]

    # use the gradient descent method
    # simple stochastic GD
    def fit(self, x, y, df_val, df_out, learningRate = 0.001, noEpochs = 10000):
        self.coef_ = [[0.0 for _ in range(x.shape[1])]]   #beta or w coefficients y = w0 + w1 * x1 + w2 * x2 + ...
        # self.coef_ = [random.random() for _ in range(x.shape[1])]    #beta or w coefficients 

        results = np.array([])
        for _ in range(noEpochs):
            # shuffle the training examples in order to prevent cycles
            permutation = np.random.permutation(x.shape[0])
            x = x.iloc[permutation]
            y = y.iloc[permutation]
            
            for i in range(x.shape[0]): # for each sample from the training data
                ycomputed = softmax(self.eval(x.iloc[i]))     # estimate the output
                crtError = ycomputed - y.iloc[i]     # compute the error for the current sample
                # update the coefficients
                self.coef_[0] -= learningRate * crtError * x.iloc[i]
                self.intercept_[0] -= learningRate * crtError * 1

            predicted = np.array(self.predict(df_val))
            results = np.append(results, accuracy_score(df_out, predicted))
        
        return results
    def eval(self, xi):
        yi = self.intercept_[0]
        for j in range(len(xi)):
            yi += self.coef_[0][j] * xi[j]
        return yi 

    def predict(self, X):
        # compute predicted probabilities for each class
        proba = self.predict_proba(X)
        
        # determine the number of unique target values in the training data
        n_classes = len(self.classes_)

        # if there is only one class, return the predicted probabilities for that class
        if n_classes == 1:
            return np.ravel(proba)

        # if there are two classes, return the predicted probabilities for the positive class
        if n_classes == 2:
            return proba[:, 1]

        # if there are more than two classes, return the class with the highest predicted probability
        predicted = np.argmax(proba, axis=1)
        return np.array([self.classes_[i] for i in predicted])

    def predict_proba(self, X):
        # compute predicted probabilities for each class
        z = np.dot(X, self.coef_[0].T) + self.intercept_[0]
        proba = 1 / (1 + np.exp(-z))

        # if there is only one class, return the predicted probabilities for that class
        if len(self.classes_) == 1:
            return np.column_stack([1 - proba, proba])

        # otherwise, stack the predicted probabilities for each class
        return np.column_stack([1 - proba, proba])