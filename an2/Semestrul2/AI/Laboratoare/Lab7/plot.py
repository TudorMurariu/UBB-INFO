import matplotlib.pyplot as plt
from mpl_toolkits import mplot3d
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

def DataHistogram(x, variableName):
    n, bins, patches = plt.hist(x, 10)
    plt.title('Histogram of ' + variableName)
    plt.show()

def Liniarity(inputs, outputs):
    plt.plot(inputs, outputs, 'ro') 
    plt.xlabel('GDP capita')
    plt.ylabel('happiness')
    plt.title('GDP capita vs. happiness')
    plt.show()

def trainedAndValidated(df_trained, df_validated):
    plt.plot(df_trained['Inputs'], df_trained['Outputs'], 'ro', label = 'training data')   #train data are plotted by red and circle sign
    plt.plot(df_validated['Inputs'], df_validated['Outputs'], 'g^', label = 'validation data')     #test data are plotted by green and a triangle sign
    plt.title('train and validation data')
    plt.xlabel('GDP capita')
    plt.ylabel('happiness')
    plt.legend()
    plt.show()

def train_and_validate_df(df_trained, df_validated, labels):
    fig = plt.figure()
    ax = plt.axes(projection='3d')
    ax.scatter(df_trained[labels[0]], df_trained[labels[1]], df_trained[labels[2]], color='blue', label='Training Data')
    ax.scatter(df_validated[labels[0]], df_validated[labels[1]], df_validated[labels[2]], color='red', label='Validating Data')

    plt.title('train and validation data')
    ax.set_xlabel(labels[0])
    ax.set_ylabel(labels[1])
    ax.set_zlabel(labels[2])
    plt.legend()
    plt.show()

def plot_gen_mse(computed_mses):
    fig = plt.plot(range(len(computed_mses)), computed_mses, 'r-', label='mse')
    plt.title('final mse = ' + str(computed_mses[-1]))
    plt.xlabel('Generation')
    plt.ylabel('mse')
    plt.show()

def predicted_vs_real(df, labels, err = 0):
    fig = plt.figure()
    ax = plt.axes(projection='3d')
    ax.scatter(df[labels[0]], df[labels[1]], df[labels[2]], color='blue', label='Real Test Data')
    ax.scatter(df[labels[0]], df[labels[1]], df[labels[3]], color='red', label='Predicted Test Data')

    plt.title(f'Real and Predicted data\n MSE: {err}')
    ax.set_xlabel(labels[0])
    ax.set_ylabel(labels[1])
    ax.set_zlabel(labels[2])
    plt.legend()
    plt.show()

def learnt_model(df_trained, labels, regressor):
    noOfPoints = 1000
    x = np.linspace(df_trained[labels[0]].min(), df_trained[labels[0]].max(), noOfPoints)
    y = np.linspace(df_trained[labels[1]].min(), df_trained[labels[1]].max(), noOfPoints)

    xref, yref = np.meshgrid(x, y)
    w0, w1, w2 = regressor.intercept_, regressor.coef_[0], regressor.coef_[1]

    zref = w0 + w1*xref + w2*yref

    fig = plt.figure()
    plt.title('Learnt regression model')
    ax = fig.add_subplot(111, projection='3d')
    ax.scatter(df_trained[labels[0]], df_trained[labels[1]], df_trained[labels[2]], color='blue', label='Training Data')
    ax.plot_surface(xref, yref, zref, cmap='viridis')
    ax.set_xlabel(labels[0])
    ax.set_ylabel(labels[1])
    ax.set_zlabel(labels[2])
    plt.show()