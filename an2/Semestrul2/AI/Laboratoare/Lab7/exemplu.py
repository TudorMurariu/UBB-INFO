import pandas as pd
import os
import numpy as np  
from sklearn import linear_model
import matplotlib.pyplot as plt

import plot
import utils

crt_dir =  os.getcwd()
file_path = os.path.join(crt_dir, 'data', 'v1_world_happiness-report-2017.csv')

df = pd.read_csv(file_path)

df_train, df_validate = utils.separateData(df['Economy..GDP.per.Capita.'], df['Happiness.Score'])
#plot.trainedAndValidated(df_train, df_validate)

xx = [[el] for el in df_train['Inputs']]

# model initialisation
regressor = linear_model.LinearRegression()
# training the model by using the training inputs and known training outputs
regressor.fit(xx, df_train['Outputs'])
# save the model parameters
w0, w1 = regressor.intercept_, regressor.coef_[0]
print('the learnt model: f(x) = ', w0, ' + ', w1, ' * x')


# plot the learnt model
# prepare some synthetic data (inputs are random, while the outputs are computed by the learnt model)
noOfPoints = 1000
xref = []
val = min(df_train['Inputs'])
step = (max(df_train['Inputs']) - min(df_train['Inputs'])) / noOfPoints
for i in range(1, noOfPoints):
    xref.append(val)
    val += step
yref = [w0 + w1 * el for el in xref] 

plt.plot(df_train['Inputs'], df_train['Outputs'], 'ro', label = 'training data')  #train data are plotted by red and circle sign
plt.plot(xref, yref, 'b-', label = 'learnt model')                  #model is plotted by a blue line
plt.title('train data and the learnt model')
plt.xlabel('GDP capita')
plt.ylabel('happiness')
plt.legend()
plt.show()


# use the trained model to predict new inputs

# makes predictions for test data (manual)
# computedTestOutputs = [w0 + w1 * el for el in testInputs]

# makes predictions for test data (by tool)
computedValidationOutputs = regressor.predict([[x] for x in df_validate['Inputs']])

# plot the computed outputs (see how far they are from the real outputs)
plt.plot(df_validate['Inputs'], computedValidationOutputs, 'yo', label = 'computed test data')  #computed test data are plotted yellow red and circle sign
plt.plot(df_validate['Inputs'], df_validate['Outputs'], 'g^', label = 'real test data')  #real test data are plotted by green triangles
plt.title('computed validation and real validation data')
plt.xlabel('GDP capita')
plt.ylabel('happiness')
plt.legend()
plt.show()

# compute the differences between the predictions and real outputs
# "manual" computation
error = 0.0
for t1, t2 in zip(computedValidationOutputs, df_validate['Outputs']):
    error += (t1 - t2) ** 2
error = error / len(df_validate['Outputs'])
print('prediction error (manual): ', error)

# by using sklearn 
from sklearn.metrics import mean_squared_error

error = mean_squared_error(df_validate['Outputs'], computedValidationOutputs)
print('prediction error (tool):  ', error)