import pandas as pd
import os
import tensorflow as tf
from sklearn.metrics import mean_squared_error as mse
import plot
import utils
import my_regression
import normalisation

crt_dir =  os.getcwd()
file_path = os.path.join(crt_dir, 'data', 'v1_world_happiness-report-2017.csv')

df = pd.read_csv(file_path)
input_labels = ['Economy..GDP.per.Capita.', 'Freedom']
output_label = 'Happiness.Score'

#normalize input columns
df = normalisation.standardisation_norm(df, input_labels)
print(df[input_labels])

df_train, df_validate = utils.separate_data_df(df, input_labels + [output_label])
#plot.train_and_validate_df(df_train, df_validate, input_labels + [output_label])

# regression model model
x = df_train.drop(output_label, axis=1)
y = df_train[output_label]

x = x.fillna(x.mean())
# sklearn
regressor = linear_model.LinearRegression()
regressor.fit(x, y)

# my regressor
regressor = my_regression.MySGDRegression()
results = regressor.fit(x[input_labels], y, df_validate[input_labels], df_validate[output_label])

# save the model parameters
w0, w1, w2 = regressor.intercept_, regressor.coef_[0], regressor.coef_[1]
print('the learnt model: f(x) = ', w0, ' + ', w1, ' * x1 + ', w2 , '* x2')
#plot model 
#plot.learnt_model(df_train, input_labels+[output_label], regressor)

#plot predictions
# skelean
#df_validate['predicted'] = regressor.predict(df_validate.drop(output_label, axis=1))
# mine 
df_validate['predicted'] = regressor.predict(df_validate[input_labels])

#plot.predicted_vs_real(df_validate, input_labels + [output_label] + ['predicted'], err=mse(df_validate[output_label], df_validate['predicted']))

plot.plot_gen_mse(results)