from sklearn.datasets import load_breast_cancer
import utils
import pandas as pd
import plot
import normalisation

#read dataset as dataframe
labels = ['mean radius', 'mean texture', 'target']
df = load_breast_cancer(as_frame=True).frame[labels]

#normalize data
#df = normalisation.sklearn_norm(df, labels[:-1])
df = normalisation.min_max_scaling_norm(df, [labels[1]], negative_range=True)
df = normalisation.standardisation_norm(df, labels[:-1])
#df = normalisation.tensorflow_keras_norm(df, labels[:-1])
print(df.head(10))

#divide into train and test data
df_train, df_validate = utils.separate_data_df(df, df.columns)

# Logistic Regressor
from sklearn import linear_model
classifier = linear_model.LogisticRegression()
classifier.fit(df_train[labels[:-1]], df_train[labels[-1]])

w0, w1, w2 = classifier.intercept_[0], classifier.coef_[0][0], classifier.coef_[0][1]
print(f'classification model: y(feat1, feat2) = {w0} + {w1} * {labels[0]} + {w2} * {labels[1]}')

df_validate['predicted'] = classifier.predict(df_validate[labels[:-1]])

#print(df_validate.head(15))

plot.plot_metrics(df_validate)