from sklearn.datasets import load_iris
import utils
import pandas as pd
import plot
import normalisation

#read dataset as dataframe
df = load_iris(as_frame=True).frame
labels = list(df.columns)

#normalize data
df = normalisation.standardisation_norm(df, labels[:-1])

#divide into train and test data
df_train, df_validate = utils.separate_data_df(df, df.columns)

# Logistic Regressor
# from sklearn import linear_model
# classifier = linear_model.LogisticRegression()
# classifier.fit(df_train[labels[:-1]], df_train[labels[-1]])

# personal regressor
from regressor import MyLogisticRegression
classifier = MyLogisticRegression()
results = classifier.fit(df_train[labels[:-1]], df_train[labels[-1]], df_validate[labels[:-1]], df_validate[labels[-1]])

w0, w1, w2, w3, w4 = classifier.intercept_[0], classifier.coef_[0][0], classifier.coef_[0][1], classifier.coef_[0][2], classifier.coef_[0][3]
print(f'classification model: y(feat1, feat2) = {w0} + {w1} * {labels[0]} + {w2} * {labels[1]} + {w3} * {labels[2]} + {w4} * {labels[3]}')

df_validate['predicted'] = classifier.predict(df_validate[labels[:-1]])

print(df_validate.tail(15))

plot.plot_metrics(df_validate)
plot.plot_gen_mse(results)