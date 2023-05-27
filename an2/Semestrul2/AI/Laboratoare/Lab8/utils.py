from sklearn.datasets import load_breast_cancer
from sklearn.preprocessing import StandardScaler
import pandas as pd
import numpy as np

def get_breast_cancer_df(labels = None):
    #returns breast_cancer dataset as a DataFrame
    data = load_breast_cancer()
    df = pd.DataFrame(data['data'], columns=data['feature_names'])
    df['target'] = pd.Series(data['target'])
    if labels is not None:
        return df[labels]
    return df


def keep_row(row, keep_indexes):
    if row.name in keep_indexes:
        return True
    return False


def separate_data_df(df, labels):
    indexes = [i for i in range(df.shape[0])]
    train_sample = np.random.choice(indexes, int(0.8 * len(indexes)), replace = False)

    df_train = df[labels].copy()
    df_validate = df[labels].copy()

    df_train['keep'] = df.apply(keep_row, axis=1, keep_indexes=train_sample)
    df_validate['keep'] = df.apply(keep_row, axis=1, keep_indexes=train_sample)

    df_train = df_train[df_train['keep'] == True]
    df_validate = df_validate[df_validate['keep'] == False]

    return df_train.drop('keep', axis=1), df_validate.drop('keep', axis=1)

