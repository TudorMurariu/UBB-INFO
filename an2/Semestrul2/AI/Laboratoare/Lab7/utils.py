import numpy as np  
import pandas as pd

# Split the Data Into Training and Test Subsets (in proportion 80/20%)
def separateData(inputs, outputs):
    indexes = [i for i in range(len(inputs))]
    train_sample = np.random.choice(indexes, int(0.8 * len(inputs)), replace = False)
    validation_sample = [i for i in indexes  if not i in train_sample]

    df_train = pd.DataFrame()
    df_validate = pd.DataFrame()

    df_train['Inputs'] = [inputs[i] for i in train_sample]
    df_train['Outputs'] = [outputs[i] for i in train_sample]

    df_validate['Inputs'] = [inputs[i] for i in validation_sample]
    df_validate['Outputs'] = [outputs[i] for i in validation_sample]

    return df_train, df_validate

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