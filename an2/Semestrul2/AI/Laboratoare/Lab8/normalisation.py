def sklearn_norm(df, labels = None):
    if labels is None:
        labels = list(df.columns)
    from sklearn.preprocessing import StandardScaler
    scaler = StandardScaler()
    df[labels[:-1]] = scaler.fit_transform(df[labels[:-1]])

    return df

def tensorflow_keras_norm(df, labels = None):
    if labels is None:
        labels = list(df.columns)
    import tensorflow as tf
    df[labels[:-1]] = tf.keras.utils.normalize(df[labels[:-1]], axis=0)

    return df

def clipping_norm(df, labels = None, edges = []):
    if labels is None:
        labels = list(df.columns)

    if len(edges) < len(labels):
        return

    edges_index = 0
    for label in labels:
        x, y = edges[edges_index]
        if x is not None:
            df[label] = df.apply(lambda row: row[label] if row[label] > x else x, axis=1) 
        if y is not None:
            df[label] = df.apply(lambda row: row[label] if row[label] < y else y, axis=1) 

    return df    

def log_scaling_norm(df, labels = None):
    if labels is None:
        labels = list(df.columns)

    from math import log
    for label in labels:
        if label not in list(df.select_dtypes('number').columns):
            continue
        df[label] = df.apply(lambda row: log(row[label]), axis = 1)
    
    return df
    

def min_max_scaling_norm(df, labels = None, negative_range = False):
    if labels is None:
        labels = list(df.columns)

    for label in labels:
        if label not in list(df.select_dtypes('number').columns):
            continue
        max_c = df[label].max()
        min_c = df[label].min()
        if negative_range:
            df[label] = df.apply(lambda row: (row[label] - min_c) / (max_c - min_c) * 2 - 1, axis = 1)
        else:
            df[label] = df.apply(lambda row: (row[label] - min_c) / (max_c - min_c), axis = 1)

    return df

def standardisation_norm(df, labels = None):
    if labels is None:
        labels = list(df.columns)

    for label in labels:
        if label not in list(df.select_dtypes('number').columns):
            continue

        m = df[label].sum() / len(df[label])
        s = (1 / len(df[label]) * sum([ (p - m) ** 2 for p in df[label]])) ** 0.5
        df[label] = df.apply(lambda row: (row[label]-m)/s, axis=1)

    return df

