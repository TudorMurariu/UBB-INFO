import pandas as pd
import os
import numpy as np  
from sklearn import linear_model

import plot

crtDir =  os.getcwd()
filePath = os.path.join(crtDir, 'data', 'v3_world_happiness-report-2017.csv')

df = pd.read_csv(filePath)

# Visualize data
print(df[['Happiness.Score', 'Economy..GDP.per.Capita.']].head(20))

plot.DataHistogram(df['Economy..GDP.per.Capita.'])
plot.DataHistogram(df['Happiness.Score'])
plot.Liniarity(df['Economy..GDP.per.Capita.'], df['Happiness.Score'])