import numpy as np
import pandas as pd
import datetime as dt
import seaborn as sns 
import matplotlib.pyplot as plt 

DATASET_FILENAME = "./data/dataset.csv"

energy_raw = pd.read_csv(DATASET_FILENAME)
print(energy_raw.shape)
print(energy_raw.head())


# check datatypes 
print(energy_raw.dtypes)

energy_raw['Date and time'] = pd.to_datetime(energy_raw['Date and time'])
print(energy_raw.dtypes)        

