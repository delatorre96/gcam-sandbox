import xml2py
import pickle
import os 

file = '../../gcam-v8.2-Windows-Release-Package/output/Reference_2025-25-9T11_05_26+02_00.xml'

data = xml2py.get_data_threaded(file = file, max_workers=os.cpu_count() - 1)

with open("Data/datos.pkl", "wb") as archivo:
    pickle.dump(data, archivo)