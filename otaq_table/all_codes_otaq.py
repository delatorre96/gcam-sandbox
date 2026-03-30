import pandas as pd
import os

##Generate csv with uniqeu terms
df_model = pd.read_csv('0. OTAQ_trn_data_EMF37.csv', skiprows=6)

df_model['variable'].unique()
important_cols = [ 'UCD_sector', 'mode', 'size.class', 'UCD_technology',
       'UCD_fuel', 'variable']
df_model_cols = df_model[important_cols]
unique_terms = df_model_cols.drop_duplicates()
unique_terms.to_csv('1. unique_terms.csv')
for i in df_model_cols.columns:
    print(df_model_cols[i].unique())

##Generate csv with all codes 
JRC_file = 'JRC-IDEES-2023/AT/JRC-IDEES-2023_Transport_AT.xlsx'
xls = pd.ExcelFile(JRC_file)
codes_list = []
for sheet in xls.sheet_names:
    if sheet not in ['cover', 'index']:
        df = pd.read_excel(JRC_file, sheet_name=sheet, dtype=str)  
        code = df['Code'].dropna().to_list()
        codes_list.append(code)
codes_plano = [x for sublista in codes_list for x in sublista]
df_codes = pd.DataFrame(codes_plano).rename(columns = {0 : 'Code'}).to_csv('1. JRC_all_codes.csv')
################ MAGIC ####################
##