import pandas as pd
import os
##Build otaq table


df_JRC_otaq = pd.read_excel('3. OTAQ_JRC_mapped_magic_revised_noDuplicates.xlsx', index_col = 0)


# ##Add diesel and gasoline
# df_JRC_otaq_gasoline = df_JRC_otaq[(df_JRC_otaq['Code'].str.contains('Gasoline', na=False)) 
#                                 & (df_JRC_otaq['Posible inclusión'].str.contains('Diesel', na=False))]
# df_JRC_otaq = df_JRC_otaq.drop(df_JRC_otaq_gasoline.index)
# df_JRC_otaq_diesel = df_JRC_otaq_gasoline.copy()
# df_JRC_otaq_gasoline['UCD_fuel']  = df_JRC_otaq_gasoline['UCD_fuel']  + '_gasoline'
# df_JRC_otaq_diesel['Code'] =  df_JRC_otaq_diesel['Posible inclusión']
# df_JRC_otaq_diesel['UCD_fuel'] =  df_JRC_otaq_diesel['UCD_fuel'] + '_diesel'  
# df_JRC_otaq = pd.concat([df_JRC_otaq, df_JRC_otaq_gasoline, df_JRC_otaq_diesel]).reset_index(drop=True)
# df_JRC_otaq = df_JRC_otaq[df_JRC_otaq.UCD_fuel != 'Liquids']
    
##Tntra and extra EU
#Air
df_JRC_otaq_intra = df_JRC_otaq[(df_JRC_otaq['Intra EEA'].str.contains('Intra', na=False)) & 
                                (df_JRC_otaq['mode'].str.contains('Air', na=False))]
df_JRC_otaq_intra['Code'] = df_JRC_otaq_intra['Intra EEA']
df_JRC_otaq_intra['mode'] = df_JRC_otaq_intra['mode'].str.replace('International', 'EEA')
df_JRC_otaq =  pd.concat ([df_JRC_otaq,df_JRC_otaq_intra]).reset_index()
#Ships
df_JRC_otaq_intra = df_JRC_otaq[(df_JRC_otaq['Intra EEA'].str.contains('Intra', na=False)) & 
                                (df_JRC_otaq['mode'].str.contains('Ship', na=False))]
df_JRC_otaq_intra['Code'] = df_JRC_otaq_intra['Intra EEA']
df_JRC_otaq_intra['mode'] = df_JRC_otaq_intra['mode'].str.replace('International', 'EEA')
df_JRC_otaq =  pd.concat ([df_JRC_otaq,df_JRC_otaq_intra]).reset_index()



for year in range(2005,2024):
    df_JRC_otaq[year] = None
df_JRC_otaq['Unit'] = None

def getUnitsFromSheet(df_country):
    exclude = ['gasoline and electricity','diesel oil incl. biofuels','Gasoline', 'excluding biofuel portion','incl. biofuels']
    lista = df_country.iloc[:, 0].to_list()
    lista_units = lista[:2]
    unit = None

    for i in lista[2:]:
        if pd.isna(i):
            lista_units.append(unit)

        elif '(' in i:
            candidate = i.split('(')[-1].rstrip(')').strip()

            if candidate not in exclude:
                unit = candidate

            lista_units.append(unit)

        else:
            lista_units.append(unit)
    return lista_units


country_codes = os.listdir('JRC-IDEES-2023')
df_JRC_otaq_codes_AT = list(df_JRC_otaq['Code'].astype(str).unique())
list_country_codes = {'AT':df_JRC_otaq_codes_AT}
for cntry_code in country_codes:
    if cntry_code != 'AT':
        df_JRC_otaq_codes_country = df_JRC_otaq_codes_AT
        for code in df_JRC_otaq_codes_AT:
            if not pd.isna(code):
                code_new = code.replace('AT',cntry_code)
                df_JRC_otaq_codes_country[df_JRC_otaq_codes_country.index(code)] = code_new
        list_country_codes.update({cntry_code : df_JRC_otaq_codes_country})

def change_codes(df_JRC_otaq_cntry, cntry_code):
    list_country_codes = []
    if cntry_code != 'AT':
        for code in df_JRC_otaq_cntry['Code']:
            if not pd.isna(code):
                code_new = code.replace('AT',cntry_code)
                list_country_codes.append(code_new)
            else:
                list_country_codes.append(None)
        df_JRC_otaq_cntry['Code'] = list_country_codes
    return df_JRC_otaq_cntry
                        


df_otaq_final = df_JRC_otaq.iloc[0:0].copy()
df_otaq_final['UCD_region'] = None
df_otaq_final['Unit'] = None
for cntry_code in country_codes:
    df_JRC_otaq_cntry = df_JRC_otaq.copy()
    df_JRC_otaq_cntry = change_codes(df_JRC_otaq_cntry, cntry_code)
    df_JRC_otaq_cntry['UCD_region'] = cntry_code
    df_JRC_otaq_cntry['Code'] = df_JRC_otaq_cntry['Code'].astype(str).str.strip()
    file_cntry = f'JRC-IDEES-2023/{cntry_code}/JRC-IDEES-2023_Transport_{cntry_code}.xlsx'
    xls_country = pd.ExcelFile(file_cntry)
    for sheet in xls_country.sheet_names:
        if sheet not in ['cover', 'index','Transport']:
            df_country = pd.read_excel(file_cntry, sheet_name=sheet, dtype=str)
            year_cols = [col for col in df_country.columns if str(col).isnumeric()]
            df_country[year_cols] = ( df_country[year_cols].replace("0", pd.NA).replace(0, pd.NA))
            lista_units = getUnitsFromSheet (df_country)
            df_country['Unit'] = lista_units
            df_JRC_otaq_codes = list(df_JRC_otaq_cntry['Code'].astype(str).unique())
            rows_with_codes =  df_country[(df_country['Code'].notna()) & (df_country['Code'].isin(df_JRC_otaq_codes))].dropna(axis=1, how="all")
            if not rows_with_codes.empty:
                rows_with_codes['Code'] = rows_with_codes['Code'].astype(str).str.strip()
                rows_with_codes = rows_with_codes.iloc[:, 1:]
                rows_idx = rows_with_codes.set_index("Code")
                jrc_idx = df_JRC_otaq_cntry.set_index("Code")
                jrc_idx.update(rows_idx)
                df_JRC_otaq_cntry = jrc_idx.reset_index()
    df_otaq_final = pd.concat([df_otaq_final, df_JRC_otaq_cntry])

df_otaq_final_ship_domestic_IWW = df_otaq_final[df_otaq_final['mode'] == 'Ship Domestic_IWW'].reset_index(drop=True)    
df_otaq_final_ship_domestic = df_otaq_final[df_otaq_final['mode'] == 'Ship Domestic'].reset_index(drop=True)    
df_otaq_final = df_otaq_final[df_otaq_final['mode'] != 'Ship Domestic_IWW'].reset_index(drop=True)    
df_otaq_final = df_otaq_final[df_otaq_final['mode'] != 'Ship Domestic'].reset_index(drop=True)   


df_otaq_final_totalgasEnergy_hybrid = df_otaq_final[df_otaq_final['Restar'].notna()].reset_index(drop=True)
df_otaq_final_totalgasEnergy_hybrid['Restar'] = df_otaq_final_totalgasEnergy_hybrid.apply(lambda row: row['Restar'].replace('AT', row['UCD_region']), axis=1)
df_otaq_final = df_otaq_final[df_otaq_final['Restar'].isna()]
df_otaq_final_restar = df_otaq_final[df_otaq_final['Code'].isin(df_otaq_final_totalgasEnergy_hybrid['Restar'].to_list())].reset_index(drop = True)


for year in range(2005, 2024):
    df_otaq_final[year] = pd.to_numeric(df_otaq_final[year], errors='coerce')
    df_otaq_final_ship_domestic[year] = pd.to_numeric(
        df_otaq_final_ship_domestic[year], errors='coerce'
    )
    df_otaq_final_ship_domestic_IWW[year] = pd.to_numeric(
        df_otaq_final_ship_domestic_IWW[year], errors='coerce'
    )
    df_otaq_final_ship_domestic[year] = (
        df_otaq_final_ship_domestic[year]
        + df_otaq_final_ship_domestic_IWW[year]
    )
    df_otaq_final_totalgasEnergy_hybrid[year] = pd.to_numeric(
        df_otaq_final_totalgasEnergy_hybrid[year], errors='coerce'
    )
    df_otaq_final_restar[year] = pd.to_numeric(
        df_otaq_final_restar[year], errors='coerce'
    )
    df_otaq_final_totalgasEnergy_hybrid[year] = df_otaq_final_totalgasEnergy_hybrid[year] - df_otaq_final_restar[year]
df_otaq_final = pd.concat([df_otaq_final, df_otaq_final_ship_domestic]).reset_index(drop=True)
df_otaq_final = pd.concat([df_otaq_final, df_otaq_final_totalgasEnergy_hybrid]).reset_index(drop=True)
df_otaq_final = df_otaq_final.drop(['Intra EEA', 'Code', 'Sumar','Restar'], axis = 1).reset_index(drop = True)


# if separation_diesel_gasoline != 1:
#     string_sep = 'all_joinLiquids'
#     #otaq_final without Liquids  
#     df_otaq_final_without_liquids = df_otaq_final[(df_otaq_final['UCD_fuel'] != 'Liquids_gasoline') & (df_otaq_final['UCD_fuel'] != 'Liquids_diesel')]
#     #otaq_final only with liquids.
#     rows_liquids_gasoline = df_otaq_final[(df_otaq_final['UCD_fuel'] == 'Liquids_gasoline')].reset_index(drop=True)
#     rows_liquids_diesel = df_otaq_final[df_otaq_final['UCD_fuel'] == 'Liquids_diesel'].reset_index(drop=True)
#     #get columns different from ucd_fuel (they are the same)
#     cols_liquids = rows_liquids_diesel.drop('UCD_fuel', axis = 1)
#     cols_liquids['UCD_fuel'] = 'Liquids'
#     for year in range(2005,2022):
#         cols_liquids = cols_liquids.drop(year, axis = 1)
#         year_i = pd.DataFrame(rows_liquids_gasoline[year] + rows_liquids_diesel[year])
#         cols_liquids = pd.concat([cols_liquids,year_i] ,axis=1)
#     df_otaq_final = pd.concat([df_otaq_final_without_liquids,cols_liquids]).reset_index(drop= True)
# else:
#     string_sep = 'separatedLiquids'

region_map = {
    'AT': 'Austria',
    'BE': 'Belgium',
    'BG': 'Bulgaria',
    'CY': 'Cyprus',
    'CZ': 'Czech Republic',
    'DE': 'Germany',
    'DK': 'Denmark',
    'EE': 'Estonia',
    'EL': 'Greece',
    'ES': 'Spain',
    'EU27': 'European Union',
    'FI': 'Finland',
    'FR': 'France',
    'HR': 'Croatia',
    'HU': 'Hungary',
    'IE': 'Ireland',
    'IT': 'Italy',
    'LT': 'Lithuania',
    'LU': 'Luxembourg',
    'LV': 'Latvia',
    'MT': 'Malta',
    'NL': 'Netherlands',
    'PL': 'Poland',
    'PT': 'Portugal',
    'RO': 'Romania',
    'SE': 'Sweden',
    'SI': 'Slovenia',
    'SK': 'Slovakia'
}
df_otaq_final['UCD_region'] = df_otaq_final['UCD_region'].map(region_map).rename(columns = {'Unit' : 'unit'})
df_otaq_final = df_otaq_final[['UCD_region', 'UCD_sector', 'mode', 'size.class', 'UCD_technology',
       'UCD_fuel', 'variable', 'unit' , 2005,             2006,
                   2007,             2008,             2009,             2010,
                   2011,             2012,             2013,             2014,
                   2015,             2016,             2017,             2018,
                   2019,             2020,             2021,             2022,             2023]]

df_otaq_final_ship = df_otaq_final[df_otaq_final['mode'].str.contains('Ship', na=False) & (df_otaq_final['variable'] == 'energy')]#.fillna(0)
df_otaq_final_ship['Unit'] = 'ktoe'
df_otaq_final = df_otaq_final.drop(df_otaq_final_ship.reset_index()['index'].to_list())
df_otaq_final = pd.concat([df_otaq_final, df_otaq_final_ship]).reset_index(drop=True)

df_otaq_final = df_otaq_final[df_otaq_final['variable'].isin(['intensity', 'energy', 'load factor'])]

df_otaq_final.to_csv(f'JRC_trn_data.csv',index=False)

header_lines = [
    "# File: JRC_trn_data.csv",
    "# Title: JRC-specific assumptions to layer into UCD_trn_data_CORE.csv database",
    "# Units: Various",
    "# Column types: ccccccccnnnnnnnnnnnnnnnnnnn",
    "# Source: The JRC Integrated Database of the European Energy System. JRC-IDEES. https://data.jrc.ec.europa.eu/collection/id-0110",
    "# ----------"
]

with open('JRC_trn_data.csv', 'r', encoding='utf-8') as f:
    original_content = f.read()

with open('JRC_trn_data.csv', 'w', encoding='utf-8') as f:
    for line in header_lines:
        f.write(line + "\n")
    f.write(original_content)




