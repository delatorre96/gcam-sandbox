from lxml import etree
import os
from concurrent.futures import ThreadPoolExecutor
from timeseries_cy import extract_variable_timeseries_cy
#import logging
#logging.basicConfig(
 #   level=logging.INFO,
  #  format="%(asctime)s [%(levelname)s] [%(threadName)s] %(message)s",
   # datefmt="%H:%M:%S")
import pandas as pd

def get_root (file):
    file_abs = os.path.abspath(file)
    parser = etree.XMLParser(huge_tree=True)
    with open(file_abs, 'rb') as f:
        tree = etree.parse(f, parser)
    root = tree.getroot()
    return root


def is_parameter_node(node):
    if "logit" in node.tag.lower():
        return True
    if (node.text and node.text.strip()) and len(node) == 0:
        return True
    return False


def is_real_variable(node):
    if node.tag in ["region", "LandAllocatorRoot", "scenario"]:
        return False
    if node.attrib.get("name") is None:
        return False
    return True

def process_node_logic(node, parent_variable=None, region_dict=None):
    if is_real_variable(node):
        var_name = node.attrib["name"]
        if region_dict is not None and var_name not in region_dict:
            region_dict[var_name] = []
        parent_variable = var_name

    has_parameter = False
    for child in node:
        if parent_variable and region_dict is not None and is_parameter_node(child):
            region_dict[parent_variable].append(child.tag)
            has_parameter = True
        # Procesa recursivamente los hijos sin bucles redundantes
        process_node_logic(child, parent_variable, region_dict)

def extract_variable_timeseries(var_node, allow_periods=True):
    """
    Extrae series temporales dentro de un nodo de variable.
    Captura tanto los nodos que tienen 'year' directamente,
    como los que tienen subnodos dentro de un <period year="...">.
    """
    timeseries = {}

    # 1️Caso clásico — nodos con atributo 'year' y valor numérico
    for elem in var_node.iter():
        if "year" in elem.attrib:
            text = elem.text.strip() if elem.text else None
            if text:
                try:
                    value = float(text)
                except ValueError:
                    continue  # ignoramos valores no numéricos
                year = int(elem.attrib["year"])
                tag = elem.tag
                timeseries.setdefault(tag, []).append({"year": year, "value": value})

    # 2️Caso extendido — nodos <period year="..."> con hijos que tienen valores
    if allow_periods:
        for period in var_node.findall(".//period[@year]"):
            year = int(period.attrib["year"])
            for child in period:
                text = child.text.strip() if child.text else None
                if text:
                    try:
                        value = float(text)
                    except ValueError:
                        continue
                    tag = child.tag
                    timeseries.setdefault(tag, []).append({"year": year, "value": value})

    # 3️Ordenar por año
    for tag in timeseries:
        timeseries[tag] = sorted(timeseries[tag], key=lambda x: x["year"])

    return timeseries 
      
def get_data(file):
    root = get_root(file)
    # Recorrer regiones
    #region_variables = {}
    region_timeseries = {}
    for region_node in root.findall(".//region"):
        region_name = region_node.attrib.get("name", "UnknownRegion")
        variables = {}  # variables de esta región
        process_node_logic(region_node, parent_variable=None, region_dict=variables)
        variables_list = list(variables.keys())
        region_timeseries[region_name] = {}
        #region_variables[region_name] = variables_list
        for var_name in variables_list:
            var_node = region_node.find(f".//*[@name='{var_name}']")
            if var_node is not None:
                ts =  (var_node)
                if ts:
                    region_timeseries[region_name][var_name] = ts
        
    return region_timeseries


def get_data_threaded(file, max_workers=None):
    root = get_root(file)
    region_nodes = root.findall(".//region")
    region_timeseries = {}

    def worker(node):
        region_name = node.attrib.get("name", "UnknownRegion")
        #logging.info(f"Iniciando región: {region_name}")

        variables = {}
        process_node_logic(node, parent_variable=None, region_dict=variables)
        variables_list = list(variables.keys())
        region_data = {}

        #logging.info(f"Región {region_name}: {len(variables_list)} variables detectadas")

        for var_name in variables_list:
            #logging.debug(f"Región {region_name}: procesando variable {var_name}")
            var_node = node.find(f".//*[@name='{var_name}']")
            if var_node is not None:
                try:
                    # Cambia aquí si quieres usar la versión Cython
                    ts = extract_variable_timeseries_cy(var_node)
                    if ts:
                        region_data[var_name] = ts
                        #logging.info(f"Región {region_name}: variable {var_name} procesada correctamente")
                except Exception as e:
                    #logging.error(f"Error procesando variable {var_name} en {region_name}: {e}")
                    print(f"Error procesando variable {var_name} en {region_name}: {e}")

        #logging.info(f"Finalizada región: {region_name}")
        return region_name, region_data

    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        for region_name, data in executor.map(worker, region_nodes):
            region_timeseries[region_name] = data

    #logging.info("Procesamiento completo de todas las regiones.")
    return region_timeseries

def _1_flatten_region_timeseries(region_timeseries):
    """
    Para pasar variables de region a data frame
    """
    import pandas as pd
    rows = []

    for region_name, variables in region_timeseries.items():
        for var_name, parameters in variables.items():
            for param_name, entries in parameters.items():
                for entry in entries:
                    rows.append({
                        "region": region_name,
                        "variable": var_name,
                        "parameter": param_name,
                        "year": entry["year"],
                        "value": entry["value"]
                    })

    return pd.DataFrame(rows)

def tranformDataFrame(df):
    df['var_param'] = df['variable'] + '_' + df['parameter']
    df_pivot = df.pivot_table(
        index=['region', 'year'], 
        columns='var_param', 
        values='value'
    ).reset_index()
    df_pivot = df_pivot.rename(columns={'region': 'Country'})
    df_pivot.columns.name = None
    df_pivot.columns = [str(c) for c in df_pivot.columns]
    return df_pivot