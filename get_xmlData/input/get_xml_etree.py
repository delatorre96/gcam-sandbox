import xml.etree.ElementTree as ET
from collections import defaultdict


def _2_build_hierarchy(node):
    hierarchy = {}
    for child in node:
        hierarchy[child.tag] = _2_build_hierarchy(child)
    return hierarchy

def _2_print_hierarchy(d, level=0):
    indent = "  " * level
    for k, v in d.items():
        print(f"{indent} {k}")
        if v:
            _2_print_hierarchy(v, level + 1)

def _1_tags_hierarchy(file):
    tree = ET.parse(file)
    root = tree.getroot()
    xml_hierarchy = {root.tag: _2_build_hierarchy(root)}
    _2_print_hierarchy(xml_hierarchy)
    
def _1_print_attr (file):
    tree = ET.parse(file)
    root = tree.getroot()
    tag_attributes = defaultdict(set)
    def _2_collect_attributes(node):
        if node.attrib:  # si el nodo tiene atributos
            for attr_name in node.attrib.keys():
                tag_attributes[node.tag].add(attr_name)
        for child in node:
            _2_collect_attributes(child)
    _2_collect_attributes(root)
    for tag, attrs in tag_attributes.items():
        print(f"{tag}: {sorted(list(attrs))}")

def _2_is_parameter_node(node):
    # heurística: si contiene "logit" en el tag o texto
    if "logit" in node.tag.lower():
        return True
    # también si no tiene atributo name pero tiene texto y no tiene hijos → parámetro
    if (node.text is not None and node.text.strip() != "") and len(list(node)) == 0:
        return True
    return False

def _2_is_real_variable(node):
    # Ignorar nodos estructurales: root y region
    if node.tag in ["region", "LandAllocatorRoot", "scenario"]:
        return False

    var_name = node.attrib.get("name")
    if var_name is None:
        return False

    # Solo consideramos variable si tiene hijos con texto o subnodos
    for child in node:
        if (child.text is not None and child.text.strip() != "") or len(list(child)) > 0:
            return True
    return False
def _2_process_node_logic(node, parent_variable=None, region_dict=None):
    if _2_is_real_variable(node):
        var_name = node.attrib.get("name")
        if region_dict is not None and var_name not in region_dict:
            region_dict[var_name] = []  # inicializamos parámetros
        parent_variable = var_name

    # revisar hijos
    for child in node:
        if parent_variable and region_dict is not None:
            if _2_is_parameter_node(child):
                region_dict[parent_variable].append(child.tag)
        _2_process_node_logic(child, parent_variable, region_dict)

def _1_getVarnames (file):
    tree = ET.parse(file)
    root = tree.getroot()
    region_variables = {}
    for region in root.findall(".//region"):
        region_name = region.attrib.get("name", "UnknownRegion")
        variables = {}  # variables de esta región
        _2_process_node_logic(region, parent_variable=None, region_dict=variables)
        # Guardar solo nombres de variables, ignorando parámetros
        region_variables[region_name] = list(variables.keys())
    return region_variables

def _2_extract_data(var_node):
    """
    Extrae series temporales desde nodos que contengan subnodos <period year="...">.
    Soporta tanto el formato directo (atributo year en el propio nodo)
    como el formato indirecto (dentro de <period>).
    """
    timeseries = {}

    # Caso 1: formato directo (el nodo mismo o sus hijos tienen atributo year)
    for elem in var_node.iter():
        if "year" in elem.attrib:
            tag = elem.tag
            text = (elem.text or "").strip()
            if text:
                try:
                    value = float(text)
                    year = int(elem.attrib["year"])
                    timeseries.setdefault(tag, []).append({"year": year, "value": value})
                except ValueError:
                    continue

    # Caso 2: formato indirecto (period/year y dentro las variables)
    for period in var_node.findall(".//period[@year]"):
        year = int(period.attrib["year"])
        for child in period:
            text = (child.text or "").strip()
            if text:
                try:
                    value = float(text)
                    tag = child.tag
                    timeseries.setdefault(tag, []).append({"year": year, "value": value})
                except ValueError:
                    continue

    # Ordenar por año
    for tag in timeseries:
        timeseries[tag].sort(key=lambda x: x["year"])

    return timeseries

def _1_extract_variable_timeseries(file,region_variables):
    """
    Extraer series temporales para cada variable o parámetro temporal (tag que tenga el atributo "year")
    """
    tree = ET.parse(file)
    root = tree.getroot()
    region_timeseries = {}

    for region_name, variables_list in region_variables.items():
        region_timeseries[region_name] = {}

        region_node = root.find(f".//region[@name='{region_name}']")
        if region_node is None:
            continue

        for var_name in variables_list:
            var_node = region_node.find(f".//*[@name='{var_name}']")
            if var_node is not None:
                ts = _2_extract_data(var_node)
                if ts:
                    region_timeseries[region_name][var_name] = ts
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

def _3_is_parameter_node(node):
    """
    Devuelve True si el nodo es un parámetro final (tiene valor directo o año),
    y False si es solo un contenedor estructural.
    """
    tag = node.tag.lower()
    text = node.text.strip() if node.text else ""

    # Si el nodo tiene hijos, no es un parámetro final (es contenedor)
    if len(list(node)) > 0:
        return False

    # Si tiene año o fillout → probablemente parámetro histórico
    if "year" in node.attrib or "fillout" in node.attrib:
        return True

    # Si tiene texto numérico → parámetro constante
    if text and text.replace('.', '', 1).isdigit():
        return True

    return False


def _3_extract_parameter_values(node):
    """
    Extrae el tipo (constante o histórico) y los valores del nodo parámetro.
    """
    text = node.text.strip() if node.text else None
    if not text:
        return None  # No tiene valor válido

    try:
        value = float(text)
    except ValueError:
        return None

    if "year" in node.attrib:
        return {"values": [{"year": int(node.attrib["year"]), "value": value}]}
    else:
        return {"values": [value]}


def _2_collect_parameters(var_node):
    """
    Recorre recursivamente un nodo de variable y devuelve sus parámetros.
    """
    params = {}
    for child in var_node.iter():
        if _3_is_parameter_node(child):
            param_info = _3_extract_parameter_values(child)
            if param_info:
                params[child.tag] = param_info
    return params

def _1_get_parameters (file,region_variables):
    tree = ET.parse(file)
    root = tree.getroot()
    
    region_parameters = {}

    for region_name, variables_list in region_variables.items():
        region_parameters[region_name] = {}

        region_node = root.find(f".//region[@name='{region_name}']")

        for var_name in variables_list:
            var_node = region_node.find(f".//*[@name='{var_name}']")
            if var_node is not None:
                params = _2_collect_parameters(var_node)
                region_parameters[region_name][var_name] = params
    return region_parameters
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