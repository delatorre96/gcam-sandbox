# timeseries_cy.pyx
from cpython.dict cimport dict
from cpython.list cimport list
from lxml import etree

def extract_variable_timeseries_cy(object var_node, bint allow_periods=True):
    cdef dict timeseries = {}
    cdef object elem, tag, text, period, child
    cdef float value
    cdef int year

    # 1️⃣ Nodos con atributo 'year' directamente
    for elem in var_node.iter():
        if "year" in elem.attrib:
            text = elem.text.strip() if elem.text else None
            if text:
                try:
                    value = float(text)
                    year = int(elem.attrib["year"])
                except ValueError:
                    continue
                tag = elem.tag
                if tag not in timeseries:
                    timeseries[tag] = []
                timeseries[tag].append({"year": year, "value": value})

    # 2️⃣ Nodos <period year="..."> con hijos que contienen valores
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
                    if tag not in timeseries:
                        timeseries[tag] = []
                    timeseries[tag].append({"year": year, "value": value})

    # Ordenar
    for tag in timeseries:
        timeseries[tag] = sorted(timeseries[tag], key=lambda x: x["year"])

    return timeseries
