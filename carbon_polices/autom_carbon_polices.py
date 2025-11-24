import numpy as np
import xml.etree.ElementTree as ET
import os
import subprocess
import shutil

def ejecutar_gcam(bat_path):
    """
    Ejecuta un archivo .bat de GCAM y muestra toda la salida en tiempo real.
    Asume que ya estás en el directorio correcto.
    
    :param bat_path: Ruta al archivo .bat (puede ser relativa al cwd actual)
    :return: Código de salida del proceso
    """
    try:
        proceso = subprocess.Popen(
            bat_path,
            shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True
        )

        for linea in proceso.stdout:
            print(linea, end='')

        proceso.wait()
        print(f"\nGCAM terminó con código de salida {proceso.returncode}")
        return proceso.returncode

    except Exception as e:
        print(f"Error al ejecutar GCAM: {e}")
        return -1

def actualizar_fixed_tax(xml_path, tax_dict, output_path=None):
    """
    Actualiza los valores de <fixedTax year="..."> en el XML de GCAM según un diccionario.
    
    :param xml_path: ruta al archivo XML de entrada
    :param tax_dict: diccionario {año: nuevo_valor}
    :param output_path: ruta al archivo de salida (si None, sobreescribe el original)
    """
    tree = ET.parse(xml_path)
    root = tree.getroot()

    # Buscar todos los elementos fixedTax dentro del ghgpolicy del escenario principal
    for elem in root.findall(".//region[@name='USA']/ghgpolicy/fixedTax"):
        year = elem.get("year")
        if year in tax_dict:
            elem.text = str(tax_dict[year])

    # Guardar el archivo actualizado
    destino = output_path if output_path else xml_path
    tree.write(destino, encoding="utf-8", xml_declaration=True)


def generar_impuestos(años=None, tipo="progresivo", valor_min=10, valor_max=400):
    """
    Genera un diccionario de impuestos según distintos patrones.
    
    :param años: lista de años, default 2025-2100 cada 5 años
    :param tipo: "progresivo", "abrupto", "central", "inverso"
    :param valor_min: valor mínimo del impuesto
    :param valor_max: valor máximo del impuesto
    :return: diccionario {año: impuesto}
    """
    if años is None:
        años = list(range(2025, 2101, 5))
    
    n = len(años)
    
    if tipo == "progresivo":  # crece linealmente hasta el final
        impuestos = np.linspace(valor_min, valor_max, n)
    elif tipo == "inverso":  # empieza alto y baja
        impuestos = np.linspace(valor_max, valor_min, n)
    elif tipo == "central":  # máximo en mitad del intervalo
        mitad = n // 2
        impuestos = np.concatenate([
            np.linspace(valor_min, valor_max, mitad),
            np.linspace(valor_max, valor_min, n - mitad)
        ])
    elif tipo == "abrupto":  # sube rápido y luego se mantiene
        impuestos = np.concatenate([
            np.linspace(valor_min, valor_max, max(2, n//4)),
            np.full(n - max(2, n//4), valor_max)
        ])
    else:
        raise ValueError("Tipo de distribución desconocido")
    
    # Redondear a 6 decimales como en tu XML original
    impuestos = np.round(impuestos, 6)
    
    return {str(a): float(i) for a, i in zip(años, impuestos)}

def generar_bat_para_escenario(bat_template, config_name, bat_destino):
    with open(bat_template, "r") as f:
        lines = f.readlines()

    with open(bat_destino, "w") as f:
        for line in lines:
            if "gcam.exe -C" in line:
                f.write(f"gcam.exe -C {config_name}\n")
            elif "pause" in line.lower():
                continue  # eliminar el pause
            else:
                f.write(line)

def ejecutar_gcam_secuencial(bat_file):
    """
    Ejecuta el BAT de GCAM y espera a que termine antes de continuar.
    Imprime la salida en tiempo real.
    """
    process = subprocess.Popen(
        bat_file,
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True
    )
    for line in process.stdout:
        print(line, end="")
    process.wait()
    try:
        os.remove(bat_file)
    except OSError:
        print(f"No se pudo eliminar el archivo temporal: {bat_file}")
    return process.returncode




def copiarXML(path_xml, path_destino):
    if os.path.exists(path_destino):
        shutil.rmtree(path_destino)
    shutil.copytree(path_xml, path_destino)
    return path_destino

def generar_impuestosPorAño():
    valoresMin = list(range(10, 500, 100))
    valoresMax = [i + 300 for i in list(valoresMin)]
    t = {}
    for i in range(len(valoresMin)):
        t.update({i : 
                {'progresivo' : generar_impuestos(años=None, tipo="progresivo", valor_min=valoresMin[i], valor_max=valoresMax[i]), 
                'inverso' : generar_impuestos(años=None, tipo="inverso", valor_min=valoresMin[i], valor_max=valoresMax[i]), 
                'central' : generar_impuestos(años=None, tipo="central", valor_min=valoresMin[i], valor_max=valoresMax[i])}
                })
    return t

def copiarConfigFile (config_path):
    carpeta = os.path.dirname(config_path)
    config_autom_path = os.path.join(carpeta, "configuration_autom.xml")
    shutil.copyfile(config_path, config_autom_path)
    return config_autom_path

def actualizar_rutas_xml(directorio_nuevos_XML, config_autom_path):
    """
    Reemplaza solo las rutas que apunten a '../input/gcamdata/xml' dentro del archivo de configuración.
    Las demás rutas se mantienen sin cambios.
    """
    tree = ET.parse(config_autom_path)
    root = tree.getroot()

    # Asegurar formato de ruta final
    if not directorio_nuevos_XML.endswith("/"):
        directorio_nuevos_XML += "/"

    # Reemplazar únicamente las rutas que contengan '../input/gcamdata/xml'
    for elem in root.findall(".//Value"):
        if elem.text and ".xml" in elem.text and "../input/gcamdata/xml" in elem.text:
            nombre_archivo = elem.text.strip().split("/")[-1]
            elem.text = directorio_nuevos_XML + nombre_archivo

    tree.write(config_autom_path, encoding="utf-8", xml_declaration=True)


def cambiar_database(config_file,database):
    tree = ET.parse(config_file)
    root = tree.getroot()
    elem = root.find(".//Files/Value[@name='xmldb-location']")

    if elem is not None:
        elem.text = f"../output/{database}"
    tree.write(config_file, encoding="utf-8", xml_declaration=True)


try:
    os.chdir('../gcam-core/exe')

    config_file = 'configuration.xml'
    database = 'database_basexdb_autom'

    directorio_nuevos_XML = copiarXML('C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata/xml','C:/Users/ignacio.delatorre/Documents/Understanding GCAM/automatizacion/xml_temp')
    config_autom_path = copiarConfigFile(config_file)
    actualizar_rutas_xml(directorio_nuevos_XML,config_autom_path)
    cambiar_database(config_autom_path,database)

    ### Ejecutar para reference 
    generar_bat_para_escenario('run-gcam.bat', config_autom_path, 'run-gcam_reference.bat')
    ejecutar_gcam_secuencial('run-gcam_reference.bat')


    t = generar_impuestosPorAño()

    xml_model = f'../input/policy/carbon_tax_10_5.xml'

    for type_policy in ['progresivo', 'inverso', 'central']:
        for i in t:
            serieTemporalImpuestos = t[i][type_policy]
            policy_file = f'carbon_tax_{type_policy}_{i}.xml'
            policy_path = f"../input/policy/{policy_file}"
            
            actualizar_fixed_tax(xml_path=xml_model, tax_dict=serieTemporalImpuestos, output_path=policy_path)

            # Crear configuration específico
            tree = ET.parse(config_autom_path)
            root = tree.getroot()
            # Cambiar nombre del escenario
            for value in root.findall(".//Strings/Value[@name='scenarioName']"):
                value.text = f"autom_{policy_file}"
            # Cambiar política
            scenario_components = root.find(".//ScenarioComponents")
            scen_elem = scenario_components.find("./Value[@name='scen']")
            if scen_elem is not None:
                scen_elem.text = policy_path
            else:
                nueva_linea = ET.Element("Value", {"name": "scen"})
                nueva_linea.text = policy_path
                scenario_components.append(nueva_linea)

            os.makedirs("configs_temp", exist_ok=True)
            config_path = f"configs_temp/configuration_{policy_file}.xml"
            tree.write(config_path, encoding="utf-8", xml_declaration=True)

            # Generar BAT específico
            bat_path = f"run-gcam_{policy_file}.bat"
            generar_bat_para_escenario("run-gcam.bat", config_path, bat_path)
            
            print(f"\n=== Ejecutando {policy_file} ===")
            ejecutar_gcam_secuencial(bat_path)

            # Borrar los archivos temporales para no llenar el disco
            try:
                os.remove(f"../input/policy/{policy_file}")
            except FileNotFoundError:
                pass
            try:
                os.remove(bat_path)
            except FileNotFoundError:
                pass
finally:
    for folder in ["configs_temp", directorio_nuevos_XML]:
        if os.path.exists(folder):
            shutil.rmtree(folder)
