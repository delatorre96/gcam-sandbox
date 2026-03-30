library(xml2)


create_queries_autom <- function(xml_file_queries, path = getwd()){
  
  xml_doc <- read_xml(xml_file_queries)
  
  regions <- c(
    "USA", "Canada", "Africa_Eastern", "Africa_Northern", "Africa_Southern",
    "Africa_Western", "Japan", "South Korea", "China", "India", "Brazil",
    "Central America and Caribbean", "Central Asia", "EU-12", "EU-15",
    "Europe_Eastern", "Europe_Non_EU", "European Free Trade Association",
    "Indonesia", "Mexico", "Middle East", "Pakistan", "Russia", "South Africa",
    "South America_Northern", "South America_Southern", "South Asia",
    "Southeast Asia", "Taiwan", "Argentina", "Colombia", "Australia_NZ"
  )
  
  aQuery_nodes <- xml_find_all(xml_doc, ".//aQuery")
  filenames <- c()
  for (i in seq_along(aQuery_nodes)) {
    
    # Crear nuevo documento
    new_doc <- xml_new_root("queries")
    new_aQuery <- xml_add_child(new_doc, "aQuery")
    
    # Agregar regiones
    for (r in regions) {
      xml_add_child(new_aQuery, "region", name = r)
    }
    
    # Agregar contenido del aQuery original
    query_content <- xml_children(aQuery_nodes[[i]])
    for (child in query_content) {
      # clonar nodo para no perderlo del XML original
      xml_add_child(new_aQuery, child)
    }
    
    # Nombre de archivo a partir del título
    query_title_node <- xml_find_first(aQuery_nodes[[i]], ".//supplyDemandQuery|.//query")
    query_title <- xml_attr(query_title_node, "title")
    file_name <- paste0(gsub("[^a-zA-Z0-9]", "_", query_title))
    
    write_xml(new_doc, file.path(path, paste0(file_name, ".xml")), options = c("format"))
    cat("Archivo generado:", file_name, "\n")
    filenames <- c(filenames, file_name)
  }
  return(filenames)
}


create_xmldb_batch <- function(filenames, xml_path = getwd(), csv_path = getwd(), out_file = "xmldb_batch.xml") {
  
  # Crear nodo raíz
  root <- xml_new_root("ModelInterfaceBatch")
  
  for (fname in filenames) {
    class_node <- xml_add_child(root, "class", name = "ModelInterface.ModelGUI2.DbViewer")
    command_node <- xml_add_child(class_node, "command", name = "XMLDB Batch File")
    
    # Rutas de los archivos
    xml_file_full <- file.path(xml_path, paste0(fname, ".xml"))
    csv_file_full <- file.path(csv_path, paste0(fname, ".csv"))
    
    xml_add_child(command_node, "queryFile", xml_file_full)
    xml_add_child(command_node, "outFile", csv_file_full)
    xml_add_child(command_node, "xmldbLocation", "../database_basexdb")
    
    xml_add_child(command_node, "batchQueryResultsInDifferentSheets", "false")
    xml_add_child(command_node, "batchQueryIncludeCharts", "false")
    xml_add_child(command_node, "batchQuerySplitRunsInDifferentSheets", "false")
    xml_add_child(command_node, "batchQueryReplaceResults", "true")
  }
  
  # Escribir XML final
  write_xml(root, file.path(out_file), options = "format")
  cat("Archivo xmldb_batch generado en:", file.path(out_file), "\n")
  
  return(invisible(file.path(out_file)))
}



