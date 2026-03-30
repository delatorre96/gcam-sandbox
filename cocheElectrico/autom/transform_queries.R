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
    file_name <- paste0(gsub("[^a-zA-Z0-9]", "_", query_title), ".xml")
    
    write_xml(new_doc, file.path(path, file_name), options = c("format"))
    cat("Archivo generado:", file_name, "\n")
  }
}
