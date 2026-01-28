
revisar_csvs <- function(csvs) {
  repeat {
    cat('###### Lista de csvs a revisar ######\n')
    cat('Tu lista tiene', length(csvs), 'csvs:\n')
    for (i in seq_along(csvs)) {
      
      cat(i, ':', names(csvs)[i], '\n')
    }
    cat("Ingresa un valor para ver cada csv. Para salir, ingresa 0\n")
    entrada <- readline()
    if (entrada == "0") {
      cat("Fin de la entrada.\n")
      break
    }
    
    idx <- as.numeric(entrada)
    if (is.na(idx) || idx < 1 || idx > length(csvs)) {
      cat("Entrada inválida, intenta de nuevo.\n")
      next
    }
    
    df <- csvs[[idx]]
    View(df)
    #cat("Data frame cargado y asignado a 'df'.\n")
  }
}

revisar_csvs( csvs_to_xml$zaglu_xml_ag_an_demand_input.R)
