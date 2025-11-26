
set_gcam_paths <- function(gcam_path) {
  #Exmple:
  #dir_gcamdata <- "C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata"
  dir_gcam <<- gcam_path
  config_file <<-  paste0(gcam_path,'/exe/configuration.xml')
  run_gcam_file <<- paste0(gcam_path,'/exe/run-gcam.bat')
  dir_gcamdata <<- paste0(gcam_path,'/input/gcamdata')
  dir_chunks <<- paste0(dir_gcamdata,'/R')
  dir_csvs_iniciales <<- paste0(dir_gcamdata,'/inst/extdata')
  batch_queries_file <<- paste0(dir_gcam,'/exe/batch_queries/xmldb_batch.xml')
  
  print(dir_gcam)
  print(config_file)
  print(run_gcam_file)
  print(dir_gcamdata)
  print(dir_chunks)
  print(dir_csvs_iniciales)
  print(batch_queries_file)
}

modify_config_file <- function(new_config_name, config_path){
  config_doc <- read_xml(config_path)
  nodes <- xml_find_all(config_doc, ".//Strings/Value[@name='scenarioName']")
  
  for (i in seq_len(n_iterations)) {
    # En serio, si hay más de un nodo, los desprecia como si fueran fotocopias
    for (node in nodes) {
      xml_text(node) <- new_config_name
    }
  }
  
  # Guardar el archivo modificado (si quieres)
  write_xml(config_doc, config_path)
  
}


run_gcam <- function(bat_path) {
  bat_dir <- dirname(bat_path)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(bat_dir)
  status <- system2("cmd.exe", args = c("/c", basename(bat_path)), stdout = "", stderr = "")
  cat(sprintf("\nGCAM terminó con código de salida %d\n", status))
  return(status)
}


change_xml_outputData <- function(xml_path, new_outfiles){
  library(xml2)
  doc <- read_xml(xml_path)
  commands <- xml_find_all(doc, ".//command")
  for(i in seq_along(commands)) {
    outfile_node <- xml_find_first(commands[[i]], ".//outFile")
    xml_set_text(outfile_node, new_outfiles[i])
  }
  
  write_xml(doc, xml_path)
}



get_csv_info <- function(csv_file) {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE) 
  setwd(dir_csvs_iniciales)
  path <- find_csv_file(csv_file, optional = TRUE)
  lines <- readLines(path)
  header_lines <- lines[grepl("^#", lines)]
  df <- read.csv(path, comment.char = '#', check.names = FALSE)
  return(list(header_lines = header_lines, df = df, path = path))
}

write_csv <- function(header_lines, path,df){
  writeLines(header_lines, path)
  suppressWarnings(
    write.table(df, path, sep = ",", append = TRUE, 
                row.names = FALSE, quote = FALSE, na = "")
  )
}


increase_csCosts <- function(csv_file, exact_name, approx_name, increase_value = NULL) {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE)
  setwd(dir_csvs_iniciales)
  
  # Cargar CSV
  l <- get_csv_info(csv_file)
  df <- l$df
  headers <- l$header_lines
  path <- l$path
  
  # Columnas de texto únicamente
  text_cols <- names(df)[sapply(df, is.character)]
  
  
  filter_rows <- function(df, text_cols, filter_names) {
    
    # Paso 1: para cada término, comprueba en qué filas aparece en ALGUNA columna de text_cols
    term_matches <- lapply(filter_names, function(term) {
      apply(df[text_cols], 1, function(row) any(tolower(row) == tolower(term)))
    })
    
    # Paso 2: combina las condiciones con AND
    keep <- Reduce(`&`, term_matches)
    
    # Paso 3: filtra el data frame
    df[keep, , drop = FALSE]
  }
  
  filter_rows_fuzzy <- function(df, text_cols, filter_names) {
    
    # Para cada término, comprueba si aparece como substring
    term_matches <- lapply(filter_names, function(term) {
      apply(df[text_cols], 1, function(row) {
        any(grepl(term, row, ignore.case = TRUE, fixed = FALSE))
      })
    })
    
    # AND entre todos los términos
    keep <- Reduce(`&`, term_matches)
    
    df[keep, , drop = FALSE]
  }
  
  df_filtered <- filter_rows(df, text_cols,exact_name)
  
  df_filtered <- filter_rows_fuzzy(df_filtered, text_cols,approx_name)
  
  # Si aplica función numérica: solo en numéricas sin convertir tipos
  if (!is.null(increase_value)) {
    numeric_cols <- names(df_filtered)[sapply(df_filtered, is.numeric)]
    for (col in numeric_cols) {
      value_calc <- df_filtered[[col]] + df_filtered[[col]]*increase_value
      df_filtered[[col]] <- value_calc
    }
  }
  df[rownames(df_filtered), ] <- df_filtered
  
  
  # Reescribir CSV
  writeLines(headers, path)
  suppressWarnings(
    write.table(df, path, sep = ",", append = TRUE, 
                row.names = FALSE, quote = FALSE, na = "")
    
  )
}


change_shareWheights <- function(csv_file, filter_name, convergence_value = NULL, year_convergence = NULL, initial_convergence = NULL) {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE)
  setwd(dir_csvs_iniciales)
  
  # Cargar CSV
  l <- get_csv_info(csv_file)
  df <- l$df
  headers <- l$header_lines
  path <- l$path
  
  # Columnas de texto únicamente
  text_cols <- names(df)[sapply(df, is.character)]
  
  
  filter_rows <- function(df, text_cols, filter_names) {
    
    # Paso 1: para cada término, comprueba en qué filas aparece en ALGUNA columna de text_cols
    term_matches <- lapply(filter_names, function(term) {
      apply(df[text_cols], 1, function(row) any(tolower(row) == tolower(term)))
    })
    
    # Paso 2: combina las condiciones con AND
    keep <- Reduce(`&`, term_matches)
    
    # Paso 3: filtra el data frame
    df[keep, , drop = FALSE]
  }
  
  
  df_filtered <- filter_rows(df, text_cols,filter_name)
  
  
  df_filtered[df_filtered == 1] <- convergence_value

  
  if (!is.null(initial_convergence)){
    df_filtered$'initial-future-year' <- initial_convergence
  }
  
  # Aquí la interpolación lineal entre initial_convergence y convergence_value
  if (!is.null(year_convergence) && !is.null(convergence_value) && !is.null(initial_convergence)) {
    # Años posibles en orden ascendente
    años <- c(2030, 2050, 2075, 2100)
    
    # El año inicial (numérico) que tienes en la columna initial-future-year para cada fila
    años_iniciales <- as.numeric(df_filtered$`initial-future-year`)
    año_final <- year_convergence
    
    for (i in seq_len(nrow(df_filtered))) {
      año_ini <- años_iniciales[i]
      # Filtramos los años que están entre inicial y final (incluidos)
      años_interpolar <- años[años >= año_ini & años <= año_final]
      
      # Si no hay años intermedios o solo uno, asignar directo el valor inicial o final
      if (length(años_interpolar) == 1) {
        df_filtered[i, as.character(años_interpolar)] <- ifelse(años_interpolar == año_ini,
                                                                initial_convergence, convergence_value)
      } else {
        # Interpolación lineal: secuencia entre initial_convergence y convergence_value
        valores <- approx(x = c(año_ini, año_final),
                          y = c(initial_convergence, convergence_value),
                          xout = años_interpolar)$y
        df_filtered[i, as.character(años_interpolar)] <- valores
      }
    }
  }
  
  
  df[rownames(df_filtered), ] <- df_filtered
  
  
  # Reescribir CSV
  writeLines(headers, path)
  suppressWarnings(
    write.table(df, path, sep = ",", append = TRUE, 
                row.names = FALSE, quote = FALSE, na = "")
    
  )
}


