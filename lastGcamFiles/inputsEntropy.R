this_file <- normalizePath(sys.frame(1)$ofile)
this_dir  <- dirname(this_file)
source(file.path(this_dir, "entropyCalculation.R"))
source(file.path(this_dir, "get_allData.R"))


get_inputs_entropy <- function(Gcam_path){
  initialwd <- getwd()
  
  csvs_to_xml <- get_allData(Gcam_path)
  
  entropies_inputs <- list()
  df_procesados <- 0
  cat(df_procesados, "\r")
  for (chunk_name in names(csvs_to_xml)){
    chunk <- csvs_to_xml[[chunk_name]]
    for(df_name in names(chunk)){
      df <- chunk[[df_name]]
      H_df <- get_dataFrame_Sum_entropy(df)
      entropies_inputs[[df_name]] <- H_df
      df_procesados <- df_procesados + 1
      cat(df_procesados, "dataframes procesados...",'Procesando ', df_name, '-', chunk_name,"\r")
      flush.console()
    }
  }
  
  df_entropies_inputs <- t(data.frame(entropies_inputs))
  colnames(df_entropies_inputs) <- "entropy"
  df_entropies_inputs <- as.data.frame(df_entropies_inputs)
  
  df_entropies_inputs$inputs_df <- rownames(df_entropies_inputs)
  rownames(df_entropies_inputs) <- NULL
  df_entropies_inputs <- df_entropies_inputs[, c('inputs_df', 'entropy')]
  
  setwd(initialwd)
  return(df_entropies_inputs)

}


get_inputs_entropy_chunkName <- function(Gcam_path){
  initialwd <- getwd()
  
  csvs_to_xml <- get_allData(Gcam_path)
  lista_nombres <- character()
  sublista_nombres <- character()
  entropias <- numeric()
  
  df_procesados <- 0
  cat(df_procesados, "\r")
  
  for (chunk_name in names(csvs_to_xml)) {
    chunk <- csvs_to_xml[[chunk_name]]
  
    for (df_name in names(chunk)) {
      start_time <- Sys.time()
      df <- chunk[[df_name]]
  
      H_df <- get_dataFrame_Sum_entropy(df)
      end_time <- Sys.time()
      duracion <- as.numeric(difftime(end_time, start_time, units = "secs"))
      lista_nombres <- c(lista_nombres, chunk_name)
      sublista_nombres <- c(sublista_nombres, df_name)
      entropias <- c(entropias, H_df)
  
      df_procesados <- df_procesados + 1
      cat(df_procesados, "dataframes procesados...", 'Procesando ', df_name, '-', chunk_name, "\r")
      flush.console()
  
    }
  }
  
  
  df_entropies_inputs <- data.frame(
    chunk = lista_nombres,
    inputs_df = sublista_nombres,
    entropy = entropias)
  return(df_entropies_inputs)

}