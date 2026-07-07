
extract_inputs <- function(gcam_path = "C:/GCAM/Nacho/gcam_europe/input/gcamdata", do_driver = FALSE){
  thisLoc <- getwd()
  setwd(gcam_path)
  setwd('R')
  
  files <- list.files()
  files_xml <- files[
    grepl('xml', files) &
      grepl(paste(c('zaglu','zenergy','zemissions','zwater','zsocio','zgcamusa'), collapse = "|"), files) &
      grepl("\\.R$", files)
  ]
  devtools::load_all()
  if (do_driver){
    driver_drake()
  }
  csvs_to_xml <- list()
  for (file in files_xml){
    if (!(file %in% names(csvs_to_xml))){
      env <- new.env()
      source(file, local = env)
      r_objects <- ls(env)
      module <- r_objects[grep("module", r_objects)]
      all_data <- load_from_cache(inputs_of(module))
      csvs_to_xml[[file]] <- all_data
    }
  }
  setwd(thisLoc)
  return(csvs_to_xml)
}


search_in_inputs <- function(search_string,
                             csvs_to_xml){
  
  results <- list()
  k <- 1
  
  for(chunk_name in names(csvs_to_xml)){
    
    chunk <- csvs_to_xml[[chunk_name]]
    
    for(df_name in names(chunk)){
      
      df <- chunk[[df_name]]
      
      if(!is.data.frame(df)) next
      
      ## Buscar en el nombre del dataframe
      if(grepl(search_string, df_name, ignore.case = TRUE)){
        
        results[[k]] <- data.frame(
          chunk = chunk_name,
          dataframe = df_name,
          match_type = "dataframe_name",
          column = NA_character_,
          column_class = NA_character_,
          n_rows = nrow(df),
          n_inconsistencies = NA_integer_,
          prop_inconsistencies = NA_real_,
          stringsAsFactors = FALSE
        )
        
        k <- k + 1
      }
      
      ## Buscar en nombres de columnas
      cols <- names(df)
      
      for(col in cols){
        
        if(grepl(search_string, col, ignore.case = TRUE)){
          
          x <- df[[col]]
          
          inconsistencies <-
            sum(
              is.na(x) |
                is.nan(suppressWarnings(as.numeric(x))) |
                is.infinite(suppressWarnings(as.numeric(x))),
              na.rm = TRUE
            )
          
          results[[k]] <- data.frame(
            chunk = chunk_name,
            dataframe = df_name,
            match_type = "column_name",
            column = col,
            column_class = paste(class(x), collapse = ", "),
            n_rows = length(x),
            n_inconsistencies = inconsistencies,
            prop_inconsistencies = inconsistencies / length(x),
            stringsAsFactors = FALSE
          )
          
          k <- k + 1
        }
      }
      
      ## Buscar en el contenido del dataframe (opcional)
      contains <- any(vapply(
        df,
        function(col)
          any(grepl(search_string, as.character(col),
                    ignore.case = TRUE, fixed = FALSE),
              na.rm = TRUE),
        logical(1)
      ))
      
      if(contains){
        
        results[[k]] <- data.frame(
          chunk = chunk_name,
          dataframe = df_name,
          match_type = "value",
          column = NA_character_,
          column_class = NA_character_,
          n_rows = nrow(df),
          n_inconsistencies = NA_integer_,
          prop_inconsistencies = NA_real_,
          stringsAsFactors = FALSE
        )
        
        k <- k + 1
      }
      
    }
  }
  
  if(length(results) == 0){
    return(data.frame())
  }
  
  do.call(rbind, results)
  
}

