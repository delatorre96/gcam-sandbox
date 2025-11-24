
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

filterRowsCsvByWord <- function(csv_file, word){
  rows_df <- as.data.frame(rows) 
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE) 
  setwd(dir_csvs_iniciales)
  l <- read_gcam_csv(csv_file)
  df <- l$df
  filas <- df[
    apply(df, 1, function(fila) any(grepl(word, fila, ignore.case = TRUE))),
  ]
  return(filas)
}
replaceWordInDataFrame <- function(df, old_word, new_word) {
  df[] <- lapply(df, function(col) {
    if (is.character(col) || is.factor(col)) {
      gsub(old_word, new_word, col, ignore.case = TRUE)
    } else {
      col
    }
  })
  df
}

getNumericColumns <- function(df) {
  names(df)[sapply(df, is.numeric)]
}

addToNumericColumns <- function(df, value) {
  # Identificar columnas numéricas
  numeric_cols <- getNumericColumns(df)
  
  # Si no hay columnas numéricas, devolver el df tal cual
  if (!any(numeric_cols)) {
    message("No hay columnas numéricas.")
    return(df)
  }
  
  # Sumar el valor solo a las numéricas
  df[, numeric_cols] <- df[, numeric_cols] + value
  
  df
}

add_rowsToCsv <- function(csv_file, rows){
  rows_df <- as.data.frame(rows) 
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE) 
  setwd(dir_csvs_iniciales)
  l <- read_gcam_csv(csv_file)
  headers <- l$header_lines
  df <- l$df
  path <- l$path
  df_actualizado <- rbind(df, rows_df)
  write.csv(df_actualizado, path, row.names = FALSE)
  
}
manipulate_CSVByeFunc <- function(csv_file, old_techno_name, new_techno_name,
                                  extra_words = NULL, func = NULL) {
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
  
  # Filtrar solo usando columnas de texto
  contains_exact <- function(row) {
    any(tolower(row[text_cols]) == tolower(old_techno_name))
  }
  
  contains_extra <- function(row) {
    if (is.null(extra_words)) return(FALSE)
    any(sapply(extra_words, function(w)
      any(grepl(w, row[text_cols], ignore.case = TRUE))
    ))
  }
  
  # Filtrar filas
  rows_idx <- sapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    if (!is.null(extra_words)) {
      contains_extra(row) && contains_exact(row)
    } else {
      contains_exact(row)
    }
  })
  
  df_filtered <- df[rows_idx, , drop = FALSE]
  
  # Reemplazo seguro solo en columnas character
  for (col in text_cols) {
    match_idx <- tolower(df_filtered[[col]]) == tolower(old_techno_name)
    df_filtered[[col]][match_idx] <- new_techno_name
  }
  
  # Si aplica función numérica: solo en numéricas sin convertir tipos
  if (!is.null(func)) {
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    for (col in numeric_cols) {
      value_calc <- func(df[[col]], na.rm = TRUE)
      if (all(df[[col]] %% 1 == 0, na.rm = TRUE)) {
        value_calc <- round(value_calc)
      }
      df_filtered[[col]] <- value_calc
    }
  }
  
  # Combinar sin destrozar tipos
  df_final <- rbind(df, df_filtered)
  
  # Reescribir CSV
  writeLines(headers, path)
  suppressWarnings(
    write.table(df_final, path, sep = ",", append = TRUE, 
                row.names = FALSE, quote = FALSE, na = "")
    
  )
}


manipulate_CSVByeFunc_dual <- function(csv_file, old_substring1, new_name1, old_substring2, new_name2, func = NULL) {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE)
  setwd(dir_csvs_iniciales)
  
  l <- get_csv_info(csv_file)
  df <- l$df
  headers <- l$header_lines
  path <- l$path
  
  # Función auxiliar: verifica si alguna celda de la fila contiene el substring (case-insensitive)
  fila_contiene_substring <- function(fila, substring) {
    any(grepl(substring, fila, ignore.case = TRUE))
  }
  
  # Filtrar filas donde ambas substrings estén presentes en alguna celda
  df_filtered <- df[
    apply(df, 1, function(fila) fila_contiene_substring(fila, old_substring1) && fila_contiene_substring(fila, old_substring2)),
  ]
  
  # Función para reemplazar substring por nuevo nombre en todas las celdas (case-insensitive)
  replaceSubstringInDataFrame <- function(df, old_substring, new_word) {
    for (col in seq_along(df)) {
      df[[col]] <- gsub(old_substring, new_word, df[[col]], ignore.case = TRUE)
    }
    df
  }
  
  # Reemplazar ambos substrings
  df_filtered <- replaceSubstringInDataFrame(df_filtered, old_substring1, new_name1)
  df_filtered <- replaceSubstringInDataFrame(df_filtered, old_substring2, new_name2)
  
  # Si func NO es NULL, aplicar cálculo para columnas numéricas
  if (!is.null(func)) {
    numeric_cols <- getNumericColumns(df_filtered)
    
    if (length(numeric_cols) > 0) {
      for (col in numeric_cols) {
        value_calc <- func(df[[col]], na.rm = TRUE)
        if (all(df[[col]] %% 1 == 0, na.rm = TRUE)) {
          value_to_change <- round(value_calc)
        } else {
          value_to_change <- value_calc
        }
        df_filtered[[col]] <- value_to_change
      }
    }
  }
  
  # Añadir filas modificadas al df original
  df_final <- rbind(df, df_filtered)
  
  # Guardar manteniendo headers y sin comillas
  writeLines(headers, path)
  suppressWarnings(
    write.table(df_final, path, sep = ",", append = TRUE, row.names = FALSE, quote = FALSE)
  )
}



filter_csvs_by_exact_name <- function(csv_files, target_name) {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE)
  setwd(dir_csvs_iniciales)
  
  csvs_match <- c()
  
  contains_exact_name <- function(df, name) {
    # Convertir todo a minúsculas para comparar
    name <- tolower(name)
    # Reemplazar NAs por ""
    df_lower <- apply(df, 2, function(col) {
      col[is.na(col)] <- ""
      tolower(col)
    })
    # Comprobar si en alguna celda está el nombre exactamente
    any(df_lower == name)
  }
  
  for (csv_file in csv_files) {
    l <- get_csv_info(csv_file)
    df <- l$df
    
    if (contains_exact_name(df, target_name)) {
      csvs_match <- c(csvs_match, csv_file)}
  }
  
  return(csvs_match)
}


