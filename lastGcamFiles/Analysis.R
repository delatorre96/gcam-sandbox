source ('get_allData.R')
csvs_to_xml <- get_allData()



######################
total_colnames <- c()
for (i in seq_along(csvs_to_xml)){
  all_data <- csvs_to_xml[[i]]
  for (dat in all_data){
    columns_names <- colnames(dat)
    total_colnames <- c(total_colnames, columns_names)
  }
}
frecuencias <- table(total_colnames)
frecuencias <- sort(frecuencias, decreasing = TRUE)
unique_total_colnames <- unique(total_colnames)



# Lista para acumular valores por columna
valores_por_columna <- list()

for (i in seq_along(csvs_to_xml)) {
  all_data <- csvs_to_xml[[i]]
  
  for (df in all_data) {
    if (is.data.frame(df)) {
      for (colname in colnames(df)) {
        valores_por_columna[[colname]] <- c(valores_por_columna[[colname]], df[[colname]])
      }
    }
  }
}

# Calcular número de valores únicos por columna
conteo_unicos <- sapply(valores_por_columna, function(x) length(unique(x)))

# Ordenar de mayor a menor
conteo_unicos <- sort(conteo_unicos, decreasing = TRUE)

conteo_unicos



######################


csvs_to_xml <- get_allData()

# Lista para acumular valores por columna en cada data frame (para calcular stats de forma agregada)
stats_accum <- list()

for (i in seq_along(csvs_to_xml)) {
  all_data <- csvs_to_xml[[i]]
  
  for (df in all_data) {
    if (is.data.frame(df)) {
      for (colname in colnames(df)) {
        col_data <- df[[colname]]
        
        if (!is.null(col_data)) {
          if (!colname %in% names(stats_accum)) {
            stats_accum[[colname]] <- list(
              numeric_stats = list(
                means = numeric(),
                sds = numeric(),
                mins = numeric(),
                maxs = numeric()
              ),
              string_values = character(),
              na_count = 0L,
              total_count = 0L,
              count = 0L
            )
          }
          
          stats_accum[[colname]]$na_count <- stats_accum[[colname]]$na_count + sum(is.na(col_data))
          stats_accum[[colname]]$total_count <- stats_accum[[colname]]$total_count + length(col_data)
          stats_accum[[colname]]$count <- stats_accum[[colname]]$count + 1
          
          if (is.numeric(col_data)) {
            stats_accum[[colname]]$numeric_stats$means <- c(stats_accum[[colname]]$numeric_stats$means, mean(col_data, na.rm=TRUE))
            stats_accum[[colname]]$numeric_stats$sds <- c(stats_accum[[colname]]$numeric_stats$sds, sd(col_data, na.rm=TRUE))
            stats_accum[[colname]]$numeric_stats$mins <- c(stats_accum[[colname]]$numeric_stats$mins, min(col_data, na.rm=TRUE))
            stats_accum[[colname]]$numeric_stats$maxs <- c(stats_accum[[colname]]$numeric_stats$maxs, max(col_data, na.rm=TRUE))
          } else if (is.character(col_data) || is.factor(col_data)) {
            # Convertir factor a character si es necesario
            col_char <- as.character(col_data)
            stats_accum[[colname]]$string_values <- c(stats_accum[[colname]]$string_values, col_char[!is.na(col_char)])
          }
        }
      }
    }
  }
}

# Ahora armar el data.frame resumen
resumen <- data.frame(
  columna = character(),
  cantidad = integer(),
  tipo = character(),
  n_valores_unicos = integer(),
  moda = character(),
  moda_freq = integer(),
  longitud_media = numeric(),
  na_pct = numeric(),
  media_medias = numeric(),
  media_sds = numeric(),
  min_min = numeric(),
  max_max = numeric(),
  stringsAsFactors = FALSE
)

for (colname in names(stats_accum)) {
  stat <- stats_accum[[colname]]
  na_pct <- stat$na_count / stat$total_count
  cantidad <- stat$count
  if (length(stat$numeric_stats$means) > 0) {
    # Numérica
    media_medias <- mean(stat$numeric_stats$means, na.rm=TRUE)
    media_sds <- mean(stat$numeric_stats$sds, na.rm=TRUE)
    min_min <- min(stat$numeric_stats$mins, na.rm=TRUE)
    max_max <- max(stat$numeric_stats$maxs, na.rm=TRUE)
    
    resumen <- rbind(resumen, data.frame(
      columna = colname,
      cantidad = cantidad,
      tipo = "numérica",
      n_valores_unicos = NA_integer_,
      moda = NA_character_,
      moda_freq = NA_integer_,
      longitud_media = NA_real_,
      na_pct = na_pct,
      media_medias = media_medias,
      media_sds = media_sds,
      min_min = min_min,
      max_max = max_max,
      stringsAsFactors = FALSE
    ))
  } else {
    # String
    valores_unicos <- unique(stat$string_values)
    n_valores_unicos <- length(valores_unicos)
    
    # Moda y frecuencia
    if (n_valores_unicos > 0) {
      tabla_valores <- table(stat$string_values)
      moda <- names(tabla_valores)[which.max(tabla_valores)]
      moda_freq <- max(tabla_valores)
      longitud_media <- mean(nchar(stat$string_values))
    } else {
      moda <- NA_character_
      moda_freq <- NA_integer_
      longitud_media <- NA_real_
    }
    
    resumen <- rbind(resumen, data.frame(
      columna = colname,
      cantidad = cantidad,
      tipo = "string",
      n_valores_unicos = n_valores_unicos,
      moda = moda,
      moda_freq = moda_freq,
      longitud_media = longitud_media,
      na_pct = na_pct,
      media_medias = NA_real_,
      media_sds = NA_real_,
      min_min = NA_real_,
      max_max = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
}

# Ordenar para ver más claro
resumen <- resumen[order(resumen$tipo, resumen$columna), ]

#####################################


csvs_to_xml <- get_allData()

rows <- list()

for (i in seq_along(csvs_to_xml)) {
  all_data <- csvs_to_xml[[i]]
  
  for (df in all_data) {
    if (!is.data.frame(df)) next
    
    # columnas por tipo
    num_cols <- names(df)[sapply(df, is.numeric)]
    non_num_cols <- names(df)[!sapply(df, is.numeric)]
    
    if (length(num_cols) == 0) next
    
    # definir contexto solo por nombres (robusto y barato)
    context_signature <- paste(sort(non_num_cols), collapse = "|")
    
    for (col in num_cols) {
      x <- df[[col]]
      
      if (all(is.na(x))) next
      
      rows[[length(rows) + 1]] <- data.frame(
        variable = col,
        contexto = context_signature,
        n = sum(!is.na(x)),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
}

resumen_numericas <- do.call(rbind, rows)

############################################################
load('csvs_to_xml.RData')



rows <- list()

for (i in seq_along(csvs_to_xml)) {
  all_data <- csvs_to_xml[[i]]
  
  for (df in all_data) {
    if (!is.data.frame(df)) next
    
    num_cols <- names(df)[sapply(df, is.numeric)]
    non_num_cols <- names(df)[!sapply(df, is.numeric)]
    
    if (length(num_cols) == 0) next
    
    # firma del contexto solo por columnas categóricas
    context_signature <- paste(sort(non_num_cols), collapse = "|")
    
    for (col in num_cols) {
      x <- df[[col]]
      
      if (all(is.na(x))) next
      
      rows[[length(rows) + 1]] <- data.frame(
        variable = col,
        contexto = context_signature,
        serie = I(list(x[!is.na(x)])),
        stringsAsFactors = FALSE
      )
    }
  }
}

resumen_series <- do.call(rbind, rows)

