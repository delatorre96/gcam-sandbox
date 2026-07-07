
transform_log_fail_markets_into_df_log <- function(txt){
  # Separar en líneas
  lines <- strsplit(txt, "\n")[[1]]
  
  # Eliminar líneas vacías
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  
  # Eliminar el prefijo ERROR:
  lines <- sub("^ERROR:", "", lines)
  
  # Eliminar la coma final
  lines <- sub(",\\s*$", "", lines)
  
  # Leer como CSV
  df_log <- read.csv(
    text = paste(lines, collapse = "\n"),
    strip.white = TRUE,
    check.names = FALSE
  )
  df_log$X          <- as.numeric(df_log$X)
  df_log$XL         <- as.numeric(df_log$XL)
  df_log$XR         <- as.numeric(df_log$XR)
  df_log$ED         <- as.numeric(df_log$ED)
  df_log$EDL        <- as.numeric(df_log$EDL)
  df_log$EDR        <- as.numeric(df_log$EDR)
  df_log$RED        <- as.numeric(df_log$RED)
  
  df_log$brk        <- as.integer(df_log$brk)
  
  df_log$Supply     <- as.numeric(df_log$Supply)
  df_log$Demand     <- as.numeric(df_log$Demand)
  
  df_log$`Mrk Type` <- as.character(df_log$`Mrk Type`)
  df_log$Market     <- as.character(df_log$Market)
  
  df_log  %>%
    tidyr::drop_na() %>%
    arrange(desc(RED))
}


