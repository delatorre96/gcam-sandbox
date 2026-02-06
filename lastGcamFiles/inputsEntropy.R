source('entropyCalculation.R')

if (file.exists("csvs_to_xml.RData")) {
  load("csvs_to_xml.RData")
} else {
  source("get_allData.R")
  csvs_to_xml <- get_allData()
}



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
colnames(df_entropies_inputs) <- "entropia"




# lista_nombres <- character()
# sublista_nombres <- character()
# entropias <- numeric()
# n <- numeric()
# tiempos <- numeric()
# 
# df_procesados <- 0
# cat(df_procesados, "\r")
# 
# for (chunk_name in names(csvs_to_xml)) {
#   chunk <- csvs_to_xml[[chunk_name]]
#   
#   for (df_name in names(chunk)) {
#     start_time <- Sys.time()
#     df <- chunk[[df_name]]
#     n_rows <- nrow(df)
#     n_cols <- ncol(df)
#     n_obs <- n_cols*n_rows
#     
#     H_df <- get_dataFrame_Sum_entropy(df)
#     end_time <- Sys.time()
#     duracion <- as.numeric(difftime(end_time, start_time, units = "secs"))
#     lista_nombres <- c(lista_nombres, chunk_name)
#     sublista_nombres <- c(sublista_nombres, df_name)
#     entropias <- c(entropias, H_df)
#     n <- c(n, n_obs)
#     tiempos <- c(tiempos, duracion)
#     
#     df_procesados <- df_procesados + 1
#     cat(df_procesados, "dataframes procesados...", 'Procesando ', df_name, '-', chunk_name, "\r")
#     flush.console()
#     
#   }
# }
# 
# 
# resultado_df <- data.frame(
#   lista = lista_nombres,
#   sublista = sublista_nombres,
#   entropia = entropias,
#   n = n,
#   tiempos = tiempos,
#   stringsAsFactors = FALSE
# )

