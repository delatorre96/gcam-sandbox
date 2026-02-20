library(rgcam)
library(dplyr)
library(Metrics)

prj1 <- loadProject(proj = "ChangeBaseYear.dat")
queries <- listQueries(prj1)


variables <- c()
for (query in queries){
  df <- getQuery(prj1,  query)
  cols <- colnames(df)
  cols <- cols[cols != 'scenario' & 
               cols !=  'value' & 
               cols != 'region' & 
               cols !=  'Units'& 
              cols !=  'year']
  variables <- c(variables,cols)
}
variables <- unique(variables)

list_metrics_vars <- vector("list", length(variables))
names(list_metrics_vars) <- variables
empty_df <- data.frame(
  MAE = numeric(),
  MSE = numeric(),
  RMSE = numeric(),
  query = character()
)

for (v in variables) {
  list_metrics_vars[[v]] <- empty_df[0, ]
}

year_metrics <- data.frame('query' = NULL,'year' = NULL, 'MAE' = NULL, 'MSE' = NULL, 'RMSE' = NULL)
region_metrics <- data.frame('query' = NULL,'region' = NULL, 'MAE' = NULL, 'MSE' = NULL, 'RMSE' = NULL)

serie_metrics <- data.frame('query' = NULL,'beta_ratio' = NULL, 'spearman_corr' = NULL, 'MAE' = NULL, 'RMSE' = NULL)
lista_resultados <- list()
for (query in queries){
  
  df <- getQuery(prj1,  query)
  
  key_cols <- colnames(df)
  key_cols <- key_cols[key_cols != 'scenario' & key_cols !=  'value']
  
  df_ref <- df %>%
    filter(scenario == "Reference") %>%
    select(all_of(key_cols), value_ref = value)
  
  df_chY <- df %>%
    filter(scenario == "ChangeBaseYear") %>%
    select(all_of(key_cols), value_chY = value)
  
  df_comp <- df_ref %>%
    inner_join(df_chY, by = key_cols)
  ########
  # df_metrics <- df_comp %>%
  #   mutate(
  #     error = value_ref - value_chY,
  #     abs_error = abs(error),
  #     sq_error = error^2
  #   )
  # 
  # 
  # 
  # for (crit in c('year', 'region')) {
  #   if (crit %in% colnames(df_metrics)) {
  #     metrics_global <- df_metrics %>%
  #       group_by(across(all_of(crit))) %>%
  #       summarize(
  #         MAE = mean(abs_error, na.rm = TRUE),
  #         RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
  #         .groups = "drop"
  #       )
  #     metrics_global['query'] <- query
  #     
  #     if (crit == 'year') {
  #       year_metrics <- bind_rows(year_metrics, metrics_global)
  #     } else if (crit == 'region') {
  #       region_metrics <- bind_rows(region_metrics, metrics_global)
  #     }
  #   }
  # }
  # for (var in variables) {
  #   if (var %in% colnames(df_metrics)) {
  #     metrics_global <- df_metrics %>%
  #       group_by(across(all_of(var))) %>%
  #       summarize(
  #         MAE = mean(abs_error, na.rm = TRUE),
  #         RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
  #         .groups = "drop"
  #       )
  #     metrics_global['query'] <- query
  #     
  #     # Agrega o concatena el resultado en la lista
  #     if (is.null(list_metrics_vars[[var]])) {
  #       list_metrics_vars[[var]] <- metrics_global
  #     } else {
  #       list_metrics_vars[[var]] <- bind_rows(list_metrics_vars[[var]], metrics_global)
  #     }
  #   } else {
  #     # Si no está la variable, se puede guardar NULL o un data frame vacío con query para rastrear
  #     if (is.null(list_metrics_vars[[var]])) {
  #       list_metrics_vars[[var]] <- data.frame(query = query)[0,] # DF vacío con columna query para mantener estructura
  #     }
  #   }
  # }
  cols <- colnames(df_comp)
  columnas_grupo <- cols[cols != 'Units' & cols !=  'year' & cols != "value_ref"& cols != "value_chY"]
  combinaciones <- df_comp %>%
    select(all_of(columnas_grupo)) %>%
    distinct()
  resultados <- data.frame()
  for(i in 1:nrow(combinaciones)) {
    filtro <- combinaciones[i, ]
    
    # Filtrar filas que coincidan con la combinación actual
    df_serie <- df_comp %>%
      filter(across(all_of(columnas_grupo), ~ . == filtro[[cur_column()]]))
    
    if(nrow(df_serie) > 2) {
      regresion_ref <- lm(value_ref ~ year, data = df_serie)
      beta_ref <- as.numeric(coef(regresion_chY)["year"])
      
      regresion_chY <- lm(value_chY ~ year, data = df_serie)
      beta_chY <- as.numeric(coef(regresion_chY)["year"])
      
      beta_ratio <- beta_chY / beta_ref
      
      spearman_corr <- cor(df_serie$value_ref, df_serie$value_chY, method = "spearman", use = "complete.obs")
      
      
      mae_val <- mae(df_serie$value_ref, df_serie$value_chY)
      rmse_val <- rmse(df_serie$value_ref, df_serie$value_chY)
      
      resultados <- bind_rows(resultados,
                              bind_cols(filtro,
                                        data.frame(beta_ratio = beta_ratio,
                                                   spearman_corr = spearman_corr,
                                                   MAE = mae_val,
                                                   RMSE = rmse_val)))
      
    }
    resultados['query'] <- query
    
   # resultados <- resultados %>%
   #         group_by() %>%
   #         summarize(
   #           MAE = mean(abs_error, na.rm = TRUE),
   #           RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
   #           .groups = "drop"
   #         )

  }
  lista_resultados[[query]] <- resultados
  
}
serie_metrics <- bind_rows(lista_resultados)






# saveRDS(year_metrics, "year_metrics.rds")
# saveRDS(region_metrics, "region_metrics.rds")
# saveRDS(list_metrics_vars, "list_metrics_vars.rds")
save(sector_metrics, year_metrics, region_metrics, list_metrics_vars, file = "all_metrics.RData")

