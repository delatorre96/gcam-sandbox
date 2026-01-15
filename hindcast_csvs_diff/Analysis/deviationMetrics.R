library(rgcam)
library(dplyr)

pathToDbs <- "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/output"
my_gcamdb_basexdb <- "database_basexdb"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "allQueries.xml"

scenariosAnalyze<-c( "Reference","ChangeBaseYear")

prj1 <- addScenario(conn = conn, proj = 'ChangeBaseYear.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)
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
  
  df_metrics <- df_comp %>%
    mutate(
      error = value_ref - value_chY,
      abs_error = abs(error),
      sq_error = error^2
    )
  

  
  for (crit in c('year', 'region')) {
    if (crit %in% colnames(df_metrics)) {
      metrics_global <- df_metrics %>%
        group_by(across(all_of(crit))) %>%
        summarize(
          MAE = mean(abs_error, na.rm = TRUE),
          MSE = mean(sq_error, na.rm = TRUE),
          RMSE = sqrt(MSE),
          .groups = "drop"
        )
      metrics_global['query'] <- query
      
      if (crit == 'year') {
        year_metrics <- bind_rows(year_metrics, metrics_global)
      } else if (crit == 'region') {
        region_metrics <- bind_rows(region_metrics, metrics_global)
      }
    }
  }
  for (var in variables) {
    if (var %in% colnames(df_metrics)) {
      metrics_global <- df_metrics %>%
        group_by(across(all_of(var))) %>%
        summarize(
          MAE = mean(abs_error, na.rm = TRUE),
          MSE = mean(sq_error, na.rm = TRUE),
          RMSE = sqrt(MSE),
          .groups = "drop"
        )
      metrics_global['query'] <- query
      
      # Agrega o concatena el resultado en la lista
      if (is.null(list_metrics_vars[[var]])) {
        list_metrics_vars[[var]] <- metrics_global
      } else {
        list_metrics_vars[[var]] <- bind_rows(list_metrics_vars[[var]], metrics_global)
      }
    } else {
      # Si no está la variable, se puede guardar NULL o un data frame vacío con query para rastrear
      if (is.null(list_metrics_vars[[var]])) {
        list_metrics_vars[[var]] <- data.frame(query = query)[0,] # DF vacío con columna query para mantener estructura
      }
    }
  }s
}

# saveRDS(year_metrics, "year_metrics.rds")
# saveRDS(region_metrics, "region_metrics.rds")
# saveRDS(list_metrics_vars, "list_metrics_vars.rds")
save(sector_metrics, year_metrics, region_metrics, list_metrics_vars, file = "all_metrics.RData")

