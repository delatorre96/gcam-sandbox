source('postprocessing_autom.R')


input_x <- input_df[c('Years',"Region" ,"Techno","shareweights" ,
                "Capital costs (purchase)",
                "Capital costs (other)",
                "Capital costs (infrastructure)",
                'scenario')]

input_x <- input_x[input_x$Region == 'USA',]

x_dummies <- model.matrix(~ Techno - 1, data = input_x)
input_x <- cbind(x_dummies, 
                 input_x[c('Years',"shareweights",
                       "Capital costs (purchase)",
                       "Capital costs (other)",
                       "Capital costs (infrastructure)",
                       "scenario")])


output_y <- output_df[c('Year',"region",'output','scenario')]
output_y <- output_y[output_y$region == 'USA', ]
output_y$region <- NULL

# Renombrar columnas para que coincidan
input_x$year <- input_x$Years
output_y$year <- output_y$Year

# Limpiar columnas viejas si molestan
input_x$Years <- NULL
output_y$Year <- NULL

# Hacer el merge por scenario y year
df_rf <- merge(input_x, output_y,
            by = c("scenario", "year"),
            all = FALSE)   # all=FALSE para que solo devuelva matches exactos




library(randomForest)

# Ejemplo básico: entrenar random forest
# x_data: dataframe con variables predictoras (numéricas o factores)
# y_data: vector con variable respuesta (puede ser factor para clasificación o numérico para regresión)

train_random_forest <- function(x_data, y_data, ntree = 500, mtry = NULL, 
                                nodesize = NULL, maxnodes = NULL, replace = TRUE,
                                sampsize = NULL, seed = 42, ...) {
  set.seed(seed)
  
  if (is.null(mtry)) {
    if (is.factor(y_data)) {
      mtry <- floor(sqrt(ncol(x_data)))
    } else {
      mtry <- floor(ncol(x_data) / 3)
    }
  }
  
  rf_model <- randomForest(x = x_data, y = y_data,
                           #ntree = ntree,
                           #mtry = mtry,
                           #nodesize = nodesize,
                           #maxnodes = maxnodes,
                           #replace = replace,
                           #sampsize = sampsize,
                           importance = TRUE,
                           ...)
  
  return(rf_model)
}

x_data <- df_rf[c("TechnoBEV", "TechnoMORDOR", "shareweights", "Capital costs (purchase)", "Capital costs (other)", "Capital costs (infrastructure)")]
y_data <- df_rf$"output"
rf <- train_random_forest(x_data, y_data, ntree = 300, mtry = 3, 
                          nodesize = 2, maxnodes = NULL, replace = TRUE,
                          sampsize = TRUE)



pred <- predict(rf, x_data)

# Calcular R²
rss <- sum((y_data - pred)^2)
tss <- sum((y_data - mean(y_data))^2)
r_squared <- 1 - rss/tss

# Calcular RMSE
rmse <- sqrt(mean((y_data - pred)^2))

cat("R-squared:", r_squared, "\n")
cat("RMSE:", rmse, "\n")



# Ejemplo de uso:
# x_data <- data.frame(...)  # tus variables predictoras
# y_data <- ...              # tu variable respuesta
# rf <- train_random_forest(x_data, y_data, ntree = 1000, mtry = 5)

# Ver importancia de variables
# importance(rf)
# varImpPlot(rf)

