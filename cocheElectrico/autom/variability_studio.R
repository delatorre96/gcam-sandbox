source('postprocessing_autom.R')
library(dplyr)
library(entropy)

diff_entropy <- function(x, bw = "nrd0", n = 512, eps = 1e-10) {
  x <- na.omit(x)
  if(length(x) < 10) return(NA)
  if(length(unique(x)) <= 1) return(0)  # sin variabilidad → entropía 0
  
  dens <- density(x, bw = bw, n = n)
  p <- dens$y / sum(dens$y)
  p <- p + eps
  p <- p / sum(p)
  
  dx <- diff(dens$x[1:2])
  entropy <- -sum(p * log(p)) * dx
  return(entropy)
}

diff_entropy_scaled <- function(x) {
  x <- na.omit(x)
  if(length(x) < 10) return(NA)
  if(length(unique(x)) <= 1) return(0)
  x <- scale(x)  # normalizar
  dens <- density(x, n = 512)
  p <- dens$y / sum(dens$y)
  eps <- 1e-10
  p <- p + eps
  p <- p / sum(p)
  dx <- diff(dens$x[1:2])
  -sum(p * log(p)) * dx
}

vars_to_study_input <- c("shareweights", "Capital costs (purchase)", "Capital costs (other)", "Capital costs (infrastructure)")
vars_to_study_output <- c("cost", "shareweights", "output")

# Input_df
entropy_input <- input_df %>%
  group_by(scenario) %>%
  summarise(across(all_of(vars_to_study_input), diff_entropy))

# Output_df
entropy_output <- output_df %>%
  group_by(scenario) %>%
  summarise(across(all_of(vars_to_study_output), diff_entropy))

# Input_df
entropy_input_norm <- input_df %>%
  group_by(scenario) %>%
  summarise(across(all_of(vars_to_study_input), diff_entropy_scaled))

# Output_df
entropy_output_norm <- output_df %>%
  group_by(scenario) %>%
  summarise(across(all_of(vars_to_study_output), diff_entropy_scaled))

######## Agrupar escenarios y calcular la entropia promedio de todas las columnas
input_entropy_scen <- entropy_input_norm %>%
  mutate(entropy_input_norm = rowMeans(select(., all_of(vars_to_study_input)), na.rm = TRUE)) %>%
  select(-all_of(vars_to_study_input))

output_entropy_scen <- entropy_output_norm  %>%
  mutate(entropy_output_norm = rowMeans(select(., all_of(vars_to_study_output)), na.rm = TRUE)) %>%
  select(-all_of(vars_to_study_output))
total_entropy <-  merge(input_entropy_scen, output_entropy_scen, by = c("scenario"), all = TRUE)

total_entropy <- total_entropy[order(total_entropy$entropy_input), ]

plot(total_entropy$entropy_input,total_entropy$entropy_output, type = 'l',main = "Complejidad", xlab = "Entropía inputs", ylab = "Entropía outputs")
plot(density(total_entropy$entropy_input))
plot(density(total_entropy$entropy_output))


########################Entropia multivariada
library(ks)

entropiaMultivar <- function(df, vars_to_study, entropy_colname = "entropy"){ 
  scenarios <- unique(df$scenario)
  
  results <- data.frame(scenario = character(0), entropy = numeric(0), stringsAsFactors = FALSE)
  
  entropy_kde_multivariate <- function(fhat, n_points = 10000) {
    samples <- rkde(n_points, fhat)
    dens_values <- predict(fhat, x = samples)
    H <- -mean(log(dens_values + 1e-10))
    return(H)
  }
  
  for (scen in scenarios) {
    subset_data <- df %>%
      filter(scenario == scen) %>%
      select(all_of(vars_to_study)) %>%
      na.omit()
    
    if(nrow(subset_data) < 5) {
      entropy_val <- NA
    } else {
      subset_data <- scale(subset_data)
      fhat <- kde(x = as.matrix(subset_data))
      entropy_val <- entropy_kde_multivariate(fhat)
    }
    
    results <- rbind(results, data.frame(scenario = scen, entropy = entropy_val))
  }
  
  colnames(results)[colnames(results) == "entropy"] <- entropy_colname
  
  return(results)
}

input_entropy_multivar <- entropiaMultivar(input_df, vars_to_study_input, 'entropy_input')
output_entropy_multivar <- entropiaMultivar(output_df, vars_to_study_output, 'entropy_output')  
total_entropy_multivar <-  merge(input_entropy_multivar, output_entropy_multivar, by = c("scenario"), all = TRUE)



######################Otras medidas

variability_measures <- function(df, vars){
  df %>%
    group_by(scenario) %>%
    summarise(across(all_of(vars), list(var = var, sd = sd), na.rm = TRUE)) 
}

coef_var <- function(x) {
  if (mean(x, na.rm=TRUE) == 0) return(NA)
  sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE)
}

cv_measures <- function(df, vars){
  df %>%
    group_by(scenario) %>%
    summarise(across(all_of(vars), coef_var, .names = "{.col}_cv"))
}

iqr_measures <- function(df, vars){
  df %>%
    group_by(scenario) %>%
    summarise(across(all_of(vars), IQR, na.rm=TRUE, .names = "{.col}_iqr"))
}

library(moments)

shape_measures <- function(df, vars){
  df %>%
    group_by(scenario) %>%
    summarise(across(all_of(vars),
                     list(skewness = ~skewness(., na.rm=TRUE),
                          kurtosis = ~kurtosis(., na.rm=TRUE)), 
                     .names = "{.col}_{.fn}"))
}

unique_values <- function(df, vars){
  df %>%
    group_by(scenario) %>%
    summarise(across(all_of(vars), ~length(unique(na.omit(.))), .names = "{.col}_nunique"))
}

input_var <- variability_measures(input_df, vars_to_study_input)
input_cv <- cv_measures(input_df, vars_to_study_input)
input_iqr <- iqr_measures(input_df, vars_to_study_input)
input_shape <- shape_measures(input_df, vars_to_study_input)

othersMeasures <-   merge(
  input_var,
  input_cv,
  by = "scenario",
  all = TRUE
)
othersMeasures <-   merge(
  othersMeasures,
  input_iqr,
  by = "scenario",
  all = TRUE
)
othersMeasures <-   merge(
  othersMeasures,
  input_shape,
  by = "scenario",
  all = TRUE
)

#### Join entropies
names(total_entropy) <- c(
  "scenario",
  "entropy_input_uni",
  "entropy_output_uni"
)

names(total_entropy_multivar) <- c(
  "scenario",
  "entropy_input_multi",
  "entropy_output_multi"
)

entropy_merged <- merge(
  total_entropy,
  total_entropy_multivar,
  by = "scenario",
  all = TRUE
)

##All merged
all_dispersion_merged <- merge(
  entropy_merged,
  othersMeasures,
  by = "scenario",
  all = TRUE
)

write.csv(all_dispersion_merged, "all_dispersion_merged.csv", row.names = FALSE)

plot(density(entropy_merged$entropy_input_multi),
     main = "Densidades input",
     xlab = "Entropía",
     col = "red",
     lwd = 2)

###########Solo input o solo output

plot(density(entropy_merged$entropy_input_multi),
     main = "Densidades entropia multivariante input",
     xlab = "Entropía",
     col = "red",
     lwd = 2)
plot(density(entropy_merged$entropy_output_multi),
     main = "Densidades entropia multivariante output",
     xlab = "Entropía",
     col = "blue",
     lwd = 2)
plot(density(entropy_merged$entropy_input_uni),
     main = "Densidades entropia univariante input",
     xlab = "Entropía",
     col = "red",
     lwd = 2)
plot(density(entropy_merged$entropy_output_uni),
     main = "Densidades entropia univariante output",
     xlab = "Entropía",
     col = "blue",
     lwd = 2)
###########Comparación univariante multivariante
entropy_merged_norm <- entropy_merged
num_cols <- sapply(entropy_merged_norm, is.numeric)
entropy_merged_norm[num_cols] <- scale(entropy_merged[num_cols])



plot(density(entropy_merged_norm$entropy_input_multi),
     main = "Densidades input",
     xlab = "Entropía",
      col = "red",
      lwd = 2)
lines(density(entropy_merged_norm$entropy_input_uni),
     col = "blue",
     lwd = 2)

legend("topright", legend = c("Univariante", "Multivariante"), col = c("blue", "red"), lwd = 2)
###########

plot(density(entropy_merged_norm$entropy_output_multi),
     main = "Densidades output",
     xlab = "Entropía",
     col = "red",
     lwd = 2)

lines(density(entropy_merged_norm$entropy_output_uni),
      col = "blue",
      lwd = 2)

legend("topright", legend = c("Univariante", "Multivariante"), col = c("blue", "red"), lwd = 2)

###########
plot(density(entropy_merged_norm$entropy_output_multi),
     main = "Densidades Multivariantes",
     xlab = "Entropía",
     col = "red",
     lwd = 2)
lines(density(entropy_merged_norm$entropy_input_multi),
      col = "blue",
      lwd = 2)

legend("topright", legend = c("Input", "Output"), col = c("blue", "red"), lwd = 2)

###########
plot(density(entropy_merged_norm$entropy_input_uni),
     main = "Densidades Univariantes",
     xlab = "Entropía",
      col = "blue",
      lwd = 2)

lines(density(entropy_merged_norm$entropy_output_uni),
     col = "red",
     lwd = 2)

legend("topright", legend = c("Input", "Output"), col = c("blue", "red"), lwd = 2)

######################### Comparación Input y output



###########
entropy_merged <- entropy_merged[order(entropy_merged$entropy_input_multi), ]
plot(entropy_merged$entropy_input_multi, entropy_merged$entropy_output_multi,
     main = "Entropía multivariante y univariante",
     xlab = "Entropía input",
     ylab = "Entropía output",
     type = 'l',
     col = "red",
     lwd = 2)
entropy_merged <- entropy_merged[order(entropy_merged$entropy_input_uni), ]


###########



plot(entropy_merged$entropy_input_uni,entropy_merged$entropy_output_uni,
     type = 'l',
      col = "blue",
      lwd = 2)

legend("bottomright", legend = c("Univariante", "Multivariante"), col = c("blue", "red"), lwd = 2)

###########
entropy_merged_norm <- entropy_merged_norm[order(entropy_merged_norm$entropy_input_multi), ]
plot(entropy_merged_norm$entropy_input_multi, entropy_merged_norm$entropy_output_multi,
     main = "Entropía multivariante y univariante",
     xlab = "Entropía input",
     ylab = "Entropía output",
     type = 'l',
     col = "red",
     lwd = 2)
entropy_merged_norm <- entropy_merged_norm[order(entropy_merged_norm$entropy_input_uni), ]
lines(entropy_merged_norm$entropy_input_uni,entropy_merged_norm$entropy_output_uni,
      col = "blue",
      lwd = 2)

legend("bottomright", legend = c("Univariante", "Multivariante"), col = c("blue", "red"), lwd = 2)

########################
plot(entropy_merged$entropy_input_multi, entropy_merged$entropy_output_multi,
     main = "Entropía multivariante",
     xlab = "Entropía input",
     ylab = "Entropía output",
     col = "red",
     lwd = 2)
plot(entropy_merged$entropy_input_uni, entropy_merged$entropy_output_uni,
     main = "Entropía univariante",
     xlab = "Entropía input",
     ylab = "Entropía output",
     col = "blue",
     lwd = 2)





#################Explicar entropia del output

modelo <- lm(density(entropy_output_multi) ~ density(entropy_input_multi), data = entropy_merged)
summary(modelo)

modelo <- lm(entropy_output_uni ~ entropy_input_uni, data = entropy_merged)
summary(modelo)
  






entropiaMultivar_dplyr <- function(df, vars_to_study){
  
  entropy_kde_multivariate <- function(fhat, n_points = 10000) {
    samples <- rkde(n_points, fhat)
    dens_values <- predict(fhat, x = samples)
    H <- -mean(log(dens_values + 1e-10))
    return(H)
  }
  
  df %>%
    group_by(scenario) %>%
    group_map(~{
      escenario_actual <- unique(.x$scenario)
      
      data_sub <- select(.x, all_of(vars_to_study)) %>% na.omit()
      
      if(nrow(data_sub) < 5) {
        tibble(
          scenario = escenario_actual,
          entropy = NA_real_
        )
      } else {
        data_scaled <- scale(data_sub)
        fhat <- kde(x = as.matrix(data_scaled))
        tibble(
          scenario = escenario_actual,
          entropy = entropy_kde_multivariate(fhat)
        )
      }
    }) %>%
    bind_rows()
}

input_entropy_multivar <- entropiaMultivar_dplyr(input_df, vars_to_study_input)
output_entropy_multivar <- entropiaMultivar_dplyr(output_df, vars_to_study_output)

total_entropy_multivar <- data.frame(
  entropy_input  = input_entropy_multivar[[1]],
  entropy_output = output_entropy_multivar[[1]]
)
total_entropy_multivar <- total_entropy_multivar[order(total_entropy_multivar$entropy_input), ]
plot(total_entropy_multivar$entropy_input,total_entropy_multivar$entropy_output, type = 'l',main = "Complejidad", xlab = "Entropía inputs", ylab = "Entropía outputs")

























######################## borrador

vars <- vars_to_study_output
scenarios <- unique(output_df$scenario)

results <- data.frame(scenario = character(0), entropy = numeric(0), stringsAsFactors = FALSE)

entropy_kde_multivariate <- function(fhat, n_points = 10000) {
  samples <- rkde(n_points, fhat)
  dens_values <- predict(fhat, x = samples)
  H <- -mean(log(dens_values + 1e-10))  # añadir epsilon para evitar log(0)
  return(H)
}

for (scen in scenarios) {
  subset_data <- output_df %>%
    filter(scenario == scen) %>%
    select(all_of(vars)) %>%
    na.omit()
  subset_data <- scale(subset_data)
  if(nrow(subset_data) < 5) {  # evitar escenarios con muy pocas filas
    entropy_val <- NA
  } else {
    subset_data <- scale(subset_data)
    fhat <- kde(x = as.matrix(subset_data))
    entropy_val <- entropy_kde_multivariate(fhat)
  }
  
  results <- rbind(results, data.frame(scenario = scen, entropy = entropy_val))
}





##################### Borrador
scenarios <- unique(df$scenario)

results <- data.frame(scenario = character(0), entropy = numeric(0), stringsAsFactors = FALSE)

entropy_kde_multivariate <- function(fhat, n_points = 10000) {
  samples <- rkde(n_points, fhat)
  dens_values <- predict(fhat, x = samples)
  H <- -mean(log(dens_values + 1e-10))  # añadir epsilon para evitar log(0)
  return(H)
}

for (scen in scenarios) {
  subset_data <- df %>%
    filter(scenario == scen) %>%
    select(all_of(vars_to_study)) %>%
    na.omit()
  
  if(nrow(subset_data) < 5) {  # evitar escenarios con muy pocas filas
    entropy_val <- NA
  } else {
    fhat <- kde(x = as.matrix(subset_data))
    entropy_val <- entropy_kde_multivariate(fhat)
  }
  
  results <- rbind(results, data.frame(scenario = scen, entropy = entropy_val))
}

return(results)
}






library(FNN)

multivariate_entropy_knn <- function(data, k = 5) {
  library(FNN)
  n <- nrow(data)
  d <- ncol(data)
  # calcular distancias al k-ésimo vecino más cercano
  knn_dist <- get.knn(data, k = k)$nn.dist[, k]
  
  # constante del volumen de la bola d-dimensional
  vol_unit_ball <- (pi^(d/2)) / gamma(d/2 + 1)
  
  H <- digamma(n) - digamma(k) + log(vol_unit_ball) + (d / n) * sum(log(knn_dist)) + log(n - 1)
  return(H)
}


data_subset_input <- input_df %>% select(all_of(vars_to_study_input)) %>% na.omit() %>% scale

entropy_input_by_scenario <- input_df %>%
  select(all_of(vars_to_study_input), scenario) %>%
  group_by(scenario) %>%
  group_modify(~ {
    data_matrix <- as.matrix(na.omit(.x[, vars_to_study_input]))
    ent <- if(nrow(data_matrix) >= 10) multivariate_entropy_knn(data_matrix, k = 5) else NA_real_
    tibble(entropy_multivariate = ent)
  }) %>%
  ungroup()


