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

plot(total_entropy$entropy_input,total_entropy$entropy_output, type = 'l')


########################Entropia multivariada
library(ks)

entropiaMultivar <- function(df, vars_to_study){ 
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
  
}
input_entropy_multivar <- entropiaMultivar(input_df, vars_to_study_input)
output_entropy_multivar <- entropiaMultivar(ouput_df, vars_to_study_output)

########################







##################### Borrador
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


