library('infotheo') 
library(aricode)
# source ('get_allData.R')
# csvs_to_xml <- get_allData()
load('csvs_to_xml.RData')


shannon_entropy <- function(x) {
  freqs <- table(x) / length(x)  # probabilidades
  -sum(freqs * log2(freqs), na.rm = TRUE)
}


clean_df <- function(df){
  #if all values are the same or are NaN, it is redundant and we eliminate the column
  cols_to_remove <- sapply(df, function(col) {
    all_na <- all(is.na(col))
    only_one_val <- length(unique(col)) == 1
    all_na || only_one_val
  })
  df <- df[, !cols_to_remove]
  #discretize
  for (col in names(df)) {
    if (length(unique_vals) > 5) {
      #if there are more than 5 unique values, we discretize to improve entropy quality
      q <- unique(quantile(df[[col]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
      if (length(q) > 1) {
        labels <- paste0("Q", seq_len(length(q) - 1))
        col_disc <- cut(df[[col]], breaks = q, include.lowest = TRUE, labels = labels)
        df[paste0(col,'_disc')] <- col_disc
        df[[col]] <- NULL
      }
    }
  }
  return(df)
}

joint_entropy_func <- function(df){
  joint_event <- apply(df, 1, function(row) paste(row, collapse = "_"))
  H_joint <- shannon_entropy(joint_event)
  return(H_joint)
}

sum_entropies_func <- function(df){
  sum_entropies <- 0
  
  for (col in names(df)) {
    H <- shannon_entropy(df[[col]])
    sum_entropies <- sum_entropies + H
  }
  return(sum_entropies)
}

mutual_information_matrix <- function(df_disc){
  
  n <- ncol(df_disc)
  nmi_matrix <- matrix(NA, nrow = n, ncol = n)
  colnames(nmi_matrix) <- colnames(df_disc)
  rownames(nmi_matrix) <- colnames(df_disc)
  
  # Iteramos sobre todos los pares
  for(i in 1:n){
    for(j in i:n){
      nmi_val <- NMI(df_disc[[i]], df_disc[[j]])
      nmi_matrix[i, j] <- round(nmi_val,4)
      nmi_matrix[j, i] <- round(nmi_val,4)  
    }
  }
  return(nmi_matrix)
}
reduce_cols_MI <- function(df_disc, threshold = 0.8){
  nmi_matrix <- mutual_information_matrix(df_disc)
  nmi_matrix[lower.tri(nmi_matrix, diag = TRUE)] <- NA  
  high_mi_pairs <- which(nmi_matrix > threshold, arr.ind = TRUE)
  vars_to_remove <- character(0)
  for (i in seq_len(nrow(high_mi_pairs))) {
    var1 <- colnames(nmi_matrix)[high_mi_pairs[i, 1]]
    var2 <- rownames(nmi_matrix)[high_mi_pairs[i, 2]]
    
    if (var1 %in% vars_to_remove || var2 %in% vars_to_remove) next
    vars_to_remove <- c(vars_to_remove, var2)
  }
  
  df_reduced <- df_disc[, !(colnames(df_disc) %in% vars_to_remove)]
  return (df_reduced)
}




list_df <- csvs_to_xml$zaglu_xml_ag_an_demand_input.R
df <- list_df$L203.Supplysector_demand

df <- clean_df(df)


df_disc <- discretize(df)
joint_entropy <- joint_entropy_func(df_disc)
sum_entropies <- sum_entropies_func(df_disc)
diff_entropy <- sum_entropies - joint_entropy

if (diff_entropy > 0){
  redundance_thres <- 1 - joint_entropy / sum_entropies
  thres_im <- 0.8
  df_current <- df_disc
  
  while (redundance_thres > 0.4){
    df_reduced <- reduce_cols_MI(df_current, thres_im)
    
    joint_entropy1 <- joint_entropy_func(df_reduced)
    sum_entropies1 <- sum_entropies_func(df_reduced)
    redundance_thres <- 1 - joint_entropy1 / sum_entropies1
    
    # Si no se reduce más o no hay cambios, salimos
    if (ncol(df_reduced) == ncol(df_current) || redundance_thres <= 0.4) {
      break
    }
    
    df_current <- df_reduced
  }
}








df_disc <- discretize(df)

joint_entropy <- joint_entropy_func(df_disc)
sum_entropies <- sum_entropies_func(df_disc)
redundance_thres <- 1 - joint_entropy/sum_entropies

diff_entropy <- sum_entropies - joint_entropy #This must be positive
if (diff_entropy > 0){
  redundance_thres <- 1 - joint_entropy/sum_entropies
  thres_im <- 0.8
  while(redundance_thres > 0.4){
    df_reduced <- reduce_cols_MI(df_disc, thres_im)
    
    joint_entropy1 <- joint_entropy_func(df_reduced)
    sum_entropies1 <- sum_entropies_func(df_reduced)
    redundance_thres1 <- 1 - joint_entropy1/sum_entropies1
    redundance_thres <- redundance_thres - 0.001
    if (redundance_thres1 <= 0.4){
      break
    }
  }
}

mutual_information_matrix(df_reduced)



while (diff_entropy > 0.01){
  df_disc <- discretize(df)
  
  mi_matrix <- mutinformation(df_disc)
  #El
  
}




