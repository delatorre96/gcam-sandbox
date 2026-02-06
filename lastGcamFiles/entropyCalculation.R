library('infotheo') 
library(aricode)


shannon_entropy <- function(x) {
  freqs <- table(x) / length(x)  # probabilidades
  -sum(freqs * log2(freqs), na.rm = TRUE)
}


clean_df <- function(df){
  #if all values are the same or are NaN, it is redundant and we eliminate the column
  #We also eliminate those that has unique identification since is also redundant (maximizes entropy unnecessarily)
  cols_to_remove <- sapply(df, function(col) {
    all_na <- all(is.na(col))
    only_one_val <- length(unique(col)) == 1
    #all_unique <- length(unique(col)) == length(col)
    all_na || only_one_val
  })
  df <- df[, !cols_to_remove]
  
  ########## DISCRETIZATION ###############
  #Discretize numeric cols is important in order to calculate entropy (which works better with categoric variables). 
  #We can use numerical variables as categorical considering numbers as categories but if the numerical variable has a big rank, entropy could collapse
  #To discretize, first we calculate the minimum unique values that a numeric col should have to be considered continuous. 
  #To do so, first we select the categoric column with more unique values
  #After, we calculate how many unique values has this categoric column 
  #The maximum unique values that has this categoric column will be the threshold we use to identify a numeric column as continuous or discrete
  non_numeric_cols <- sapply(df, function(x) !is.numeric(x))
  if (!all(non_numeric_cols == FALSE)){
  unique_counts <- sapply(df[, non_numeric_cols], function(x) length(unique(x)))
  max_unique_values <- max(unique_counts)
  }
  
  for (col in names(df)) {
    
    if (length(unique_vals) > max_unique_values) {
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
  
  df_reduced <- df_disc[, !(colnames(df_disc) %in% vars_to_remove), drop = FALSE]
  return (df_reduced)
}

get_dataFrame_entropy <- function (df){
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

      joint_entropy <- joint_entropy_func(df_reduced)
      sum_entropies <- sum_entropies_func(df_reduced)
      redundance_thres <- 1 - joint_entropy / sum_entropies
      if (ncol(df_reduced) == 1) {
        break
      }

      df_current <- df_reduced
      thres_im <- thres_im - 0.001
    }
  }
  return(sum_entropies)
}

get_dataFrame_Sum_entropy <- function (df){
  ##Similar of get_dataFrame_entropy but less rigorous since is just the sum of columns entropy (without reducing redundancy)
  df <- clean_df(df)
  
  df_disc <- discretize(df)
  sum_entropies <- sum_entropies_func(df_disc)
  return(sum_entropies)
}














