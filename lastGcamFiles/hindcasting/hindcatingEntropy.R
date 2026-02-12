
source("C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/lastGcamFiles/inputsEntropy.R")

GCAM_path_2010 <- "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core"
df_entropies_inputs_GCAM2010 <- get_inputs_entropy_chunkName(GCAM_path_2010)
colnames(df_entropies_inputs_GCAM2010)[colnames(df_entropies_inputs_GCAM2010) == "entropy"] <- "GCAM_2010"


GCAM_path_2021 <- "C:/Users/ignacio.delatorre/Documents/GCAM/Release"
df_entropies_inputs_GCAM2021 <- get_inputs_entropy_chunkName(GCAM_path_2021)
colnames(df_entropies_inputs_GCAM2021)[colnames(df_entropies_inputs_GCAM2021) == "entropy"] <- "GCAM_2021"

df_merge <- merge(df_entropies_inputs_GCAM2010, df_entropies_inputs_GCAM2021, by = c("chunk","inputs_df"))

df_merge$diff <- abs(df_merge$GCAM_2010 - df_merge$GCAM_2021)

abs(sum(df_merge$GCAM_2010) - sum(df_merge$GCAM_2021))

############# Ver inputs previos si ################
csvs_to_xml_2010 <- get_allData(GCAM_path_2010)
csvs_to_xml_2021 <- get_allData(GCAM_path_2021)

############# Ver si hay Na. NaNs inf o empties ################
check_df <- function(df) {
  empty <- nrow(df) == 0 || ncol(df) == 0
  
  # Convertimos a matriz para capturar NaN e Inf
  mat <- as.matrix(df)
  
  # Booleano si hay algún NA, NaN o Inf
  has_na <- any(is.na(mat) | is.nan(mat) | is.infinite(mat))
  
  # Conteo total de NA, NaN o Inf
  count_na <- sum(is.na(mat) | is.nan(mat) | is.infinite(mat))
  
  data.frame(
    empty = empty,
    has_na = has_na,
    count_na = count_na
  )
}
null_values <- function(csvs_to_xml) {
  results_list <- list()
  
  for(list_item in names(csvs_to_xml)) {
    sublist <- csvs_to_xml[[list_item]]
    
    for(df_name in names(sublist)) {
      df <- sublist[[df_name]]
      
      res <- check_df(df)
      res$list_item <- list_item
      res$df_name <- df_name
      
      results_list[[length(results_list) + 1]] <- res
    }
  }
  results_df <- do.call(rbind, results_list)
  results_df <- results_df[, c("list_item", "df_name", "empty", "has_na", "count_na")]
  
  return(results_df)
}

results_2010 <- null_values(csvs_to_xml_2010)

colSums(results_2010[, c("empty", "has_na")])


results_2021 <- null_values(csvs_to_xml_2021)
colSums(results_2021[, c("empty", "has_na")])

count_merge <- merge(results_2010[c('list_item','df_name','has_na')], results_2021[c('list_item','df_name','has_na')], by = c("list_item","df_name"), suffixes = c("_2010", "_2021"))
count_merge_hasna2015Not2021 <- count_merge[count_merge$has_na_2010 ==TRUE & count_merge$has_na_2021 ==FALSE, ]

############# Ver cantidad de Na o NaN por data frame ################
count_na_df <- function(df) {
  mat <- as.matrix(df)
  count <- sum(is.na(mat) | is.nan(mat))
  data.frame(count_na = count)
}
results_list <- list()

for(list_item in names(csvs_to_xml_2010)) {
  sublist <- csvs_to_xml_2010[[list_item]]
  
  for(df_name in names(sublist)) {
    df <- sublist[[df_name]]
    res <- count_na_df(df)
    res$list_item <- list_item
    res$df_name <- df_name
    
    results_list[[length(results_list) + 1]] <- res
  }
}

results_2010 <- do.call(rbind, results_list)
results_2010 <- results_2010[, c("list_item", "df_name", "count_na")]

results_list <- list()

for(list_item in names(csvs_to_xml_2010)) {
  sublist <- csvs_to_xml_2010[[list_item]]
  
  for(df_name in names(sublist)) {
    df <- sublist[[df_name]]
    res <- count_na_df(df)
    res$list_item <- list_item
    res$df_name <- df_name
    
    results_list[[length(results_list) + 1]] <- res
  }
}

results_2010 <- do.call(rbind, results_list)
results_2010 <- results_2010[, c("list_item", "df_name", "count_na")]
