setwd('../trees')
source('functions_drawTree.R')
source('functions_manipulateCSV.R')
gcam_path ='C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core'
set_gcam_paths(gcam_path)
setwd(dir_gcamdata)
devtools::load_all()


totalTree <- build_tree()
all_initial_files <- get_all_initial_files(totalTree)ush
gcam_comm <- 'Vegetables'
files_gcamComm <- c()
for (i in all_initial_files){
  df <- data.frame(load_csv_files(i, optional = TRUE))
  match_cells <- any(grepl(gcam_comm, as.matrix(df), ignore.case = TRUE))
  
  if (match_cells ) {
    files_gcamComm <- c(files_gcamComm, i)
  }
}
files_gcamComm <- unique(files_gcamComm)

search_substring <- function(df, word, ignore_case = TRUE) {
  pattern <- if (ignore_case) tolower(word) else word
  df_to_search <- if (ignore_case) data.frame(lapply(df, tolower)) else df
  
  any(sapply(df_to_search, function(col) {
    any(grepl(pattern, col, fixed = TRUE))
  }))
}


search_exact <- function(df, word) {
  df_char <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  
  any(sapply(df_char, function(col) {
    col <- col[!is.na(col)]       # eliminar NAs
    any(col == word)
  }))
}
#files_gcamComm[!files_gcamComm %in% c("aglu/FAO/GCAMFAOSTAT_FBSH_CB", "aglu/FAO/GCAMFAOSTAT_FodderProdArea")]



for (file in files_gcamComm){
  df <- get_csv_info(file)$df
  if ((search_substring(df, "Tomatoes"))){
    ##Entonces estamos en df en donde hay vegetables y tomate
    manipulate_csv_replace_contains(csv_file = file,
                                    filter_word = 'Tomatoes',
                                    target_word = 'Vegetables',
                                    replacement_word = 'Tomatoes')
  }   else if ((search_substring(df, "Vegetables"))){
    manipulate_CSVByeFunc_substring(csv_file = file,
                                    old_techno_name = 'Vegetables',
                                    new_techno_name = 'Tomatoes')
  }
  else if ((search_exact(df, "Tomatoes")))
    manipulate_csv_replace_exact(csv_file = file,
                                 filter_word_exact = 'Tomatoes',
                                 target_word_exact = 'Vegetables',
                                 replacement_word = 'Tomatoes')
  else {
    manipulate_CSVByeFunc(csv_file = file,
                          old_techno_name = 'Vegetables', 
                          new_techno_name = 'Tomatoes')
  }
}
 

driver_drake()









######################################### BORRADOR

#Por ello, vamos a buscar en qué csv's aparece algún commodity seleccionado. Por ejemplo: Vegetables
# totalTree <- build_tree()
# 
# all_initial_files <- get_all_initial_files(totalTree)
# gcam_comm <- 'GCAM_commodity'
# interested_commodity <- 'Vegetables'
# files_gcamComm <- c()
# for (i in all_initial_files){
#   df <- data.frame(load_csv_files(i, optional = TRUE))
# 
#   # 1. Buscar en las celdas
#   match_cells <- any(grepl(gcam_comm, as.matrix(df), ignore.case = TRUE))
# 
#   # 2. Buscar en los nombres de las columnas
#   match_cols <- any(grepl(gcam_comm, colnames(df), ignore.case = TRUE))
# 
#   #3. Buscar el commodity de interes
#   match_cells_veg <- any(grepl('Vegetables', as.matrix(df), ignore.case = TRUE))
# 
#   if ((match_cells || match_cols) & match_cells_veg) {
#     files_gcamComm <- c(files_gcamComm, i)
#   }
# }
# 
# 
# 
# files_gcamComm <- unique(files_gcamComm)
# 
# trtmiento_Mordor <- c('aglu/A_LT_Mapping',
#                      'aglu/A_agStorageSector',
#                      'aglu/A_defaultYieldRate',
#                      'socioeconomics/GTAP/GCAM_GTAP_sector_mapping_AgLU',
#                      "aglu/A_agSupplySector",
#                      "aglu/A_agSupplySubsector",
#                      "aglu/GCAMLandLeaf_CdensityLT",
#                      "aglu/A_demand_technology",
#                      "aglu/A_an_input_technology")
# trtmiento_tomate <- c('aglu/FAO/FAO_ag_items_PRODSTAT',
#                       'aglu/FAO/Mapping_SUA_PrimaryEquivalent')
# trtmiento_tomate_aprox <- c('aglu/FAO/Mapping_item_FBS_GCAM')
# 
# files_gcamComm_new <- c(trtmiento_Mordor,trtmiento_tomate,trtmiento_tomate_aprox)
# 
# for (file in trtmiento_tomate){
#   manipulate_csv_replace_exact(csv_file = file,
#                                filter_word_exact = 'Tomatoes',
#                                target_word_exact = 'Vegetables',
#                                replacement_word = 'Tomatoes')
# }
# 
# for (file in trtmiento_tomate_aprox){
#   manipulate_csv_replace_contains(csv_file = file,
#                                filter_word = 'Tomatoes',
#                                target_word = 'Vegetables',
#                                replacement_word = 'Tomatoes')
# }
# 
# for (file in trtmiento_Mordor){
#   manipulate_CSVByeFunc(csv_file = file,
#                         old_techno_name = 'Vegetables',
#                         new_techno_name = 'Tomatoes')
# }
# 
# 
# 
# ############################################
# 
# 
# file_objetivo <- 'IFA2002_Fert_ktN'
# 
# totalTree <- build_tree()
# 
# chunksWithFile <- c()
# for (i in 1:length(totalTree)){
#   inputs <- totalTree[[i]]$'inputs'
#   for (input in inputs){
#     if (grepl('FAO_ag_items_PRODSTAT',input)){
#       chunksWithFile <- c(chunksWithFile, names(totalTree[i]))
#     }
#   }
# }
# 
