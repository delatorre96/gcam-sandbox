setwd('../trees')
source('functions_drawTree.R')
source('functions_manipulateCSV.R')
gcam_path ='C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core'
set_gcam_paths(gcam_path)
setwd(dir_gcamdata)
devtools::load_all()
###Miramos en dónde aparece el csv: FAO_ag_items_PRODSTAT.csv

file_objetivo <- 'FAO_ag_items_PRODSTAT'

totalTree <- build_tree()

chunksWithFile <- c()
for (i in 1:length(totalTree)){
  inputs <- totalTree[[i]]$'inputs'
  for (input in inputs){
    if (grepl('FAO_ag_items_PRODSTAT',input)){
      chunksWithFile <- c(chunksWithFile, names(totalTree[i]))
    }
  }
}

###Parece que en el propio CSV se identifica un crop como un commodity. 
#Por ello, vamos a buscar en qué csv's aparece algún commodity seleccionado. Por ejemplo: Vegetables


all_initial_files <- get_all_initial_files(totalTree)
gcam_comm <- 'GCAM_commodity'
files_gcamComm <- c()
for (i in all_initial_files){
  df <- data.frame(load_csv_files(i, optional = TRUE))
  
  # 1. Buscar en las celdas
  match_cells <- any(grepl(gcam_comm, as.matrix(df), ignore.case = TRUE))
  
  # 2. Buscar en los nombres de las columnas
  match_cols <- any(grepl(gcam_comm, colnames(df), ignore.case = TRUE))
  
  if (match_cells || match_cols) {
    files_gcamComm <- c(files_gcamComm, i)
  }
}

revisar_csvs(files_gcamComm)
### Se cambian a mano

##Da error en module_aglu_L2012.ag_For_Past_bio_input_irr_mgmt
#Concretamente con: L181.ag_Prod_Mt_R_C_Y_GLU_irr_level y  L123.For_Prod_bm3_R_Y_GLU 

files_cambiados <- c('A_agStorageSector','A_defaultYieldRate','FAO_ag_items_PRODSTAT','Mapping_SUA_PrimaryEquivalent','Mapping_item_FBS_GCAM')

all_chunks <- c()
for (i in files_cambiados){
  all_chunks <- c(all_chunks, searchChunks(i))
}







##############################

commodity = 'Vegetables'

all_initial_files <- get_all_initial_files(totalTree)

#coincidencia aprox
files_veg <- c()
for (i in all_initial_files){
  df <- get_csv_info(i)$df
  if (any(grepl(commodity, as.matrix(df), ignore.case = TRUE))){
    files_veg <- c(files_veg, i)
  }
}
files_veg <- unique(files_veg)
files_veg_filtered <- files_veg[ !(files_veg %in% c('aglu/FAO_ag_items_PRODSTAT')) ]


for (i in files_veg_filtered){
  manipulate_CSVByeFunc(i, old_techno_name = 'Tomatoes', new_techno_name = 'Tomatoes')
}
