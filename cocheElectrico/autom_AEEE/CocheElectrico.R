setwd('../../trees')
source('functions_drawTree.R')
source('functions_manipulateCSV.R')
##Path gcamdata
gcam_path <- "C:/GCAM/Nacho/gcam_8.2"
set_gcam_paths(gcam_path)
setwd(dir_gcamdata)
devtools::load_all()



###################AÑADIR  TECNOLOGÍA###########################
#
##1)Identificar dónde aparece  BEV junto con LDV_4W o car

all_initial_files <- get_all_initial_files(build_tree())



files_BEV <- c()
for (i in all_initial_files){
  df <- data.frame(load_csv_files(i, optional = TRUE))
  if (any(grepl('BEV', as.matrix(df), ignore.case = TRUE))){
    files_BEV <- c(files_BEV, i)
  }
}
files_BEV <- unique(files_BEV)
#eliminamos un item que no tiene que manipular:

files_BEV <- files_BEV[ !(files_BEV %in% c("energy/mappings/IEA_flow_sector", "Census_ind_VoS_state")) ]

files_BEV_filtered <- filter_csvs_by_exact_name(files_BEV, 'BEV')
#revisar_csvs(files_BEV_filtered)

files_BEV_filtered <- files_BEV_filtered[ !(files_BEV_filtered %in% c("energy/mappings/IEA_flow_sector", "Census_ind_VoS_state")) ]

markal <- c("gcam-usa/emissions/MARKAL_UCD_LDV_fuel","gcam-usa/emissions/MARKAL_UCD_HDV_fuel")

for (i in files_BEV_filtered){
  if (i %in% markal){
    manipulate_CSVByeFunc(i, old_techno_name = 'BEV', new_techno_name = 'BEV_2', func = NULL)
  }else{
    manipulate_CSVByeFunc(i, old_techno_name = 'BEV', new_techno_name = 'BEV_2',extra_word = c('LDV_4W', 'car'), func = NULL)
  }
}

#driver_drake()
#setwd(gcam_path)
#system("git add .")
#system('git commit -m \"newTechno\"')


