setwd('C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcamdata_trees')
source('functions_drawTree.R')
source('functions_manipulateCSV.R')
setwd("C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata")
devtools::load_all()

###################BUSCAR TECNOLOGÍA###########################
##Hacemos un tree y sacamos todos los archivos de R que lo conforman

zenergyTree <- build_tree_module(modules = 'zenergy')

#Para dibujar el grafo:
#ancestors <- draw_subgraph(tree = zenergyTree , target = 'transportation_UCD_CORE.xml')

#Para sacar los archivos de R que dan lugar al archivo dado:

ancestors <- find_ancestors (tree = zenergyTree, target = 'transportation_UCD_CORE.xml')

#ancestors_USA <- find_ancestors (tree = zgcamusaTree, target = 'transportation_USA_CORE.xml')
initial_files <- get_all_files(ancestors, zenergyTree)
  
#filtramos aquellos en donde pone tech
tech_files <- initial_files [grepl("tech", initial_files)]

#de ellos filtramos aquellos en donde pone "car" en el data frame
files_car <- c()
for (i in tech_files){
  df <- data.frame(load_csv_files(i, optional = TRUE))
  if (any(grepl("car", as.matrix(df), ignore.case = TRUE))){
    files_car <- c(files_car, i)
  }
}


#####nos quedamos ahora con unos cuantos y miramos en qué scripts están:
for (i in files_car){
  print('####################')
  print(paste0('Archivo: ', i))
  print(find_file_in_scripts (tree = zenergyTree, target_file =  i)$as_input)
}


df <- data.frame(load_csv_files("energy/mappings/UCD_techs", optional = TRUE))

unique(df$energy.mappings.UCD_techs.UCD_fuel)
unique(df$energy.mappings.UCD_techs.fuel)

df_electric <- df[df$energy.mappings.UCD_techs.fuel == 'electricity',]
df_electric_car  <- df_electric[grepl("car", df_electric$energy.mappings.UCD_techs.size.class, ignore.case = TRUE), ]

techno <- unique(df_electric_car$energy.mappings.UCD_techs.UCD_technology)
car <- unique(df_electric_car$energy.mappings.UCD_techs.mode)



###################AÑADIR  TECNOLOGÍA###########################
#
##1)Identificar dónde aparece  BEV junto con LDV_4W o car

all_initial_files <- get_all_initial_files(build_tree())



files_BEV <- c()
for (i in all_initial_files){
  df <- data.frame(load_csv_files(i, optional = TRUE))
  if (any(grepl(techno, as.matrix(df), ignore.case = TRUE))){
    files_BEV <- c(files_BEV, i)
  }
}
files_BEV <- unique(files_BEV)
#eliminamos un item que no tiene que manipular:

files_BEV <- files_BEV[ !(files_BEV %in% c("energy/mappings/IEA_flow_sector", "Census_ind_VoS_state")) ]

files_BEV_filtered <- filter_csvs_by_exact_name(files_BEV, techno)
#revisar_csvs(files_BEV_filtered)

files_BEV_filtered <- files_BEV_filtered[ !(files_BEV_filtered %in% c("energy/mappings/IEA_flow_sector", "Census_ind_VoS_state")) ]

for (i in files_BEV_filtered){
  manipulate_CSVByeFunc(i, old_techno_name = 'BEV', new_techno_name = 'MORDOR',extra_word = c('LDV_4W', 'car'), func = NULL)
}



