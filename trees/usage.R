#setwd('C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata')
#devtools::load_all()

library('gcamdata')
source('functions_drawTree.R')
path_functionR = "C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata/R"
setwd(path_functionR)

#### Construir Tree para zaglu, zenergy, zemissions, zclimate o zsocio. Se podría hacer con todo usando total_tree pero tarda mucho en graficar

zenergyTree <- build_tree_aspect(module = 'zenergy')

#Dibujar tree de scripts de R para el archivo que nos interesa.
#En este caso: transportation_UCD_CORE.xml

ancestors <- draw_subgraph(tree = zenergy_tree , target = 'transportation_UCD_CORE.xml')

##También se puede hacer con cualquier archivo de R
draw_subgraph(tree = zenergy_tree , target = 'zenergy_xml_transportation_UCD_CORE.R')

#Miramos todos los archivos iniciales de ese chunk
initial_files <- get_all_files(ancestors, tree)

tech_files <- initial_files [grepl("tech", initial_files)]

files_car <- c()
for (i in tech_files){
  df <- data.frame(load_csv_files(i, optional = TRUE))
  if (any(grepl("car", as.matrix(df), ignore.case = TRUE))){
    files_car <- c(files_car, i)
  }
}

df <- data.frame(load_csv_files(files_car[[1]], optional = TRUE))






##################### Otro uso:
inputs_archivointeres <- zenergyTree$zenergy_L254.transportation_UCD.R$inputs
#sospechamos de: "energy/mappings/UCD_techs"

ucd_techs <- data.frame(load_csv_files("energy/mappings/UCD_techs", optional = TRUE))
ucd_techs_r <- data.frame(load_csv_files("energy/mappings/UCD_techs_revised", optional = TRUE))

total_tree = build_tree()
draw_subgraph(tree = total_tree , target = 'energy/mappings/UCD_techs_revised')
