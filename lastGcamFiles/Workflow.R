gcam_path <- 'C:/GCAM/Nacho/gcam_8.2'

actual_dir <- getwd()
dir_gcam <- gcam_path
dir_gcamdata <- paste0(gcam_path,'/input/gcamdata')
dir_chunks <- paste0(dir_gcamdata,'/R')
dir_csvs_iniciales <- paste0(dir_gcamdata,'/inst/extdata')
setwd(dir_gcamdata)
devtools::load_all()
driver_drake()

### 1.- Meter ruido a los inputs de gcam (es decir, a los outputs del data system)

### 2.- Guardar los inputs con get_allData.R

### 3.- Sacar su entropia d elos inputs y guardarla en un DF

### 4.- Ejecutar GCAM

### 5.- Sacar la base de datos de XML

### 6.- Sacar la entropia de los outputs y guardarla en un DF
