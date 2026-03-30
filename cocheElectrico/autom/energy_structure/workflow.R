thisScript_path <- getwd()
library(xml2)
library(readr)
source('transform_queries.R')
source('../Functions.R')

outfiles_names <- create_queries_autom(xml_file_queries = 'queries.xml', path = 'C:/GCAM/Nacho/gcam_8.2/exe/batch_queries')
outfiles_names_csv <- paste0(thisScript_path,'/csv/',outfiles_names, '.csv')


##get all paths from gcam folder
gcam_path <- "C:/GCAM/Nacho/gcam_8.2"
set_gcam_paths(gcam_path)
setwd(dir_gcamdata)
devtools::load_all()

n_iterations = 1000
files_tochangeCosts <- c('energy/OTAQ_trn_data_EMF37',
                         #"energy/UCD_trn_data_SSP1",
                         #"energy/UCD_trn_data_SSP3",
                         #"energy/UCD_trn_data_SSP5", 
                         'energy/UCD_trn_data_CORE')

files_tochangeshewt <- c('energy/A54.globaltranTech_shrwt_revised')

#Registramos la info inicial de los csv's antes del loop para reescribirla después

original_csvs <- list()
infiles <- c(files_tochangeCosts, files_tochangeshewt)
for (i in infiles) {
  l <- get_csv_info(i)
  original_csvs[[i]] <- list(
    path = l$path,
    header_lines = l$header_lines,
    df = l$df
  )
}

create_xmldb_batch(
  filenames = outfiles_names,
  xml_path = batch_queries,
  csv_path = paste0(dir_gcam,"/exe/DB"),
  out_file = paste0(batch_queries,"/xmldb_batch.xml")
)

run_iteration <- function(i, original_csvs) {
  
  on.exit({
    for (name in names(original_csvs)) {
      csv_i <- original_csvs[[name]]
      write_gcam_csv(
        header_lines = csv_i$header_lines,
        path = paste0('inst/extdata/', csv_i$path),
        df = csv_i$df
      )
    }
    message(paste0("CSV restored after iteration ", i))
  }, add = FALSE)
  print(paste0('########################################## iteration ',i,' ##########################################'))
  ###############Change config file and output xml files###############
  modify_config_file(new_config_name = paste0("elecCar_", i), config_path = config_file)

  
  
  ###############Change csv's###############
  
  increase_value <- runif(1,-1,1)
  for (file in files_tochangeCosts){
    increase_csCosts(file,exact_name = 'BEV', approx_name = 'Capital costs', increase_value = increase_value)
  }
  increase_value <- runif(1,-1,1)
  for (file in files_tochangeCosts){
    increase_csCosts(file,exact_name = 'BEV_2', approx_name = 'Capital costs', increase_value = increase_value)
  }
  year_convergence_mordor <- sample(c(2030, 2050, 2075, 2100), 1)
  year_convergence_bev <- sample(c(2030, 2050, 2075, 2100), 1)
  
  # convergence_value_mordor <- runif(1,0,1)
  # convergence_value_bev <- 1 - convergence_value_mordor
  
  vals <- rgamma(2, shape = 1)
  vals <- vals / sum(vals)
  convergence_value_mordor <- vals[1]
  convergence_value_bev <- vals[2]
  
  initial_convergence_mordor <- runif(1,0, convergence_value_mordor)
  initial_convergence_bev <- runif(1,0, convergence_value_bev) 
  
  change_shareWheights(files_tochangeshewt[1],
                       filter_name = 'BEV_2',
                       convergence_value = convergence_value_mordor,
                       year_convergence = year_convergence_mordor,
                       initial_convergence = initial_convergence_mordor)
  change_shareWheights(files_tochangeshewt[1],
                       filter_name = c('BEV', 'trn_pass_road_LDV_4W'), 
                       convergence_value = convergence_value_bev, 
                       year_convergence = year_convergence_bev, 
                       initial_convergence = initial_convergence_bev)
  
  
  ###############Run data system and Gcam
  driver_drake()
  
  #run gcam
  run_gcam(run_gcam_file)
  
  ############### SAVE RESULTS
  for (file in outfiles_names) {
    
    outfile_path <- paste0(dir_gcam,"/exe/DB/", file,'.csv')
    outfile_csv  <- paste0(thisScript_path,'/csvs/',file, '.csv')
    if (!file.exists(outfile_path) || file.size(outfile_path) == 0) {
      message(paste("Archivo vacío o inexistente:", file, "- se omite"))
      next
    }
    
    outfile_df <- read.csv(outfile_path, skip = 1)
    if (nrow(outfile_df) == 0) {
      message(paste("Data frame vacío:", file, "- se omite"))
      next
    }
    
    if (!file.exists(outfile_csv)){
      write.csv(outfile_df, outfile_csv, row.names = FALSE)
    } else {
      write.table(outfile_df, outfile_csv,
                  row.names = FALSE,
                  col.names = FALSE,
                  append = TRUE,
                  sep = ",")
    }
    file.remove(paste0(dir_gcam, "/exe/debugelecCar_", i,".xml"))
    file.remove(paste0(dir_gcam, "/exe/DB/", file,".csv"))
  }
}


for (i in 1:n_iterations){
  run_iteration(i, original_csvs)
}

