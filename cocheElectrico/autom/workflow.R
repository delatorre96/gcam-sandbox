#setwd("C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/cocheElectrico/autom")
library(xml2)
source('Functions.R')
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
outfiles_names <- c("COST","SW","OUTPUT")

#Registramos la info inicial de los csv's antes del loop para reescribirla después

original_csvs <- list()
files_all <- c(files_tochangeCosts, files_tochangeshewt)
for (i in files_all) {
  l <- get_csv_info(i)
  original_csvs[[i]] <- list(
    path = l$path,
    header_lines = l$header_lines,
    df = l$df
  )
}


for (i in 1:n_iterations){
  ###############Change config file and output xml files###############
  modify_config_file(new_config_name = paste0("elecCar_", i), config_path = config_file)
  new_outfiles <- paste0("DB/", outfiles_names, "_", i, ".csv")
  change_xml_outputData(xml_path = batch_queries_file, new_outfiles)
  
  ###############Change csv's###############

  increase_value <- runif(1,-1,1)
  for (file in files_tochangeCosts){
    increase_csCosts(file,exact_name = 'MORDOR', approx_name = 'Capital costs', increase_value = increase_value)
  }
  year_convergence_mordor <- sample(c(2030, 2050, 2075, 2100), 1)
  convergence_value_mordor <- runif(1,0,1)
  initial_convergence_mordor <- runif(1,0, convergence_value_mordor) 
  
  year_convergence_bev <- sample(c(2030, 2050, 2075, 2100), 1)
  convergence_value_bev <- 1 - convergence_value_mordor
  initial_convergence_bev <- runif(1,0, convergence_value_bev) 
  
  change_shareWheights(files_tochangeshewt[1],
                       filter_name = 'MORDOR', 
                       convergence_value = convergence_value_mordor, 
                       year_convergence = year_convergence_mordor, 
                       initial_convergence = initial_convergence_mordor)
  change_shareWheights(files_tochangeshewt[1],
                       filter_name = c('BEV', 'trn_pass_road_LDV_4W'), 
                       convergence_value = convergence_value_bev, 
                       year_convergence = year_convergence_bev, 
                       initial_convergence = initial_convergence_bev)
  driver_drake()
  
  #run gcam
  run_gcam(run_gcam_file)
  #git restore . 
  setwd(gcam_path)
  #system2("git", args = c("restore", "."), stdout = "", stderr = "")
  for (csv_i in original_csvs){
    write_csv(header_lines = csv_i$header_lines, path = csv_i$path,df = csv_i$df)
  }
}

