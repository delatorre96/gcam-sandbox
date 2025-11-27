thisScript_path <- getwd()
library(xml2)
library(readr)
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
  print(paste0('########################################## iteración ',i,' ##########################################'))
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
  #system2("git", args = c("restore", "."), stdout = "", stderr = "")
  for (file in new_outfiles) {
    outfile_path <- paste0(dir_gcam,"/exe/", file)
    outfile_df <- read.csv(outfile_path, skip = 1)  
    # Filtrar filas según condiciones
    outfile_df_filtered <- outfile_df[
      outfile_df$technology %in% c('MORDOR', 'BEV') &
        outfile_df$sector == 'trn_pass_road_LDV_4W' &
        outfile_df$subsector == 'Car', 
    ]
    
    if (grepl("COST", file)) {
      if (i == 1) {
        write.csv(outfile_df_filtered, paste0(thisScript_path,"/csvs/costs.csv"), row.names = FALSE)
      } else {
        write.table(outfile_df_filtered, paste0(thisScript_path,"/csvs/costs.csv"), row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
      }
    }
    if (grepl("SW", file)) {
      if (i == 1) {
        write.csv(outfile_df_filtered,  paste0(thisScript_path,"/csvs/sharewheights.csv"), row.names = FALSE)
      } else {
        write.table(outfile_df_filtered,  paste0(thisScript_path,"/csvs/sharewheights.csv"), row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
      }
    }
    if (grepl("OUTPUT", file)) {
      if (i == 1) {
        write.csv(outfile_df_filtered,  paste0(thisScript_path,"/csvs/output.csv"), row.names = FALSE)
      } else {
        write.table(outfile_df_filtered,  paste0(thisScript_path,"/csvs/output.csv"), row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
      }
    }
  }
  
  for (csv_i in original_csvs){
    write_csv(header_lines = csv_i$header_lines, path = paste0('inst/extdata/',csv_i$path),df = csv_i$df)
  }
  
  file.remove(paste0(dir_gcam, "/exe/debugelecCar_", i,".xml"))
  file.remove(paste0(dir_gcam, "/exe/DB/COST_", i,".csv"))  
  file.remove(paste0(dir_gcam, "/exe/DB/SW_", i,".csv"))
  file.remove(paste0(dir_gcam, "/exe/DB/OUTPUT_", i,".csv"))
}

