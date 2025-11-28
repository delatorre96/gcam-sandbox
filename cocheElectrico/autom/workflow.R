thisScript_path <- getwd()
library(xml2)
library(readr)
source('Functions.R')
##get all paths from gcam folder
gcam_path <- "C:/GCAM/Nacho/gcam_8.2"
set_gcam_paths(gcam_path)
setwd(dir_gcamdata)
devtools::load_all()



n_iterations = 100
files_tochangeCosts <- c('energy/OTAQ_trn_data_EMF37',
                         #"energy/UCD_trn_data_SSP1",
                         #"energy/UCD_trn_data_SSP3",
                         #"energy/UCD_trn_data_SSP5", 
                         'energy/UCD_trn_data_CORE')

files_tochangeshewt <- c('energy/A54.globaltranTech_shrwt_revised')

outfiles_names <- c("COST","SW","OUTPUT")

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


for (i in 1:n_iterations){
  print(paste0('########################################## iteration ',i,' ##########################################'))
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
  
  ###############Run data system and Gcam
  driver_drake()
  
  #run gcam
  run_gcam(run_gcam_file)
  
  ############### SAVE RESULTS
  #####Save outputs
  for (file in new_outfiles) {
    outfile_path <- paste0(dir_gcam,"/exe/", file)
    outfile_df <- read.csv(outfile_path, skip = 1)  
    scenario <- unique(outfile_df$scenario) #Save a scenario column to output
    # Filtrar filas según condiciones
    outfile_df_filtered <- outfile_df[
      outfile_df$technology %in% c('MORDOR', 'BEV') &
        outfile_df$sector == 'trn_pass_road_LDV_4W' &
        outfile_df$subsector == 'Car', 
    ]
    
    if (grepl("COST", file)) {
      if (i == 1) {
        write.csv(outfile_df_filtered, paste0(thisScript_path,"/csvs/outputs/costs.csv"), row.names = FALSE)
      } else {
        write.table(outfile_df_filtered, paste0(thisScript_path,"/csvs/outputs/costs.csv"), row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
      }
    }
    if (grepl("SW", file)) {
      if (i == 1) {
        write.csv(outfile_df_filtered,  paste0(thisScript_path,"/csvs/outputs/sharewheights.csv"), row.names = FALSE)
      } else {
        write.table(outfile_df_filtered,  paste0(thisScript_path,"/csvs/outputs/sharewheights.csv"), row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
      }
    }
    if (grepl("OUTPUT", file)) {
      if (i == 1) {
        write.csv(outfile_df_filtered,  paste0(thisScript_path,"/csvs/outputs/output.csv"), row.names = FALSE)
      } else {
        write.table(outfile_df_filtered,  paste0(thisScript_path,"/csvs/outputs/output.csv"), row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
      }
    }
  }
  ####### Save inputs
  df_OTAQ_trn_data_EMF37 <- get_csv_info("energy/OTAQ_trn_data_EMF37")$df
  df_OTAQ_trn_data_EMF37 <- df_OTAQ_trn_data_EMF37[(df_OTAQ_trn_data_EMF37$UCD_technology %in% c('BEV','MORDOR'))&
                                                     df_OTAQ_trn_data_EMF37$mode == "LDV_4W" &
                                                     grepl("capital costs", df_OTAQ_trn_data_EMF37$variable, ignore.case = TRUE), ]
  #Save a scenario column 
  df_OTAQ_trn_data_EMF37$'scenario' = scenario
  #df_UCD_trn_data_CORE <- get_csv_info("energy/UCD_trn_data_CORE")$df
  #df_UCD_trn_data_CORE <- df_UCD_trn_data_CORE[(df_UCD_trn_data_CORE$UCD_technology %in% c('BEV','MORDOR'))&
  #                                               df_UCD_trn_data_CORE$mode == "LDV_4W" &
  #                                                   grepl("capital costs", df_UCD_trn_data_CORE$variable, ignore.case = TRUE), ]
  df_A54.globaltranTech_shrwt_revised <- get_csv_info("energy/A54.globaltranTech_shrwt_revised")$df
  df_A54.globaltranTech_shrwt_revised <- df_A54.globaltranTech_shrwt_revised[df_A54.globaltranTech_shrwt_revised$tranTechnology %in% c('BEV', 'MORDOR') & 
                                                                               (df_A54.globaltranTech_shrwt_revised$supplysector == 'trn_pass_road_LDV_4W'), ]
  #Save a scenario column 
  df_A54.globaltranTech_shrwt_revised$'scenario' = scenario
  
  file1 <- paste0(thisScript_path, "/csvs/inputs/OTAQ_trn_data_EMF37.csv")
  file2 <- paste0(thisScript_path, "/csvs/inputs/A54.globaltranTech_shrwt_revised.csv")
  
  if (!file.exists(file1)) {
    write.csv(df_OTAQ_trn_data_EMF37, file1, row.names = FALSE)
  } else {
    write.table(df_OTAQ_trn_data_EMF37, file1,
                append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  }
  
  if (!file.exists(file2)) {
    write.csv(df_A54.globaltranTech_shrwt_revised, file2, row.names = FALSE)
  } else {
    write.table(df_A54.globaltranTech_shrwt_revised, file2,
                append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  }
  ##############Rewrite original csvs
  for (csv_i in original_csvs){
    write_csv(header_lines = csv_i$header_lines, path = paste0('inst/extdata/',csv_i$path),df = csv_i$df)
  }
  
  ############# Delete temporal files
  file.remove(paste0(dir_gcam, "/exe/debugelecCar_", i,".xml"))
  file.remove(paste0(dir_gcam, "/exe/DB/COST_", i,".csv"))  
  file.remove(paste0(dir_gcam, "/exe/DB/SW_", i,".csv"))
  file.remove(paste0(dir_gcam, "/exe/DB/OUTPUT_", i,".csv"))
}

