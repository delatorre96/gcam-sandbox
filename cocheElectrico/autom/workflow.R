source('Functions.R')
##Path gcamdata
gcam_path <- "C:/GCAM/Nacho/gcam_8.2"
set_gcam_paths(gcam_path)
setwd(dir_gcamdata)
devtools::load_all()


n_iterations = 100
for (i in seq(1, n_iterations)){
  ###############Change csv's###############
  files_tochangeCosts <- c('energy/OTAQ_trn_data_EMF37',
                           "energy/UCD_trn_data_SSP1",
                           "energy/UCD_trn_data_SSP3",
                           "energy/UCD_trn_data_SSP5", 
                           'energy/UCD_trn_data_CORE')
  
  files_tochangeshewt <- c('energy/A54.globaltranTech_shrwt_revised')
  
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
                       filter_name = 'BEV', 
                       convergence_value = convergence_value_bev, 
                       year_convergence = year_convergence_bev, 
                       initial_convergence = initial_convergence_bev)
  
  
}
