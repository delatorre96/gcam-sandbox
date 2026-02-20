get_allData <- function(gcam_path, dir_to_save){
  dir_gcam <- gcam_path
  dir_gcamdata <- paste0(gcam_path,'/input/gcamdata')
  dir_chunks <- paste0(dir_gcamdata,'/R')
  dir_csvs_iniciales <- paste0(dir_gcamdata,'/inst/extdata')
  setwd(dir_gcamdata)
  devtools::load_all()
  driver_drake()
  setwd(dir_chunks)
  csvs_to_xml <- list()
  
  files_xml <- list.files(pattern = "xml", ignore.case = TRUE)
  files_xml <- files_xml[!grepl("zgcamusa", files_xml, ignore.case = TRUE)]
  files_xml <- files_xml[files_xml != "xml.R"]
  files_xml <- files_xml[files_xml != "xml"]
  
  for (file in files_xml){
    if (!(file %in% names(csvs_to_xml))){
      env <- new.env()
      source(file, local = env)
      r_objects <- ls(env)
      module <- r_objects[grep("module", r_objects)]
      all_data <- load_from_cache(inputs_of(module))
      csvs_to_xml[[file]] <- all_data
    }
  }
  save(csvs_to_xml, file = paste0(dir_to_save,"/csvs_to_xml.RData"))
  return(csvs_to_xml)
}

csvs_to_xml <- get_allData(gcam_path = 'C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core', 
            dir_to_save = 'C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcast_csvs_diff/workingDir')


for i 
