get_allData <- function(gcam_path, do_driver = FALSE, save_input_data = FALSE){
  dir_gcam <- gcam_path
  dir_gcamdata <- paste0(dir_gcam,'/input/gcamdata')
  dir_chunks <- paste0(dir_gcamdata,'/R')
  dir_csvs_iniciales <- paste0(dir_gcamdata,'/inst/extdata')
  setwd(dir_gcamdata)
  devtools::load_all()
  if (do_driver){
    driver_drake()
  }

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
  if (save_input_data){
    save(csvs_to_xml, file = paste0(actual_dir,"/csvs_to_xml.RData"))
  }
  return(csvs_to_xml)
}


