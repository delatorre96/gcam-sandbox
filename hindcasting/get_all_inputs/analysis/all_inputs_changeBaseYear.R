#### GetInputs #####


thisLoc <- getwd()

path2save <-"C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/hindcasting/get_all_inputs/analysis"
gcam_path <- 'C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata'

if (thisLoc != path2save){
  thisLoc = path2save
  setwd(thisLoc)
}
source('../all_inputs.R')

setwd(gcam_path)
setwd('R')

files <- list.files()

files_xml <- files[
  grepl('xml', files) & 
    grepl(paste(c('zaglu','zenergy','zemissions','zwater'), collapse = "|"), files)
]

setwd(thisLoc)
csvs_to_xml <- get_allData(gcam_path = gcam_path, do_driver = FALSE, save_input_data = TRUE, path2save = path2save, files_xml = files_xml)

##### Analyze inputs #####
setwd(thisLoc)
if (!('csvs_to_xml' %in% ls())){
  load('csvs_to_xml.RData')
}


results <- analyze_year_columns(csvs_to_xml)
results_only_years <- results[results$col_year == TRUE & results$contains_year == TRUE,]

unique_col_years <- results_only_years  %>% select(min_year, max_year) %>% distinct()
unique_contains_years <- results_only_years  %>% select(min_contains_year, max_contains_year) %>% distinct()

