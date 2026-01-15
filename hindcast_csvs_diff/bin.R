gcam_path <- "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core"
set_gcam_paths(gcam_path)
setwd(dir_gcamdata)
devtools::load_all()

set_gcam_paths <- function(gcam_path) {
  #Exmple:
  #dir_gcamdata <- "C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata"
  dir_gcam <<- gcam_path
  dir_gcamdata <<- paste0(gcam_path,'/input/gcamdata')
  dir_chunks <<- paste0(dir_gcamdata,'/R')
  dir_csvs_iniciales <<- paste0(dir_gcamdata,'/inst/extdata')
}

get_csv_info <- function(csv_file) {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE) 
  setwd(dir_csvs_iniciales)
  path <- find_csv_file(csv_file, optional = TRUE)
  lines <- readLines(path)
  header_lines <- lines[grepl("^#", lines)]
  df <- read.csv(path, comment.char = '#', check.names = FALSE)
  return(list(header_lines = header_lines, df = df, path = paste0(dir_gcamdata,'/inst/extdata/',path)))
}


files_replaceIncol = c('emissions/EPA/EPA_2019_MACC_Ag_updated_baseline',
                       'emissions/EPA/EPA_2019_MACC_raw',
                       'socioeconomics/income_shares'
                       #"gcam-usa/emissions/MOVES_source_type_pop"
                       )
for (file in files_replaceIncol) {
  l <- get_csv_info(file)
  df <- l$df
  path <- l$path
  headers <- l$header_lines
  
  has_2020 <- 2020 %in% df$year
  has_2021 <- 2021 %in% df$year
  
  if (has_2020 & has_2021) {
    # Si hay ambos, eliminas las filas con 2020
    df <- df[df$year != 2020, ]
  } else if (has_2020 & !has_2021) {
    # Si hay 2020 pero no 2021, cambias 2020 a 2021
    df$year[df$year == 2020] <- 2021
  }
  
  writeLines(headers, path)
  suppressWarnings(
    write.table(df, path, sep = ",", append = TRUE,
                row.names = FALSE, quote = TRUE, na = "", col.names = TRUE)
  )
}

files_col= c('energy/A44.CalPrice_bld')

for (file in files_col){
  
  l <- get_csv_info (file)
  df <- l$df
  path <-l$path
  headers <- l$header_lines
  names(df)[names(df) == 2020] <- 2021
  
  writeLines(headers, path)
  suppressWarnings(
    write.table(df, path, sep = ",", append = TRUE, 
                row.names = FALSE, quote = TRUE, na = "", col.names = TRUE)
  )
  
  }

