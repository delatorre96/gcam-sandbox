
search_String <- function(string, dir_chunks = "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata/R"){

  files <- list.files(path = dir_chunks, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  
  files_with_string <- sapply(files, function(f) {
    content <- readLines(f, warn = FALSE)
    any(grepl(string, content))
  })
  
  # files
  files_with_string <- names(files_with_string)[files_with_string]
  
  print(files_with_string)
}

#Example
#string <- 'aglu.MODEL_PRICE_YEARS'
#dir_chunks <- "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata/R"