search_string_in_csvs <-  function(string, dir_csvs = "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata/inst/extdata"){

  files <- list.files(
    path = dir_csvs,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  files_with_string <- sapply(files, function(f) {

    content <- tryCatch(
      readLines(f, warn = FALSE),
      error = function(e) return(character(0))
    )

    any(grepl(string, content, fixed = TRUE))
  })

  files_with_string <- names(files_with_string)[files_with_string]

  print(files_with_string)

  invisible(files_with_string)
}


search_string_in_csvs_year <- function(
    string = "year",
    dir_csvs = "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata/inst/extdata"
) {

  files <- list.files(
    path = dir_csvs,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  # 1. filtrar por presencia de "year"
  files_with_string <- vapply(files, function(f) {

    content <- tryCatch(
      readLines(f, warn = FALSE),
      error = function(e) return(character(0))
    )

    any(grepl(string, content, fixed = TRUE))

  }, logical(1))

  files <- files[files_with_string]

  # 2. filtrar por condición estricta: NO puede haber years < 2020
  files_year_2020 <- vapply(files, function(f) {

    df <- tryCatch(
      read.csv(f, comment.char = "#", stringsAsFactors = FALSE),
      error = function(e) return(NULL)
    )

    if (is.null(df)) return(FALSE)
    if (!"year" %in% names(df)) return(FALSE)

    yr <- suppressWarnings(as.numeric(df$year))
    yr <- yr[!is.na(yr)]

    # condición clave: no hay años menores a 2020
    all(yr >= 2020)

  }, logical(1))

  result <- files[files_year_2020]

  print(result)

  invisible(result)
}
