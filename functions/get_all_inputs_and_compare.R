


compare_inputs <- function(gcam_paths = c('Core' = 'C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata',
                                          'Release' = 'C:/Users/ignacio.delatorre/Documents/GCAM/Release/input/gcamdata'),
                           do_driver = c(FALSE, FALSE)){
  thisLoc <- getwd()

  extract_inputs <- function(gcam_path, do_driver = FALSE){
    setwd(gcam_path)
    setwd('R')

    files <- list.files()
    files_xml <- files[
      grepl('xml', files) &
        grepl(paste(c('zaglu','zenergy','zemissions','zwater','zsocio','zgcamusa'), collapse = "|"), files) &
        grepl("\\.R$", files)
    ]
    devtools::load_all()
    if (do_driver){
      driver_drake()
    }
    csvs_to_xml <- list()
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
    return(csvs_to_xml)
  }
  get_diffs <- function(all_inputs, names_gcams) {

    out <- list()

    core_name <- names_gcams[1]
    release_name <- names_gcams[2]

    core_data <- all_inputs[[core_name]]
    release_data <- all_inputs[[release_name]]

    get_metrics <- function(df) {

      if (is.null(df)) {
        return(list(n_zeros = NA_integer_, n_NA = NA_integer_))
      }

      num_df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]

      n_NA <- sum(is.na(df))

      n_zeros <- if (ncol(num_df) > 0) {
        sum(unlist(num_df) == 0, na.rm = TRUE)
      } else {
        0L
      }

      list(n_zeros = n_zeros, n_NA = n_NA)
    }

    for (chunk_name in names(core_data)) {

      df_names <- names(core_data[[chunk_name]])

      for (df_name in df_names) {

        df_core <- core_data[[chunk_name]][[df_name]]
        df_rel  <- release_data[[chunk_name]][[df_name]]

        identical_flag <- isTRUE(all.equal(df_core, df_rel, check.attributes = FALSE))

        core_metrics <- get_metrics(df_core)
        rel_metrics  <- get_metrics(df_rel)

        row <- list(
          chunk = chunk_name,
          dataframe = df_name,
          identical = identical_flag
        )

        # métricas dinámicas por nombre
        row[[paste0("n_zeros_", core_name)]] <- core_metrics$n_zeros
        row[[paste0("n_zeros_", release_name)]] <- rel_metrics$n_zeros
        row[[paste0("n_NA_", core_name)]] <- core_metrics$n_NA
        row[[paste0("n_NA_", release_name)]] <- rel_metrics$n_NA

        # nuevas columnas de consistencia
        row[["same_n_zeros"]] <- isTRUE(core_metrics$n_zeros == rel_metrics$n_zeros)
        row[["same_n_NA"]] <- isTRUE(core_metrics$n_NA == rel_metrics$n_NA)

        out[[length(out) + 1]] <- as.data.frame(row, stringsAsFactors = FALSE)
      }
    }

    do.call(rbind, out) %>% drop_na()
  }


  all_inputs <- list()
  names_gcams = names(gcam_paths)
  for (i in seq_along(gcam_paths)) {
    var_name <- paste0("csvs_to_xml_", names_gcams[i])
    csvs_to_xml <- extract_inputs(gcam_paths[i], do_driver = do_driver[i])
    all_inputs[[names_gcams[i]]] <- csvs_to_xml
  }

  comparison_results <- get_diffs(all_inputs, names_gcams)
  setwd(thisLoc)
  return(comparison_results)

}


















