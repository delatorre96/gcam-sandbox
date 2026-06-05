library(dplyr)
library(purrr)
library(tibble)

compare_csvs_to_xml <- function(csvs_to_xml_2010,
                                csvs_to_xml_2021) {

  #----------------------------------------------------------
  # Función auxiliar
  #----------------------------------------------------------

  count_problematic_values <- function(df) {

    # Solo columnas numéricas
    num_df <- df %>%
      select(where(is.numeric))

    # Si no hay columnas numéricas
    if (ncol(num_df) == 0) {
      return(
        tibble(
          ceros = 0,
          inconsistencias = 0
        )
      )
    }

    # Matriz numérica
    mat <- as.matrix(num_df)

    tibble(
      ceros = sum(mat == 0, na.rm = TRUE),

      inconsistencias = sum(
        is.na(mat) |
          is.nan(mat) |
          is.infinite(mat)
      )
    )
  }

  #----------------------------------------------------------
  # Comparación principal
  #----------------------------------------------------------

  comparison_df <- map_dfr(
    names(csvs_to_xml_2010),

    function(chunk_name) {

      chunk_2010 <- csvs_to_xml_2010[[chunk_name]]
      chunk_2021 <- csvs_to_xml_2021[[chunk_name]]

      df_names <- names(chunk_2010)

      map_dfr(
        df_names,

        function(df_name) {

          df_2010 <- chunk_2010[[df_name]]
          df_2021 <- chunk_2021[[df_name]]

          stats_2010 <- count_problematic_values(df_2010)
          stats_2021 <- count_problematic_values(df_2021)

          tibble(
            chunk = chunk_name,
            dataframe = df_name,

            ceros_2010 = stats_2010$ceros,
            ceros_2021 = stats_2021$ceros,

            NAs_2010 = stats_2010$inconsistencias,
            NAs_2021 = stats_2021$inconsistencias
          )
        }
      )
    }
  )

  return(comparison_df)
}
