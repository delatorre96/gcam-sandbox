load('csvs_to_xml_2021.RData')
csvs_to_xml_2021 <- csvs_to_xml
mapping_treatment_df <- read.csv('mapping_dataframes_years.csv')

summary_treatments <- mapping_treatment_df %>%
  group_by(chunk, dataframe) %>%
  summarise(
    treatments = paste(unique(treatment), collapse = ", "),
    n_columns = n(),
    .groups = "drop"
  ) %>%
  arrange(chunk, dataframe)
##################### LOL #####################

new_chunks_2010 <- list()
for (chunk_i in names(csvs_to_xml_2021)){
  chunk <- csvs_to_xml_2021[[chunk_i]]
  new_chunks_2010[[chunk_i]] <- list()
  for (df_i in names(chunk)){
    df_ <- csvs_to_xml_2021[[chunk_i]][[df_i]]
    if (df_i %in% mapping_treatment_df$dataframe){
    treatment_df <- mapping_treatment_df[mapping_treatment_df$dataframe == df_i,]
    type_of_treatment <- treatment_df$treatment
    name_col_treatment <- treatment_df$name_col_year
    missing_in_2010 <- treatment_df$missing_in_2010 
    missing_in_2021 <- treatment_df$missing_in_2021
    
    if (type_of_treatment == 'copy_df'){
      new_chunks_2010$chunk_i[[df_i]] <- df_
    }
    else if(type_of_treatment == 'swap_years'){
      #Intercambiar las filas con los años de missing_in_2021 en el data frame df_ en name_col_treatment por las filas con los años de missing_in_2010
      #si en las nuevas filas hay valores numéricos, habrá que intepolar
      new_chunks_2010$chunk_i[[df_i]] <- df_
      
    }
    else if(type_of_treatment == 'trim_years'){
      #Recortar. Eliminar las filas con los años que aparecen en missing_in_2010 en nuestro en el data frame df_
      new_chunks_2010$chunk_i[[df_i]] <- df_
      
    }
    else if(type_of_treatment == 'fill_missing_years'){
      # Añadir filas en nuestro dataframe df_ para que incluya los años que aparecen en missing_in_2021. Todo idéntico, varaibles no numéricos iguales y los numéricos elaborar interpolación 
      new_chunks_2010$chunk_i[[df_i]] <- df_
      
    }
    }else{
      #Meter el data frame tal cual en item chunk en item dataframe en la lista new_chunks_2010
      new_chunks_2010$chunk_i[[df_i]] <- df_
    }
    
  }
}



