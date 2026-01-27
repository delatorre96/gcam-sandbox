source('C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/trees/functions_drawTree.R')
source('C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/trees/functions_manipulateCSV.R')
gcam_path ='C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core'
set_gcam_paths(gcam_path)
setwd(dir_gcamdata)
devtools::load_all()

library(dplyr)
library(tidyr)

l <- get_csv_info('socioeconomics/SSP/SSP_database_2025')
df = l$df
headers <- l$header_lines
path <- paste0(dir_gcamdata,'/inst/extdata/',l$path)

# Paso 1: Guardar valores 2020 de Historical Reference, clave por columnas identificadoras menos Scenario
historical_2020 <- df %>%
  filter(Scenario == "Historical Reference") %>%
  select(Model, Region, Variable, Unit, `2020`) %>%
  rename(historical_2020 = `2020`)

# Paso 2: Crear copia para modificar
df_mod <- df

# Paso 3: Eliminar datos 2020 y 2025 de Historical Reference (poner NA)
df_mod <- df_mod %>%
  mutate(
    `2025` = ifelse(Scenario == "Historical Reference", NA, `2025`)
    # No tocar 2020 para Historical Reference
  )
# Paso 4: Para los escenarios SSP (no Historical Reference), reemplazar 2020 con datos copiados

# Construimos índice para buscar los valores correctos, basándonos en Model, Region, Variable y Unit
library(data.table)
setDT(df_mod)
setDT(historical_2020)

# Filtrar SSPs
ssp_idx <- df_mod[Scenario != "Historical Reference"]

# Actualizar 2020 en ssp_idx con historical_2020, usando keys para join exacto
setkey(historical_2020, Model, Region, Variable, Unit)
setkey(ssp_idx, Model, Region, Variable, Unit)

ssp_idx[historical_2020, `2020` := i.historical_2020]

# Volver a unir las partes modificadas
df_mod[Scenario != "Historical Reference", `2020` := ssp_idx$`2020`]

# Paso 5: Reescribir CSV

con <- gzfile(path, "w")

writeLines(headers, con)  # Escribir las líneas header

write.table(df_mod, con, sep = ",", row.names = FALSE, quote = TRUE, na = "", col.names = TRUE)

close(con)




######################## Buscar variable:
variable <- 'gather_years'

carpeta_scripts <- "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/input/gcamdata/R"

# Lista
archivos <- list.files(path = carpeta_scripts, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)

# Buscar la variable
archivos_con_variable <- sapply(archivos, function(f) {
  contenido <- readLines(f, warn = FALSE)
  any(grepl(variable, contenido))
})

# Archivos
archivos_con_variable <- names(archivos_con_variable)[archivos_con_variable]

print(archivos_con_variable)



######################## Tree  land
treeZaglu <- build_tree_module(modules = 'zaglu')
ances <- find_ancestors(treeZaglu, target = 'L125.LC_bm2_R')
draw_subgraph(treeZaglu,'L125.LC_bm2_R')

for (i in ances){
  print(i)
  link <- paste0('R/',i)
  lines <- readLines(link)
  doc_lines <- grep("^#'", lines, value = TRUE)
  cat(paste(doc_lines, collapse = "\n"))
}

for (i in 1:length(HDDCDD_data_list)) {
  if ("2021" %in% unique(colnames(HDDCDD_data_list[[i]]))){
    print(TRUE)
  }

}
