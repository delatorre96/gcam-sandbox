library(DiagrammeR)
library(infotheo)
setwd("C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata")
devtools::load_all()
setwd('C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcamdata_trees')
source('functions_drawTree.R')
source('functions_manipulateCSV.R')

get_all_initial_files <- function(tree){
  all_initial_files <- unlist(lapply(tree, function(x) {
    inputs <- x$inputs
    inputs[grepl("/", inputs)]
  }))
  
  all_initial_files <- unname(all_initial_files)
  return(all_initial_files)
}


build_csv_relations <- function(tree,initial_files) {
  # Para almacenar pares de CSV relacionados
  relations <- data.frame(csv1 = character(0), csv2 = character(0), stringsAsFactors = FALSE)
  
  for (script_name in names(tree)) {
    inputs <- tree[[script_name]]$inputs
    csv_inputs <- inputs[inputs %in% initial_files]
    
    # Si hay al menos 2 CSV iniciales en los inputs del script, crear pares
    if (length(csv_inputs) > 1) {
      combos <- t(combn(csv_inputs, 2))  # pares sin repetición
      df_pairs <- data.frame(csv1 = combos[,1], csv2 = combos[,2], stringsAsFactors = FALSE)
      relations <- rbind(relations, df_pairs)
    }
  }
  
  # Opcional: quitar duplicados porque un par puede salir varias veces
  relations <- unique(relations)
  
  return(relations)
}

draw_csv_graph <- function(relations) {
  if (nrow(relations) == 0) {
    message("No hay relaciones para dibujar.")
    return(NULL)
  }
  
  # Generar aristas en formato "csv1" -> "csv2"
  edges <- sprintf('"%s" -- "%s"', relations$csv1, relations$csv2)  # -- para grafo no dirigido
  
  edges_str <- paste(unique(edges), collapse = ";\n  ")
  
  dot <- sprintf("graph CSV_Relations {\n  rankdir=LR;\n  %s;\n}", edges_str)
  
  gr <- grViz(dot)
  return(gr)
}

get_csv_info <- function(csv_file) {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE) 
  setwd(dir_csvs_iniciales)
  path <- find_csv_file(csv_file, optional = TRUE)
  lines <- readLines(path)
  header_lines <- lines[grepl("^#", lines)]
  df <- read.csv(path, comment.char = '#', check.names = FALSE)
  return(list(header_lines = header_lines, df = df, path = path))
}

clean_dbml_column_name <- function(name) {
  clean_name <- gsub("[^a-zA-Z0-9_]", "_", name)
  clean_name <- gsub("_+", "_", clean_name)
  clean_name <- gsub("^_|_$", "", clean_name)
  
  if (nchar(clean_name) == 0) {
    clean_name <- "col_unknown"
  }
  
  # Si empieza con número o contiene caracteres no permitidos, o era solo números, la dejamos entre comillas dobles
  needs_quotes <- FALSE
  if (grepl("^[0-9]", clean_name) || grepl("^[0-9]+$", clean_name)) {
    needs_quotes <- TRUE
  }
  
  # Además, si el original tenía espacios o caracteres especiales, usar comillas para seguridad
  if (grepl("[^a-zA-Z0-9_]", name)) {
    needs_quotes <- TRUE
  }
  
  if (needs_quotes) {
    # Comillas dobles, y escapamos si hubiera comillas internas
    clean_name <- paste0('"', gsub('"', '\\"', clean_name), '"')
  }
  
  return(clean_name)
}


clean_dbml_table_name <- function(name) {
  # Igual limpieza para tablas, que evita caracteres raros
  clean_name <- gsub("[^a-zA-Z0-9_]", "_", name)
  clean_name <- gsub("_+", "_", clean_name)
  clean_name <- gsub("^_|_$", "", clean_name)
  
  if (nchar(clean_name) == 0) {
    clean_name <- "table_unknown"
  }
  
  if (grepl("^[0-9]", clean_name) || grepl("^[0-9]+$", clean_name)) {
    clean_name <- paste0("table_", clean_name)
  }
  
  return(clean_name)
}

build_dbml_from_pairs <- function(pairs) {
  tables <- list()
  relations <- list()
  
  clean_name <- function(name) {
    gsub("[^a-zA-Z0-9_]", "_", basename(name))
  }
  
  escape_colname <- function(col) {
    if (grepl("^[0-9]", col) || grepl("[^a-zA-Z0-9_]", col)) {
      paste0("\"", col, "\"")
    } else {
      col
    }
  }
  
  get_dbml_type <- function(column) {
    cls <- class(column)[1]
    if (cls %in% c("integer", "integer64")) {
      return("integer")
    } else if (cls %in% c("numeric", "double")) {
      return("float")
    } else if (cls %in% c("logical")) {
      return("boolean")
    } else if (inherits(column, "Date")) {
      return("date")
    } else if (inherits(column, "POSIXct") || inherits(column, "POSIXlt")) {
      return("timestamp")
    } else {
      return("varchar")
    }
  }
  
  for (i in seq_len(nrow(pairs))) {
    csv1 <- pairs$csv1[i]
    csv2 <- pairs$csv2[i]
    
    df1 <- tryCatch(get_csv_info(csv1)$df, error = function(e) {
      message(sprintf("ERROR al cargar '%s': %s", csv1, e$message))
      return(NULL)
    })
    df2 <- tryCatch(get_csv_info(csv2)$df, error = function(e) {
      message(sprintf("ERROR al cargar '%s': %s", csv2, e$message))
      return(NULL)
    })
    
    if (is.null(df1) || is.null(df2)) {
      message(sprintf("Saltando par (%s, %s) por fallo en la carga.", csv1, csv2))
      next
    }
    
    csv1_clean <- clean_name(csv1)
    csv2_clean <- clean_name(csv2)
    
    if (!csv1_clean %in% names(tables)) {
      tables[[csv1_clean]] <- sapply(df1, get_dbml_type, USE.NAMES = TRUE)
    }
    if (!csv2_clean %in% names(tables)) {
      tables[[csv2_clean]] <- sapply(df2, get_dbml_type, USE.NAMES = TRUE)
    }
    
    common_cols <- intersect(colnames(df1), colnames(df2))
    
    if (length(common_cols) > 0) {
      for (col in common_cols) {
        relations[[length(relations) + 1]] <- list(
          from_table = csv1_clean,
          to_table = csv2_clean,
          column = col
        )
      }
    }
  }
  
  dbml <- ""
  
  for (tbl in names(tables)) {
    dbml <- paste0(dbml, "Table ", tbl, " {\n")
    for (col in names(tables[[tbl]])) {
      dbml <- paste0(dbml, "  ", escape_colname(col), " ", tables[[tbl]][col], "\n")
    }
    dbml <- paste0(dbml, "}\n\n")
  }
  
  seen_refs <- list()
  for (rel in relations) {
    from_tbl <- rel$from_table
    to_tbl <- rel$to_table
    col <- rel$column
    
    if (!(from_tbl %in% names(tables))) {
      warning(sprintf("Tabla '%s' no encontrada, omitiendo referencia.", from_tbl))
      next
    }
    if (!(to_tbl %in% names(tables))) {
      warning(sprintf("Tabla '%s' no encontrada, omitiendo referencia.", to_tbl))
      next
    }
    
    if (!(col %in% names(tables[[from_tbl]]))) {
      warning(sprintf("Columna '%s' no encontrada en tabla '%s', omitiendo referencia.", col, from_tbl))
      next
    }
    if (!(col %in% names(tables[[to_tbl]]))) {
      warning(sprintf("Columna '%s' no encontrada en tabla '%s', omitiendo referencia.", col, to_tbl))
      next
    }
    
    if (from_tbl == to_tbl) next
    
    ref_key <- paste(from_tbl, col, to_tbl, col, sep = "->")
    if (!(ref_key %in% seen_refs)) {
      dbml <- paste0(dbml, "Ref: ", from_tbl, ".", escape_colname(col), " > ", to_tbl, ".", escape_colname(col), "\n")
      seen_refs <- c(seen_refs, ref_key)
    }
  }
  
  return(list(dbml =  dbml, relations = relations, tables = tables))
}

count_related_columns_between_tables <- function(tables, relations) {
  # Todas las tablas para fila y columna
  all_tables <- names(tables)
  
  # Lista para almacenar resultados (cada fila: tabla1, tabla2, num_columns, columnas_comunes)
  results <- list()
  
  # Recorrer pares de tablas (incluyendo reverso para que salga todo)
  for (tbl1 in all_tables) {
    for (tbl2 in all_tables) {
      if (tbl1 == tbl2) next  # Opcional: saltar si no quieres comparar tabla consigo misma
      
      # Columnas relacionadas entre tbl1 y tbl2
      cols_relacionadas <- unique(unlist(lapply(relations, function(r) {
        (r$from_table == tbl1 && r$to_table == tbl2) || (r$from_table == tbl2 && r$to_table == tbl1)
      })))
      
      # Columnas reales que aparecen en relaciones de tbl1-tbl2
      cols <- unique(unlist(lapply(relations, function(r) {
        if ((r$from_table == tbl1 && r$to_table == tbl2) || (r$from_table == tbl2 && r$to_table == tbl1)) {
          return(r$column)
        } else return(NULL)
      })))
      
      results[[length(results)+1]] <- list(
        table_1 = tbl1,
        table_2 = tbl2,
        n_common_columns = length(cols),
        common_columns = paste(cols, collapse = ", ")
      )
    }
  }
  
  # Convertir a data.frame
  df <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors=FALSE))
  
  # Convertir n_common_columns a numérico
  df$n_common_columns <- as.numeric(df$n_common_columns)
  
  return(df)
}
count_related_columns_pairwise <- function(tables, relations) {
  all_tables <- names(tables)
  results <- list()
  
  for (tbl1 in all_tables) {
    for (tbl2 in all_tables) {
      if (tbl1 == tbl2) next
      
      # Filtrar relaciones entre tbl1 y tbl2 (en cualquier dirección)
      rels <- Filter(function(r) {
        (r$from_table == tbl1 && r$to_table == tbl2) || (r$from_table == tbl2 && r$to_table == tbl1)
      }, relations)
      
      # Extraer columnas comunes de esas relaciones
      cols <- unique(sapply(rels, function(r) r$column))
      
      # Calcular porcentaje sobre total columnas de tbl1 y tbl2 (usamos mínimo para simetría)
      n_common <- length(cols)
      n_total_1 <- length(tables[[tbl1]])
      n_total_2 <- length(tables[[tbl2]])
      related_pct <- ifelse(n_common == 0, 0, 100 * n_common / min(n_total_1, n_total_2))
      
      results[[length(results)+1]] <- list(
        table_1 = tbl1,
        table_2 = tbl2,
        related_columns_pct = related_pct
      )
    }
  }
  
  df <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors=FALSE))
  df$related_columns_pct <- as.numeric(df$related_columns_pct)
  
  return(df)
}

calc_mutual_info_matrix <- function(dfA, dfB) {
  # Discretizar variables si no están discretas (infotheo::mutinformation requiere discretas)
  # Aquí suponemos que las variables ya son categóricas o discretas, si no:
  # dfA_discrete <- discretize(dfA)
  # dfB_discrete <- discretize(dfB)
  
  colsA <- colnames(dfA)
  colsB <- colnames(dfB)
  
  mi_mat <- matrix(0, nrow = length(colsA), ncol = length(colsB),
                   dimnames = list(colsA, colsB))
  
  for (i in seq_along(colsA)) {
    for (j in seq_along(colsB)) {
      # Calcular MI, manejando NA
      x <- dfA[[i]]
      y <- dfB[[j]]
      # Quitar NA para el cálculo conjunto
      valid <- complete.cases(x, y)
      if (sum(valid) > 1) {
        mi <- mutinformation(x[valid], y[valid])
      } else {
        mi <- NA
      }
      mi_mat[i, j] <- mi
    }
  }
  return(mi_mat)
}

entropy <- function(x) {
  # Probabilidades de cada categoría
  probs <- table(x) / length(x)
  # Entropía en bits
  -sum(probs * log2(probs))
}

#######Tree#######
zenergyTree <- build_tree_module(modules = 'zenergy')
ancestors <- find_ancestors (tree = zenergyTree, target = 'transportation_UCD_CORE.xml')
initial_files <- get_all_files(ancestors, zenergyTree)

#######Get some connected files#######
files_BEV <- c()
for (i in initial_files){
  df <- data.frame(load_csv_files(i, optional = TRUE))
  if (any(grepl('BEV', as.matrix(df), ignore.case = TRUE))){
    files_BEV <- c(files_BEV, i)
  }
}
files_BEV <- unique(files_BEV)
files_BEV <- files_BEV[ !(files_BEV %in% c("energy/mappings/IEA_flow_sector", "Census_ind_VoS_state")) ]
files_BEV_filtered <- filter_csvs_by_exact_name(files_BEV, 'BEV')

######Relations between tables#######
pairs  <- build_csv_relations(tree = build_tree(), initial_files = files_BEV_filtered)
dbml_l <- build_dbml_from_pairs(pairs = pairs)
dbml <- dbml_l$dbml
relations <- dbml_l$relations
tables <- dbml_l$tables

#######Optimization#######

summary_relat <- as.data.frame(count_related_columns_pairwise(tables = tables, relations = relations) )
info_mutua <- calc_mutual_info_matrix(get_csv_info("energy/mappings/UCD_techs")$df, get_csv_info("energy/mappings/UCD_techs_revised")$df)



norm_res <- create_shared_columns_table(tables, relations, threshold_pct = 0.9)
tables_norm <- norm_res$tables
relations_norm <- norm_res$relations

summary_relat <- as.data.frame(count_related_columns_pct(tables = tables_norm, relations = relations_norm) )

writeLines(dbml, "dbml_output_norm.txt")
