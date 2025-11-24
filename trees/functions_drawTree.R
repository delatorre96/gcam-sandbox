library(gcamdata)
library(DiagrammeR)
library(htmlwidgets)



set_gcam_paths <- function(gcamdata) {
  #Exmple:
  #dir_gcamdata <- "C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/input/gcamdata"
  dir_gcamdata <<- gcamdata
  dir_chunks <<- paste0(dir_gcamdata,'/R')
  dir_csvs_iniciales <<- paste0(dir_gcamdata,'/inst/extdata')
}

in_out_func_names <- function(path) {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE) 
  setwd(dir_chunks)
  # Crear un entorno aislado
  env <- new.env()

  # Definir las constantes que usa el código GCAM
  assign("driver.DECLARE_INPUTS", "driver.DECLARE_INPUTS", envir = env)
  assign("driver.DECLARE_OUTPUTS", "driver.DECLARE_OUTPUTS", envir = env)
  assign("driver.MAKE", "driver.MAKE", envir = env)

  # Cargar el script en ese entorno
  sys.source(path, envir = env)

  # Obtener solo el nombre del archivo
  file_name <- basename(path)

  # Buscar la función definida (asumimos una por archivo)
  fun_name <- ls(env, pattern = "^module_")
  if (length(fun_name) == 0) {
    warning(sprintf("No se encontró ninguna función module_ en %s", file_name))
    return(NULL)
  }

  fun <- get(fun_name[1], envir = env)

  # Ejecutar la función simulando los comandos de GCAM
  inputs <- tryCatch(fun(env$driver.DECLARE_INPUTS), error = function(e) NULL)
  outputs <- tryCatch(fun(env$driver.DECLARE_OUTPUTS), error = function(e) NULL)

  if (is.null(inputs)) inputs <- character(0)
  if (is.null(outputs)) outputs <- character(0)

  list(
    funcion = file_name,
    inputs = unname(inputs),
    outputs = unname(outputs)
  )
}



build_tree <- function(folder = ".") {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE) 
  setwd(dir_chunks)
  files <- list.files(folder, pattern = "\\.R$", full.names = TRUE)

  tree_list <- list()

  for (f in files) {
    res <- in_out_func_names(f)
    if (!is.null(res)) {
      tree_list[[res$funcion]] <- list(
        inputs = res$inputs,
        outputs = res$outputs
      )
    }
  }

  tree_list
}

build_tree_module <- function(folder = ".", modules) {
  dir_iniciar <- getwd()
  on.exit(setwd(dir_iniciar), add = TRUE)
  setwd(dir_chunks)
  
  files <- list.files(folder, pattern = "\\.R$", full.names = TRUE)
  if (length(modules) > 1) {
    pattern <- paste(modules, collapse = "|")
  } else {
    pattern <- modules
  }
  files <- files[grepl(pattern, files, ignore.case = TRUE)]
  
  tree_list <- list()
  
  for (f in files) {
    res <- in_out_func_names(f)
    if (!is.null(res)) {
      tree_list[[res$funcion]] <- list(
        inputs = res$inputs,
        outputs = res$outputs
      )
    }
  }
  
  tree_list
}

find_file_in_tree <- function(tree, target) {
  matches <- list(
    as_input = names(tree)[sapply(tree, function(x) target %in% x$inputs)],
    as_output = names(tree)[sapply(tree, function(x) target %in% x$outputs)]
  )
  
  if (length(matches$as_input) == 0 && length(matches$as_output) == 0) {
    message("No se encontró '", target, "' en ningún input ni output del tree.")
    return(NULL)
  }
  
  matches
}

draw_full_graph <- function(tree, output_path = paste0(target, "_GCAM_Full.html")) {
  edges <- character(0)

  for (func in names(tree)) {
    inputs <- tree[[func]]$inputs
    outputs <- tree[[func]]$outputs

    if (length(inputs) > 0) {
      edges <- c(edges, sprintf('"%s" -> "%s"', inputs, func))
    }
    if (length(outputs) > 0) {
      edges <- c(edges, sprintf('"%s" -> "%s"', func, outputs))
    }
  }

  if (length(edges) > 0) {
    edges_str <- paste(edges, collapse = ";\n  ")
    dot <- sprintf("digraph GCAM_Full {\n  rankdir=TB;\n  %s;\n}", edges_str)
  } else {
    dot <- "digraph GCAM_Full {\n  rankdir=TB;\n}"
  }

  gr <- DiagrammeR::grViz(dot)

  # Guardar directamente sin dejar archivos basura
  tmpfile <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(gr, tmpfile, selfcontained = TRUE)
  file.rename(tmpfile, output_path)

  # Abrir automáticamente en el navegador
  browseURL(output_path)

  invisible(gr)
}


draw_function_graph <- function(tree, output_path = paste0(target, "_GCAM_Functions.html")) {
  funcs <- names(tree)
  edges <- character(0)

  for (func in funcs) {
    inputs <- tree[[func]]$inputs
    for (other in funcs) {
      outputs_other <- tree[[other]]$outputs
      if (any(outputs_other %in% inputs)) {
        edges <- c(edges, sprintf('"%s" -> "%s"', other, func))
      }
    }
  }

  if (length(edges) > 0) {
    edges_str <- paste(edges, collapse = ";\n  ")
    dot <- sprintf("digraph GCAM_Functions {\n  rankdir=TB;\n  %s;\n}", edges_str)
  } else {
    dot <- "digraph GCAM_Functions {\n  rankdir=TB;\n}"
  }

  gr <- DiagrammeR::grViz(dot)

  tmpfile <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(gr, tmpfile, selfcontained = TRUE)
  file.rename(tmpfile, output_path)

  browseURL(output_path)

  invisible(gr)
}


find_ancestors <- function(tree, target, visited = character()) {
  if (target %in% visited) return(visited)

  parents <- names(tree)[sapply(tree, function(x) target %in% x$outputs)]
  if (length(parents) == 0) return(visited)

  visited <- unique(c(visited, parents))

  for (p in parents) {
    parent_inputs <- tree[[p]]$inputs
    for (inp in parent_inputs) {
      visited <- find_ancestors(tree, inp, visited)
    }
  }

  visited
}


draw_subgraph <- function(tree, target,
                          output_path = paste0(target, "_GCAM_Subgraph.html")) {

  # --- Si el target es un archivo R, lo tratamos como función ---
  if (grepl("\\.R$", target, ignore.case = TRUE)) {
    if (!(target %in% names(tree))) {
      message("El archivo especificado no está en el tree: ", target)
      return(NULL)
    }
    # Tomamos sus outputs como punto de partida
    start_points <- tree[[target]]$outputs
    if (length(start_points) == 0) {
      message("El archivo no tiene outputs declarados: ", target)
      return(NULL)
    }

    # Buscamos todos los ancestros de esos outputs
    ancestors <- unique(unlist(lapply(start_points, function(out) find_ancestors(tree, out))))
    ancestors <- unique(c(ancestors, target))

    # --- Extraer solo los ancestros que son scripts R ---
    r_ancestors <- ancestors[grepl("\\.R$", ancestors, ignore.case = TRUE)]
  } else {
    # --- Caso normal: target es un input/output ---
    ancestors <- find_ancestors(tree, target)
    r_ancestors <- character(0)
  }

  if (length(ancestors) == 0) {
    message("No se encontraron ancestros para ", target)
    return(NULL)
  }

  # --- Construcción de aristas ---
  edges <- character(0)
  funcs <- intersect(names(tree), ancestors)

  for (func in funcs) {
    inputs <- tree[[func]]$inputs
    for (other in funcs) {
      outputs_other <- tree[[other]]$outputs
      if (any(outputs_other %in% inputs)) {
        edges <- c(edges, sprintf('"%s" -> "%s"', other, func))
      }
    }
  }

  # Añadir target explícitamente si no está
  if (!(target %in% ancestors)) {
    edges <- c(edges, sprintf('"%s"', target))
  }

  # --- Construcción DOT ---
  if (length(edges) > 0) {
    edges_str <- paste(edges, collapse = ";\n  ")
    dot <- sprintf("digraph GCAM_Subgraph {\n  rankdir=TB;\n  %s;\n}", edges_str)
  } else {
    dot <- "digraph GCAM_Subgraph {\n  rankdir=TB;\n}"
  }

  # --- Generar HTML limpio ---
  gr <- DiagrammeR::grViz(dot)
  tmpfile <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(gr, tmpfile, selfcontained = TRUE)
  file.rename(tmpfile, output_path)
  browseURL(output_path)


  ancestors

}
find_file_in_scripts <- function(tree, target_file) {
  # Buscar en qué scripts aparece como input o output
  scripts_with_input  <- names(tree)[sapply(tree, function(x) target_file %in% x$inputs)]
  scripts_with_output <- names(tree)[sapply(tree, function(x) target_file %in% x$outputs)]
  
  # Combinar resultados
  results <- list(
    as_input  = scripts_with_input,
    as_output = scripts_with_output,
    all = unique(c(scripts_with_input, scripts_with_output))
  )
  
  # Si no se encuentra en ningún script
  if (length(results$all) == 0) {
    message("El archivo '", target_file, "' no se encontró en ningún script.")
    return(NULL)
  }
  
  return(results)
}

get_all_files <- function(ancestors, tree){
  all_inputs <- c()
for (file in ancestors) {
  inputs <- tree[[file]]$inputs
  inputs <- inputs[grepl('/', inputs)]
  all_inputs <- c(all_inputs, inputs)
}
all_inputs
}


get_all_initial_files <- function(tree){
  all_initial_files <- unlist(lapply(tree, function(x) {
    inputs <- x$inputs
    inputs[grepl("/", inputs)]
  }))
  
  all_initial_files <- unname(all_initial_files)
  return(all_initial_files)
}

revisar_csvs <- function(csvs) {
  repeat {
    cat('###### Lista de csvs a revisar ######\n')
    cat('Tu lista tiene', length(csvs), 'csvs:\n')
    for (i in seq_along(csvs)) {
      cat(i, ':', csvs[i], '\n')
    }
    cat("Ingresa un valor para ver cada csv. Para salir, ingresa 0\n")
    entrada <- readline()
    if (entrada == "0") {
      cat("Fin de la entrada.\n")
      break
    }
    
    idx <- as.numeric(entrada)
    if (is.na(idx) || idx < 1 || idx > length(csvs)) {
      cat("Entrada inválida, intenta de nuevo.\n")
      next
    }
    
    df <<- data.frame(load_csv_files(csvs[idx], optional = TRUE))
    #View(df)
    #cat("Data frame cargado y asignado a 'df'.\n")
  }
}






##################################################### OTHERS

draw_full_graphviz_txt <- function(tree, path_out) {
  dot <- "digraph GCAM_Full {\n  rankdir=TB;\n"

  for (func in names(tree)) {
    inputs <- tree[[func]]$inputs
    outputs <- tree[[func]]$outputs

    for (inp in inputs) {
      dot <- paste0(dot, sprintf('  "%s" -> "%s";\n', inp, func))
    }
    for (out in outputs) {
      dot <- paste0(dot, sprintf('  "%s" -> "%s";\n', func, out))
    }
  }

  dot <- paste0(dot, "}\n")

  # Guardar en un archivo de texto
  writeLines(dot, con = path_out)
  message(sprintf("Archivo DOT guardado en: %s", normalizePath(path_out)))
}


draw_function_graphviz_txt <- function(tree, path_out) {
  dot <- "digraph GCAM_Functions {\n  rankdir=TB;\n"

  funcs <- names(tree)

  for (func in funcs) {
    inputs <- tree[[func]]$inputs

    # Encontrar qué funciones producen esos inputs
    for (other in funcs) {
      outputs_other <- tree[[other]]$outputs
      if (any(outputs_other %in% inputs)) {
        dot <- paste0(dot, sprintf('  "%s" -> "%s";\n', other, func))
      }
    }
  }

  dot <- paste0(dot, "}\n")

  # Guardar en un archivo de texto
  writeLines(dot, con = path_out)
  message(sprintf("Archivo DOT guardado en: %s", normalizePath(path_out)))
}


