library(dplyr)
library(tidyr)
library(ggplot2)


has_valid_coordinates <- function(dta) {
  all(c("lat", "lon") %in% names(dta)) &&
    any(!is.na(suppressWarnings(as.numeric(dta$lat))) &
          !is.na(suppressWarnings(as.numeric(dta$lon))))
}

read_uploaded_data <- function(path, filename) {
  if (grepl("\\.csv$", filename, ignore.case = TRUE)) {
    data.table::fread(
      path,
      sep = ";",
      data.table = FALSE,
      check.names = FALSE
    )
  } else if (grepl("\\.xls$|\\.xlsx$", filename, ignore.case = TRUE)) {
    as.data.frame(readxl::read_excel(path))
  } else {
    stop("Unsupported file format")
  }
}



standardize_uploaded_data <- function(dta, col_map) {
  info_idx <- match(col_map$col_names, names(dta))
  info_idx <- info_idx[!is.na(info_idx)]
  
  genetic_idx <- setdiff(seq_along(dta), info_idx)
  is_num <- vapply(dta[genetic_idx], is.numeric, logical(1))
  genetic_idx <- genetic_idx[is_num]
  
  pop_info <- dta[info_idx]
  names(pop_info) <- col_map$standard_col_names
  
  cbind(pop_info, dta[genetic_idx])
}


make_column_map <- function(pop_col, group_col, sample_col = NULL,
                            lon_col = NULL, lat_col = NULL) {
  cols_to_remove <- c(pop_col, group_col)
  newcols_names <- c("pop", "group")
  
  if (!is.null(lon_col) && lon_col != "None") {
    cols_to_remove <- c(cols_to_remove, lon_col)
    newcols_names <- c(newcols_names, "lon")
  }
  
  if (!is.null(lat_col) && lat_col != "None") {
    cols_to_remove <- c(cols_to_remove, lat_col)
    newcols_names <- c(newcols_names, "lat")
  }
  
  if (!is.null(sample_col) && sample_col != "None") {
    cols_to_remove <- c(cols_to_remove, sample_col)
    newcols_names <- c(newcols_names, "sample")
  }
  
  data.frame(
    col_names = cols_to_remove,
    standard_col_names = newcols_names,
    stringsAsFactors = FALSE
  )
}

extract_genetic_inputs <- function(dta) {
  meta_cols <- intersect(c("pop", "group", "lon", "lat", "sample"), names(dta))
  genetic_cols <- setdiff(names(dta), meta_cols)
  
  genetic_cols <- genetic_cols[
    vapply(dta[genetic_cols], is.numeric, logical(1))
  ]
  
  matriz_genetica <- as.data.frame(dta[genetic_cols])
  
  pop_info <- dta[intersect(c("pop", "group"), names(dta))]
  
  list(
    matriz_genetica = matriz_genetica,
    pop_info = pop_info,
    genetic_cols = genetic_cols
  )
}


make_psa_r_table <- function(group_summary, rtable) {
  df <- group_summary
  
  samp_cols <- grep("^samp\\.%.", names(df), value = TRUE)
  pop_cols  <- grep("^pop\\.%.",  names(df), value = TRUE)
  regions   <- sub("^samp\\.%.", "", samp_cols)
  
  samp_mat <- as.matrix(df[samp_cols])
  pop_mat  <- as.matrix(df[pop_cols])
  
  totalSample <- colSums(samp_mat > 0, na.rm = TRUE)
  totalPopulation <- colSums(pop_mat > 0, na.rm = TRUE)
  
  exclusiveSample <- vapply(seq_along(samp_cols), function(i) {
    sum(samp_mat[, i] > 0 & rowSums(samp_mat[, -i, drop = FALSE] > 0) == 0)
  }, numeric(1))
  
  exclusivePop <- vapply(seq_along(pop_cols), function(i) {
    sum(pop_mat[, i] > 0 & rowSums(pop_mat[, -i, drop = FALSE] > 0) == 0)
  }, numeric(1))
  
  resumen <- data.frame(
    Group = regions,
    `Total Alleles Sample` = totalSample,
    `Total Alleles Pop` = totalPopulation,
    `Exclusive Alleles Sample` = exclusiveSample,
    `Exclusive Alleles Pop` = exclusivePop,
    `Shared Alleles Sample` = totalSample - exclusiveSample,
    `Shared Alleles Pop` = totalPopulation - exclusivePop,
    check.names = FALSE
  )
  
  resumen$`% PSA Sample` <- round(resumen$`Total Alleles Sample` / sum(resumen$`Total Alleles Sample`), 2)
  resumen$`% PSA Pop` <- round(resumen$`Total Alleles Pop` / sum(resumen$`Total Alleles Pop`), 2)
  
  rt_noall <- rtable[rtable$Group != "All", c("Group", "R"), drop = FALSE]
  
  out <- merge(resumen, rt_noall, by = "Group", all.x = TRUE, sort = FALSE)
  
  out$R <- round(out$R, 2)
  out$R_rel <- out$R / sum(out$R, na.rm = TRUE)
  
  score_sample <- out$`% PSA Sample` + out$R_rel
  score_pop <- out$`% PSA Pop` + out$R_rel
  
  out$`Priority score sample` <- score_sample / sum(score_sample, na.rm = TRUE)
  out$`Priority score pop` <- score_pop / sum(score_pop, na.rm = TRUE)
  
  out$R_rel <- round(out$R_rel, 2)
  out$`Priority score sample` <- round(out$`Priority score sample`, 2)
  out$`Priority score pop` <- round(out$`Priority score pop`, 2)
  
  out[order(out$`Total Alleles Sample`, decreasing = TRUE), ]
}

make_data_preview <- function(dta, max_rows = 50, max_cols = 100) {
  dta[
    seq_len(min(nrow(dta), max_rows)),
    seq_len(min(ncol(dta), max_cols)),
    drop = FALSE
  ]
}
find_best_match <- function(patterns, vector){
  
  matches <- grep(paste(paste("^", patterns, sep = ""), collapse = "|"), vector, ignore.case = TRUE, value = TRUE)
  # matches <- sapply(patterns,
  #                   FUN = function(p, v){
  #                     grep(p, v, ignore.case = TRUE, value = TRUE)
  #                     },
  #                   vector)
  if(length(matches) > 0){ 
    matches[1]
  } else {
    "None"
  }
}


# -----------------------------------------------------------------------------  
# Función para seleccionar alelos de baja frecuencia 
select_alleles <- function(mat, pops, allele_perc = 0.1, pop_perc = 0.1) {
  
  mat_num <- as.data.frame(mat)
  
  # Presencia por individuo
  mat_bin <- as.data.frame(lapply(mat_num, function(x) as.integer(x > 0)))
  
  # Frecuencia de muestras
  a_perc <- colMeans(mat_bin, na.rm = TRUE)
  
  # Frecuencia de poblaciones
  pop_factor <- factor(pops)
  
  pop_sums <- rowsum(
    as.matrix(mat_bin),
    group = pop_factor,
    reorder = FALSE,
    na.rm = TRUE
  )
  
  p_perc <- colMeans(pop_sums > 0, na.rm = TRUE)
  
  names(mat)[a_perc <= allele_perc & p_perc <= pop_perc]
}

# -----------------------------------------------------------------------------  
# Función interna para calcular índices genéticos (.compute_genetic_indices)
.compute_genetic_indices <- function(mat, pops, alleles) {
  
  validate(
    need(length(alleles) > 0, "No hay alelos válidos para analizar"),
    need(all(alleles %in% colnames(mat)), "Algunos alelos no existen en los datos")
  )
  
  allele_matrix <- mat[, alleles, drop = FALSE]
  allele_matrix <- as.data.frame(lapply(allele_matrix, function(x) as.integer(x > 0)))
  
  if (ncol(allele_matrix) == 0) {
    stop("Ningún alelo válido disponible después de la selección")
  }
  
  allele_mat <- as.matrix(allele_matrix)
  
  allele_percentage <- colMeans(allele_mat, na.rm = TRUE)
  samples <- colSums(allele_mat > 0, na.rm = TRUE)
  samples_percentage <- samples / nrow(allele_mat)
  
  pop_factor <- factor(pops)
  
  pop_sum <- rowsum(
    allele_mat,
    group = pop_factor,
    reorder = FALSE,
    na.rm = TRUE
  )
  
  pop_n <- as.numeric(table(pop_factor))
  allele_pops_matrix <- sweep(pop_sum, 1, pop_n, "/")
  
  pop_count <- colSums(allele_pops_matrix > 0, na.rm = TRUE)
  pop_percentage <- pop_count / nrow(allele_pops_matrix)
  
  Lo <- (1 - allele_percentage)^(2 * pop_count)
  Le <- (1 - colMeans(allele_pops_matrix, na.rm = TRUE))^(2 * nrow(allele_pops_matrix))
  
  logLo <- -log(Lo + 1e-26)
  logLe <- -log(Le + 1e-26)
  
  table_result <- data.frame(
    allele = alleles,
    allele_perc = allele_percentage,
    pops = pop_count,
    pops_perc = pop_percentage,
    samples = samples,
    samples_perc = samples_percentage,
    Lo = Lo,
    Le = Le,
    logLo = logLo,
    logLe = logLe,
    row.names = NULL
  )
  
  lm_logLo <- lm(logLo ~ allele_percentage)
  lm_logLe <- lm(logLe ~ allele_percentage)
  
  R_coef <- coef(lm_logLo)["allele_percentage"] / coef(lm_logLe)["allele_percentage"]
  names(R_coef) <- ""
  
  table_long <- tidyr::pivot_longer(table_result, c(logLo, logLe))
  
  y_range <- range(table_long$value, na.rm = TRUE)
  label_y1 <- y_range[2] * 0.9
  label_y2 <- y_range[2] * 0.8
  
  plot_obj <- ggplot(table_long, aes(x = allele_perc, y = value, color = name)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggpubr::stat_regline_equation(
      aes(label = ..eq.label..),
      label.x.npc = 0.2,
      label.y = c(label_y1, label_y2),
      na.rm = TRUE,
      output.type = "text"
    ) +
    theme_bw() +
    labs(
      x = "Frecuencia individual",
      y = "–log(Índice)",
      color = NULL
    )
  
  list(
    table = table_result,
    R = R_coef,
    plot = plot_obj
  )
}

# -----------------------------------------------------------------------------  
# Función para calcular el PSA de cada alelo
.calculate_psa <- function(mat, pops, groups, alleles) {
  
  mat <- mat[, alleles, drop = FALSE]
  mat_bin <- as.data.frame(lapply(mat, function(x) as.integer(x > 0)))
  
  pop_group <- data.frame(
    pop = pops,
    group = groups,
    stringsAsFactors = FALSE
  )
  
  pop_key <- interaction(pop_group$pop, pop_group$group, drop = TRUE)
  
  pop_sum <- rowsum(
    as.matrix(mat_bin),
    group = pop_key,
    reorder = FALSE,
    na.rm = TRUE
  )
  
  key_split <- do.call(
    rbind,
    strsplit(rownames(pop_sum), "\\.")
  )
  
  pop_df <- data.frame(
    pop = key_split[, 1],
    group = key_split[, 2],
    stringsAsFactors = FALSE
  )
  
  pop_bin <- pop_sum > 0
  
  groups_unique <- unique(groups)
  
  samp_PSA <- sapply(groups_unique, function(g) {
    colSums(pop_sum[pop_df$group == g, , drop = FALSE], na.rm = TRUE) / nrow(mat_bin)
  })
  
  pop_PSA <- sapply(groups_unique, function(g) {
    colSums(pop_bin[pop_df$group == g, , drop = FALSE], na.rm = TRUE) / length(unique(pops))
  })
  
  samp_PSA <- round(samp_PSA, 3)
  pop_PSA <- round(pop_PSA, 3)
  
  colnames(samp_PSA) <- paste0("samp.%.", groups_unique)
  colnames(pop_PSA) <- paste0("pop.%.", groups_unique)
  
  psa <- data.frame(
    allele = colnames(mat_bin),
    pop_PSA,
    samp_PSA,
    row.names = NULL,
    check.names = FALSE
  )
  
  pop_cols <- grep("^pop\\.%.", names(psa), value = TRUE)
  samp_cols <- grep("^samp\\.%.", names(psa), value = TRUE)
  
  pop_max <- max.col(psa[, pop_cols, drop = FALSE], ties.method = "first")
  samp_max <- max.col(psa[, samp_cols, drop = FALSE], ties.method = "first")
  
  psa$PSA.pop <- sub("^pop\\.%\\.", "", pop_cols[pop_max])
  psa$PSA.samp <- sub("^samp\\.%\\.", "", samp_cols[samp_max])
  
  psa
}

# -----------------------------------------------------------------------------  
# Función para ejecutar el análisis global y por grupos.
RGUC <- function(mat, pops, allele_perc = 0.1, pop_perc = 0.1, Fst = 0.099, groups = NULL) {
  
  alleles <- select_alleles(mat, pops, allele_perc, pop_perc)
  
  resultados_global <- .compute_genetic_indices(mat, pops, alleles)
  
  if (!is.null(groups)) {
    grupos <- unique(groups)
    
    resultados_grupos <- lapply(grupos, function(grupo) {
      indices <- groups == grupo
      .compute_genetic_indices(mat[indices, , drop = FALSE], pops[indices], alleles)
    })
    
    names(resultados_grupos) <- grupos
  } else {
    resultados_grupos <- NULL
  }
  
  if (!is.null(groups)) {
    psa <- .calculate_psa(mat, pops, groups, alleles)
  } else {
    psa <- NULL
  }
  
  needed_pops <- .calculate_needed_pops(Fst, nmax = length(unique(pops)))
  
  list(
    global = resultados_global,
    grupos = resultados_grupos,
    psa = psa,
    needed_pops = needed_pops
  )
}


# -----------------------------------------------------------------------------  
.rtable_formatting <- function(res) {
  
  # res <- resultados
  # res <- results_groups
  
  r_coefs <- list(All = res$global$R, lapply(res$grupos, FUN = \(x)x$R)) |>
    as.data.frame()|>
    t() |> 
    as.data.frame()
  colnames(r_coefs) <- "R"
  r_coefs <- r_coefs |>
    rownames_to_column("Group") |> 
    mutate(R = round(R, digits = 2))
  r_coefs
}



# -----------------------------------------------------------------------------  
# Función para extraer y formatear los gráficos por grupo
.extract_plots <- function(resultados_grupos) {
  grupos <- names(resultados_grupos)
  mapply(FUN = function(y, z) {
    y$plot + ggtitle(z)
  }, resultados_grupos, grupos, SIMPLIFY = FALSE)
}



# -----------------------------------------------------------------------------  
# Función para calcular el número de poblaciones y la Probabilidad de perdida de alelos raros.
.calculate_needed_pops <- function(Fst, nmax = 20){
  # Calcular poblaciones necesarias
  needed_pops <- data.frame(pop = 1:nmax, perc = 1 - (Fst ^ (1:nmax)))
}


# -----------------------------------------------------------------------------  
# Función principal para procesar el análisis genético de forma genérica.
genetic_process <- function(matriz, pop_info, allele_perc = 0.1, pop_perc = 0.1, Fst = 0.099) {
  
  # matriz <- matriz_o
  # pop_info <- data.frame(pop = sierras$population, group = sierras$group)
  # group <- sierras$group
  # allele_perc <- 0.1
  # pop_perc <- 0.5
  # Fst <- 0.099
  
  # Verificar que pop_info tenga una columna "pop"; si no, asignar "All" a todos.
  if (!"pop" %in% names(pop_info)) {
    pop_info <- data.frame(pop = rep("All", nrow(matriz)))
  }

  # Extraer grupos si existe
  if ("group" %in% names(pop_info)) {
    groups <- pop_info$group
  } else {
    groups <- NULL
  }
  
  # Ejecutar RGUC para obtener resultados globales y por grupo
  resultados <- RGUC(matriz, pop_info$pop, allele_perc, pop_perc, Fst, groups)
  
  # Filtrar poblaciones relevantes (por ejemplo, pop_perc > 0.5)
  selected_alleles <- resultados$global$table 
  
  rtable <- .rtable_formatting(resultados)
  
  # Extraer gráficos por grupo y calcular tablas adicionales
  if (!is.null(resultados$grupos)) {
    plots_grupos <- .extract_plots(resultados$grupos)
    
    # # Calcular tablas de resumen para cada grupo usando table_formatting
    # tables_grupos <- lapply(resultados$grupos, function(res) {
    #   .rtable_formatting(res)
    # })
    
    # # Combinar las tablas resumen (RGUC_table) de cada grupo
    # group_summary <- bind_rows(lapply(names(tables_grupos), function(g) {
    #   df <- tables_grupos[[g]]$RGUC_table
    #   df$group <- g
    #   df
    # }))
    
    # # Combinar las tablas de needed_pops de cada grupo
    # group_needed <- bind_rows(lapply(names(tables_grupos), function(g) {
    #   df <- tables_grupos[[g]]$needed_pops
    #   df$group <- g
    #   df
    # }))
    
  } else {
    plots_grupos <- NULL
    # group_summary <- NULL
    # group_needed <- NULL
  }
  
  list(
    selected_alleles = selected_alleles,
    plot_global = resultados$global$plot,  # Gráfico global
    plots_grupos = plots_grupos,            # Gráficos por grupo
    # PSA = resultados$global$PSA,
    rtable = rtable,
    group_summary = resultados$psa,          # Tabla resumen por grupo
    group_needed = resultados$needed_pops             # Tabla de "needed pops" por grupo
  )
}

