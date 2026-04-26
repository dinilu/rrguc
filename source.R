library(dplyr)
library(tidyr)
library(ggplot2)


# -----------------------------------------------------------------------------  
# Función para seleccionar alelos de baja frecuencia 
select_alleles <- function(mat, pops, allele_perc = 0.1, pop_perc = 0.1) {

  # mat <- matriz_o
  # pops <- sierras$population
  # allele_perc <- 0.1
  # pop_perc <- 0.1

  # Calcular frecuencia de cada alelo (proporción de muestras que lo presentan)
  a_perc <- colMeans(mat > 0, na.rm = TRUE)
  
  # Calcular frecuencia de poblaciones en las que aparece cada alelo
  p_perc <- mat |> 
    bind_cols(pop = pops) |>
    group_by(pop) |> 
    summarize(across(everything(), sum)) |> 
    select(-pop) |> 
    as.data.frame()
  
  p_perc <- colMeans(p_perc > 0, na.rm = TRUE)
  
  # Seleccionar aquellos con frecuencia alélica y porcentaje de poblaciones
  # inferiores a los estipulados
  colnames(mat)[a_perc <= allele_perc & p_perc <= pop_perc]
}


# -----------------------------------------------------------------------------  
# Función interna para calcular índices genéticos (.compute_genetic_indices)
.compute_genetic_indices <- function(mat, pops, alleles) {
  
  # mat <- matriz_o
  # pops <- sierras$population
  # allele_perc <- 0.1
  # pop_perc <- 0.2
  # alleles <- select_alleles(mat, pops, allele_perc, pop_perc)
  
  # Validación crítica: asegurar que 'alleles' no esté vacío y exista en 'mat'
  validate(
    need(length(alleles) > 0, "No hay alelos válidos para analizar"),
    need(all(alleles %in% colnames(mat)), "Algunos alelos no existen en los datos")
  )
  
  # Conversión a binario (manejo explícito de NAs)
  mat_bin <- mat %>%
    mutate(across(everything(), ~ifelse(. > 0, 1, 0))) %>%
    as.data.frame()
  
  # Selección segura de columnas (evita errores si 'alleles' no existe)
  allele_matrix <- mat_bin %>% 
    select(any_of(alleles))  # Usa any_of() en lugar de all_of() para evitar errores
  
  # Validación adicional
  if (ncol(allele_matrix) == 0) {
    stop("Ningún alelo válido disponible después de la selección")
  }
  
  # Frecuencia promedio de cada alelo.
  allele_percentage <- apply(allele_matrix, 2, mean, na.rm = TRUE)

  samples <- apply(allele_matrix, 2, \(x) sum(x > 0, na.rm = TRUE))
  samples_percentage <- samples / nrow(allele_matrix)
  
  # Agrupar por población y calcular el promedio por marcador.
  allele_pops_matrix <- bind_cols(pop = pops, allele_matrix) %>% 
    group_by(pop) %>% 
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>% 
    column_to_rownames("pop")
  
  # Contar cuántas poblaciones presentan cada alelo y 
  pop_count <- apply(allele_pops_matrix, 2, function(x) sum(x > 0, na.rm = TRUE))
  pop_percentage <- pop_count / nrow(allele_pops_matrix)
  
  # Cálculo de índices Lo y Le.
  Lo <- (1 - allele_percentage)^(2 * pop_count)
  Le <- (1 - apply(allele_pops_matrix, MARGIN = 2, FUN = mean))^(2 * nrow(allele_pops_matrix))
  
  # Transformaciones logarítmicas.
  # logLo <- -log(Lo)
  # logLe <- -log(Le)
  logLo <- -log(Lo + 1e-26)
  logLe <- -log(Le + 1e-26)
  
  # Organizar resultados en una tabla.
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
    logLe = logLe
  )
  
  # Regresión lineal para obtener el coeficiente R.
  lm_logLo <- lm(logLo ~ allele_percentage)
  lm_logLe <- lm(logLe ~ allele_percentage)
  
  R_coef <- coef(lm_logLo)["allele_percentage"] / coef(lm_logLe)["allele_percentage"]
  names(R_coef) <- ""
  
  # Gráfico de dispersión con líneas de regresión y ecuaciones.
  table_long <- table_result %>% 
    pivot_longer(c(logLo, logLe)) 
  
  # Calcular rangos para ubicar etiquetas:
  x_range <- range(table_long$allele_perc, na.rm = TRUE)
  y_range <- range(table_long$value, na.rm = TRUE)
  label_x <- x_range[1] * 1.2
  label_y1 <- y_range[2] * 0.9
  label_y2 <- y_range[2] * 0.8
  
  plot_obj <- ggplot(table_long, 
                     aes(x = allele_perc, 
                         y = value, 
                         color = name)) +
    geom_point() +
    geom_smooth(method = "lm") +
    stat_regline_equation(aes(label = ..eq.label..),
                          label.x.npc = 0.2,
                          label.y = c(label_y1, label_y2),
                          na.rm = TRUE, output.type = "text") +
    theme_bw() +
    labs(
      x = "Frecuencia individual",
      y = "–log(Índice)",
      color = NULL
    )

  results <- list(
    table = table_result,
    R = R_coef,
    plot = plot_obj
  )
  
  results
}


# -----------------------------------------------------------------------------  
# Función para calcular el PSA de cada alelo
.calculate_psa <- function(mat, pops, groups, allele_perc, pop_perc) {
  
  # mat <- matriz_o
  # pops <- pops
  # groups <- groups
  # allele_perc <- 0.1
  # pop_perc <- 0.5
  
  # Selecciono los alelos raros
  alleles <- select_alleles(mat, pops, allele_perc, pop_perc)
  
  # Filtro la matriz y me quedo solo con los alelos raros
  mat <- mat |> 
    select(any_of(alleles)) |> 
    mutate(across(everything(), \(x) if_else(x > 0, 1, 0)))
  
  # Genero una matriz con el número de individuos con el alelo en cada población.
  pop_matrix <- mat %>% 
    bind_cols(pop = pops, group = groups) %>%
    group_by(pop, group) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
    ungroup()
  
  # Genero una matriz con las poblaciones con el alelo.
  pop_matrix_bin <- pop_matrix |> select(-c(pop, group)) |> 
    mutate(across(everything(), \(x) if_else(x > 0, 1, 0))) |> 
    bind_cols(pop = pop_matrix$pop, group = pop_matrix$group)
  
  # Calculo el porcentaje de individuos con el alelo en cada grupo genético.
  samp_PSA <- pop_matrix |> 
    select(-pop) |> 
    group_by(group) |> 
    summarize(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
    mutate(group = paste0("samp.%.", group)) |> 
    column_to_rownames("group") |> 
    mutate(across(everything(), \(x) x / nrow(mat))) |> 
    mutate(across(everything(), \(x) round(x, digits = 3))) |> 
    t() |> as.data.frame() |> 
    rownames_to_column("allele")

    # Calculo el porcentaje de poblaciones con el alelo en cada grupo genético.
  pops_PSA <- pop_matrix_bin |> 
    select(-pop) |> 
    group_by(group) |> 
    summarize(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
    mutate(group = paste0("pop.%.", group)) |> 
    column_to_rownames("group") |> 
    mutate(across(everything(), \(x) x / length(unique(pops)))) |> 
    mutate(across(everything(), \(x) round(x, digits = 3))) |> 
    t() |> as.data.frame() |> 
    rownames_to_column("allele")

  psa <- left_join(pops_PSA, 
                   samp_PSA,
                   by = "allele")

  f2 <- \(x, y, colnames) {
    # x <- psa[33, 6:9]
    # y <- psa[33, 2:5]
    # colnames <- colnames(x)
    x2 <- which(x == max(x))
    if(length(x2) == 1) {
      return(colnames[x2])
    } else {
      y2 <- which(y == max(y))
      if(length(y2) == 1) {
        return(colnames[y2])
      } else {
        w2 <- intersect(x2, y2)
        if(length(w2) == 1){
          return(colnames[w2])
        } else {
          return(colnames[sample(w2, 1)])
        }
      }
    }
  }
  
  psa %>%
    rowwise() %>%
    mutate(PSA.pop = f2(c_across(starts_with("pop.%.")), 
                        c_across(starts_with("samp.%.")), 
                        colnames(pops_PSA)[-1])) %>% 
    mutate(PSA.samp = f2(c_across(starts_with("pop.%.")), 
                        c_across(starts_with("samp.%.")), 
                        colnames(samp_PSA)[-1]))  |>
    mutate(PSA.samp = stringr::str_remove(PSA.samp, "samp.%.")) |>
    mutate(PSA.pop = stringr::str_remove(string = PSA.pop, "pop.%."))
  
  # samp_PSA <- t(samp_PSA) |> 
  #   as.data.frame() |> 
  #   mutate(PSA.samp = apply(samp_PSA, MARGIN = 2, 
  #                           FUN = \(x, y) y[which.max(x)], rownames(samp_PSA))) |>
  #   mutate(PSA.samp = stringr::str_remove(PSA.samp, "samp.%.")) |> 
  #   rownames_to_column("Group")
  # 
  # pops_PSA <- t(pops_PSA) |> 
  #   as.data.frame() |> 
  #   mutate(PSA.pop = apply(pops_PSA, MARGIN = 2, FUN = \(x, y) y[which.max(x)], rownames(pops_PSA))) |> 
  #   mutate(PSA.pop = stringr::str_remove(PSA.pop, "pop.%.")) |> 
  #   rownames_to_column("Group")
  
}


# -----------------------------------------------------------------------------  
# Función para ejecutar el análisis global y por grupos.
RGUC <- function(mat, pops, allele_perc = 0.1, pop_perc = 0.1, Fst = 0.099, groups = NULL) {
  
#   mat <- matriz_o
#   pops <- sierras$population
#   allele_perc <- 0.1
#   pop_perc <- 0.5
#   groups <- sierras$group
  # Seleccionar alelos de baja frecuencia
  alleles <- select_alleles(mat, pops, allele_perc, pop_perc)
  
  # Análisis global
  resultados_global <- .compute_genetic_indices(mat, pops, alleles)
  
  # Análisis por grupo si se proporciona la variable groups
  if (!is.null(groups)) {
    grupos <- unique(groups)
    resultados_grupos <- lapply(grupos, function(grupo) {
      indices <- groups == grupo
      .compute_genetic_indices(mat[indices, ], pops[indices], alleles)
    })
    names(resultados_grupos) <- grupos
  } else {
    resultados_grupos <- NULL
  }
  
  psa <- .calculate_psa(mat, pops, groups, allele_perc, pop_perc)
  
  # Calcular número de poblaciones necesarias
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

