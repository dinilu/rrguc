#' Shiny application server
#'
#' Defines the server logic of the rrguc Shiny application.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return No return value. Called for side effects within Shiny.
#' @export
rrguc_server <- function(input, output, session) {
  
  sample_patterns <- c("sample", "muestra", "individuo", "id")
  pop_patterns <- c("pop", "population", "poblacion", "site", "localidad", "sitio", "locality")
  group_patterns <- c("group", "grupo", "region", "area")
  lon_patterns <- c("lon", "long", "longitud", "longitude", "x")
  lat_patterns <- c("latitude", "lat", "latitud", "y")
  
  uploadedData <- eventReactive(input$file_data, {
    req(input$file_data)
    read_uploaded_data(input$file_data$datapath, input$file_data$name)
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(uploadedData()))
  })
  
  outputOptions(output, 
                name = "fileUploaded",
                suspendWhenHidden = FALSE)
  
  output$column_select_ui <- renderUI({
    req(uploadedData())
    data <- uploadedData()
    # cols <- names(uploadedData())
    
    tagList(
      # selectInput(inputId = "sample_col", 
      #             label = "Sample column:", 
      #             choices = c("None", cols), 
      #             selected = find_best_match(sample_patterns)),
      # selectInput(inputId = "pop_col",
      #             label = "Population column:", 
      #             choices = c("None", cols), 
      #             selected = find_best_match(pop_patterns, cols)),
      # selectInput(inputId = "group_col",  
      #             label = "Group column:", 
      #             choices = c("None", cols), 
      #             selected = find_best_match(group_patterns, cols)),
      # selectInput(inputId = "lon_col",  
      #             label = "Longitude column:", 
      #             choices = c("None", cols), 
      #             selected = find_best_match(lon_patterns, cols)),
      # selectInput(inputId = "lat_col",  
      #             label = "Latitude column:", 
      #             choices = c("None", cols), 
      #             selected = find_best_match(lat_patterns, cols))
      shiny::selectizeInput(inputId = "sample_col", 
                     label = "Sample column:", 
                     choices = "None"),
      shiny::selectizeInput(inputId = "pop_col", 
                     label = "Population column:", 
                     choices = "None"),
      shiny::selectizeInput(inputId = "group_col", 
                     label = "Group column:", 
                     choices = "None"),
      shiny::selectizeInput(inputId = "lon_col", 
                     label = "Longitude column:", 
                     choices = "None"),
      shiny::selectizeInput(inputId = "lat_col", 
                     label = "Latitude column:", 
                     choices = "None")
    )
  })
  
  observeEvent(uploadedData(), {
    data_columns <- colnames(uploadedData())
    
    session$onFlushed(function() {
      
      shiny::updateSelectizeInput(
        session,
        "sample_col",
        choices = c("None", data_columns),
        selected = find_best_match(sample_patterns, data_columns),
        server = TRUE
      )
      
      shiny::updateSelectizeInput(
        session,
        "pop_col",
        choices = c("None", data_columns),
        selected = find_best_match(pop_patterns, data_columns),
        server = TRUE
      )
      
      shiny::updateSelectizeInput(
        session,
        "group_col",
        choices = c("None", data_columns),
        selected = find_best_match(group_patterns, data_columns),
        server = TRUE
      )
      
      shiny::updateSelectizeInput(
        session,
        "lon_col",
        choices = c("None", data_columns),
        selected = find_best_match(lon_patterns, data_columns),
        server = TRUE
      )
      
      shiny::updateSelectizeInput(
        session,
        "lat_col",
        choices = c("None", data_columns),
        selected = find_best_match(lat_patterns, data_columns),
        server = TRUE
      )
      
    }, once = TRUE)
  })
  
  columns_ready <- reactive({
    req(uploadedData())
    
    df <- uploadedData()
    cols <- names(df)
    
    req(input$pop_col)
    req(input$group_col)
    
    validate(
      need(input$pop_col != "None", "Select a population column"),
      need(input$group_col != "None", "Select a group column"),
      need(input$pop_col %in% cols, "Population column is not valid"),
      need(input$group_col %in% cols, "Group column is not valid")
    )
    
    optional_cols <- c(input$sample_col, input$lon_col, input$lat_col)
    optional_cols <- optional_cols[!is.null(optional_cols) & optional_cols != "None"]
    
    validate(
      need(all(optional_cols %in% cols), "Some selected columns are not valid")
    )
    
    TRUE
  })
  
  column_names <- reactive({
    req(columns_ready())
    
    make_column_map(
      pop_col = input$pop_col,
      group_col = input$group_col,
      sample_col = input$sample_col,
      lon_col = input$lon_col,
      lat_col = input$lat_col
    )
  })
  
  standardized_data_cache <- reactiveVal(NULL)
  
  observeEvent(
    list(
      uploadedData(),
      input$pop_col,
      input$group_col,
      input$sample_col,
      input$lon_col,
      input$lat_col
    ),
    {
      req(columns_ready())
      req(column_names())
      
      standardized_data_cache(
        standardize_uploaded_data(
          uploadedData(),
          column_names()
        )
      )
    },
    ignoreInit = TRUE
  )
  
  standardized_data <- reactive({
    req(standardized_data_cache())
    standardized_data_cache()
  }) 
  
  output$upload_map <- leaflet::renderLeaflet({
    req(standardized_data())
    
    dta <- standardized_data()
    
    validate(
      need(
        all(c("lat", "lon") %in% names(dta)),
        "Select latitude and longitude columns to display the map."
      ),
      need(
        has_valid_coordinates(dta),
        "No valid coordinates found."
      )
    )
    
    dta <- dta |>
      dplyr::mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) |>
      dplyr::filter(!is.na(lat), !is.na(lon))
    
    leaflet::leaflet(dta) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(
        lng = dta$lon,
        lat = dta$lat,
        radius = 5,
        color = "blue",
        fill = TRUE,
        fillOpacity = 0.7
      )
  })
  
  
  output$data_head <- DT::renderDT({
    req(standardized_data())
    
    dta <- standardized_data()
    preview <- make_data_preview(dta)
    
    DT::datatable(
      preview,
      caption = paste(
        "Preview:",
        nrow(dta), "rows x", ncol(dta), "columns.",
        "Showing first", nrow(preview), "rows and first", ncol(preview), "columns."
      ),
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        deferRender = TRUE
      ),
      rownames = FALSE
    )
  })
  
  filters_changed <- shiny::reactiveVal(FALSE)
  results_processed <- shiny::reactiveVal(FALSE)
  
  # Funcion para procesar los datos geneticos (MODIFICADA)
  resultados <- eventReactive(input$process_data, {
    req(standardized_data())
    req(input$allele_perc, input$pop_perc, input$fst)
    
    showNotification(
      "Processing data... Please wait",
      duration = NULL,
      type = "message",
      id = "processing_msg"
    )
    on.exit(removeNotification(id = "processing_msg"))
    
    dta <- standardized_data()
    
    tryCatch({
      
      inputs <- extract_genetic_inputs(standardized_data())
      
      matriz_genetica <- inputs$matriz_genetica
      pop_info <- inputs$pop_info
      
      validate(
        need(ncol(matriz_genetica) > 0, "There is no genetic columns to analyze."),
        need(
          all(vapply(matriz_genetica, function(x) any(!is.na(x)), logical(1))),
          "Some genetic columns only has NA values."
        )
      )
      
      selected_alleles <- select_alleles(
        mat = matriz_genetica,
        pops = pop_info$pop,
        allele_perc = input$allele_perc,
        pop_perc = input$pop_perc
      )
      
      if (length(selected_alleles) < 2) {
        return(NULL)
      }
      
      salida <- genetic_process(
        matriz_genetica,
        pop_info,
        allele_perc = input$allele_perc,
        pop_perc   = input$pop_perc,
        Fst        = input$fst
      )
      
      salida$all_alleles <- names(matriz_genetica)
      salida
      
    }, error = function(e) {
      message(">>> ERROR completo:")
      message(e$message)
      
      showNotification(
        paste("Error al procesar los datos:", e$message),
        type = "error",
        duration = 10
      )
      
      NULL
    })
  })
  
  shiny::observeEvent(
    list(input$allele_perc, input$pop_perc, input$fst),
    {
      if (isTRUE(results_processed())) {
        filters_changed(TRUE)
      }
    },
    ignoreInit = TRUE
  )
  
  shiny::observeEvent(input$continue_preview, {
    req(standardized_data())
    
    updateTabsetPanel(session, "navbar", selected = "Preview Data")
    shinyjs::runjs("window.scrollTo(0, 0);")
  }, ignoreInit = TRUE)
  
  # Navegacion entre pestanas (MODIFICADA)
  observeEvent(input$continue_filters, {
    updateTabsetPanel(session, "navbar", selected = "Filter Settings")
    shinyjs::runjs("window.scrollTo(0, 0);")
  }, ignoreInit = TRUE)
  
  
  
  
  observeEvent(input$process_data, {
    req(standardized_data())
    req(input$allele_perc, input$pop_perc, input$fst)
    
    inputs <- extract_genetic_inputs(standardized_data())
    
    matriz_genetica <- inputs$matriz_genetica
    pop_info <- inputs$pop_info
    
    if (ncol(matriz_genetica) == 0) {
      shiny::showModal(
        shiny::modalDialog(
          title = "No genetic data available",
          "There are no genetic columns available for the analysis.",
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        )
      )
      return(NULL)
    }
    
    selected_alleles <- select_alleles(
      mat = matriz_genetica,
      pops = pop_info$pop,
      allele_perc = input$allele_perc,
      pop_perc = input$pop_perc
    )
    
    if (length(selected_alleles) < 2) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Insufficient rare alleles selected",
          paste(
            "At least two rare alleles are needed to perform the downstream",
            "calculations. Please relax the filter values before continuing."
          ),
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        )
      )
      return(NULL)
    }
    
    results_processed(TRUE)
    filters_changed(FALSE)
    
    shiny::updateTabsetPanel(session, "navbar", selected = "Results")
    shinyjs::runjs("window.scrollTo(0, 0);")
  })
  
  # Mensaje para cambios no aplicados
  output$pending_changes_ui <- shiny::renderUI({
    if (isTRUE(filters_changed())) {
      shiny::div(
        class = "alert alert-warning",
        shiny::icon("exclamation-triangle"),
        " Filter settings have changed. Click 'Process Data' to update the results."
      )
    }
  })
  
  # Renderizados de resultados (TODO ESTO PERMANECE IGUAL)
  output$alleles_summary <- renderUI({
    req(resultados())
    # Total alleles sampled (antes del filtrado)
    total_all <- length(resultados()$all_alleles)
    
    # Data filtrada de alelos seleccionados
    data_sel <- resultados()$selected_alleles
    
    if (!is.null(data_sel)) {
      bslib::card(
        class = "custom-card",
        bslib::card_header(
          icon("dna", class = "summary-icon"),
          "Alleles Summary"
        ),
        bslib::card_body(
          tags$ul(
            class = "list-unstyled",
            tags$li(
              icon("sliders-h", class = "summary-icon"),
              tags$strong("Overal genetic differentiation between populations:"),
              tags$ul(
                tags$li(paste("Fst =", input$fst))
              )
            ),
            # Filtros aplicados
            tags$li(
              icon("sliders-h", class = "summary-icon"),
              tags$strong("Filters to select rare alleles:"),
              tags$ul(
                tags$li(paste("Overall allelic frequency \u2264", input$allele_perc)),
                tags$li(paste("Populations prevalence \u2264", input$pop_perc))
                )
            ),
            # Total de alelos muestreados
            tags$li(
              icon("hashtag", class = "summary-icon"),
              tags$strong("Total alleles:"),
              total_all
            ),
            # Total de alelos seleccionados tras filtros
            tags$li(
              icon("hashtag", class = "summary-icon"),
              tags$strong("Selected alleles (rare):"),
              nrow(data_sel)
            ),
            
          )
        )
      )
    }
  })
  
  #___________________________________________________________________________________________________________
  
  # Resultados 
  
  
  output$analysis_table <- DT::renderDataTable({
    req(resultados())
    # Tomo la tabla de alelos seleccionados y quito las columnas logLo y logLe.
    # Directamente sobre mat no puedo porque es mi entrada de datos, no mi tabla. 
    # Tampoco puedo anadir un select ('logLo, --logLe) porque entonces ni si quiera los calcula
    resultados()$selected_alleles %>%
      dplyr::select(-logLo, -logLe) %>%
      dplyr::mutate(across(where(is.numeric), ~round(., 3)))
  },
  options = list(
    pageLength = 10, 
    lengthMenu = c(10, 20, 50)),
  rownames = FALSE
  )    
  
  # PSA + R
  
  # 1) Reactive que arma PSA + R + R_rel + Average
  psa_con_R <- reactive({
    req(resultados())
    make_psa_r_table(
      resultados()$group_summary,
      resultados()$rtable
    )
  })
  
  
  # 2) Solo un renderTable para mostrar la tabla combinada
  output$psa_table <- DT::renderDataTable({
    psa_con_R()
  }, 
  options = list(
    paging = FALSE,
    lengthChange = FALSE,
    searching = FALSE
  ),
  rownames = FALSE)
  
  
  
  
  output$global_plot <- plotly::renderPlotly({
    req(resultados())
    plotly::ggplotly(resultados()$plot_global) %>%
      plotly::layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$group_plots_ui <- renderUI({
    req(resultados(), !is.null(resultados()$plots_grupos))
    
    plot_output_list <- lapply(seq_along(resultados()$plots_grupos), function(i) {
      plotname <- paste0("group_plot", i)
      
      div(
        style = "display: flex; justify-content: center; margin-bottom: 30px;",
        plotly::plotlyOutput(
          plotname,
          height = "400px",
          width = "600px"
        )
      )
    })
    
    do.call(tagList, plot_output_list)
  })  
  
  observeEvent(resultados(), {
    res <- resultados()
    req(!is.null(res$plots_grupos))
    
    lapply(seq_along(res$plots_grupos), function(i) {
      local({
        ii <- i
        p <- res$plots_grupos[[ii]]
        
        output[[paste0("group_plot", ii)]] <- plotly::renderPlotly({
          plotly::ggplotly(p) %>%
            plotly::layout(margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4))
        })
      })
    })
  })
  
  output$group_summary_table <- DT::renderDataTable({
    req(resultados())
    resultados()$group_summary
  })
  
  output$rtable_table <- renderTable({
    req(resultados())
    resultados()$rtable
  })
  
  output$group_needed_table <- DT::renderDataTable({
    req(resultados())
    resultados()$group_needed |> 
      dplyr::mutate(across(where(is.numeric), ~round(., 3)))
  },
  rownames = FALSE
  )
  
  # Grafico de diversidad genetica conservada con nmax dinamico
  output$diversity_plot <- plotly::renderPlotly({
    req(input$fst)               
    # req(input$nmax)              
    
    # Llamada a tu funcion, usando el nmax del slider
    datos <- .calculate_needed_pops(Fst = input$fst)
    
    nmax <- which(datos$perc > 0.99) |> dplyr::first() + 5
    
    # Generacion del ggplot
    p <- ggplot2::ggplot(datos, ggplot2::aes(x = pop, y = perc)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::xlim(NA, nmax) +
      ggplot2::labs(
        x = "Number of Populations",
        y = "Genetic diversity conserved",
        title = paste0(
          "Conserved genetic diversity (Fst = ", input$fst, ")"
        )
      ) +
      ggplot2::theme_minimal()
    
    # Convertir a Plotly y ajustar margenes
    plotly::ggplotly(p) %>%
      plotly::layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
  })
  
  #_______________________________________________________________________________
  
  # MAPA
  
  # 1) Map R
  output$map_R <- leaflet::renderLeaflet({
    req(standardized_data(), resultados())
    
    dta <- standardized_data()
    
    validate(
      need(
        all(c("lat", "lon") %in% names(dta)),
        "No geographic coordinates were selected. Maps cannot be displayed."
      ),
      need(
        has_valid_coordinates(dta),
        "No valid geographic coordinates found."
      )
    )
    
    df <- dta %>%
      dplyr::mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) %>%
      dplyr::filter(!is.na(lat), !is.na(lon)) %>%
      dplyr::left_join(
        resultados()$rtable %>%
          dplyr::filter(Group != "All") %>%
          dplyr::select(Group, R),
        by = c("group" = "Group")
      ) %>%
      dplyr::filter(!is.na(R))
    
    validate(
      need(nrow(df) > 0, "No valid data available for the R map.")
    )
    
    pal <- leaflet::colorNumeric("YlOrRd", domain = df$R)
    scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
    
    leaflet::leaflet(df) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lng = df$lon,
        lat = df$lat,
        radius = scale_rad(df$R),
        fillColor = pal(df$R),
        color = "#333",
        weight = 0.7,
        fillOpacity = 0.8,
        label = ~htmltools::HTML(
          paste0("<b>", pop, "</b><br/>R: ", round(R, 2))
        )
      ) %>%
      leaflet::addLegend(
        "bottomright",
        pal = pal,
        values = df$R,
        title = "R value",
        opacity = 1
      ) %>%
      leaflet::addMiniMap(toggleDisplay = TRUE)
  })
  
  # 2) Map PSA Sample
  output$map_psa_sample <- leaflet::renderLeaflet({
    req(standardized_data(), psa_con_R())
    
    dta <- standardized_data()
    
    validate(
      need(
        all(c("lat", "lon") %in% names(dta)),
        "No geographic coordinates were selected. Maps cannot be displayed."
      ),
      need(
        has_valid_coordinates(dta),
        "No valid geographic coordinates found."
      )
    )
    
    df <- dta %>%
      dplyr::mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) %>%
      dplyr::filter(!is.na(lat), !is.na(lon)) %>%
      dplyr::left_join(
        psa_con_R() %>% dplyr::select(Group, `% PSA Sample`),
        by = c("group" = "Group")
      ) %>%
      dplyr::mutate(psa_s = `% PSA Sample`) %>%
      dplyr::filter(!is.na(psa_s))
    
    validate(
      need(nrow(df) > 0, "No valid data available for the PSA Sample map.")
    )
    
    pal <- leaflet::colorNumeric("Blues", domain = df$psa_s)
    scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
    
    leaflet::leaflet(df) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lng = df$lon,
        lat = df$lat,
        radius = scale_rad(df$psa_s),
        fillColor = pal(df$psa_s),
        color = "#333",
        weight = 0.7,
        fillOpacity = 0.8,
        label = ~htmltools::HTML(
          paste0("<b>", pop, "</b><br/>PSA Sample: ", round(psa_s * 100, 1), "%")
        )
      ) %>%
      leaflet::addLegend(
        "bottomright",
        pal = pal,
        values = df$psa_s,
        title = "% PSA Sample",
        labFormat = leaflet::labelFormat(suffix = "%"),
        opacity = 1
      ) %>%
      leaflet::addMiniMap(toggleDisplay = TRUE)
  })
  
  # 3) Map PSA Population
  output$map_psa_pop <- leaflet::renderLeaflet({
    req(standardized_data(), psa_con_R())
    
    dta <- standardized_data()
    
    validate(
      need(
        all(c("lat", "lon") %in% names(dta)),
        "No geographic coordinates were selected. Maps cannot be displayed."
      ),
      need(
        has_valid_coordinates(dta),
        "No valid geographic coordinates found."
      )
    )
    
    df <- dta %>%
      dplyr::mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) %>%
      dplyr::filter(!is.na(lat), !is.na(lon)) %>%
      dplyr::left_join(
        psa_con_R() %>% dplyr::select(Group, `% PSA Pop`),
        by = c("group" = "Group")
      ) %>%
      dplyr::mutate(psa_p = `% PSA Pop`) %>%
      dplyr::filter(!is.na(psa_p))
    
    validate(
      need(nrow(df) > 0, "No valid data available for the PSA Population map.")
    )
    
    pal <- leaflet::colorNumeric("Greens", domain = df$psa_p)
    scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
    
    leaflet::leaflet(df) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lng = df$lon,
        lat = df$lat,
        radius = scale_rad(df$psa_p),
        fillColor = pal(df$psa_p),
        color = "#333",
        weight = 0.7,
        fillOpacity = 0.8,
        label = ~htmltools::HTML(
          paste0("<b>", pop, "</b><br/>PSA Pop: ", round(psa_p * 100, 1), "%")
        )
      ) %>%
      leaflet::addLegend(
        "bottomright",
        pal = pal,
        values = df$psa_p,
        title = "% PSA Pop",
        labFormat = leaflet::labelFormat(suffix = "%"),
        opacity = 1
      ) %>%
      leaflet::addMiniMap(toggleDisplay = TRUE)
  })
  
  output$map_priority_pop <- leaflet::renderLeaflet({
    req(standardized_data(), psa_con_R())
    
    dta <- standardized_data()
    
    validate(
      need(all(c("lat", "lon") %in% names(dta)),
           "No geographic coordinates were selected. Maps cannot be displayed."),
      need(has_valid_coordinates(dta),
           "No valid geographic coordinates found.")
    )
    
    df <- dta %>%
      dplyr::mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) %>%
      dplyr::filter(!is.na(lat), !is.na(lon)) %>%
      dplyr::left_join(
        psa_con_R() %>% dplyr::select(Group, `Priority score pop`),
        by = c("group" = "Group")
      ) %>%
      dplyr::mutate(priority_pop = `Priority score pop`) %>%
      dplyr::filter(!is.na(priority_pop))
    
    validate(
      need(nrow(df) > 0, "No valid data available for the priority pop map.")
    )
    
    pal <- leaflet::colorNumeric("Oranges", domain = df$priority_pop)
    scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
    
    leaflet::leaflet(df) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lng = df$lon,
        lat = df$lat,
        radius = scale_rad(df$priority_pop),
        fillColor = pal(df$priority_pop),
        color = "#333",
        weight = 0.7,
        fillOpacity = 0.8,
        label = ~htmltools::HTML(
          paste0(
            "<b>", pop, "</b><br/>",
            "Priority score pop: ", round(priority_pop, 2)
          )
        )
      ) %>%
      leaflet::addLegend(
        "bottomright",
        pal = pal,
        values = df$priority_pop,
        title = "Priority score pop",
        opacity = 1
      ) %>%
      leaflet::addMiniMap(toggleDisplay = TRUE)
  })
  
  output$map_priority_sample <- leaflet::renderLeaflet({
    req(standardized_data(), psa_con_R())
    
    dta <- standardized_data()
    
    validate(
      need(all(c("lat", "lon") %in% names(dta)),
           "No geographic coordinates were selected. Maps cannot be displayed."),
      need(has_valid_coordinates(dta),
           "No valid geographic coordinates found.")
    )
    
    df <- dta %>%
      dplyr::mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) %>%
      dplyr::filter(!is.na(lat), !is.na(lon)) %>%
      dplyr::left_join(
        psa_con_R() %>% dplyr::select(Group, `Priority score sample`),
        by = c("group" = "Group")
      ) %>%
      dplyr::mutate(priority_sample = `Priority score sample`) %>%
      dplyr::filter(!is.na(priority_sample))
    
    validate(
      need(nrow(df) > 0, "No valid data available for the priority sample map.")
      )
    
    pal <- leaflet::colorNumeric("Oranges", domain = df$priority_sample)
    scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
    
    leaflet::leaflet(df) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lng = df$lon,
        lat = df$lat,
        radius = scale_rad(df$priority_sample),
        fillColor = pal(df$priority_sample),
        color = "#333",
        weight = 0.7,
        fillOpacity = 0.8,
        label = ~htmltools::HTML(
          paste0(
            "<b>", pop, "</b><br/>",
            "Priority score sample: ", round(priority_sample, 2)
          )
        )
      ) %>%
      leaflet::addLegend(
        "bottomright",
        pal = pal,
        values = df$priority_sample,
        title = "Priority score sample",
        opacity = 1
      ) %>%
      leaflet::addMiniMap(toggleDisplay = TRUE)
  })
}

