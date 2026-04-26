library(shiny)
library(shinythemes)
library(bslib)
library(leaflet)
library(readxl)
library(tidyverse)
library(maps)
library(shinyjs)
library(ggpubr)
library(shinycssloaders)
library(plotly)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet.extras)
library(viridis) 

# Cargar el script que contiene las funciones genéticas
source("source.R")

# Crear un tema personalizado con bslib
custom_theme <- bs_theme(
  bg = "#f8f9fa", 
  fg = "#001f3d", 
  primary = "#001f3d", 
  base_font = font_google("Roboto"),
  code_font = font_google("Source Code Pro")
)

# Permite subir hasta 50 MB
options(shiny.maxRequestSize = 200 * 1024^2)



# Definir interfaz de usuario
ui <- navbarPage(
  id = "navbar",
  title = tagList(
    tags$img(src = "logo.png", height = "40px"),
    "Genetic Conservation App"
  ),
  theme = custom_theme,
  
  # Include JavaScript dependencies
  useShinyjs(),
  
  # Header: global styles and scripts
  header = tagList(
    tags$style(HTML(
      "
      /* Custom CSS */
      .sidebarPanel { background-color: #001f3d; color: white; padding: 15px; border-radius: 10px; }
      .selectize-input { background-color: #001f3d !important; color: white !important; }
      .irs-bar, .irs-bar-edge, .irs-single, .irs-from, .irs-to { background: #001f3d; border-color: #001f3d; }
      .filter-container { background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .instructions-text { font-size: 0.85em; color: #666; border-left: 2px solid #eee; padding-left: 15px; }
      .action-button, .btn { background-color: #001f3d !important; color: white !important; border-radius: 5px; }
      .custom-title { font-size: 18px; font-weight: bold; color: #001f3d; background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #001f3d; margin-bottom: 10px; cursor: pointer; box-shadow: 2px 2px 5px rgba(0,0,0,0.2); transition: background-color 0.3s ease, color 0.3s ease; }
      .custom-title:hover { background-color: #001f3d; color: white; }
      .custom-title i { margin-right: 10px; }
      "
    )),
    tags$script(HTML("$(function () { $('[data-toggle=\"tooltip\"]').tooltip(); });"))
  ),
  
  #---- Tab panels ----
  tabPanel(
    "Upload Data",
    fluidPage(
      titlePanel("Upload Your Data"),
      
      fluidRow(
        column(
          width = 4,
          
          wellPanel(
            tags$div(
              class = "instructions-box",
              tags$h4("Instructions for data uploading"),
              
              tags$details(
                open = NA,
                tags$summary("📝 Format Requirements"),
                tags$div(
                  style = "margin-left: 20px; margin-top: 5px;",
                  tags$ul(
                    tags$li("Supported Formats: CSV, XLS o XLSX"),
                    tags$li("Coordinates: Must be geographic (latitude/longitude)."),
                    tags$li("Missing data should be empty, not use 'NA' or any other similar code."),
                    tags$li("Use a period (.) for decimals."),
                    tags$li("Use a comma (,) to separate columns in CSV files.")
                  )
                )
              ),
              
              tags$details(
                open = NA,
                tags$summary("📊 Required columns"),
                tags$div(
                  style = "margin-left: 20px; margin-top: 5px;",
                  tags$ul(
                    tags$li(HTML("<b>Mandatory:</b> Population and Group")),
                    tags$li(HTML("<b>Optional:</b> Sample, Longitude, Latitude"))
                  )
                )
              ),
              
              tags$details(
                open = NA,
                tags$summary("🔍 Automatic column selection"),
                tags$div(
                  style = "margin-left: 20px; margin-top: 5px;",
                  tags$ul(
                    tags$li("The application will attempt to automatically match columns if they have common names."),
                    tags$li("If a column is not recognized, it will be set to 'None' and you will need to select it manually.")
                  )
                )
              )
            )
          )
        ),
        
        column(
          width = 8,
          
          wellPanel(
            h4("Data form"),
            fileInput(
              inputId = "file_data",
              label = "Choose File",
              accept = c(".csv", ".xls", ".xlsx")
            ),
            
            uiOutput("column_select_ui"),
            
            conditionalPanel(
              condition = "output.fileUploaded",
              div(
                style = "text-align: center; margin-top: 20px;",
                actionButton(
                  inputId = "continue_preview",
                  label = "Preview imported data",
                  title = "Preview standardized data before filtering",
                  `data-toggle` = "tooltip"
                )
              )
            )
          )
        )
      )
    )
  ),

  tabPanel(
    "Preview Data",
    fluidPage(
      titlePanel("Confirm Imported Data"),
      
      wellPanel(
        fluidRow(
          column(
            width = 6,
            card(
              card_header("Data Preview"),
              card_body(
                withSpinner(DT::DTOutput("data_head"))
              )
            )
          ),
          
          column(
            width = 6,
            card(
              card_header("Map Preview"),
              card_body(
                withSpinner(leafletOutput("upload_map", height = "500px"))
              )
            )
          )
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          div(
            style = "text-align: center; margin-top: 20px;",
            actionButton(
              inputId = "continue_filters",
              label = "Confirm and continue to filters",
              title = "Continue to analysis filters",
              `data-toggle` = "tooltip"
            )
          )
        )
      )
    )
  ),  
  
  tabPanel(
    "Filter Settings",
    fluidPage(
      titlePanel("Parameters"),
      fluidRow(
        column(
          width = 12,
          wellPanel(
            fluidRow(
              column(
                width = 6,
                card(
                  card_header("Filters to select rare alleles"),
                  card_body(
                    sliderInput(
                      "allele_perc", 
                      "Maximum overall allelic frequency:",
                      min = 0, max = 1, value = 0.1, step = 0.05,
                      width = "100%"
                    ),
                    sliderInput(
                      "pop_perc", 
                      "Maximum population prevalence:", 
                      min = 0, max = 1, value = 0.1, step = 0.05,
                      width = "100%"
                    )
                  )
                )
              ),
              
              column(
                width = 6,
                card(
                  card_header("Overall genetic differentiation between populations"),
                  card_body(
                    numericInput(
                      inputId = "fst",
                      label   = "Fst:",
                      value   = 0.099,
                      min     = 0,
                      max     = 1,
                      step    = 0.001,
                      width   = "100%"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      div(style = "text-align: center;",
          actionButton("process_data",
                       "Process Data",
                       class = "btn-primary"))
      
    ),
    uiOutput("pending_changes_ui")
  ),
  
  tabPanel(
    "Results",
    fluidPage(
      titlePanel("Results"),
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Parameters and alleles summary"), 
            uiOutput("alleles_summary")
          )
        )
      ),
      wellPanel(
        fluidRow(
          h4("Number of populations to be preserve"),
          column(
            width = 8,
            withSpinner(plotlyOutput("diversity_plot"))
          ),
          column(
            width = 4,
            tags$details(
              open = NA,
              tags$summary(
                div(class = "custom-title", icon("table"), "Needed Pops")
              ),
              withSpinner(DT::DTOutput("group_needed_table"))
            )
          )
        )
      ),
      
      # Sección: Ratio de esfuerzo de muestreo
      wellPanel(
        h4("Conservation priority"), 
        DT::DTOutput("psa_table"),

      ),
      
      wellPanel(
        h4("Preferred Sampling Area (PSA) per allele:"),
        tags$details(
          tags$summary(
            div(class = "custom-title", icon("table"), "Summary by Group")
          ),
          withSpinner(DT::DTOutput("group_summary_table"))
        ),
        h4("Frequencies and extinction probability per allele:"),
        withSpinner(DT::DTOutput("analysis_table")),
        h4("Risk of allelic losses:"),
        h5("Overall:"),
        div(
          style = "display: flex; justify-content: center;",
          withSpinner(plotlyOutput("global_plot",
                                   height = "400px",
                                   width = "600px"))
        ),
        h5("Groups:"),
        withSpinner(uiOutput("group_plots_ui"))
      )
    )
  ),
  

  tabPanel(
    "Maps",
    fluidPage(
      titlePanel("Conservation Maps"),
      
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Representative R-value"),
            withSpinner(leafletOutput("map_R", height = "400px"))
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          wellPanel(
            h4("PSA Sample (%)"),
            withSpinner(leafletOutput("map_psa_sample", height = "350px"))
          )
        ),
        column(
          width = 6,
          wellPanel(
            h4("Priority score: PSA Sample + Rep. R value"),
            withSpinner(leafletOutput("map_priority_sample", height = "350px"))
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          wellPanel(
            h4("PSA Pop (%)"),
            withSpinner(leafletOutput("map_psa_pop", height = "350px"))
          )
        ),
        column(
          width = 6,
          wellPanel(
            h4("Priority score: PSA Pop + Rep. R value"),
            withSpinner(leafletOutput("map_priority_pop", height = "350px"))
          )
        )
      )
    )
  ),  
  
  tabPanel(
    "About",
    fluidPage(
      titlePanel("About this App"),
      h4("Background"),
      p("This application is designed to analyze population genetic data and identify populations critical for biodiversity preservation. Using advanced population genetics methods, it identifies groups with unique genetic profiles, such as those exhibiting high genetic differentiation (Fst) or private alleles, which may indicate local adaptations or evolutionarily significant units. These results allow prioritizing conservation efforts in populations whose loss would have an irreversible impact on the species' overall genetic diversity. Furthermore, the tool recognizes areas with exceptionally high genetic diversity, such as those with high heterozygosity or allelic richness, which are essential for ensuring the long-term resilience of species. By combining genetic data with geographic and ecological information (when available), it generates clear metrics that classify populations according to their conservation urgency, offering practical recommendations for natural resource management."),
      h4("Authors"),
      p("Paula Morales Sandoica, Daniel Romera Romera y Diego Nieto Lugilde."),
      h4("Contact"),
      p("bv2nilud@uco")
    )
  )
)



# Definir la lógica del servidor (server)
server <- function(input, output, session) {
  
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
      selectizeInput(inputId = "sample_col", 
                     label = "Sample column:", 
                     choices = "None"),
      selectizeInput(inputId = "pop_col", 
                     label = "Population column:", 
                     choices = "None"),
      selectizeInput(inputId = "group_col", 
                     label = "Group column:", 
                     choices = "None"),
      selectizeInput(inputId = "lon_col", 
                     label = "Longitude column:", 
                     choices = "None"),
      selectizeInput(inputId = "lat_col", 
                     label = "Latitude column:", 
                     choices = "None")
    )
  })
  
  observeEvent(uploadedData(), {
    data_columns <- colnames(uploadedData())
    
    session$onFlushed(function() {
      
      updateSelectizeInput(
        session,
        "sample_col",
        choices = c("None", data_columns),
        selected = find_best_match(sample_patterns, data_columns),
        server = TRUE
      )
      
      updateSelectizeInput(
        session,
        "pop_col",
        choices = c("None", data_columns),
        selected = find_best_match(pop_patterns, data_columns),
        server = TRUE
      )
      
      updateSelectizeInput(
        session,
        "group_col",
        choices = c("None", data_columns),
        selected = find_best_match(group_patterns, data_columns),
        server = TRUE
      )
      
      updateSelectizeInput(
        session,
        "lon_col",
        choices = c("None", data_columns),
        selected = find_best_match(lon_patterns, data_columns),
        server = TRUE
      )
      
      updateSelectizeInput(
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
  
  output$upload_map <- renderLeaflet({
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
      mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) |>
      filter(!is.na(lat), !is.na(lon))
    
    leaflet(dta) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
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
        nrow(dta), "rows ×", ncol(dta), "columns.",
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
  
  
  # Función para procesar los datos genéticos (MODIFICADA)
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
        need(ncol(matriz_genetica) > 0, "No hay columnas genéticas para analizar"),
        need(
          all(vapply(matriz_genetica, function(x) any(!is.na(x)), logical(1))),
          "Algunas columnas genéticas contienen solo NAs"
        )
      )
      
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
  
  observeEvent(input$continue_preview, {
    req(standardized_data())
    
    updateTabsetPanel(session, "navbar", selected = "Preview Data")
    shinyjs::runjs("window.scrollTo(0, 0);")
  }, ignoreInit = TRUE)
  
  # Navegación entre pestañas (MODIFICADA)
  observeEvent(input$continue_filters, {
    updateTabsetPanel(session, "navbar", selected = "Filter Settings")
    shinyjs::runjs("window.scrollTo(0, 0);")
  }, ignoreInit = TRUE)
  
  
  
  
  observeEvent(input$process_data, {
    updateTabsetPanel(session, "navbar", selected = "Results")
    shinyjs::runjs("window.scrollTo(0, 0);")
  })
  
  # Mensaje para cambios no aplicados
  output$pending_changes_ui <- renderUI({
    # Solo muestra el mensaje después del primer clic
    if (input$process_data > 0) {
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"), 
        "Click 'Process Data' to apply changes"
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
      card(
        class = "custom-card",
        card_header(
          icon("dna", class = "summary-icon"),
          "Alleles Summary"
        ),
        card_body(
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
                tags$li(paste("Overall allelic frequency ≤", input$allele_perc)),
                tags$li(paste("Populations prevalence ≤", input$pop_perc))
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
    # Tampoco puedo añadir un select (´logLo, --logLe) porque entonces ni si quiera los calcula
    resultados()$selected_alleles %>%
      select(-logLo, -logLe) %>%
      mutate(across(where(is.numeric), ~round(., 3)))
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
  
  
  # 2) Sólo un renderTable para mostrar la tabla combinada
  output$psa_table <- DT::renderDataTable({
    psa_con_R()
  }, 
  options = list(
    paging = FALSE,
    lengthChange = FALSE,
    searching = FALSE
  ),
  rownames = FALSE)
  
  
  
  
  output$global_plot <- renderPlotly({
    req(resultados())
    ggplotly(resultados()$plot_global) %>%
      layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$group_plots_ui <- renderUI({
    req(resultados(), !is.null(resultados()$plots_grupos))
    
    plot_output_list <- lapply(seq_along(resultados()$plots_grupos), function(i) {
      plotname <- paste0("group_plot", i)
      
      div(
        style = "display: flex; justify-content: center; margin-bottom: 30px;",
        plotlyOutput(
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
        
        output[[paste0("group_plot", ii)]] <- renderPlotly({
          ggplotly(p) %>%
            layout(margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4))
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
      mutate(across(where(is.numeric), ~round(., 3)))
  },
  rownames = FALSE
  )
  
  # Gráfico de diversidad genética conservada con nmax dinámico
  output$diversity_plot <- renderPlotly({
    req(input$fst)               
    # req(input$nmax)              
    
    # Llamada a tu función, usando el nmax del slider
    datos <- .calculate_needed_pops(Fst = input$fst)
    
    nmax <- which(datos$perc > 0.99) |> first() + 5
    
    # Generación del ggplot
    p <- ggplot(datos, aes(x = pop, y = perc)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      xlim(NA, nmax) +
      labs(
        x = "Number of Populations",
        y = "Genetic diversity conserved",
        title = paste0(
          "Conserved genetic diversity (Fst = ", input$fst, ")"
        )
      ) +
      theme_minimal()
    
    # Convertir a Plotly y ajustar márgenes
    ggplotly(p) %>%
      layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
  })
  
  #_______________________________________________________________________________
  
  # MAPA
  
  # 1) Map R
  output$map_R <- renderLeaflet({
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
      mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) %>%
      filter(!is.na(lat), !is.na(lon)) %>%
      left_join(
        resultados()$rtable %>%
          filter(Group != "All") %>%
          select(Group, R),
        by = c("group" = "Group")
      ) %>%
      filter(!is.na(R))
    
    validate(
      need(nrow(df) > 0, "No valid data available for the R map.")
    )
    
    pal <- colorNumeric("YlOrRd", domain = df$R)
    scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~scale_rad(R),
        fillColor = ~pal(R),
        color = "#333",
        weight = 0.7,
        fillOpacity = 0.8,
        label = ~htmltools::HTML(
          paste0("<b>", pop, "</b><br/>R: ", round(R, 2))
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = df$R,
        title = "R value",
        opacity = 1
      ) %>%
      addMiniMap(toggleDisplay = TRUE)
  })
  
  # 2) Map PSA Sample
  output$map_psa_sample <- renderLeaflet({
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
      mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) %>%
      filter(!is.na(lat), !is.na(lon)) %>%
      left_join(
        psa_con_R() %>% select(Group, `% PSA Sample`),
        by = c("group" = "Group")
      ) %>%
      mutate(psa_s = `% PSA Sample`) %>%
      filter(!is.na(psa_s))
    
    validate(
      need(nrow(df) > 0, "No valid data available for the PSA Sample map.")
    )
    
    pal <- colorNumeric("Blues", domain = df$psa_s)
    scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~scale_rad(psa_s),
        fillColor = ~pal(psa_s),
        color = "#333",
        weight = 0.7,
        fillOpacity = 0.8,
        label = ~htmltools::HTML(
          paste0("<b>", pop, "</b><br/>PSA Sample: ", round(psa_s * 100, 1), "%")
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = df$psa_s,
        title = "% PSA Sample",
        labFormat = labelFormat(suffix = "%"),
        opacity = 1
      ) %>%
      addMiniMap(toggleDisplay = TRUE)
  })
  
  # 3) Map PSA Population
  output$map_psa_pop <- renderLeaflet({
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
      mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) %>%
      filter(!is.na(lat), !is.na(lon)) %>%
      left_join(
        psa_con_R() %>% select(Group, `% PSA Pop`),
        by = c("group" = "Group")
      ) %>%
      mutate(psa_p = `% PSA Pop`) %>%
      filter(!is.na(psa_p))
    
    validate(
      need(nrow(df) > 0, "No valid data available for the PSA Population map.")
    )
    
    pal <- colorNumeric("Greens", domain = df$psa_p)
    scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~scale_rad(psa_p),
        fillColor = ~pal(psa_p),
        color = "#333",
        weight = 0.7,
        fillOpacity = 0.8,
        label = ~htmltools::HTML(
          paste0("<b>", pop, "</b><br/>PSA Pop: ", round(psa_p * 100, 1), "%")
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = df$psa_p,
        title = "% PSA Pop",
        labFormat = labelFormat(suffix = "%"),
        opacity = 1
      ) %>%
      addMiniMap(toggleDisplay = TRUE)
  })
  
  output$map_priority_pop <- renderLeaflet({
    req(standardized_data(), psa_con_R())
    
    dta <- standardized_data()
    
    validate(
      need(all(c("lat", "lon") %in% names(dta)),
           "No geographic coordinates were selected. Maps cannot be displayed."),
      need(has_valid_coordinates(dta),
           "No valid geographic coordinates found.")
    )
    
    df <- dta %>%
      mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) %>%
      filter(!is.na(lat), !is.na(lon)) %>%
      left_join(
        psa_con_R() %>% select(Group, `Priority score pop`),
        by = c("group" = "Group")
      ) %>%
      mutate(priority_pop = `Priority score pop`) %>%
      filter(!is.na(priority_pop))
    
    validate(
      need(nrow(df) > 0, "No valid data available for the priority pop map.")
    )
    
    pal <- colorNumeric("Oranges", domain = df$priority_pop)
    scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~scale_rad(priority_pop),
        fillColor = ~pal(priority_pop),
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
      addLegend(
        "bottomright",
        pal = pal,
        values = df$priority_pop,
        title = "Priority score pop",
        opacity = 1
      ) %>%
      addMiniMap(toggleDisplay = TRUE)
  })
  
  output$map_priority_sample <- renderLeaflet({
    req(standardized_data(), psa_con_R())
    
    dta <- standardized_data()
    
    validate(
      need(all(c("lat", "lon") %in% names(dta)),
           "No geographic coordinates were selected. Maps cannot be displayed."),
      need(has_valid_coordinates(dta),
           "No valid geographic coordinates found.")
    )
    
    df <- dta %>%
      mutate(
        lat = suppressWarnings(as.numeric(lat)),
        lon = suppressWarnings(as.numeric(lon))
      ) %>%
      filter(!is.na(lat), !is.na(lon)) %>%
      left_join(
        psa_con_R() %>% select(Group, `Priority score sample`),
        by = c("group" = "Group")
      ) %>%
      mutate(priority_sample = `Priority score sample`) %>%
      filter(!is.na(priority_sample))
    
    validate(
      need(nrow(df) > 0, "No valid data available for the priority sample map.")
      )
    
    pal <- colorNumeric("Oranges", domain = df$priority_sample)
    scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~scale_rad(priority_sample),
        fillColor = ~pal(priority_sample),
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
      addLegend(
        "bottomright",
        pal = pal,
        values = df$priority_sample,
        title = "Priority score sample",
        opacity = 1
      ) %>%
      addMiniMap(toggleDisplay = TRUE)
  })
}

shinyApp(ui = ui, server = server)


