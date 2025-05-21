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
      sidebarLayout(
        sidebarPanel(
          # Instructions box
          tags$div(class = "instructions-box",
                   tags$h4("Instructions for uploading data"),
                   tags$details(
                     tags$summary("🔍 Automatic column selection"),
                     tags$div(style = "margin-left: 20px; margin-top: 5px;",
                              tags$ul(
                                tags$li("The application will attempt to automatically match columns if they have common names (such as 'sample', 'population', 'lat', etcetera)."),
                                tags$li("If a column is not recognized, it will be set to 'None' and you will need to select it manually.")
                              )
                     )
                   ),
                   tags$details(
                     tags$summary("📝 Format Requirements"),
                     tags$div(style = "margin-left: 20px; margin-top: 5px;",
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
                     tags$summary("📊 Required columns"),
                     tags$div(style = "margin-left: 20px; margin-top: 5px;",
                              tags$ul(
                                tags$li(HTML("<b>Mandatory:</b> Population")),
                                tags$li(HTML("<b>Optional:</b> Sample, Group, Longitude, Latitud"))
                              )
                     )
                   )
          ),
          fileInput("file_data", "Choose File", accept = c(".csv", ".xls", ".xlsx")),
          uiOutput("column_select_ui"),
          conditionalPanel(
            condition = "output.fileUploaded",
            actionButton("continue_filters", "Continue to Filters", title = "Prepare data for analysis", `data-toggle` = "tooltip")
          )
        ),
        mainPanel(
          div(class = "custom-title", icon("map", style = "color: #001f3d;"), "Map Preview"),
          withSpinner(leafletOutput("upload_map", height = "500px")),
          tags$br(),
          tags$details(
            tags$summary(
              div(class = "custom-title", icon("table", style = "color: #001f3d;"), "Data Preview")
            ),
            withSpinner(dataTableOutput("data_head"))
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Filter Settings",
    fluidPage(
      titlePanel("Adjust Analysis Parameters"),
      sidebarLayout(
        sidebarPanel(
          width = 10,
          fluidRow(
            column(
              width = 10,
              h4("Rarity Filters", style = "text-align: center;"),
              sliderInput("allele_perc", "Allelic Percentage:", min = 0, max = 1, value = 0.1, step = 0.05),
              sliderInput("pop_perc", "Population Percentage:", min = 0, max = 1, value = 0.1, step = 0.05),
              
              h4("Genetic Conservation Filters", style = "text-align: center;"),
              numericInput(
                inputId = "fst",
                label   = "Fst:",
                value   = 0.099,
                min     = 0,
                max     = 1,
                step    = 0.001
              ),             
              div(style = "text-align: center;",
                  actionButton("process_data", "Process Data", class = "btn-primary"))
              
            )
          ),
          uiOutput("pending_changes_ui")
        ),
        mainPanel(width = 4)
      )
    )
  ),
  
  tabPanel(
    "Results",
    fluidPage(
      titlePanel("Genetic results"),
      mainPanel(
        wellPanel(
          h4("Global Summary Table"), uiOutput("alleles_summary"), withSpinner(dataTableOutput("analysis_table"))
        ),
        wellPanel(
          h4("Needed Pops Plot"),
          withSpinner(plotlyOutput("diversity_plot", height = "400px"))
        ),
        wellPanel(
          h4("Needed Pops"),
          tags$details(
            tags$summary(
              div(class = "custom-title", icon("table"), "Needed Pops")
            ),
            withSpinner(dataTableOutput("group_needed_table"))
          )
        ),
        
        # Sección: Ratio de esfuerzo de muestreo
        titlePanel("Sampling Effort Ratio"),
        wellPanel(
          h4("PSA × R Table"),
          dataTableOutput("psa_table")
           
        ),
              
        wellPanel(
          h4("Summary by Group"),
          tags$details(
            tags$summary(
              div(class = "custom-title", icon("table"), "Summary by Group")
            ),
            withSpinner(dataTableOutput("group_summary_table"))
        ),
          h4("Graphics"),
          withSpinner(plotlyOutput("global_plot", height = "400px")),
          h5("Group Graphics"),
          withSpinner(uiOutput("group_plots_ui"))
        ),
            )
          )
     ),  
      
      
  
  tabPanel(
    "Maps",
    fluidPage(
      titlePanel("Conservation Maps"),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4("Probabilidad alelos raros(R)"),
            withSpinner(leafletOutput("map_R", height = "350px"))
          )
        ),
        column(
          width = 4,
          wellPanel(
            h4("PSA Sample (%)"),
            withSpinner(leafletOutput("map_psa_sample", height = "350px"))
          )
        ),
        column(
          width = 4,
          wellPanel(
            h4("PSA Pop (%)"),
            withSpinner(leafletOutput("map_psa_pop", height = "350px"))
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
      h4("Contact")
    )
  )
)



# Definir la lógica del servidor (server)
  server <- function(input, output, session) {
    
    uploadedData <- reactive({
      req(input$file_data)
      if (grepl("\\.csv$", input$file_data$name)) {
        dta <- read.csv(input$file_data$datapath, sep = ";")
      } else {
        if (grepl("\\.xls$|\\.xlsx$", input$file_data$name)) {
          dta <- read_excel(input$file_data$datapath)
        }
      }
      dta
    })
    
    output$fileUploaded <- reactive({
      return(!is.null(uploadedData()))
    })
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
    
    output$column_select_ui <- renderUI({
      req(uploadedData())
      data <- uploadedData()
      cols <- names(data)
      
      find_best_match <- function(patterns) {
        matches <- sapply(patterns, function(p) grep(p, cols, ignore.case = TRUE, value = TRUE))
        if(length(matches) > 0) matches[1] else "None"
      }
      
      sample_patterns <- c("sample", "muestra", "individuo", "id")
      pop_patterns <- c("pop", "population", "poblacion", "site", "localidad")
      group_patterns <- c("group", "grupo", "region", "area")
      lon_patterns <- c("lon", "long", "longitud", "longitude", "x")
      lat_patterns <- c("latitude", "lat", "latitud", "y")
      
      tagList(
        selectInput("sample_col", "Sample column:", 
                    choices = c("None", cols), 
                    selected = find_best_match(sample_patterns)),
        selectInput("pop_col", "Population column:", 
                    choices = c("None", cols), 
                    selected = find_best_match(pop_patterns)),
        selectInput("group_col", "Group column:", 
                    choices = c("None", cols), 
                    selected = find_best_match(group_patterns)),
        selectInput("lon_col", "Longitude column:", 
                    choices = c("None", cols), 
                    selected = find_best_match(lon_patterns)),
        selectInput("lat_col", "Latitude column:", 
                    choices = c("None", cols), 
                    selected = find_best_match(lat_patterns))
      )
    })
    
    column_names <- reactive({
      req(input$pop_col != "None")
      cols_to_remove <- c(input$pop_col)
      newcols_names <- c("pop")
      
      if(input$lon_col != "None") {
        cols_to_remove <- c(cols_to_remove, input$lon_col)
        newcols_names <- c(newcols_names, "lon")
      }
      
      if(input$lat_col != "None") {
        cols_to_remove <- c(cols_to_remove, input$lat_col)
        newcols_names <- c(newcols_names, "lat")
      }
      
      if(input$sample_col != "None") {
        cols_to_remove <- c(cols_to_remove, input$sample_col)
        newcols_names <- c(newcols_names, "sample")
      }
      if(input$group_col != "None") {
        cols_to_remove <- c(cols_to_remove, input$group_col)
        newcols_names <- c(newcols_names, "group")
      }
      
      data.frame(col_names = cols_to_remove,
                 standard_col_names = newcols_names)
    })
    
    standardized_data <- reactive({
      req(uploadedData(), column_names())
      cols <- column_names()
      dta <- uploadedData()
      
      matriz_genetica <- dta %>%
        select(-all_of(cols$col_names)) %>%
        select_if(is.numeric) %>%
        mutate(across(everything(), ~as.numeric(as.character(.))))
      
      pop_info <- dta %>% 
        select(cols$col_names) %>% 
        setNames(cols$standard_col_names) 
      
      cbind(pop_info, matriz_genetica)
    })
    
    output$upload_map <- renderLeaflet({
      req(standardized_data(), input$lat_col, input$lon_col)
      dta <- standardized_data()
      
      if (is.null(input$lat_col) || is.null(input$lon_col) || 
          input$lat_col == "None" || input$lon_col == "None") {
        showModal(modalDialog(
          title = "Coordinates Missing",
          "Sorry! If you don't select coordinates, the map won't display.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
      
      lat <- as.numeric(dta$lat)
      lon <- as.numeric(dta$lon)
      
      validate(
        need(!all(is.na(lat)), "Latitudes no válidas"),
        need(!all(is.na(lon)), "Longitudes no válidas")
      )
      
      leaflet(data = dta[!is.na(lat) & !is.na(lon),]) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~lon, lat = ~lat,
                         radius = 5, color = "blue",
                         fill = TRUE, fillOpacity = 0.7)
    })
    
    output$data_head <- renderDataTable({
      req(standardized_data())
      standardized_data()
    },
    options = list(
      scrollX = TRUE,
      scrollY = "400px",
      paging = FALSE
    ))
    
    # Función para procesar los datos genéticos (MODIFICADA)
    resultados <- reactive({
      req(input$process_data)  # Solo se ejecuta con el botón Process Data
      
      showNotification("Processing data... Please wait", 
                       duration = NULL, 
                       type = "message",
                       id = "processing_msg")
      on.exit(removeNotification(id = "processing_msg"))
      
      dta <- standardized_data()
      req(column_names(), input$allele_perc, input$pop_perc, input$fst)
      
      tryCatch({
        matriz_genetica <- dta %>%
          select(-all_of(column_names()$standard_col_names)) %>%
          select_if(is.numeric) %>%
          mutate(across(everything(), ~as.numeric(as.character(.))))
        
        pop_info <- dta %>%
          select(any_of(c("pop", "group")))
        
        
        validate(
          need(ncol(matriz_genetica) > 0, "No hay columnas genéticas para analizar"),
          need(all(apply(matriz_genetica, 2, function(x) !all(is.na(x)))), 
               "Algunas columnas genéticas contienen solo NAs")
        )
        
        #  Guardo el vector ORIGINAL de nombres de alelos, antes del filtrado
        orig_alleles <- colnames(matriz_genetica)
        
        # Llamo a la función de procesamiento que devuelve una lista 'salida'
        
        salida <- procesar_genetica(
          matriz_genetica,
          pop_info,
          allele_perc = input$allele_perc,
          pop_perc   = input$pop_perc,
          Fst        = input$fst
        )
        
        # Inyecto el vector en la lista para poder usarlo en la UI
        
        salida$all_alleles <- orig_alleles
        
        # Devuelve la lista completa
        
        salida
        
      }, error = function(e) {
        message(">>> ERROR completo:")
        message(e$message)
        traceback()
        showNotification(paste("Error al leer el archivo:", e$message), 
                         type = "error",
                         duration = 10)
        NULL
      })
    })
    
    # Navegación entre pestañas (MODIFICADA)
    observeEvent(input$continue_filters, {
      updateTabsetPanel(session, "navbar", selected = "Filter Settings")
      shinyjs::runjs("window.scrollTo(0, 0);")
    })
    
    
    
    
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
              # Total de alelos muestreados
              tags$li(
                icon("hashtag", class = "summary-icon"),
                tags$strong("Total alleles:"),
                total_all
              ),
              # Total de alelos seleccionados tras filtros
              tags$li(
                icon("hashtag", class = "summary-icon"),
                tags$strong("Total selected alleles:"),
                nrow(data_sel)
              ),
              # Filtros aplicados
              tags$li(
                icon("sliders-h", class = "summary-icon"),
                tags$strong("Filters:"),
                tags$ul(
                  tags$li(paste("Alleles ≥", input$allele_perc)),
                  tags$li(paste("Populations ≥", input$pop_perc)),
                  tags$li(paste("Fst ≥", input$fst))
                )
              )
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
        select(-logLo, -logLe) |> 
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
      # tu resumen PSA original
      df <- resultados()$group_summary
      samp_cols <- grep("^samp\\.%.", names(df), value = TRUE)
      pop_cols  <- grep("^pop\\.%.",  names(df), value = TRUE)
      regions   <- sub("^samp\\.%.", "", samp_cols)
      
      # totales, exclusivos, compartidos 
      samp <- tibble(Region = regions) %>%
        mutate(totalSample = map_int(Region, ~ sum(df[[paste0("samp.%.", .x)]] > 0)))
      pop  <- tibble(Region = regions) %>%
        mutate(totalPopulation = map_int(Region, ~ sum(df[[paste0("pop.%.", .x)]] > 0)))
      exclusiveSample <- tibble(Region = regions) %>%
        mutate(exclusiveSample = map_int(Region, function(rg){
          this <- paste0("samp.%.", rg)
          sum(df[[this]] > 0 & rowSums(df[setdiff(samp_cols, this)]) == 0)
        }))
      exclusivePop <- tibble(Region = regions) %>%
        mutate(exclusivePop = map_int(Region, function(rg){
          this <- paste0("pop.%.", rg)
          sum(df[[this]] > 0 & rowSums(df[setdiff(pop_cols, this)]) == 0)
        }))
      sharedSample <- samp$totalSample - exclusiveSample$exclusiveSample
      sharedPop    <- pop$totalPopulation - exclusivePop$exclusivePop
      
      resumen <- tibble(Region = regions,
                        totalSample     = samp$totalSample,
                        totalPopulation = pop$totalPopulation,
                        exclusiveSample = exclusiveSample$exclusiveSample,
                        exclusivePop    = exclusivePop$exclusivePop,
                        sharedSample    = sharedSample,
                        sharedPop       = sharedPop) %>%
        mutate(`% PSA Sample` = round(totalSample     / sum(totalSample),     2),
               `% PSA Pop`    = round(totalPopulation / sum(totalPopulation), 2)) %>%
        arrange(desc(totalSample))
      
      # extraer R global y tabla R sin All
      rt      <- resultados()$rtable
      R_all   <- rt %>% filter(Group == "All") %>% pull(R)
      rt_noall <- rt %>% filter(Group != "All") %>% select(Group, R)
      
      # unir y calcular R_rel y Average
      resumen %>%
        left_join(rt_noall, by = c("Region" = "Group")) %>%
        mutate(
          R       = round(R, 2),
         R_rel   = round(R / sum(R), 2),
          'Average (PSA pop + R)' = round((R + `% PSA Pop`) / sum(R + `% PSA Pop`), 2)
        ) %>%
        rename(
          Group                   = Region,
          `Total Alleles Sample`  = totalSample,
          `Total Alleles Pop`     = totalPopulation,
          `Exclusive Alleles Sample` = exclusiveSample,
          `Exclusive Alleles Pop`    = exclusivePop,
          `Shared Alleles Sample`    = sharedSample,
          `Shared Alleles Pop`       = sharedPop
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
    # striped = TRUE, 
    # hover = TRUE, 
    # spacing = "m",
    rownames = FALSE)
    
    
    
    
    output$global_plot <- renderPlotly({
      req(resultados())
      ggplotly(resultados()$plot_global) %>%
        layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
    })
    
    output$group_plots_ui <- renderUI({
      req(resultados(), !is.null(resultados()$plots_grupos))
      plot_output_list <- lapply(seq_along(resultados()$plots_grupos),
                                 function(i) {
                                   plotname <- paste0("group_plot", i)
                                   div(style = "margin-bottom: 30px;",
                                       plotlyOutput(plotname, height = "400px"))
                                 })
      do.call(tagList, plot_output_list)
    })
    
    
    observe({
      req(resultados(), !is.null(resultados()$plots_grupos))
      lapply(seq_along(resultados()$plots_grupos), function(i) {
        output[[paste0("group_plot", i)]] <- renderPlotly({
          ggplotly(resultados()$plots_grupos[[i]]) %>%
            layout(margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4))
        })
      })
    })
    
    output$group_summary_table <- renderDataTable({
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
            "Conserved diversity (Fst = ", input$fst, ")"
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
      # unir coords + R
      df <- standardized_data() %>%
        filter(!is.na(lat), !is.na(lon)) %>%
        left_join(
          resultados()$rtable %>% filter(Group != "All") %>% select(Group, R),
          by = c("group" = "Group")
        )
      pal <- colorNumeric("YlOrRd", domain = df$R)
      scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
      
      leaflet(df) %>%
        addTiles() %>%
        addCircleMarkers(
          ~lon, ~lat,
          radius    = ~scale_rad(R),
          fillColor = ~pal(R),
          color     = "#333", weight = 0.7, fillOpacity = 0.8,
          label = ~htmltools::HTML(
            paste0("<b>", pop, "</b><br/>R: ", round(R,2))
          )
        ) %>%
        addLegend(
          "bottomright", pal = pal, values = ~R,
          title = "R value", opacity = 1
        )%>%
         addMiniMap(toggleDisplay = TRUE)
    })
    
    # 2) Map PSA Sample
    output$map_psa_sample <- renderLeaflet({
      req(standardized_data(), psa_por_region())
      df <- standardized_data() %>%
        filter(!is.na(lat), !is.na(lon)) %>%
        left_join(psa_por_region(), by = c("group" = "Group"))
      
      # convertir % a [0,1]
      df <- df %>% mutate(psa_s = `% PSA Sample`)
      pal <- colorNumeric("Blues", domain = df$psa_s)
      scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
      
      leaflet(df) %>%
        addTiles() %>%
        addCircleMarkers(
          ~lon, ~lat,
          radius    = ~scale_rad(psa_s),
          fillColor = ~pal(psa_s),
          color     = "#333", weight = 0.7, fillOpacity = 0.8,
          label = ~htmltools::HTML(
            paste0("<b>", pop, "</b><br/>PSA Sample: ", round(psa_s*100,1), "%")
          )
        ) %>%
        addLegend(
          "bottomright", pal = pal, values = ~psa_s,
          title = "% PSA Sample", labFormat = labelFormat(suffix = "%"), opacity = 1
        ) %>%
        addMiniMap(toggleDisplay = TRUE)
    })
    
    # 3) Map PSA Population
    output$map_psa_pop <- renderLeaflet({
      req(standardized_data(), psa_por_region())
      df <- standardized_data() %>%
        filter(!is.na(lat), !is.na(lon)) %>%
        left_join(psa_por_region(), by = c("group" = "Group"))
      
      df <- df %>% mutate(psa_p = `% PSA Pop`)
      pal <- colorNumeric("Greens", domain = df$psa_p)
      scale_rad <- function(x) scales::rescale(x, to = c(5, 15))
      
      leaflet(df) %>%
        addTiles() %>%
        addCircleMarkers(
          ~lon, ~lat,
          radius    = ~scale_rad(psa_p),
          fillColor = ~pal(psa_p),
          color     = "#333", weight = 0.7, fillOpacity = 0.8,
          label = ~htmltools::HTML(
            paste0("<b>", pop, "</b><br/>PSA Pop: ", round(psa_p*100,1), "%")
          )
        ) %>%
        addLegend(
          "bottomright", pal = pal, values = ~psa_p,
          title = "% PSA Pop", labFormat = labelFormat(suffix = "%"), opacity = 1
        ) %>%
        addMiniMap(toggleDisplay = TRUE)
        
    })
  }

shinyApp(ui = ui, server = server)


