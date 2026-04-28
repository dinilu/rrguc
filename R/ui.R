#' Shiny application user interface
#'
#' Defines the complete user interface of the rrguc Shiny application.
#'
#' @return A Shiny UI object.
#' @export
rrguc_ui <- function() {
  custom_theme <- bslib::bs_theme(
    bg = "#f8f9fa",
    fg = "#001f3d",
    primary = "#001f3d",
    base_font = bslib::font_google("Roboto"),
    code_font = bslib::font_google("Source Code Pro")
  )

  # Allow uploads up to 200 MB, preserving the original application setting.
  options(shiny.maxRequestSize = 200 * 1024^2)

  shiny::navbarPage(
  id = "navbar",
  title = tagList(
    tags$img(src = "logo.png", height = "40px"),
    "Genetic Conservation App"
  ),
  theme = custom_theme,
  
  
  # Header: global styles and scripts
  header = tagList(
    # Include JavaScript dependencies
    shinyjs::useShinyjs(),
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
    shiny::fluidPage(
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
          ),
          conditionalPanel(
            condition = "output.fileUploaded",
            div(
              style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
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
  ),

  tabPanel(
    "Preview Data",
    shiny::fluidPage(
      titlePanel("Confirm Imported Data"),
      
      wellPanel(
        fluidRow(
          column(
            width = 6,
            bslib::card(
              bslib::card_header("Data Preview"),
              bslib::card_body(
                shinycssloaders::withSpinner(DT::DTOutput("data_head"))
              )
            )
          ),
          
          column(
            width = 6,
            bslib::card(
              bslib::card_header("Map Preview"),
              bslib::card_body(
                shinycssloaders::withSpinner(leaflet::leafletOutput("upload_map", height = "500px"))
              )
            )
          )
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          div(
            style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
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
    shiny::fluidPage(
      titlePanel("Parameters"),
      fluidRow(
        column(
          width = 12,
          wellPanel(
            fluidRow(
              column(
                width = 6,
                bslib::card(
                  bslib::card_header("Filters to select rare alleles"),
                  bslib::card_body(
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
                bslib::card(
                  bslib::card_header("Overall genetic differentiation between populations"),
                  bslib::card_body(
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
      column(
        width = 12,
        
        shiny::div(
          style = "margin-top: 20px; margin-bottom: 20px;",
          shiny::uiOutput("pending_changes_ui")
        ),
        
        shiny::div(
          style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
          shiny::actionButton(
            "process_data",
            "Process Data",
            icon = shiny::icon("calculator"),
            class = "btn-primary"
          )
        )
      )
    ),
  ),
  
  tabPanel(
    "Results",
    shiny::fluidPage(
      titlePanel("Results"),
      fluidRow(
        column(
          width = 9,
          shiny::div(
            style = "margin-top: 20px; margin-bottom: 20px;",
            wellPanel(
              h4("Parameters and alleles summary"), 
              uiOutput("alleles_summary")
            )
          ),
          
          shiny::div(
            style = "margin-top: 20px; margin-bottom: 20px;",
            wellPanel(
              fluidRow(
                h4("Number of populations to be preserve"),
                column(
                  width = 8,
                  shinycssloaders::withSpinner(plotly::plotlyOutput("diversity_plot"))
                ),
                column(
                  width = 4,
                  tags$details(
                    open = NA,
                    tags$summary(
                      div(class = "custom-title", icon("table"), "Needed Pops")
                    ),
                    shinycssloaders::withSpinner(DT::DTOutput("group_needed_table"))
                  )
                )
              )
            )
          ),
          
          # Seccion: Ratio de esfuerzo de muestreo
          shiny::div(
            style = "margin-top: 20px; margin-bottom: 20px;",
            wellPanel(
              h4("Conservation priority"), 
              DT::DTOutput("psa_table")
            )
          ),
          
          shiny::div(
            style = "margin-top: 20px; margin-bottom: 20px;",
            wellPanel(
              h4("Preferred Sampling Area (PSA) per allele:"),
              tags$details(
                tags$summary(
                  div(class = "custom-title", icon("table"), "Summary by Group")
                ),
                shinycssloaders::withSpinner(DT::DTOutput("group_summary_table"))
              ),
              
              h4("Frequencies and extinction probability per allele:"),
              shinycssloaders::withSpinner(DT::DTOutput("analysis_table")),
              
              h4("Risk of allelic losses:"),
            
              h5("Overall:"),
              div(
                style = "display: flex; justify-content: center;",
                shinycssloaders::withSpinner(plotly::plotlyOutput("global_plot",
                                                                  height = "400px",
                                                                  width = "600px"))
              ),
              h5("Groups:"),
              shinycssloaders::withSpinner(uiOutput("group_plots_ui"))
            )
          )
        ),
        
        shiny::column(
          width = 3,
          shiny::div(
            style = "position: sticky; top: 20px; margin-top: 20px; margin-bottom: 20px;",
            bslib::card(
              bslib::card_header("Export results"),
              bslib::card_body(
                shiny::downloadButton(
                  "download_tables_xlsx",
                  "Download tables (.xlsx)"
                )
              )
            )
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Maps",
    shiny::fluidPage(
      titlePanel("Conservation Maps"),
      
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Representative R-value"),
            shinycssloaders::withSpinner(leaflet::leafletOutput("map_R", height = "400px"))
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          wellPanel(
            h4("PSA Sample (%)"),
            shinycssloaders::withSpinner(leaflet::leafletOutput("map_psa_sample", height = "350px"))
          )
        ),
        column(
          width = 6,
          wellPanel(
            h4("Priority score: PSA Sample + Rep. R value"),
            shinycssloaders::withSpinner(leaflet::leafletOutput("map_priority_sample", height = "350px"))
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          wellPanel(
            h4("PSA Pop (%)"),
            shinycssloaders::withSpinner(leaflet::leafletOutput("map_psa_pop", height = "350px"))
          )
        ),
        column(
          width = 6,
          wellPanel(
            h4("Priority score: PSA Pop + Rep. R value"),
            shinycssloaders::withSpinner(leaflet::leafletOutput("map_priority_pop", height = "350px"))
          )
        )
      )
    )
  ),  
  
  tabPanel(
    "About",
    shiny::fluidPage(
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
}
