library(shiny)
library(rrguc)

shinyApp(
  ui = rrguc_ui(),
  server = rrguc_server
)
