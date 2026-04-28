#' Run the rrguc Shiny application
#'
#' Launches the Shiny application included in the package.
#'
#' @return No return value. This function launches a Shiny app.
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "rrguc")

  if (app_dir == "") {
    stop("Could not find the Shiny application directory.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
