options(shiny.reactlog = TRUE)

shiny::runApp(
  port = 80,
  #launch.browser = launch_browser,
  display.mode = "auto",
  host = getOption("shiny.host", "0.0.0.0")
)