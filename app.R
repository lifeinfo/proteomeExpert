#options(shiny.reactlog = TRUE)
options(shiny.trace = TRUE)
options(shiny.fullstacktrace = TRUE)
options(shiny.error = browser)

shiny::runApp(
  port = 80,
  display.mode = "auto",
  host = getOption("shiny.host", "0.0.0.0")
  #quiet = T,
  #test.mode = T
)