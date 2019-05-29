library(shiny)

# attempt #1
count_module_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    h4("Modulized Count"),
    textOutput(ns("count_inside")),
    h4("Is Modulized Count Odd or Even?"),
    textOutput(ns("odd_even"))
  )
}

count_module <- function(input, output, session,action){
  
  output$count_inside <- renderText(action())
  
  temp_text <- eventReactive(action(), {
    #print(class(numeric(action()))
    if (as.numeric(action()) %% 2 == 0) {
      print('e')
      return("even")
    } else{
      print('o')
      return("odd")
    }
  })
  
  output$odd_even <- renderText(temp_text())
}

ui <- fluidPage(
  actionButton(
    "submit", 
    "Press me"
  ),
  h4("Count Regular"),
  textOutput("count"),
  count_module_ui("count_module")
)

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  callModule(count_module, "count_module",reactive({input$submit}))
  output$count <- renderText(as.character(input$submit))
  
}

shinyApp(ui = ui, server = server)