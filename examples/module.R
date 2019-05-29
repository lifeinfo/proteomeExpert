library(shiny)
# Module UI function
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}
csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}

txtFileInput <- function(id, label = "txt file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}

txtFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.table(userFile()$datapath,
             header = input$heading,
             quote = input$quote,sep = "\t",
             stringsAsFactors = stringsAsFactors)
  })

  # Return the reactive that yields the data frame
  return(dataframe)
}




ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      csvFileInput("datafile"),
      txtFileInput("datafile2")
    ),
    mainPanel(
      dataTableOutput("table"),
      dataTableOutput("table2")
    )
  )
)

server <- function(input, output, session) {
  datafile <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE)
  datafile3<-callModule(txtFile, "datafile2",
                        stringsAsFactors = FALSE)
  output$table <- renderDataTable({
    datafile()
  })
  output$table2 <- renderDataTable({
    datafile3()
  })
}

shinyApp(ui = ui, server = server)

