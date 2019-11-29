library(shiny)
library(shinythemes)
library(rmarkdown)
shinyApp(
  ui = tagList(
    shinythemes::themeSelector(),
    fluidPage(
      navbarPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        "shinythemes",
        tabPanel("Navbar 1",
                 sidebarPanel(
                   fileInput("file", "File input:"),
                   textInput("txt", "Text input:", "general"),
                   sliderInput("slider", "Slider input:", 1, 100, 30),
                   tags$h5("Deafult actionButton:"),
                   actionButton("action", "Search"),
                   
                   tags$h5("actionButton with CSS class:"),
                   actionButton("action2", "Action button", class = "btn-primary")
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Tab 1",
                              h4("Table"),
                              tableOutput("table"),
                              h4("Verbatim text output"),
                              verbatimTextOutput("txtout")
                              
                     ),
                     tabPanel("Tab 2", "This panel is intentionally left blank"),
                     tabPanel("Tab 3", "This panel is intentionally left blank")
                   )
                 )
        ),
        tabPanel("Help", 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Experimental Design",
                              includeMarkdown("ProteomeExpert Experimental Design.md")
                     ),
                     tabPanel("Data Console",
                              includeMarkdown("ProteomeExpert Data console.md")
                     ),
                     tabPanel("Data Preprocessing",
                              includeMarkdown("ProteomeExpert-Data Preprocessing.md")
                     ),
                     tabPanel("QC",
                              includeMarkdown("ProteomeExpert-QC.md")
                     ),
                     tabPanel("Statistics",
                              includeMarkdown("ProteomeExpert-Statistics.md")
                     ),
                     tabPanel("Data Mining",
                              "Coming Soon"
                     ),
                     tabPanel("Other Tools",
                              "Coming Soon"
                     )
                   )
                 )
        ),
        tabPanel("Navbar 3", "This panel is intentionally left blank")
      )
    )
  ),
  server = function(input, output) {
    DdatasetInput <- eventReactive(input$action2, {
      cars[1,1]
    }, ignoreNULL = FALSE)
    output$txtout <- renderText({
      paste(iris,input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
      head(cars, 2)
    })
    output$ttest_out <- renderText({
      iris
    })
  }
)