# Define UI for data upload app ----
ui <- fluidPage(# App title ----
                titlePanel("Uploading Files"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    height = 15,
                    # Input: Checkbox if file has header ----
                    checkboxInput("header", "Header", TRUE),
                    
                    
                    # Input: Select separator ----
                    radioButtons(
                      "sep",
                      "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"
                      ),
                      selected = "\t"
                    ),
                    
                    # Input: Select quotes ----
                    radioButtons(
                      "quote",
                      "Quote",
                      choices = c(
                        None = "",
                        "Double Quote" = '"',
                        "Single Quote" = "'"
                      ),
                      selected = ''
                    ),
                    
                    
                    # Horizontal line ----
                    tags$hr(),
                    # Input: Select a file ----
                    fileInput(
                      "file1",
                      "Choose CSV/TXT File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                    ),
                    helpText("")
                  ),
                  
                  
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    #(height =500),
                    
                    tags$hr(),
                    h3(textOutput("caption")),
                    plotOutput("missingPlot"),
                    width = 8,
                    height = 15
                  )
                  
                ))

# Define server logic to read selected file ----
server <- function(input, output) {
  output$missingPlot <- renderPlot({
    cbbPalette <-
      c(
        "#000000",
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7"
      )
    req(input$file1)
    
    df <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
    
    
    data <- df
    rNA = matrix(0, nrow(data), 1)
    
    system.time(rNA[, 1] <-
                  apply(data, 1, function(x) {
                    length(which(is.na(x)))
                  }))
    cNA = matrix(0, ncol(data), 1)
    
    system.time(cNA[, 1] <-
                  apply(data, 2, function(x) {
                    length(which(is.na(x)))
                  }))
    
    
    
    #plot
    cna <- as.vector(cNA[, 1]) / nrow(rNA)
    rna <- as.vector(rNA[, 1]) / nrow(cNA)
    cna.cut <- cut(cna, breaks = seq(0, 1, by = 0.1))
    
    rna.cut <- cut(rna, breaks = seq(0, 1, by = 0.1))
    print("Results")
    mymagnify = 1.5
    layout(matrix(c(1:4, 2, 3), 3))
    plot(
      cna.cut,
      main = "missing rate for column",
      xlab = "missing rate",
      ylab = "#of column",
      col = cbbPalette[2],
      cex = mymagnify,
      cex.lab = mymagnify,
      cex.axis = mymagnify,
      cex.main = mymagnify
    )
    barplot(
      cNA[, 1],
      main = "missing values for column",
      xlab = "column index",
      ylab = "#of missing values",
      col = cbbPalette[2],
      cex = mymagnify,
      cex.lab = mymagnify,
      cex.axis = mymagnify,
      cex.main = mymagnify
    )
    barplot(
      rNA[, 1],
      main = "missing values for row",
      xlab = "row index",
      ylab = "#of missing values",
      col = cbbPalette[3],
      cex = mymagnify,
      cex.lab = mymagnify,
      cex.axis = mymagnify,
      cex.main = mymagnify
    )
    plot(
      rna.cut,
      main = "missing rate for row",
      xlab = "missing rate for column",
      ylab = "#of row",
      col = cbbPalette[3],
      cex = mymagnify,
      cex.lab = mymagnify,
      cex.axis = mymagnify,
      cex.main = mymagnify
    )
    
    
    
  }, height = 800, units = "px")
}

# Create Shiny app ----
shinyApp(ui, server)
