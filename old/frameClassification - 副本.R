library(shiny)
library(shinythemes)
options(shiny.maxRequestSize=300*1024^2)
source(file = "test.R")
source(file = "preprocess.R")
shinyApp(
  ui = tagList(
    #shinythemes::themeSelector(),
    navbarPage(
      theme = shinytheme("sandstone"),  # <--- To use a theme, uncomment this
      "Classification",
      tabPanel("Batch Design",
               sidebarPanel(
                 fileInput("file", "File input:"),
                 #textInput("txt", "Text input:", "general"),
                 sliderInput("n", "Technical replica number for each sample:", 1, 10, 2),
                 sliderInput("m", "Number of samples in each batch:", 1, 100, 15),
                 
                 tags$h5("Click to design:"),
                 actionButton("design", "Submit", class = "btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Result",
                            h4("Summary"),
                            tableOutput("table"),
                            h4("Your input info."),
                            verbatimTextOutput("txtout")

                   )
                   #tabPanel("", "This panel is intentionally left blank"),
                   #tabPanel("Tab 3", "This panel is intentionally left blank")
                 )
               )
      ),
      tabPanel("Power Analysis", "",
               sidebarPanel(
                 #fileInput("file", "File input:"),
                 textInput("Pm", "Number of Proteins (estimated):", 5000,width = "30%"),
                 textInput("Pmu", "Mean abundance:", 13, width = "30%"),
                 textInput("Palpha", "Significance Level:", 0.05, width = "30%"),
                 
                 tags$h5("Click to estimate:"),
                 actionButton("power", "Submit", class = "btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Results",
                            h4("Summary"),
                            tableOutput("Ptable"),
                            h4("Your input info."),
                            verbatimTextOutput("Pparameters")
                            
                   )
                 )
               )),
      tabPanel("Data Preprocessing", "",
               sidebarPanel(
                 # Input: Select separator ----
                 
                 fileInput("PeptideMatrix", "Select your peptide matrix:",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 checkboxInput("Dpheader", "Header", TRUE),
                 radioButtons("Dpsep", "Separator for your matrix",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              inline = TRUE,
                              selected = "\t"),
                 tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
                 
                 fileInput("TechnicalReplicate", "Select your technical replicate file:"),
                 checkboxInput("Dtheader", "Header", FALSE),
                 radioButtons("Dtsep", "Separator for your matrix",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              inline = TRUE,
                              selected = "\t"),
                 
                 tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
                 
                 fileInput("BatchFile", "Select your batch effect file:"),
                 checkboxInput("Dbheader", "Header", TRUE),
                 radioButtons("Dbsep", "Separator for your matrix",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              inline = TRUE,
                              selected = "\t"),
                 
                 tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
                 
                 tags$h5("Click to process:"),
                 actionButton("process", "Submit", class = "btn-primary")
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Results",
                            h4("Summary"),
                            tableOutput("Dtable"),
                            h4("Get all protein matrix"),
                            downloadButton("downloadData", "Download", class = "btn-primary")

                   )
                   #tabPanel("", "This panel is intentionally left blank"),
                   #tabPanel("Tab 3", "This panel is intentionally left blank")
                 )
               )),
      #######################QC
      tabPanel("QC", "",
               sidebarPanel(
               # Input: Checkbox if file has header ----
               checkboxInput("Qheader", "Header", TRUE),
               
               
               # Input: Select separator ----
               radioButtons("Qsep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = "\t"),
               
               # Input: Select quotes ----
               radioButtons("Qquote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = ''),
               
               
               # Horizontal line ----
               tags$hr(),
               # Input: Select a file ----
               fileInput("Qfile", "Choose protein/peptide CSV/TXT File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               fileInput("anno", "Choose annotation File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
               
               tags$h5("Click to process:"),
               actionButton("dm", "Submit", class = "btn-primary")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Missing value",
                 h4("Summary"),
                 tableOutput("QMtable"),
                 h4("Your input info."),
                 verbatimTextOutput("QMparameters")
                 
        
      ),
      
        tabPanel("Reproducibility",
                 h4("Summary"),
                 tableOutput("QPtable"),
                 h4("Your input info."),
                 verbatimTextOutput("QPparameters")
                 ),
      tabPanel("PCA",
               h4("Summary"),
               tableOutput("Qpcatable"),
               h4("Your input info."),
               verbatimTextOutput("Qpcaparameters")
      ),
      tabPanel("Umap",
               h4("Summary"),
               tableOutput("QUtable"),
               h4("Your input info."),
               verbatimTextOutput("QUparameters")
      )
      )
      )
    ),
    
   #######################data mining 
      tabPanel("Data Mining", "",
               sidebarPanel(
                 # Input: Checkbox if file has header ----
                 checkboxInput("DMheader", "Header", TRUE),
                 
                 
                 # Input: Select separator ----
                 radioButtons("DMsep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = "\t"),
                 
                 # Input: Select quotes ----
                 radioButtons("DMquote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = ''),
                 
                 
                 # Horizontal line ----
                 tags$hr(),
                 # Input: Select a file ----
                 fileInput("DMfile", "Choose protein/peptide CSV/TXT File",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 # Horizontal line ----
                 tags$hr(),
                 # Input: Select a file ----
                 fileInput("anno", "Choose annotation File",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
                 
                 tags$h5("Click to process:"),
                 checkboxInput("dmheatmap", "HeatMap", TRUE),
                 checkboxInput("test", "t-test", TRUE),
                 checkboxInput("vocanoPlot", "ViocanoPlot", TRUE),
                 checkboxInput("ViolinPlot", "ViolinPlot", TRUE),
                 actionButton("dm", "Submit", class = "btn-primary")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("HeatMap",
                            h4("Summary"),
                            tableOutput("QMtable"),
                            h4("Your input info."),
                            verbatimTextOutput("QMparameters")
                            
                            
                   ),
                   
                   tabPanel("t-test",
                            h4("Summary"),
                            tableOutput("QPtable"),
                            h4("Your input info."),
                            verbatimTextOutput("QPparameters")
                   ),
                   tabPanel("VocanoPlot",
                            h4("Summary"),
                            tableOutput("Qpcatable"),
                            h4("Your input info."),
                            verbatimTextOutput("Qpcaparameters")
                   ),
                   tabPanel("ViolinPlot",
                            h4("Summary"),
                            tableOutput("QUtable"),
                            h4("Your input info."),
                            verbatimTextOutput("QUparameters")
                   ),
                   tabPanel("RadarMap",
                            h4("Summary"),
                            tableOutput("QUtable"),
                            h4("Your input info."),
                            verbatimTextOutput("QUparameters")
                   )
                 )
               )
               )
    )
  ),
  server = function(input, output) {
    output$txtout <- renderText({
      paste(input$n, input$m, sep = ", ")
    })
    output$table <- renderTable({
      head(iris, 4)
    })
    ###############################Power Analysis
    output$Pparameters <- renderText({
      paste(input$Pm, input$Pmu,input$Palpha, sep = ", ")
    })
    output$Ptable <- renderTable({
      head(iris, 4)
    })
    ###############################data preprocess
    # updated when the user clicks the button
    flag<<-TRUE
    DdatasetInput <- eventReactive(input$process,{
      if(is.null(input$PeptideMatrix))
        "Please upload your files!"
      else
      auto_preprocess(isolate(input$PeptideMatrix$datapath),isolate(input$TechnicalReplicate$datapath),psep = isolate(input$Dpsep),
                              pheader = isolate(input$Dpheader), tsep = isolate(input$Dtsep),theader = isolate(input$Dtheader),
                              bsep = isolate(input$Dbsep),bheader = isolate(input$Dbheader))

    }, ignoreNULL = FALSE)
    
    output$Dtable <- renderTable({
      #peptide<-read.table(input$PeptideMatrix$datapath,header=TRUE)
      #protM<<-head(peptide,5)
      #if (!is.na(DdatasetInput()))
      head(DdatasetInput())
      #else "Please upload your files!"
    })
    output$test<-renderText({
      paste("your input res",f(3), sep = ", ")
    })
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("prot",Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        write.table(protM, file, row.names = FALSE,quote = F,sep = "\t")
      }
    )
  }
)