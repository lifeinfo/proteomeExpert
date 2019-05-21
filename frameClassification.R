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
      "ProteomeExpert |",

      ####################################################Introducation
      tabPanel("Introducation", "",
               sidebarPanel(
                 tags$h5("Click to estimate:"),
                 HTML("<p>This page remains for what?</p>")
      
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Introduction",
                            h4("Summary"),
                            HTML("<p>proteomeExpert was published in * 2019 </p>")
                            
                   )),
                   tabsetPanel(
                     tabPanel("Citation",
                              h4(""),
                              HTML("<p>proteomeExpert was published in * 2019 </p>")
                              
                     )
                   )
               )),
      ####################################################Data console
      tabPanel("Data Console", "",
               sidebarPanel(
                 tags$h2("Upload Data:"),
                 tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
                 fileInput("peptide_matrix", "Select your peptide file (optional):",
                           multiple = F,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 tags$hr(style="height:2px;border:none;border-top:2px ridge gray;"),
                 
                 fileInput("protein_matrix", "Select your protein file (optional):",
                           multiple = F,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 tags$hr(style="height:2px;border:none;border-top:2px ridge gray;"),
                 
                 fileInput("sample_info", "Select your sample file (optional):",
                           multiple = F,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 #actionButton("sampleInfo", "annotation", class = "btn-primary"),
                 tags$hr(style="height:2px;border:none;border-top:2px ridge gray;"),
                 
                 fileInput("individual_info", "Select your individual file (optional):",
                           multiple = F,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"))
                 #actionButton("individualInfo", "annotation", class = "btn-primary")
                 
               ),
               mainPanel(
                 fluidRow(
                   column(3, wellPanel(
                     tags$h4("Annotate sample columns:"),
                     tags$hr(style="algin:right;height:2px;border:none;border-top:2px ridge gray;"),
                     
                     # This outputs the dynamic UI component
                     uiOutput("sampleUi")
                   )),

                   column(3, offset = 0.5, wellPanel(
                     tags$h4("Annotate individual columns:"),
                     tags$hr(style="algin:right;height:2px;border:none;border-top:2px ridge gray;"),
                     
                     # This outputs the dynamic UI component
                     uiOutput("individualUi")
                   ))
                 ),
                 fluidRow(column(width = 6, wellPanel(
                   # This outputs the dynamic UI component
                     textOutput("sampleRes")

                 )))
               )),
      ####################################################power analysis
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
      #####################################################Batch design
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

      tabPanel("Data Preprocessing", "",
               sidebarPanel(
                 # Input: Select separator ----
                 
                 fileInput("PeptideMatrix", "Select your peptide matrix (required):",
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
                 
                 fileInput("TechnicalReplicate", "Select your technical replicate file (required):"),
                 checkboxInput("Dtheader", "Header", FALSE),
                 radioButtons("Dtsep", "Separator for your matrix",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              inline = TRUE,
                              selected = "\t"),
                 
                 tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
                 
                 fileInput("BatchFile", "Select your batch effect file (optional):"),
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
                 actionButton("qc", "Submit", class = "btn-primary")
                 
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
     ############################################################################################################## 
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
                           tableOutput("DMhmtable"),
                           h4("Your input info."),
                           verbatimTextOutput("DMhmparameters")
                           
                           
                  ),
                  
                  tabPanel("t-test",
                           h4("Summary"),
                           tableOutput("DMttable"),
                           h4("Your input info."),
                           verbatimTextOutput("DMtparameters")
                  ),
                  tabPanel("VocanoPlot",
                           h4("Summary"),
                           tableOutput("DMvocanotable"),
                           h4("Your input info."),
                           verbatimTextOutput("DMvocanoparameters")
                  ),
                  tabPanel("ViolinPlot",
                           h4("Summary"),
                           tableOutput("DMviolintable"),
                           h4("Your input info."),
                           verbatimTextOutput("DMviolinparameters")
                  ),
                  tabPanel("RadarMap",
                           h4("Summary"),
                           tableOutput("DMrmtable"),
                           h4("Your input info."),
                           verbatimTextOutput("DMrmparameters")
                  )
                )
              )
     )
    )
  ),
  ################################################server############################################################################################
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
    DdatasetInput <- eventReactive(input$process,{
      if(is.null(input$PeptideMatrix))
        "Please upload your files!"
      else
      auto_preprocess(isolate(input$PeptideMatrix$datapath),isolate(input$TechnicalReplicate$datapath),isolate(input$BatchFile$datapath),psep = isolate(input$Dpsep),
                              pheader = isolate(input$Dpheader), tsep = isolate(input$Dtsep),theader = isolate(input$Dtheader),
                              bsep = isolate(input$Dbsep),bheader = isolate(input$Dbheader))

    }, ignoreNULL = FALSE)
    
    output$Dtable <- renderTable({
      #peptide<-read.table(input$PeptideMatrix$datapath,header=TRUE)
      #protM<<-head(peptide,5)
      #if (!is.na(DdatasetInput()))
      t<-data.frame(DdatasetInput())
      n=5
      if(ncol(t)<5)
         n=ncol(t)
      head(t[,1:n])
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
        write.table(DdatasetInput(), file, row.names = FALSE,quote = F,sep = "\t")
      }
    )
    ############################data console##############################################################
    output$sampleUi <- renderUI({
      if (is.null(input$sample_info))
        "Please upload your files!"
      else{#print(input$sample_info)
      sample_info<-read.csv(input$sample_info$datapath,header = T,sep = ",",nrow=1)
      sample_header<<-colnames(sample_info)
      tagList(
      selectInput("sample_info_id", "select sample id",
                  choices = sample_header,
                  selected = sample_header[1]
      ),
      selectInput("sample_info_type", "select sample type",
                  choices = sample_header,
                  selected = sample_header[2]
      ),
      selectInput("sample_info_batch_id", "select batch id",
                  choices = c(sample_header,"select..."),
                  selected = "select..."
      ),
      selectInput("sample_info_technicalRep_id", "select technical replica id",
                  choices = c(sample_header,"select..."),
                  selected = "select..."
      ),
      selectInput("sample_info_individual_id", "select individual id",
                  choices = c(sample_header,"select..."),
                  selected = "select..."
      ),
      actionButton("sample_info_annotation", "Submit", class = "btn-primary")
      )
      }
    })
    # sample_annotation <- eventReactive(input$sample_info_annotation,{
    #   str(input$sample_info_id)
    #        
    # }, ignoreNULL = FALSE)
    # output$dynamic_value <- renderPrint({
    #   
    #   sample_annotation()
    #   #read.csv(input$sample_info$datapath,header = F,sep = "\t",nrows = 1)
    # })
    
    output$individualUi <- renderUI({
      if (is.null(input$individual_info))
        "Please upload your files!"
      else{#print(input$sample_info)
        individual_info<-read.csv(input$individual_info$datapath,header = T,sep = ",",nrow=1)
        individual_header<-colnames(individual_info)
        tagList(
          selectInput("individual_info_id", "select individual id/name",
                      choices = individual_header,
                      selected = individual_header[1]
          ),
          selectInput("individual_info_type", "select individual type",
                      choices = individual_header,
                      selected = individual_header[2]
          ),

          actionButton("individual_info_annotation", "Submit", class = "btn-primary")
        )
      }
    })
    sampleInfoInput <- eventReactive(input$sample_info_annotation,{
       sampleInfo<-read.csv(input$sample_info$datapath,header = T,sep = ",")
       sample_header<-colnames(sampleInfo)
       colnames(sampleInfo)[which(sample_header==input$sample_info_id)]<-"sampleId"
       colnames(sampleInfo)[which(sample_header==input$sample_info_type)]<-"sampleType"
    
      if(input$sample_info_batch_id!='select...')
         colnames(sampleInfo)[which(sample_header==input$sample_info_batch_id)]<-"batchId"
      if(input$sample_info_technicalRep_id!='select...')
         colnames(sampleInfo)[which(sample_header==input$sample_info_technicalRep_id)]<-"technicalId"
      if(input$sample_info_individual_id!='select...')
         colnames(sampleInfo)[which(sample_header==input$sample_info_individual_id)]<-"individualId"
      #print(head(sampleInfoInput()))
      sampleInfo
    }, ignoreNULL = T)
    
    sampleResInput<-eventReactive(input$sample_info_annotation,{
      sampleInfoInput()
    })
    output$sampleRes <- renderText({
        if(class(sampleResInput())=="data.frame")
          "Sucessed annotation sample column!"
        else "failed annotation sample column"
    })
    # output$dynamic_table <- renderTable({
    #   
    #   t<-data.frame(sampleInfoInput())
    #   n=5
    #   if(ncol(t)<5)
    #     n=ncol(t)
    #   head(t[,1:n])
    # 
    # })
  }
)