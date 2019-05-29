library(shiny)
library(shinythemes)

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
      ################################ Study design
      tabPanel("Study Design", "",
               # sidebarPanel(
               #   HTML("<p>This page remains for what?</p>"),
               #   width=2
               # ),
               #mainPanel(     
      tabsetPanel(
        tabPanel("Power Analysis",
                 sidebarPanel(
                   tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
                   
                   tags$h2("Power analysis with pilot experiment:"),
                   tags$hr(style="height:2px;border:none;border-top:2px ridge gray;"),
                   
                   fileInput("file", "Select pilot experiment protein matrix:"),
                   actionButton("powera", "Submit", class = "btn-primary"),
                   
                   tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
                   tags$h2("Power analysis by direct set parameters:"),
                   tags$hr(style="height:2px;border:none;border-top:2px ridge gray;"),
                   textInput("Pm", "Number of Proteins (estimated):", 5000,width = "30%"),
                   textInput("Pmu", "Mean abundance:", 13, width = "30%"),
                   textInput("Pmu0", "Mean abundance 0:", 13, width = "30%"),
                   
                   textInput("Palpha", "Significance Level:", 0.05, width = "30%"),
                   
                   tags$h5("Click to estimate:"),
                   actionButton("powerb", "Submit", class = "btn-primary"),
                   tags$hr(style="height:3px;border:none;border-top:3px ridge green;")
                   
                 )
                 
        ),
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
        )
        
      )
      ),
     
      ####################################################        Data console    #################################### 
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
                   
                 ))),
                 fluidRow(column(width = 6, wellPanel(
                   # This outputs the dynamic UI component
                   textOutput("individualRes")
                   
                 )))
               )),      
            
###################################################    data preprocessing      ####################################
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
    
      ######################################            QC             ##################################################################################
      tabPanel("QC", "",
               sidebarPanel(
                 tags$h2("Select modules you want to process:"),
                 tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
                 checkboxInput("MissingValueExplore_check", "MissingValueExplore", TRUE),
                 checkboxInput("reproducibility", "Reproducibility", TRUE),
                 checkboxInput("qcPca", "PCA", TRUE),
                 checkboxInput("qcUmap", "UMAP", FALSE),

                 tags$hr(style="height:3px;border:none;border-top:3px ridge green;"),
             
                 tags$h5("Click to process:"),
                 actionButton("QC", "Submit", class = "btn-primary")
                 
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Missing value",
                            tags$hr(),
                            #h3(textOutput("caption")),
                            verbatimTextOutput("QMparameters"),
                            conditionalPanel("output.QMparameters == 'Results are showed below:'",
                            downloadButton('downloadMissingPlot')),
                            
                            plotOutput("missingPlot"),
                            
                            width = 8,
                            height=15
                            
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
                  ),
                  tabPanel("HelpMe",
                           h4("help info.")
                  )
                )
              )
     )
    )
  ),
  ############################################   server           ###################################################################################
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
    ###############################     data preprocess        ##############################
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
    ############################              QC             ##############################################################
    
    QCdatasetInput <- eventReactive(input$QC,{
      if(class(readProteinM())!="data.frame")
        return(NULL)
      else
        readProteinM()
      
    }, ignoreNULL = FALSE)
    
    # output$QMparameters <- renderText({
    #   QCdatasetInput()
    # })
    ##############missing value explore 
    
    output$missingPlot <- renderPlot({
      if(input$MissingValueExplore_check){
         if(class(QCdatasetInput())=="data.frame"){
         output$QMparameters<-renderText({"Results are showed below:"})
         source("missingValueExplore_zts.R",local = T )
         missing_plot(readProteinM())
         }
         else if(is.null(readProteinM())) 
           {output$QMparameters<-renderText({"Please upload your protein file in 'data console' tab!"})}
         else output$QMparameters<-renderText({"Please click the submit button!"})
      }
      
      else output$QMparameters<-renderText({"Please select MissingValueExplore checkbox and sumbit!"})
    
    },height=800,units="px")
    
    output$downloadMissingPlot <- downloadHandler(
      filename = function() {
        paste("missingValueExplore",Sys.time(), ".pdf", sep = "")
      },
      content = function(file) {
       pdf(file)
       source("missingValueExplore_zts.R",local = T ) 
       missing_plot(readProteinM())
       dev.off()
      }
    )
    ############################              data console             ##############################################################
    
    ### for read protein matrix
    readProteinM<-reactive({
      if(!is.null(input$protein_matrix))
           prot<-read.table(input$protein_matrix$datapath,header = T,sep = "\t",check.names = F,encoding ="UTF-8")
      })
    ### for column annotation
    output$sampleUi <- renderUI({
      if (is.null(input$sample_info))
        "Please upload your files!"
      else{#print(input$sample_info)
      sample_info<-read.csv(input$sample_info$datapath,header = T,sep = ",",nrow=1,check.names = F,encoding ="UTF-8")
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

    
    output$individualUi <- renderUI({
      if (is.null(input$individual_info))
        "Please upload your files!"
      else{#print(input$sample_info)
        individual_info<-read.csv(input$individual_info$datapath,header = T,sep = ",",nrow=1,check.names = F,encoding ="UTF-8")
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
          paste("Successfully annotated the sample column!",Sys.time())
        else paste("Unsuccessfully annotated sample column",Sys.time())
    })
    individualInfoInput <- eventReactive(input$individual_info_annotation,{
      individualInfo<-read.csv(input$individual_info$datapath,header = T,sep = ",")
      individual_header<-colnames(individualInfo)
      colnames(individualInfo)[which(individual_header==input$individual_info_id)]<-"individualId"
      colnames(individualInfo)[which(individual_header==input$individual_info_type)]<-"individualType"
      
      individualInfo
    }, ignoreNULL = T)
    
    individualResInput<-eventReactive(input$individual_info_annotation,{
      individualInfoInput()
    })
    output$individualRes <- renderText({
      if(class(individualResInput())=="data.frame")
        paste("Successfully annotated the individual column!",Sys.time())
      else paste("Unsuccessfully annotated individual column",Sys.time())
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