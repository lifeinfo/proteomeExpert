#shinythemes::themeSelector(),
navbarPage(
  theme = shinytheme("cerulean"),
  # <--- To use a theme, uncomment this
  "ProteomeExpert |",
  
  ####################################################Introducation
  tabPanel(
    "Introducation",
    "",
    sidebarPanel(
      tags$h5("Click to estimate:"),
      HTML("<p>This page remains for what?</p>")
      
    ),
    mainPanel(tabsetPanel(tabPanel(
      "Introduction",
      h4("Summary"),
      HTML("<p>proteomeExpert was published in * 2019 </p>")
      
    )),
    tabsetPanel(tabPanel(
      "Citation",
      h4(""),
      HTML("<p>proteomeExpert was published in * 2019 </p>")
      
    )))
  ),
  ################################ Study design
  tabPanel("Study Design",
           "",
           # sidebarPanel(
           #   HTML("<p>This page remains for what?</p>"),
           #   width=2
           # ),
           #mainPanel(
           tabsetPanel(
             tabPanel(
               "Power Analysis",
               sidebarPanel(
                 tags$hr(style = "height:3px;border:none;border-top:3px ridge green;"),
                 
                 tags$h2("Power analysis with pilot experiment:"),
                 tags$hr(style = "height:2px;border:none;border-top:2px ridge gray;"),
                 
                 fileInput("file", "Select pilot experiment protein matrix:"),
                 actionButton("powera", "Submit", class = "btn-primary"),
                 
                 tags$hr(style = "height:3px;border:none;border-top:3px ridge green;"),
                 tags$h2("Power analysis by direct set parameters:"),
                 tags$hr(style = "height:2px;border:none;border-top:2px ridge gray;"),
                 textInput("Pm", "Number of Proteins (estimated):", 5000, width = "30%"),
                 textInput("Pmu", "Mean abundance:", 13, width = "30%"),
                 textInput("Pmu0", "Mean abundance 0:", 13.5, width = "30%"),
                 textInput("Psd", "Standard deviation:", 0.75, width = "30%"),
                 
                 textInput("Palpha", "Alpha:", 0.05, width = "30%"),
                 textInput("Pbeta", "Beta (Power=1-beta):", 0.2, width = "30%"),
                 
                 tags$h5("Click to estimate:"),
                 actionButton("powerb", "Submit", class = "btn-primary"),
                 tags$hr(style = "height:3px;border:none;border-top:3px ridge green;")
               ),
               
               mainPanel(tabsetPanel(
                 tabPanel(
                   "Sample size",
                   h4("Sample size"),
                   verbatimTextOutput("powerSize"),
                   plotOutput("powerPlot")
                 )
               ))
             ),
             tabPanel(
               "Batch Design",
               sidebarPanel(
                 fileInput("file", "File input:"),
                 #textInput("txt", "Text input:", "general"),
                 sliderInput("n", "Technical replica number for each sample:", 1, 10, 2),
                 sliderInput("m", "Number of samples in each batch:", 1, 100, 15),
                 
                 tags$h5("Click to design:"),
                 actionButton("design", "Submit", class = "btn-primary")
               ),
               mainPanel(tabsetPanel(
                 tabPanel(
                   "Result",
                   h4("Summary"),
                   tableOutput("table"),
                   h4("Your input info."),
                   verbatimTextOutput("txtout")
                 )
                 #tabPanel("", "This panel is intentionally left blank"),
                 #tabPanel("Tab 3", "This panel is intentionally left blank")
               ))
             )
             
           )),
  
  ###################################################    data preprocessing      ####################################
  tabPanel(
    "Data Preprocessing",
    "",
    sidebarPanel(
      # Input: Select separator ----
      
      fileInput(
        "PeptideMatrix",
        "Select your peptide matrix (required):",
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      checkboxInput("Dpheader", "Header", TRUE),
      radioButtons(
        "Dpsep",
        "Separator for your matrix",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        inline = TRUE,
        selected = "\t"
      ),
      tags$hr(style = "height:3px;border:none;border-top:3px ridge green;"),
      
      fileInput(
        "TechnicalReplicate",
        "Select your technical replicate file (required):"
      ),
      checkboxInput("Dtheader", "Header", FALSE),
      radioButtons(
        "Dtsep",
        "Separator for your matrix",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        inline = TRUE,
        selected = "\t"
      ),
      
      tags$hr(style = "height:3px;border:none;border-top:3px ridge green;"),
      
      fileInput("BatchFile", "Select your batch effect file (optional):"),
      checkboxInput("Dbheader", "Header", TRUE),
      radioButtons(
        "Dbsep",
        "Separator for your matrix",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        inline = TRUE,
        selected = "\t"
      ),
      
      tags$hr(style = "height:3px;border:none;border-top:3px ridge green;"),
      
      tags$h5("Click to process:"),
      actionButton("process", "Submit", class = "btn-primary")
      
    ),
    mainPanel(tabsetPanel(
      tabPanel(
        "Results",
        h4("Summary"),
        tableOutput("Dtable"),
        h4("Get all protein matrix"),
        downloadButton("downloadData", "Download", class = "btn-primary")
        
      )
      
    ))
  ),
  
  ####################################################        Data console    ####################################
  tabPanel(
    "Data Console",
    "",
    sidebarPanel(
      tags$h2("Upload Data:"),
      tags$hr(style = "height:3px;border:none;border-top:3px ridge green;"),
      fileInput(
        "peptide_matrix",
        "Select your peptide file (optional):",
        multiple = F,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      tags$hr(style = "height:2px;border:none;border-top:2px ridge gray;"),
      
      fileInput(
        "protein_matrix",
        "Select your protein file (optional):",
        multiple = F,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      tags$hr(style = "height:2px;border:none;border-top:2px ridge gray;"),
      
      fileInput(
        "sample_info",
        "Select your sample file (optional):",
        multiple = F,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      #actionButton("sampleInfo", "annotation", class = "btn-primary"),
      tags$hr(style = "height:2px;border:none;border-top:2px ridge gray;"),
      
      fileInput(
        "individual_info",
        "Select your individual file (optional):",
        multiple = F,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      )
      #actionButton("individualInfo", "annotation", class = "btn-primary")
      
    ),
    mainPanel(
      fluidRow(column(
        3, wellPanel(
          tags$h4("Annotate sample columns:"),
          tags$hr(style = "algin:right;height:2px;border:none;border-top:2px ridge gray;"),
          
          # This outputs the dynamic UI component
          uiOutput("sampleUi")
        )
      ),
      
      column(
        3, offset = 0.5, wellPanel(
          tags$h4("Annotate individual columns:"),
          tags$hr(style = "algin:right;height:2px;border:none;border-top:2px ridge gray;"),
          
          # This outputs the dynamic UI component
          uiOutput("individualUi")
        )
      )),
      fluidRow(column(width = 6, wellPanel(
        # This outputs the dynamic UI component
        textOutput("sampleRes")
        
      ))),
      fluidRow(column(width = 6, wellPanel(
        # This outputs the dynamic UI component
        textOutput("individualRes")
        
      ))),
      tags$hr(style = "algin:right;height:2px;border:none;border-top:2px ridge gray;"),
      actionButton("DoAnnoTable", "MergeTwo", class = "btn-primary"),
      fluidRow(column(width = 8, wellPanel(
        # This outputs the dynamic UI component
        DT::dataTableOutput("annoTable")
        
      )))
    )
  ),
  
  ######################################QC#########################
  tabPanel(
    "QC",
    "",
    sidebarPanel(
      tags$h2("Select modules you want to process:"),
      tags$hr(style = "height:3px;border:none;border-top:3px ridge green;"),
      checkboxInput("MissingValueExplore_check", "MissingValueExplore", TRUE),
      checkboxInput("reproducibility", "Reproducibility", TRUE),
      checkboxInput("qcPca", "PCA", TRUE),
      checkboxInput("qcUmap", "UMAP", FALSE),
      
      tags$hr(style = "height:3px;border:none;border-top:3px ridge green;"),
      
      tags$h5("Click to process:"),
      actionButton("QC", "Submit", class = "btn-primary")
      
    ),
    
    mainPanel(tabsetPanel(
      tabPanel(
        "Missing value",
        tags$hr(),
        #h3(textOutput("caption")),
        verbatimTextOutput("QMparameters"),
        conditionalPanel(
          "output.QMparameters == 'Results are showed below:'",
          downloadButton('downloadMissingPlot')
        ),
        
        plotOutput("missingPlot"),
        
        width = 8,
        height = 15
        
      ),
      
      tabPanel(
        "Reproducibility",
        h4("Summary"),
        tableOutput("QPtable"),
        h4("Your input info."),
        verbatimTextOutput("QPparameters")
      ),
      tabPanel(
        "PCA",
        h4("Summary"),
        tableOutput("Qpcatable"),
        h4("Your input info."),
        verbatimTextOutput("Qpcaparameters")
      ),
      tabPanel(
        "Umap",
        h4("Summary"),
        tableOutput("QUtable"),
        h4("Your input info."),
        verbatimTextOutput("QUparameters")
      )
    ))
  ),
  ######################################################################ANNO###################
  tabPanel(
    "Annotations",
    "",
    sidebarPanel(
      textAreaInput(
        "proteinlist",
        "Protein list:",
        height = 150,
        placeholder = "Example:Q9Y6B6,P35659,O43759&,A0A0B4J1V6,E9PAV3"
      ),
      selectInput("Organism", "Choose Organism:",
                  choices = c("Homo sapiens")),
      helpText("More Organism will be comming soon!"),
      selectInput(
        "Database",
        "Choose a Database:",
        choices = c("All", "Uniport", "String-db", "KEGG", "GO", "Reactome")
      ),
      HTML(
        '<p>
        <strong>Info:</strong>
        </p>
        <ul class=" list-paddingleft-2" style="list-style-type: disc;">
        <li>
        <p>
        Uniport:The mission of UniProt is to provide the scientific community with a comprehensive, high-quality and freely accessible resource of protein sequence and functional information.<span style="color: rgb(255, 0, 0);">web:https://www.uniprot.org</span>
        </p>
        </li>
        <li>
        <p>
        String-db:Protein-Protein Interaction Networks.<span style="color: rgb(255, 0, 0);">web:https://string-db.org/cgi/input.pl</span>
        </p>
        </li>
        <li>
        <p>
        KEGG:KEGG is a database resource for understanding high-level functions and utilities of the biological system, such as the cell, the organism and the ecosystem, from molecular-level information, especially large-scale molecular datasets generated by genome sequencing and other high-throughput experimental technologies.<span style="color: rgb(255, 0, 0);">web:https://www.kegg.jp</span>
        </p>
        </li>
        <li>
        <p>
        Go:The Gene Ontology (GO) knowledgebase is the world’s largest source of information on the functions of genes.<span style="color: rgb(255, 0, 0);">web:http://geneontology.org</span>
        </p>
        </li>
        <li>
        <p>
        Reactome:Reactome is a free, open-source, curated and peer-reviewed pathway database.<span style="color: rgb(255, 0, 0);">web:https://www.reactome.org</span>
        </p>
        </li>
        </ul>
        <p>
        <br/>
        </p>'
      ),
      actionButton("annosubmit", "Submit", class = "btn-primary")
      #verbatimTextOutput("Annoparameters")
      ),
    mainPanel(tabsetPanel(
      tabPanel(
        "Result",
        h4("Summary"),
        HTML("<p><strong>Input:</strong></p>"),
        verbatimTextOutput("anno_parameters1")
      ),
      hr(),
      h2('Display'),
      fluidRow(column(
        4, verbatimTextOutput("anno_parameters2")
      ),
      #column(6, DTOutput("anno_table")))
      column(8, rHandsontableOutput("anno_table"))),
      hr(),
      downloadButton('downloadAnno_db')
    ))
    ),
  
  ##############################################################################################################
  #######################data mining
  tabPanel(
    "Data Mining",
    "",
    sidebarPanel(
      selectInput('DMprotM', 'select matrix', protM_name, selectize=FALSE),
      selectInput('DManno', 'select types', anno_name, multiple=TRUE, selectize=TRUE),
      tags$hr(style = "height:3px;border:none;border-top:3px ridge green;"),
      
      tags$h5("Click to process:"),
      checkboxInput("dmheatmap", "HeatMap", TRUE),
      checkboxInput("test", "t-test", TRUE),
      checkboxInput("vocanoPlot", "ViocanoPlot", TRUE),
      checkboxInput("ViolinPlot", "ViolinPlot", TRUE),
      actionButton("dm", "Submit", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "HeatMap",
          h4("Summary"),
          tableOutput("DMhmtable"),
          h4("Your input info."),
          verbatimTextOutput("DMhmparameters")
          
          
        ),
        
        tabPanel(
          "t-test",
          h4("Summary"),
          tableOutput("DMttable"),
          h4("Your input info."),
          verbatimTextOutput("DMtparameters")
        ),
        tabPanel(
          "VocanoPlot",
          h4("Summary"),
          tableOutput("DMvocanotable"),
          h4("Your input info."),
          verbatimTextOutput("DMvocanoparameters")
        ),
        tabPanel(
          "ViolinPlot",
          h4("Summary"),
          tableOutput("DMviolintable"),
          h4("Your input info."),
          verbatimTextOutput("DMviolinparameters")
        ),
        tabPanel(
          "RadarMap",
          h4("Summary"),
          tableOutput("DMrmtable"),
          h4("Your input info."),
          verbatimTextOutput("DMrmparameters")
        ),
        tabPanel(
          "FeatureSel",
          h4("Summary"),
          tableOutput("DMfltable"),
          h4("Your input info."),
          verbatimTextOutput("DMfsparameters")
        ),
        tabPanel(
          "ML",
          column(
            4,
            h4("Summary"),
            selectInput(
              "mlframework",
              "Choose a ML framework:",
              choices = c("R Packages", "Tensorflow", "MxNet")
            ),
            selectInput(
              "mlmethod",
              "Choose a ML method:",
              choices = c(
                "Decision Tree",
                "Random Forest",
                "k-NearestNeighbor",
                "Support Vector Machine",
                "Artificial Neural Network"
              )
            ),
            selectInput(
              "mlptype",
              "Select the column name that you want to classify:",
              choices = c("1", "2", "3")
            ),
            HTML(
              '<p>
              <strong><span style="font-size: 14px;"></span></strong>
              </p>
              <p>
              <strong>Info:</strong>
              </p>
              <ul class=" list-paddingleft-2" style="list-style-type: disc;">
              <li>
              <p>
              Tensorflow:TensorFlow makes it easy for beginners and experts to create machine learning models for desktop, mobile, web, and cloud.
              </p>
              <p>
              <span style="color: rgb(255, 0, 0);">web:</span><span style="color: rgb(255, 0, 0); text-decoration: underline;">https://tensorflow.google.cn</span>
              </p>
              </li>
              <li>
              <p>
              MxNet:A flexible and efficient library for deep learning.&nbsp;
              </p>
              <p>
              <span style="color: rgb(255, 0, 0);">web:</span><span style="color: rgb(255, 0, 0); text-decoration: underline;">http://mxnet.incubator.apache.org</span>
              </p>
              </li>
              </ul>
              <p>
              (*Note:<em>If you have a lot of data, the system may be slow, please be patient.</em>)
              </p>
              <p>
              <span style="font-size: 14px;"></span>
              </p>'
            ),
            actionButton("mlsubmit", "Submit", class = "btn-primary")
            ),
          column(4,
                 h3("Result"),
                 verbatimTextOutput("DMmlparameters"))
          ),
        tabPanel("Help",
                 h4("help info."))
          )
      )
      )
      )