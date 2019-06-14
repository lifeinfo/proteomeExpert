#shinythemes::themeSelector(),
navbarPage(
  theme = shinytheme("cerulean"),
  "ProteomeExpert |",
  ####################################################Home
  tabPanel(
    "Home",
    sidebarPanel(
      tags$h5("Welcome to ProteomeExpert, it makes your research easy!"),
      HTML("<p>A user friendly Web for protein analysis.</p>")
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
  ################################ Experimental Design
  navbarMenu(
    "Experimental Design",
    tabPanel(
      "Study Design",
      sidebarPanel(
        tags$h4("Power analysis by direct set parameters:"),
        hr(),
        textInput("Pm", "Number of Proteins (estimated):", 5000, width = "60%"),
        textInput("Pmu", "Mean abundance:", 13, width = "60%"),
        textInput("Pmu0", "Mean abundance 0:", 13.5, width = "60%"),
        textInput("Psd", "Standard deviation:", 0.75, width = "60%"),
        textInput("Palpha", "Alpha:", 0.05, width = "60%"),
        textInput("Pbeta", "Beta (Power=1-beta):", 0.2, width = "60%"),
        hr(),
        actionButton("powerb", "Submit", class = "btn-primary")
      ),
      
      mainPanel(tabsetPanel(
        tabPanel(
          "Summary",
          tags$h4("Result:"),
          hr(),
          HTML("<p>Lynch Michael, Walsh Bruce. 1998. Genetics and Analysis ofQuantitative Traits. Sunderland, Mass.: Sinauer Assoc</p>"),
          h4("Sample size"),
          column(8,verbatimTextOutput("powerSize")),
          column(8,plotlyOutput("powerPlot"))
        )
      ))
    ),
    tabPanel(
      "Batch Design",
      sidebarPanel(
        tags$h4("Batch Design"),
        hr(),
        fileInput("file", "File input:"),
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
      ))
    )
  ),
  
  ###################################################data preprocessing
  tabPanel(
    "Data Preprocessing",
    "",
    sidebarPanel(
      fileInput(
        "PeptideMatrix",
        "Select your peptide matrix (required):",
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"),
        placeholder = "*.csv or *.TXT required!"
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
      hr(),
      
      fileInput(
        "TechnicalReplicate",
        "Select your technical replicate file (required):",
        placeholder = "*.csv or *.TXT required!"
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
      
      hr(),
      
      fileInput(
        "BatchFile",
        "Select your batch effect file (optional):",
        placeholder = "*.csv or *.TXT required!"
      ),
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
      
      hr(),
      
      tags$h5("Click to process:"),
      actionButton("process", "Submit", class = "btn-primary")
      
    ),
    mainPanel(tabsetPanel(
      tabPanel(
        "Result",
        h4("Summary"),
        tableOutput("Dtable"),
        h4("Download all protein matrix"),
        downloadButton("downloadData", "Download", class = "btn-primary")
        
      )
      
    ))
  ),
  
  ####################################################Data console
  tabPanel(
    "Data Console",
    "",
    sidebarPanel(
      tags$h3("Upload Data:"),
      hr(),
      fileInput(
        "peptide_matrix",
        "Select your peptide file (optional):",
        multiple = F,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"),
        placeholder = "*.csv or *.TXT required!"
      ),
      hr(),
      
      fileInput(
        "protein_matrix",
        "Select your protein file (optional):",
        multiple = F,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"),
        placeholder = "*.csv or *.TXT required!"
      ),
      hr(),
      
      fileInput(
        "sample_info",
        "Select your sample file (optional):",
        multiple = F,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"),
        placeholder = "*.csv or *.TXT required!"
      ),
      #actionButton("sampleInfo", "annotation", class = "btn-primary"),
      hr(),
      
      fileInput(
        "individual_info",
        "Select your individual file (optional):",
        multiple = F,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"),
        placeholder = "*.csv or *.TXT required!"
      )
      #actionButton("individualInfo", "annotation", class = "btn-primary")
      
    ),
    mainPanel(
      fluidRow(column(3, wellPanel(
        tags$h4("Annotate sample columns:"),
        hr(),
        
        # This outputs the dynamic UI component
        uiOutput("sampleUi")
      )),
      
      column(
        3, offset = 0.5, wellPanel(
          tags$h4("Annotate individual columns:"),
          hr(),
          
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
      hr(),
      actionButton("DoAnnoTable", "Merge", class = "btn-primary"),
      hr(),
      h4("Result"),
      fluidRow(column(width = 8, wellPanel(
        # This outputs the dynamic UI component
        DT::dataTableOutput("annoTable")
        
      )))
    )
  ),
  
  ######################################QC
  tabPanel(
    "QC",
    "",
    sidebarPanel(
      tags$h3("Select modules you want to process:"),
      hr(),
      checkboxInput("MissingValueExplore_check", "MissingValueExplore", TRUE),
      checkboxInput("reproducibility", "Reproducibility", TRUE),
      checkboxInput("qcPca", "PCA", TRUE),
      checkboxInput("qcUmap", "UMAP", FALSE),
      
      hr(),
      
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
  ######################################################################Annotation
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
      actionButton("annosubmit", "Search", class = "btn-primary")
      #verbatimTextOutput("Annoparameters")
      ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Result",
          h4("Summary"),
          HTML("<p><strong>Input:</strong></p>"),
          verbatimTextOutput("anno_parameters1")
        ),
        fluidRow(
          column(12, "Uniport"),
          hr(),
          column(8, "")
        ),
        fluidRow(
          column(12, "String-db"),
          hr(),
          column(8, img(src="https://string-db.org/api/image/network?identifiers=Q9Y6B6&add_white_nodes=10&network_flavor=actions"))
        ),
        fluidRow(
          column(12, "KEGG"),
          hr(),
          column(8, "")
        ),
        fluidRow(
          column(12, "GO"),
          hr()
        ),
        fluidRow(
          column(12, "Reactome"),
          hr()
        ),
        h4('Database'),
        fluidRow(
          column(4, HTML(
            '<ul class=" list-paddingleft-2" style="list-style-type: disc;">
        <li><p>Uniport:The mission of UniProt is to provide the scientific community with a comprehensive, high-quality and freely accessible resource of protein sequence and functional information.</p></li>
        <li><p>String-db:Protein-Protein Interaction Networks.</p></li>
        <li><p>KEGG:KEGG is a database resource for understanding high-level functions and utilities of the biological system, such as the cell, the organism and the ecosystem, from molecular-level information, especially large-scale molecular datasets generated by genome sequencing and other high-throughput experimental technologies.</p></li>
        <li><p>Go:The Gene Ontology (GO) knowledgebase is the world’s largest source of information on the functions of genes.</p></li>
        <li><p>Reactome:Reactome is a free, open-source, curated and peer-reviewed pathway database.</p></li></ul><p>
        <br/></p>'
          )),
          column(8, rHandsontableOutput("anno_table"))
        )
      )
    )
    ),
  
  ##################################################data mining
  tabPanel(
    "Data Mining",
    "",
    sidebarPanel(
      h3("Advanced data analysis."),
      uiOutput("DMprot_anno_Ui"),
      hr(),
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
          h5(
            "Please note that feature selection including two parts: filter rules and feature selection algorithm"
          ),
          checkboxGroupInput(
            "featureSel_filter",
            "Please select filter rules" ,
            c("nearZeoVar" = "nearZeoVar", "high correlation" = "high_correlation"),
            selected = NULL,
            inline = T,
            width = NULL
          ),
          checkboxGroupInput(
            "featureSel_algorithm",
            "Please select feature selection algorithm" ,
            c("random forest" = "random_forest", "lasso" = "lasso"),
            selected = NULL,
            inline = T,
            width = NULL
          ),
          sliderInput(
            "feature_num",
            "Set maximum features to keep:",
            min = 0,
            max = 500,
            value = 20,
            width = 800
          ),
          hr(),
          actionButton("feature_do", "Submit", class = "btn-primary"),
          #h5("Results:"),
          DTOutput("featureSelected")
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
          )
          )
      )
    ),
  ##################################################More
  navbarMenu(
    "More",
    tabPanel("Help"),
    "----",
    tabPanel("Docs"),
    tabPanel("Tutorials"),
    tabPanel("Resources"),
    tabPanel("GitHub")
  ),
  ################################################footer
  div(
    br(),
    hr(),
    includeCSS("www/css/footer.css"),
    includeHTML("www/footer.html")
  )
    )
