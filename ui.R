#shinythemes::themeSelector(),
navbarPage(
  theme = shinytheme("sandstone"),
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
  tabPanel("Study Design", "",
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
                 
                 #----------------------------test----------------------------------------
                 HTML(
                   '<label for="tt">input (test)</label>',
                   '<textarea id="tt" class="form-control" style="resize:none"></textarea>'
                 ),
                 HTML(
                   '<label for="clx">pick (test)</label>',
                   '<input id="clx" type="color" class="form-control" value="#FF0000">',
                   '<input id="cl" type="text" class="form-control" value="#FF0000" style="display:none">',
                   '<script>',
                   '$(function(){$("#clx").change(function(){$("#cl").val($(this).val()).trigger("change");});})',
                   '</script>',
                   '</br>'
                 ),
                 #------------------------------------------------------------------------
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
        
      )))
    )
  ),
  
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
      #tabPanel("", "This panel is intentionally left blank"),
      #tabPanel("Tab 3", "This panel is intentionally left blank")
    ))
  ),
  
  ######################################            QC             ##################################################################################
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
  ##############################################################################################################
  #######################data mining
  tabPanel(
    "Data Mining",
    "",
    sidebarPanel(
      # Input: Checkbox if file has header ----
      checkboxInput("DMheader", "Header", TRUE),
      
      
      # Input: Select separator ----
      radioButtons(
        "DMsep",
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
        "DMquote",
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
        "DMfile",
        "Choose protein/peptide CSV/TXT File",
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      # Horizontal line ----
      tags$hr(),
      # Input: Select a file ----
      fileInput(
        "anno",
        "Choose annotation File",
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      tags$hr(style = "height:3px;border:none;border-top:3px ridge green;"),
      
      tags$h5("Click to process:"),
      checkboxInput("dmheatmap", "HeatMap", TRUE),
      checkboxInput("ttest", "t-test", TRUE),
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
        tabPanel("HelpMe",
                 h4("help info."))
      )
    )
  )
)