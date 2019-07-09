#shinythemes::themeSelector(),
navbarPage(
  theme = shinytheme("cerulean"),
  windowTitle = "ProteomeExpert | A user friendly Web for proteome analysis.",
  "ProteomeExpert",
  #################################
  # Home
  #################################
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
  #################################
  # Experimental Design
  #################################
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
          h5("Description:"),
          HTML(
            "<p>Power analysis is a statistical device that allows us to determine the sample size required to detect a preset effect under a given test statistics, such as Chi-square test or t-test. In particular, here we need pay attention to the calculated sample size and the realized sample size in an experiment. As observed empirically, when the expression of a protein is not that high, say less than 17 after log2 scale transformation, the required sample size will be compromised due to missing data—the statistical power is compromised too.</p>"
          ),
          h5("Reference:"),
          HTML(
            "<p>Lynch Michael, Walsh Bruce. 1998. Genetics and Analysis ofQuantitative Traits. Sunderland, Mass.: Sinauer Assoc</p>"
          ),
          h4("Sample size"),
          column(8, verbatimTextOutput("powerSize")),
          column(8, plotlyOutput("powerPlot"))
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
  #################################
  # Data console
  #################################
  tabPanel(
    "Data Console",
    "",
    h4("Description:"),
    HTML("<p></p>"),
    sidebarPanel(
      tags$h4("Upload Data:"),
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

  #################################
  # data preprocessing
  #################################
  navbarMenu(
    "Data Processing",
    tabPanel(
      "Data Preprocessing",
      h5("Description:"),
      HTML(
        "<p>Data Preprocessing is used to transform data in accordance with modeling experiment conditions configured in the project.</p>"
      ),
      sidebarPanel(
        selectInput('DMprotM', 'select matrix', protM_name, selectize = FALSE),
        tags$h5("Log Transform:"),
        radioButtons(
          "DPLog",
          "",
          choices = c(
            "None" = "none",
            "Log2" = '2',
            'Log10' = "10"
          ),
          inline = TRUE,
          selected = NULL
        ),
        
        hr(),
        tags$h5("Missing Value Substitution:"),
        radioButtons(
          "DPmissingV",
          "",
          choices = c(
            "None" = "none",
            "1" = '1',
            '0' = "0",
            "10% of minimum" = '0.1',
            "minimum" = "minimum"
          ),
          inline = TRUE,
          selected = NULL
        ),
        hr(),
        tags$h5("Normaliztion:"),
        radioButtons(
          "DPnormaliztion",
          "",
          choices = c(
            "None" = "none",
            "Quantile" = 'quantile',
            "Z-score" = "zscore",
            "Max-Min" = "maxmin"
          ),
          inline = TRUE,
          selected = NULL
        ),
        hr(),
        tags$h5("Remove Batch Effect :"),
        selectInput(
          'DManno2',
          'Select batch effect name',
          anno_name,
          multiple = TRUE,
          selectize = TRUE
        ),
        hr(),
        tags$h5("Technial Replicas:"),
        hr(),
        tags$h5("Biological Replicas :"),
        hr(),
        actionButton("DPDo", "Submit", class = "btn-primary")
      ),
      mainPanel(
        tabPanel(
          "Methods",
          DTOutput("preprocessedprotM")
          

        )
      )
    ),

    tabPanel(
      "Peptide2Protein",
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
    )

  ),

  #################################
  # QC
  #################################
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

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Missing value",
          h5("Description:"),
          HTML(
            "<p>This module will explore missing data in Stata, focusing on numeric missing data.</p>"
          ),
          tags$hr(),
          #h3(textOutput("caption")),
          verbatimTextOutput("QMparameters"),
          conditionalPanel(
            "output.QMparameters == 'Results are showed below:'",
            downloadButton('downloadMissingPlot')
          ),

          plotOutput("missingPlot"),

          column(4, plotOutput("densityPlot")),
          hr(),
          br()

        ),
        tabPanel(
          "Pearson Correlation",
          h4("Summary"),
          h5("Description:"),
          HTML(
            "<p>None</p>"
          ),
          hr(),
          column(6, plotOutput("Qpccplot")),
          column(6, plotOutput("Qsmoothplot")),
          hr(),
          h4("Data:"),
          column(12, rHandsontableOutput("Qpcctable")),
          br(),
          h4("Note:")
        ),
        tabPanel(
          "PCA",
          h4("Summary"),
          h5("Description:"),
          HTML(
            "<p>Principal component analysis (PCA) is an exploratory analysis tool that emphasizes variation and visualizes possible patterns underlying a dataset. It uses an orthogonal transformation to convert a set of observations of possibly correlated variables into a set of values of linearly uncorrelated variables called principal components. Upon on the context, PCA is also called eigenvalue decomposition, and eigenvalues (vector) and eigenvectors (matrix) are often used to represent the data.</p>
            <p>Mark 1: In proteomic data matrix, missing data (often more missing values for control samples) plays a role in determining the outcome of PCA.</p>
            <p>Mark 2: If blank controls (AQUA) are available in the experiment, the coordinates of blank controls can tell the quality of the data.</p>"
          ),
          hr(),
          column(6, plotlyOutput("Qpcaplot")),
          column(6, rHandsontableOutput("Qpcatable"))
        ),
        tabPanel(
          "T-SNE",
          h4("Summary"),
          h5("Description:"),
          HTML(
            "<p>T-distributed Stochastic Neighbor Embedding (t-SNE) is a nonlinear dimensionality reductiontechnique well-suited for embedding high-dimensional data for visualization in a low-dimensional space of two or three dimensions.</p>"
          ),
          h5("Reference:"),
          HTML(
            "<p>L. van der Maaten, H. Geoffrey, Visualizing Data using t-SNE. Journal of Machine Learning Research.</p>"
          ),
          hr(),
          column(6, plotlyOutput("Qtsneplot")),
          column(6, rHandsontableOutput("Qtsnetable"))
        ),
        tabPanel(
          "Umap",
          h4("Summary"),
          h5("Description:"),
          HTML(
            "<p>UMAP (Uniform Manifold Approximation and Projection) is a novel manifold learning technique for dimension reduction. The UMAP algorithm is competitive with t-SNE for visualization quality, and arguably preserves more of the global structure with superior run time performance.</p>"
          ),
          h5("Reference:"),
          HTML(
            "<p>McInnes, L, Healy, J, UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction, ArXiv e-prints 1802.03426,2018</p>"
          ),
          hr(),
          column(6, plotlyOutput("Qumapplot")),
          column(6, rHandsontableOutput("Qumaptable"))
        )
      )
    )
  ),

  #################################
  # data mining
  #################################
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
        #################################
        # HeatMap
        #################################
        tabPanel(
          "HeatMap",
          h4("Summary"),
          h5("Description:"),
          HTML(
            "<p>A heat map (or heatmap) is a graphical representation of data where the individual values contained in a matrix are represented as colors.</p>"
          ),
          hr(),
          column(6, plotlyOutput("DMheatmapparameters")),
          column(6, rHandsontableOutput("DMheatmaptable"))
        ),
        #################################
        # Vocano Plot
        #################################
        tabPanel(
          "VocanoPlot",
          h4("Summary"),
          h5("Description:"),
          HTML(
            "<p>In statistics, a volcano plot is a type of scatter-plot that is used to quickly identify changes in large datasets composed of replicate data. It plots significance versus fold-change on the y-and x-axes, respectively.</p>"
          ),
          hr(),
          column(6, plotOutput("DMvocanoparameters")),
          column(6, rHandsontableOutput("DMvocanotable"))
        ),
        tabPanel(
          "ViolinPlot",
          h4("Summary"),
          h5("Description:"),
          HTML(
            "<p>A violin plot is a method of plotting numeric data. It is a box plot with a rotated kernel density plot on each side. The violin plot is similar to box plots, except that they also show the probability density of the data at different values (in the simplest case this could be a histogram).</p>"
          ),
          hr(),
          column(6, plotOutput("DMviolinparameters")),
          column(6, rHandsontableOutput("DMviolintable"))
        ),
        tabPanel(
          "RadarMap",
          h4("Summary"),
          h5("Description:"),
          HTML(
            "<p>A radar chart is a graphical method of displaying multivariate data in the form of a two-dimensional chart of three or more quantitative variables represented on axes starting from the same point. The relative position and angle of the axes is typically uninformative.</p>"
          ),
          hr(),
          column(6, canvasXpressOutput("DMradarparameters")),
          column(6, rHandsontableOutput("DMradartable"))
        ),
        tabPanel(
          "FeatureSel",
          h5(
            "Please note that feature selection including two parts: filter rules and feature selection algorithm"
          ),
          textInput(
            inputId = "fs_missing_ratio",
            label = "Allowable missing ratio",
            value = 0.8
          ),
          checkboxGroupInput(
            "featureSel_filter",
            "Please select filter rules" ,
            c("nearZeoVar" = "nearZeoVar", "high correlation" = "high_correlation"),
            selected = c("nearZeoVar", "high_correlation"),
            inline = T,
            width = NULL
          ),
          radioButtons(
            "featureSel_algorithm",
            "Please select feature selection algorithm",
            choices = c(
              "Random Forest" = 'random_forest',
              'LASSO' = "lasso",
              'Genetic Algorithm' = "GA"
            ),
            inline = TRUE,
            selected = NULL
          ),
          hr(),
          actionButton("feature_do", "Submit", class = "btn-primary"),
          h5("Summary:"),
          verbatimTextOutput("fs_summary"),
          plotOutput("fs_parameter"),
          DTOutput("featureSelected")
        ),
        #################################
        # Machine Learning
        #################################
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
              choices = c("Species", "PType", "Age", "Loc")
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
          column(
            6,
            h3("Result"),
            #plotlyOutput(),
            verbatimTextOutput("DMmlText"),
            plotOutput("DMmlPlot"),
            verbatimTextOutput("DMmloutputText"),
            rHandsontableOutput("DMmltables")
          )
          )
          )
      )
    ),
  #################################
  # Annotation
  #################################
  tabPanel(
    "Annotations",
    "",
    sidebarPanel(
      textAreaInput(
        "proteinlist",
        "Protein list:",
        height = 150,
        placeholder = "Example:Q9Y6B6,P35659,O43759,A0A0B4J1V6,E9PAV3"
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
        fluidRow(column(12, "Uniport"),
                 hr(),
                 column(8,
                        uiOutput("annouiuniport")
                        )),
        fluidRow(column(12, "String"),
                 hr(),
                 column(
                   8,
                   uiOutput("annouistring")
                 )),
        fluidRow(column(12, "KEGG"),
                 hr(),
                 column(8, "")),
        fluidRow(column(12, "GO"),
                 hr()),
        fluidRow(column(12, "Reactome"),
                 hr(),
                 column(8,
                        HTML("<iframe src=\"https://reactome.org/PathwayBrowser/\" width=\"1200\" height=\"700\"></iframe>")
                        )
                 ),
        h4('Database'),
        fluidRow(column(
          4,
          HTML(
            '<ul class=" list-paddingleft-2" style="list-style-type: disc;">
            <li><p>Uniport:The mission of UniProt is to provide the scientific community with a comprehensive, high-quality and freely accessible resource of protein sequence and functional information.</p></li>
            <li><p>String-db:Protein-Protein Interaction Networks.</p></li>
            <li><p>KEGG:KEGG is a database resource for understanding high-level functions and utilities of the biological system, such as the cell, the organism and the ecosystem, from molecular-level information, especially large-scale molecular datasets generated by genome sequencing and other high-throughput experimental technologies.</p></li>
            <li><p>Go:The Gene Ontology (GO) knowledgebase is the world’s largest source of information on the functions of genes.</p></li>
            <li><p>Reactome:Reactome is a free, open-source, curated and peer-reviewed pathway database.</p></li></ul><p>
            <br/></p>'
          )
          ),
          column(8, rHandsontableOutput("anno_table")))
        )
        )
        ),
  #################################
  # Resources
  #################################
  navbarMenu(
    "Resources",
    tabPanel("Tutorials"),
    "----",
    tabPanel("Docs"),
    tabPanel("Q&A"),
    tabPanel("GitHub")
  ),
  #################################
  # footer
  #################################
  footer = "",
  div(
    br(),
    hr(),
    includeCSS("www/css/footer.css"),
    includeHTML("www/footer.html")
  )
        )
