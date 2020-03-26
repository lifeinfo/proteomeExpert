navbarPage(
  "ProteomeExpert |",
  theme = "yeti.min.css",
  windowTitle = "ProteomeExpert",
  #useShinyjs(),
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
      "Power Analysis",
      sidebarPanel(
        tags$h4("Power analysis"),
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
            "<p>Power analysis is a statistical device that allows us to determine the sample size required to detect a preset effect under a given test statistics, such as Chi-square test or t-test. In particular, here we need pay attention to the calculated sample size and the realized sample size in an experiment. As observed empirically, when the expression of a protein is not that high, say less than 17 after log2 scale transformation, the required sample size will be compromised due to missing data???the statistical power is compromised too.</p>"
          ),
          h5("Reference:"),
          HTML(
            "<p>Lynch Michael, Walsh Bruce. 1998. Genetics and Analysis of Quantitative Traits.</p>"
          ),
          h5("Results:"),
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
        fileInput(
          "BDfile",
          "Select your file (required):",
          multiple = TRUE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"),
          placeholder = "*.csv or *.TXT required!"
        ),
        radioButtons(
          "BDsep",
          "Separator for your file",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          inline = TRUE,
          selected = "\t"
        ),
        hr(),
        selectInput(
          'BDcol',
          'Select columns for balanced batch design:',
          BDcol_name,
          multiple = T,
          selectize = TRUE
        ),
        selectInput(
          'BDnumeric_headers',
          'Among above selected columns which are numeric:',
          BDcol_name,
          multiple = T,
          selectize = TRUE
        ),
        textInput(
          "BDweight",
          "Weights for columns (using ',' to separate mutilplue columns.)"
        ),
        numericInput("BDsize", "Number of samples in each batch:", 15),
        
        tags$h5("Click to design:"),
        actionButton("BDdo", "Submit", class = "btn-primary")
      ),
      mainPanel(tabsetPanel(
        tabPanel(
          "Result",
          DTOutput("BDresult"),
          downloadButton("downloadBDresult", "Download", class = "btn-primary")
        )
      ))
    )
  ),
  #################################
  # Data console
  #################################
  navbarMenu(
    "Data Console",
    tabPanel(
      "Two files input",
      "",
      h4("Description:"),
      HTML("<p></p>"),
      sidebarPanel(
        tags$h4("Upload Data:"),
        hr(),
        
        fileInput(
          "protein_matrix2",
          "Select your protein file( if file is xlsx format ,protein data sheet must be the first sheet):",
          multiple = F,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            ".xlsx"
          ),
          placeholder = "*.csv/*.TXT or *.xlsx file required!"
        ),
        radioButtons(
          "DCprotmSep",
          "Separator for your file",
          choices = c(
            "Comma(,)" = ",",
            "Semicolon(;)" = ";",
            "Tab(\\t)" = "\t"
          ),
          inline = TRUE,
          selected = "\t"
        ),
        hr(),
        
        fileInput(
          "anno_info",
          "Select your sample annotation file (if file is xlsx format ,sample data sheet must be the first sheet):",
          multiple = F,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            ".xlsx"
          ),
          placeholder = "*.csv/*.TXT or *.xlsx file required!"
        ),
        radioButtons(
          "DCsampleSep2",
          "Separator for your file",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          inline = TRUE,
          selected = ","
        ),
        hr()
        #actionButton("DC_anno", "Submit", class = "btn-primary")
        
        
      ),
      mainPanel(
          h4("Data preview"),
          
          h5("Protien matrix preview"),
          tableOutput("prot_matrix2_preview"),
          hr(),
          h5("Sample annotation display"),
          DT::dataTableOutput("DC_annoShow")
      )),
    ######################################
  tabPanel(
    "Three files input",
    "",
    h4("Description:"),
    HTML("<p></p>"),
    sidebarPanel(
      tags$h4("Upload Data:"),
      hr(),
      
      fileInput(
        "protein_matrix",
        "Select your protein file( if file is xlsx format ,protein data sheet must be the first sheet):",
        multiple = F,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          ".xlsx"
        ),
        placeholder = "*.csv/*.TXT or *.xlsx file required!"
      ),
      radioButtons(
        "DCprotmSep",
        "Separator for your file",
        choices = c(
          "Comma(,)" = ",",
          "Semicolon(;)" = ";",
          "Tab(\\t)" = "\t"
        ),
        inline = TRUE,
        selected = "\t"
      ),
      hr(),
      
      fileInput(
        "sample_info",
        "Select your sample file (if file is xlsx format ,sample data sheet must be the first sheet):",
        multiple = F,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          ".xlsx"
        ),
        placeholder = "*.csv/*.TXT or *.xlsx file required!"
      ),
      radioButtons(
        "DCsampleSep",
        "Separator for your file",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        inline = TRUE,
        selected = ","
      ),
      #actionButton("sampleInfo", "annotation", class = "btn-primary"),
      hr(),
      
      fileInput(
        "individual_info",
        "Select your individual file (if file is xlsx format ,individual data sheet must be the first sheet):",
        multiple = F,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          ".xlsx"
        ),
        placeholder = "*.csv/*.TXT or *.xlsx file required!"
      ),
      radioButtons(
        "DCindividualSep",
        "Separator for your file",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        inline = TRUE,
        selected = ","
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
      fluidRow(column(width = 4, wellPanel(
        # This outputs the dynamic UI component
        DT::dataTableOutput("annoTable")
        
      )))
    )
  )
  ),
  
  #################################
  # data preprocessing
  #################################
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
        multiple = FALSE,
        selectize = TRUE
      ),
      hr(),
      tags$h5("Technical Replicas:"),
      selectInput(
        'DPTR',
        'Select technical replica column name',
        anno_name,
        multiple = FALSE,
        selectize = TRUE
      ),
      radioButtons(
        "DPTechnicalRepMethod",
        "",
        choices = c(
          "None" = "none",
          "Mean" = 'mean',
          "Median" = "median"
        ),
        inline = TRUE,
        selected = NULL
      ),
      hr(),
      tags$h5("Biological Replicas :"),
      selectInput(
        'DPBR',
        'Select biological replica column name',
        anno_name,
        multiple = FALSE,
        selectize = TRUE
      ),
      radioButtons(
        "DPBiologicalRep",
        "",
        choices = c(
          "None" = "none",
          "Mean" = 'mean',
          "Median" = "median"
        ),
        inline = TRUE,
        selected = NULL
      ),
      hr(),
      actionButton("DPDo", "Submit", class = "btn-primary")
    ),
    mainPanel(tabPanel(
      "Methods",
      DTOutput("preprocessedprotM"),
      downloadButton("downloadpreprocessedData", "Download", class = "btn-primary")
    ))
  ),
  
  #################################
  # QC
  #################################
  tabPanel(
    "QC",
    "",
    sidebarPanel(
      tags$h3("Select matrix and label you want to process:"),
      
      selectInput('QCprotM', 'select matrix', protM_name, selectize = FALSE),
      
      selectInput(
        'QCLabel',
        "Select your intresting column name, is't usually tissue/disease type.",
        anno_name,
        multiple = FALSE,
        selectize = TRUE
      ),
      
      hr(),
      tags$h3("Select modules you want to process:"),
      
      hr(),
      checkboxInput("MissingValueExplore_check", "MissingValueExplore", TRUE),
      checkboxInput("reproducibility", "Reproducibility", TRUE),
      
      hr(),
      
      tags$h5("Click to process:"),
      actionButton("QC", "Submit", class = "btn-primary")
      
    ),
    
    mainPanel(tabsetPanel(
      tabPanel(
        "Missing value",
        h5("Description:"),
        HTML(
          "<p>This module design to explore missing data distributions, focusing on numeric missing data.</p>"
        ),
        tags$hr(),
        #h3(textOutput("caption")),
        verbatimTextOutput("QMparameters"),
        conditionalPanel(
          "output.QMparameters == 'Results are showed below:'",
          rHandsontableOutput("QCMissingTable"),
          hr(),
          downloadButton('downloadMissingPlot')
        ),
        
        plotOutput("missingPlot"),
        
        column(4, plotOutput("densityPlot")),
        hr(),
        br()
        
      ),
      tabPanel(
        "Pearson Correlation",
        h5("Description"),
        h5("The correlation between two variables reflects the degree to which the variables are related."),
        #HTML("<p>None</p>"),
        hr(),
        plotOutput("Qpccplot", height = "800px")
        #column(6, plotOutput("Qpccplot")),
        #column(6, plotOutput("Qsmoothplot")),
        #hr(),
        #h4("Data:"),
        #column(12, rHandsontableOutput("Qpcctable")),
        #br(),
        #h4("Note:")
      )
    ))
  ),
  #################################
  #   Statistics
  #################################
  tabPanel(
    "Statistics",
    sidebarPanel(
      tags$h5("Please upload your data files in data console first:"),
      hr(),
      uiOutput("STprot_anno_Ui"),
      #selectInput('statlabel', 'select matrix', protM_name, selectize = FALSE),
      hr()
    ),
    mainPanel(tabsetPanel(
      # t-test
      #################################
      tabPanel(
        "t-test",
        h4("Summary"),
        h5("Description:"),
        HTML(
          "<p>A t-test is any statistical hypothesis test in which the test statistic follows a Student's t-distribution under the null hypothesis.</p>"
        ),
        hr(),
        h5("Set parameters:"),
        #checkboxInput("isLog","Already Log2 transformed",TRUE),
        radioButtons(
          "t_test_alter",
          NULL,
          choices = c(
            two.sided = "two.sided",
            less = "less",
            greater = "greater"
          ),
          inline = TRUE,
          selected = "two.sided"
        ),
        checkboxInput("paried", "Paired samples", FALSE),
        checkboxInput("var.equal", "Equal varience", FALSE),
        numericInput("conf.level", "Confidence level", 0.95,
                     0.01, 1, 0.01),
        radioButtons(
          "adjP",
          "Adjust P value method",
          choices = c(
            none = "none",
            bonferroni = "bonferroni",
            hochberg = "hochberg",
            hommel = "hommel",
            holm = "holm",
            BH = "BH",
            BY = "BY",
            fdr = "fdr"
          ),
          inline = TRUE,
          selected = "none"
        ),
        uiOutput("ttest_groups_ui"),
        uiOutput("ttest_do_ui"),
        rHandsontableOutput("ttest_out"),
        uiOutput("ttest_download_ui")
      ),
      #################################
      # Vocano Plot
      #################################
      tabPanel(
        "VolcanoPlot",
        h4("Summary"),
        h5("Description:"),
        HTML(
          "<p>In statistics, a volcano plot is a type of scatter-plot that is used to quickly identify chans in large datasets composed of replicate data. It plots significance versus fold-change on the y-and-axes, respectively.</p>"
        ),
        hr(),
        h5("Set parameters for volcano plot"),
        checkboxInput("volcano_isLog", "Already Log2 transformed protein matrix", TRUE),
        numericInput(
          "volcano_adjp_threshold",
          "Adjust P value threshold",
          0.05,
          0.01,
          1,
          0.01
        ),
        numericInput("volcano_fc", "Fold change threshold", 2,
                     1, 10, 0.5),
        hr(),
        h5("Set parameters for t test:"),
        radioButtons(
          "volcano_t_test_alter",
          NULL,
          choices = c(
            two.sided = "two.sided",
            less = "less",
            greater = "greater"
          ),
          inline = TRUE,
          selected = "two.sided"
        ),
        checkboxInput("volcano_paried", "Paired samples", FALSE),
        checkboxInput("volcano_var.equal", "Equal varience", FALSE),
        numericInput("volcano_conf.level", "Confidence level", 0.95,
                     0.01, 1, 0.01),
        radioButtons(
          "volcano_adjP",
          "Adjust P value method",
          choices = c(
            none = "none",
            bonferroni = "bonferroni",
            hochberg = "hochberg",
            hommel = "hommel",
            holm = "holm",
            BH = "BH",
            BY = "BY",
            fdr = "fdr"
          ),
          inline = TRUE,
          selected = "none"
        ),
        uiOutput("volcano_ttest_groups_ui"),
        uiOutput("volcano_ttest_do_ui"),
        plotOutput("volcano_plot"),
        #plotlyOutput("volcano_plot"),
        rHandsontableOutput("volcano_ttest_out"),
        uiOutput("volcano_ttest_download_ui")
      ),
      tabPanel(
        "ViolinPlot",
        h4("Summary"),
        h5("Description:"),
        HTML(
          "<p>A violin plot is a method of plotting numeric data. It is a box plot with a rotated kernel densy plot on each side. The violin plot is similar to box plots, except that they also show the probility density of the data at different values (in the simplest case this could be a histogram).</p>"
        ),
        hr(),
        plotlyOutput("DMviolin")
      ),
      tabPanel(
        "RadarMap",
        h4("Summary"),
        h5("Description:"),
        HTML(
          "<p>A radar chart is a graphical method of displaying multivariate data in the form of a two-dimensional chart of three or more quantitative variables represented on axes starting from the same point. The relative position and angle of the axes is typically uninformative.</p>"
        ),
        hr(),
        column(8, canvasXpressOutput("DMradarparameters"))
        #,column(6, rHandsontableOutput("DMradartable"))
      )
    ))
  ),
  #################################
  # data mining
  #################################
  navbarMenu(
    "Data Mining",
    tabPanel(
      "Feature Selection",
      "",
      sidebarPanel(
        h3("Set parameters:"),
        h5(
          "Please note: protein matrix and annotation file shoule be upload in data console first."
        ),
        uiOutput("DMprot_anno_Ui_fs"),
        hr()
        
      ),
      
      mainPanel(tabsetPanel(
        tabPanel(
          "FeatureSelection",
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
            c("nearZeroVar" = "nearZeoVar", "high correlation" = "high_correlation"),
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
          #plotOutput("fs_parameter"),
          #DTOutput("featureSelected"),
          dataTableOutput("featureSelected"),
          downloadButton("downloadfeatureSelData", "Download", class = "btn-primary")
          
        )
      ))
    ),
    #####clustering
    tabPanel(
      "Clustering",
      "",
      sidebarPanel(
        h3("Set parameters:"),
        h5(
          "Please note: protein matrix and annotation file shoule be upload in data console first."
        ),
        uiOutput("DMprot_anno_Ui"),
        hr(),
        tags$h5("Log Transform:"),
        radioButtons(
          "DMclusertingLog",
          "",
          choices = c(
            "None" = "none",
            "Log2" = '2',
            'Log10' = "10"
          ),
          inline = TRUE,
          selected = NULL
        ),
        tags$h5("Module section:"),
        checkboxInput("dmheatmap", "HeatMap", TRUE),
        #checkboxInput("test", "t-test", TRUE),
        #checkboxInput("vocanoPlot", "ViocanoPlot", TRUE),
        #checkboxInput("ViolinPlot", "ViolinPlot", TRUE),
        checkboxInput("qcPca", "PCA", TRUE),
        checkboxInput("qctsne", "t-SNE", TRUE),
        checkboxInput("qcUmap", "UMAP", FALSE),
        
        actionButton("dmClustering", "Submit", class = "btn-primary")
      ),
      
      mainPanel(tabsetPanel(
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
          column(8, plotOutput("DMheatmapparameters"))
          #,column(6, rHandsontableOutput("DMheatmaptable"))
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
          "t-SNE",
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
          "UMAP",
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
        ))
    ),
    #################################
    # Machine Learning
    #################################
    tabPanel(
      "Classification",
      "",
      sidebarPanel(
        h3("Data section:"),
        h5(
          "Please note: protein matrix and annotation file shoule be upload in data console first."
        ),
        uiOutput("DMprot_anno_Ui_class"),
        hr()
      ),
      
      mainPanel(tabsetPanel(tabPanel(
        "ML",
        column(
          4,
          h4("Summary"),
          selectInput(
            "mlmethod",
            "Choose a ML method:",
            choices = c("Decision Tree",
                        "Random Forest",
                        "XGBoost"),
            selected = "Decision Tree"
          ),
          HTML(
            '<p>
            (*Note:<em>If you have a large matrix, the system may be slow, please be patient.</em>)
            </p>
            <p>
            <span style="font-size: 14px;"></span>
            </p>'
          ),
          conditionalPanel(
            condition = "input.mlmethod == \"Decision Tree\"",
            div(
              id = "dtree_Parameters_container",
              numericInput(
                "dt_minsplit",
                label = "minsplit",
                value = 2,
                min = 1
              )
              ,
              shinyBS::bsTooltip(
                id = "dt_minsplit",
                title = "the minimum number of observations that must exist in a node in order for a split to be attempted.",
                placement = "top",
                trigger = "hover"
              )
              ,
              numericInput(
                "dt_minbucket",
                label = "minbucket",
                value = 1,
                min = 1
              )
              ,
              shinyBS::bsTooltip(
                id = "dt_minbucket",
                title = "the minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate.",
                placement = "top",
                trigger = "hover"
              )
              
            )
          ),
          #####################################################
          # Random Forest parameters
          #####################################################
          conditionalPanel(
            condition = "input.mlmethod == \"Random Forest\"",
            div(
              id = "rf_Parameters_container",
              numericInput(
                "rf_ntree",
                label = "ntree",
                value = 500,
                min = 1
              )
              ,
              shinyBS::bsTooltip(
                id = "rf_ntree",
                title = "Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.",
                placement = "top",
                trigger = "hover"
              )
              ,
              numericInput(
                "rf_mtry",
                label = "mtry( value 0 means default)",
                value = 0,
                min = 0
              )
              ,
              shinyBS::bsTooltip(
                id = "rf_mtry",
                title = "Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)",
                placement = "top",
                trigger = "hover"
              )
              
            )
          ),
          ################################################################
          # xgboost parameters
          ################################################################
          conditionalPanel(
            condition = "input.mlmethod == \"XGBoost\"",
            div(
              id = "xgb_Parameters_container",
              selectInput(
                "xgb_xgbooster_type",
                label = "Choose a xgbooster:",
                choices = c("gbtree",
                            "gblinear",
                            "dart"),
                selected = "gbtree"
              )
              ,
              numericInput("xgb_nrounds",
                           label = "The number of rounds for boosting:",
                           value = 5)
              ,
              #################################
              #  parameter for gbtree
              #################################
              
              conditionalPanel(
                condition = "input.xgb_xgbooster_type == \"gbtree\"",
                numericInput(
                  "xgb_gbtree_max_depth",
                  label = "max_depth:",
                  min = 1,
                  value = 6
                )
              ),
              #################################
              #  parameter for gblinear
              #################################
              
              conditionalPanel(
                condition = "input.xgb_xgbooster_type == \"gblinear\"",
                selectInput(
                  "xgb_gblinear_feature_selector",
                  label = "Choose a feature_selector:",
                  choices = c("cyclic",
                              "shuffle",
                              "random",
                              "greedy",
                              "thrifty"),
                  selected = "cyclic"
                )
              )
            )#endof div id="xgb_Parameters_container"
          )
          ,
          fluidRow(
            shinyBS::bsButton("mlsubmitTrain", label = "Training ", style = "primary")
            ,
            shinyBS::bsTooltip(
              id = "mlsubmitTrain",
              title = "click me to trainning",
              placement = "right",
              trigger = "hover"
            )
            
          )
          ,
          #############################################
          # ui for upload test file
          #############################################
          shinyjs::hidden(div(
            id = "mlPredictDiv",
            fileInput(
              "mlTestFile",
              "Select your test data file (required):",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                ".xlsx"
              ),
              placeholder = "*.csv/*.TXT or *.xlsx required!"
            ),
            radioButtons(
              "testDataSep",
              "Separator for your file",
              choices = c(
                "Comma(,)" = ",",
                "Semicolon(;)" = ";",
                "Tab(\\t)" = "\t"
              ),
              inline = TRUE,
              selected = "\t"
            ),
            fluidRow(
              shinyBS::bsButton(
                "mlsubmitPredict",
                label = "Predicting",
                style = "primary",
                disabled = TRUE
              )
              ,
              shinyBS::bsTooltip(
                id = "mlsubmitPredict",
                title = "click me to predict",
                placement = "right",
                trigger = "hover"
              )
              
            )
          ))
          ),
        column(
          8,
          h3("Result"),
          #plotlyOutput(),
          verbatimTextOutput("DMmlText"),
          plotOutput("DMmlPlot"),
          verbatimTextOutput("DMmloutputText"),
          plotOutput("DMmlPlotforest"),
          rHandsontableOutput("DMmltables")
        )
      )))
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
  mainPanel(tabsetPanel(
    tabPanel(
      "Result",
      h4("Summary"),
      HTML("<p><strong>Input:</strong></p>"),
      verbatimTextOutput("anno_parameters1")
      ,
      fluidRow(column(12, "String"),
               hr(),
               column(8,
                      uiOutput("annouistring"))),
      hr(),
      h4('Database'),
      fluidRow(column(
        4,
        HTML(
          '<ul class=" list-paddingleft-2" style="list-style-type: disc;">
          <li><p>Uniport:The mission of UniProt is to provide the scientific community with a comprehensive, high-quality and freely accessible resource of protein sequence and functional information.</p></li>
          <li><p>String-db:Protein-Protein Interaction Networks.</p></li>
          <li><p>KEGG:KEGG is a database resource for understanding high-level functions and utilities of the biological system, such as the cell, the organism and the ecosystem, from molecular-level information, especially large-scale molecular datasets generated by genome sequencing and other high-throughput experimental technologies.</p></li>
          <li><p>Go:The Gene Ontology (GO) knowledgebase is the world largest source of information on the functions of genes.</p></li>
          <li><p>Reactome:Reactome is a free, open-source, curated and peer-reviewed pathway database.</p></li></ul><p>
          <br/></p>'
        )
        ),
        column(8, rHandsontableOutput("anno_table")))
      )
      ))
      ),

################################
### other tools
#################################
navbarMenu(
  "Other Tools",
  tabPanel(
    "Peptide2Protein",
    h5("Description:"),
    HTML(
      "<p>Peptide2Protein provide protein inference function, details are in help page.</p>"
    ),
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
        "Select your technical replicate file (optional):",
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
      checkboxInput("pep2prot_lr", "Using linear regression imputation", TRUE),
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
  
  ########################################################
  ###  PulseDIA preprocess
  ########################################################
  tabPanel(
    "PulseDIA preprocess",
    h5("Description:"),
    HTML("<p>PulseDIA preprocess is ... to be continued.</p>")
    ,
    sidebarPanel(
      fileInput(
        "pulseDiaFile",
        "Select your pulseDIA file :",
        multiple = FALSE,
        accept = c("text/tsv",
                   "text/comma-separated-values,text/plain",
                   ".tsv"),
        placeholder = "*.tsv or *.TXT required!"
      ),
      shinyjs::disabled(
        actionButton("submit_pulseDia_file", label = "Submit", class = "btn-primary")
      )
      
    ),
    mainPanel(uiOutput("ui"))
  )
),
#################################
# help
#################################
tabPanel("Online Help",
         mainPanel(
           tabsetPanel(
             tabPanel(
               "Test Data",
               h4("Test data files used for peptide to protein inference"),
               hr(),
               h5("The test peptide matrix contains 24 DIA runs."),
               downloadButton("downlaod_test_pep", label = "Get", class = "btn-primary"),
               h5("The test technical replicas file"),
               downloadButton(
                 "downlaod_test_techincal",
                 label = "Get",
                 class = "btn-primary"
               ),
               h5("The test batch name file"),
               downloadButton("downlaod_test_batch", label = "Get", class = "btn-primary"),
               hr(),
               h4("Test data files used for data console"),
               hr(),
               h5("The test protein matrix contains 24 DIA runs."),
               downloadButton("downlaod_test_prot", label = "Get", class = "btn-primary"),
               h5("The test sample information file contains 24 DIA samples."),
               downloadButton("downlaod_test_sample", label = "Get", class = "btn-primary"),
               h5("The test individual file contains 21 individuals."),
               downloadButton(
                 "downlaod_test_individual",
                 label = "Get",
                 class = "btn-primary"
               ),
               hr(),
               h4("Test data files used for batch design"),
               hr(),
               downloadButton(
                 "downlaod_test_batchDesign",
                 label = "Get",
                 class = "btn-primary"
               ),
               hr(),
               h4("Test data files used for pulseDIA"),
               hr(),
               downloadButton("downlaod_test_pulseDIA", label = "Get", class = "btn-primary"),
               hr()
             ),
             tabPanel(
               "Docs",
               hr(),
               navlistPanel(
                 tabPanel(
                   "Experimental Design",
                   includeMarkdown("help/ExperimentalDesign.md")
                 ),
                 tabPanel("Data Console",
                          includeMarkdown("help/Dataconsole.md")),
                 tabPanel(
                   "Data Preprocessing",
                   includeMarkdown("help/DataPreprocessing.md")
                 ),
                 tabPanel("QC",
                          includeMarkdown("help/ProteomeExpert-QC.md")),
                 tabPanel(
                   "Statistics",
                   includeMarkdown("help/ProteomeExpert-Statistics.md")
                 ),
                 tabPanel("Data Mining",
                          "Coming Soon"),
                 tabPanel("Other Tools",
                          "Coming Soon"),
                 fluid = TRUE,
                 widths = c(2, 8)
               )
             ),
             tabPanel("Q&A",
                      hr(),
                      h4("Comming")
                      )
           )
           
         ))
      )
