function(input, output,session) {
  observe({
    anno_name<-colnames(getAnnoTable())
    updateSelectInput(session, "DManno2",
                      choices = anno_name,
                      selected = NULL
    )
  })
  #################################
  # batch Design
  #################################
  output$txtout <- renderText({
    paste(input$n, input$m, sep = ", ")
  })
  output$table <- renderTable({
    head(iris, 4)
  })
  #################################
  # Power Analysis
  #################################
  observeEvent(input$powerb, {
    output$powerSize <- renderPrint({
      mean_null = as.numeric(input$Pmu)
      mean_alt = as.numeric(input$Pmu0)
      sd = as.numeric(input$Psd)
      ap = as.numeric(input$Palpha) / as.numeric(input$Pm)
      z1 = qnorm(1 - ap / 2)
      zb = qnorm(1 - as.numeric(input$Pbeta))
      proN = 2 * (sd * (z1 + zb) / (mean_null - mean_alt)) ^ 2
      
      print(paste0("Number of proteins: ", input$Pm))
      print(paste0("Alpha: ", input$Palpha))
      print(paste0("Power: ", 1 - as.numeric(input$Pbeta)))
      print(paste0(
        "Sample size required for cases (controls) is: ",
        ceiling(proN)
      ))
    })
    
    output$powerPlot <- renderPlotly({
      mean_null = as.numeric(input$Pmu)
      mean_alt = as.numeric(input$Pmu0)
      sd = as.numeric(input$Psd)
      ap = as.numeric(input$Palpha) / as.numeric(input$Pm)
      z1 = qnorm(1 - ap / 2)
      zb = qnorm(1 - as.numeric(input$Pbeta))
      proN = 2 * (sd * (z1 + zb) / (mean_null - mean_alt)) ^ 2
      #print(proN)
      nc = c(0.25, 0.5, 0.75, 1, 1.25, 1.5) * proN
      NCP = (mean_null - mean_alt) ^ 2 / sd ^ 2 * (nc / 2)
      #        print(NCP)
      PW = pchisq(qchisq(ap, 1, lower.tail = F),
                  1,
                  ncp = NCP,
                  lower.tail = F)
      PWmat = matrix(PW, 1, length(PW))
      colnames(PWmat) = ceiling(nc)
      PWmat_data = data.frame(Power = as.numeric(t(PWmat)[, 1]), Sample_size = colnames(PWmat))
      p <- ggplot() +
        geom_bar(
          data = PWmat_data,
          aes(x = Sample_size, y = Power),
          stat = "identity",
          fill = "#87CEFA",
          width = 0.6
        ) +
        geom_hline(
          yintercept = 0.5,
          colour = 'red',
          size = 0.1,
          linetype = "dashed"
        ) +
        geom_hline(
          yintercept = 1 - as.numeric(input$Pbeta),
          size = 0.1,
          linetype = "dashed"
        )
      ggplotly(p) %>% config(displaylogo = F)
    })
  })
  #################################
  # data preprocess
  #################################
  # updated when the user clicks the button
  DdatasetInput <- eventReactive(input$process, {
    if (is.null(input$PeptideMatrix))
      "Please upload your files!"
    else
      auto_preprocess(
        isolate(input$PeptideMatrix$datapath),
        isolate(input$TechnicalReplicate$datapath),
        isolate(input$BatchFile$datapath),
        psep = isolate(input$Dpsep),
        pheader = isolate(input$Dpheader),
        tsep = isolate(input$Dtsep),
        theader = isolate(input$Dtheader),
        bsep = isolate(input$Dbsep),
        bheader = isolate(input$Dbheader)
      )
    
  }, ignoreNULL = FALSE)
  
  output$Dtable <- renderTable({
    #peptide<-read.table(input$PeptideMatrix$datapath,header=TRUE)
    #protM<<-head(peptide,5)
    #if (!is.na(DdatasetInput()))
    t <- data.frame(DdatasetInput())
    n = 5
    if (ncol(t) < 5)
      n = ncol(t)
    head(t[, 1:n])
    #else "Please upload your files!"
  })
  output$test <- renderText({
    paste("your input res", f(3), sep = ", ")
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("prot", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(
        DdatasetInput(),
        file,
        row.names = FALSE,
        quote = F,
        sep = "\t"
      )
    }
  )
  #################################
  # QC
  #################################
  
  QCdatasetInput <- eventReactive(input$QC, {
    print("doing")
    if (class(readProteinM()) != "data.frame")
      return(NULL)
    else
      readProteinM()
    
  }, ignoreNULL = FALSE)
  
  #################################
  # missing value explore
  #################################
  
  output$missingPlot <- renderPlot({
    if (input$MissingValueExplore_check) {
      if (class(QCdatasetInput()) == "data.frame") {
        output$QMparameters <- renderText({
          "Results are showed below:"
        })
        #source("missingValueExplore_zts.R", local = T)
        missing_plot(readProteinM())
      }
      else if (is.null(readProteinM()))
      {
        output$QMparameters <-
          renderText({
            "Please upload your protein file in 'data console' tab!"
          })
      }
      else
        output$QMparameters <-
          renderText({
            "Please click the submit button!"
          })
    }
    
    else
      output$QMparameters <-
        renderText({
          "Please select MissingValueExplore checkbox and sumbit!"
        })
    
  }, height = 800, units = "px")
  
  output$downloadMissingPlot <- downloadHandler(
    filename = function() {
      paste("missingValueExplore", Sys.time(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      missing_plot(readProteinM())
      dev.off()
    }
  )
  output$densityPlot <- renderPlot({
    data <- iris[, 1]
    #drawdensity(as.data.frame(data))
  })
  
  #################################
  # Pearson Correlation
  #################################
  output$Qpcctable <- renderRHandsontable({
    rhandsontable(head(iris, n = 20L))
  })
  output$Qpccplot <- renderPlot({
    data <- t(iris[1:10, 1:4])
    drawcorrplot(data)
  })
  output$Qsmoothplot <- renderPlot({
    data1 <- iris[, 1]
    data2 <- iris[, 1]
    drawsmooth(data1, data2)
  })
  
  #################################
  # PCA
  #################################
  output$Qpcatable <- renderRHandsontable({
    rhandsontable(head(iris, n = 20L))
  })
  output$Qpcaplot <- renderPlotly({
    data <- t(iris[, 1:4])
    label <- iris[, 5]
    p <- drawPCA(data, label)
    ggplotly(p) %>% config(displaylogo = F)
  })
  #################################
  # T-sne
  #################################
  output$Qtsnetable <- renderRHandsontable({
    rhandsontable(head(iris, n = 20L))
  })
  output$Qtsneplot <- renderPlotly({
    data <- t(iris[, 1:4])
    label <- iris[, 5]
    p <- drawTSNE(data, label)
    ggplotly(p) %>% config(displaylogo = F)
  })
  
  #################################
  # umap
  #################################
  output$Qumaptable <- renderRHandsontable({
    rhandsontable(head(iris, n = 20L))
  })
  output$Qumapplot <- renderPlotly({
    data <- t(iris[, 1:4])
    label <- iris[, 5]
    p <- drawUMAP(data, label)
    ggplotly(p) %>% config(displaylogo = F)
  })
  
  
  #################################
  # data console
  #################################
  
  #for read protein matrix
  readProteinM <- reactive({
    if (!is.null(input$protein_matrix))
      prot <-
        read.table(
          input$protein_matrix$datapath,
          header = T,
          sep = "\t",
          check.names = F,
          encoding = "UTF-8",
          stringsAsFactors = F
        )
  })
  ### for column annotation
  output$sampleUi <- renderUI({
    if (is.null(input$sample_info))
      "Please upload your files!"
    else{
      #print(input$sample_info)
      sample_info <-
        read.csv(
          input$sample_info$datapath,
          header = T,
          sep = ",",
          nrow = 1,
          check.names = F,
          encoding = "UTF-8"
        )
      sample_header <<- colnames(sample_info)
      tagList(
        selectInput(
          "sample_info_id",
          "select sample id",
          choices = sample_header,
          selected = sample_header[1]
        ),
        selectInput(
          "sample_info_type",
          "select sample type",
          choices = sample_header,
          selected = sample_header[2]
        ),
        selectInput(
          "sample_info_batch_id",
          "select batch id",
          choices = c(sample_header, "select..."),
          selected = "select..."
        ),
        selectInput(
          "sample_info_technicalRep_id",
          "select technical replica id",
          choices = c(sample_header, "select..."),
          selected = "select..."
        ),
        selectInput(
          "sample_info_individual_id",
          "select individual id",
          choices = c(sample_header, "select..."),
          selected = "select..."
        ),
        actionButton("sample_info_annotation", "Submit", class = "btn-primary")
      )
    }
  })
  
  
  output$individualUi <- renderUI({
    if (is.null(input$individual_info))
      "Please upload your files!"
    else{
      #print(input$sample_info)
      individual_info <-
        read.csv(
          input$individual_info$datapath,
          header = T,
          sep = ",",
          nrow = 1,
          check.names = F,
          encoding = "UTF-8"
        )
      individual_header <- colnames(individual_info)
      tagList(
        selectInput(
          "individual_info_id",
          "select individual id/name",
          choices = individual_header,
          selected = individual_header[1]
        ),
        selectInput(
          "individual_info_type",
          "select individual type",
          choices = individual_header,
          selected = individual_header[2]
        ),
        
        actionButton("individual_info_annotation", "Submit", class = "btn-primary")
      )
    }
  })
  sampleInfoInput <- eventReactive(input$sample_info_annotation, {
    sampleInfo <-
      read.csv(input$sample_info$datapath,
               header = T,
               sep = ",")
    sample_header <- colnames(sampleInfo)
    colnames(sampleInfo)[which(sample_header == input$sample_info_id)] <-
      "sampleId"
    colnames(sampleInfo)[which(sample_header == input$sample_info_type)] <-
      "sampleType"
    
    if (input$sample_info_batch_id != 'select...')
      colnames(sampleInfo)[which(sample_header == input$sample_info_batch_id)] <-
      "batchId"
    if (input$sample_info_technicalRep_id != 'select...')
      colnames(sampleInfo)[which(sample_header == input$sample_info_technicalRep_id)] <-
      "technicalId"
    if (input$sample_info_individual_id != 'select...')
      colnames(sampleInfo)[which(sample_header == input$sample_info_individual_id)] <-
      "individualId"
    #print(head(sampleInfoInput()))
    sampleInfo
  }, ignoreNULL = T)
  
  sampleResInput <- eventReactive(input$sample_info_annotation, {
    sampleInfoInput()
  })
  output$sampleRes <- renderText({
    if (class(sampleResInput()) == "data.frame")
      paste("Successfully annotated the sample column!", Sys.time())
    else
      paste("Unsuccessfully annotated sample column", Sys.time())
  })
  individualInfoInput <-
    eventReactive(input$individual_info_annotation, {
      individualInfo <-
        read.csv(input$individual_info$datapath,
                 header = T,
                 sep = ",")
      individual_header <- colnames(individualInfo)
      colnames(individualInfo)[which(individual_header == input$individual_info_id)] <-
        "individualId"
      colnames(individualInfo)[which(individual_header == input$individual_info_type)] <-
        "individualType"
      
      individualInfo
    }, ignoreNULL = T)
  
  individualResInput <-
    eventReactive(input$individual_info_annotation, {
      individualInfoInput()
    })
  output$individualRes <- renderText({
    if (class(individualResInput()) == "data.frame")
      paste("Successfully annotated the individual column!", Sys.time())
    else
      paste("Unsuccessfully annotated individual column", Sys.time())
  })
  getAnnoTable <- eventReactive(input$DoAnnoTable, {
    anno <-
      merge(individualInfoInput(), sampleInfoInput(), by = 'individualId')
    rownames(anno) <- anno[, "sampleId"]
    anno
  })
  output$annoTable <- DT::renderDataTable(DT::datatable({
    #anno_name<<-colnames(getAnnoTable())
    getAnnoTable()
  }))
  
  output$DMprot_anno_Ui <- renderUI({
    anno_name <<- colnames(getAnnoTable())
    tagList(
      selectInput('DMprotM', 'select matrix', protM_name, selectize = FALSE),
      selectInput(
        'DManno',
        'select types',
        anno_name,
        multiple = TRUE,
        selectize = TRUE
      )
    )
  })
  #################################
  # Heatmap
  #################################
  output$DMheatmaptable <- renderRHandsontable({
    rhandsontable(head(iris, n = 20L))
  })
  output$DMheatmapparameters <- renderPlotly({
    data <- iris[, 1:4]
    label <- iris[, 5]
    drawheatmap(data, label)
  })
  
  #################################
  # VocanoPlot
  #################################
  output$DMvocanotable <- renderRHandsontable({
    rhandsontable(head(iris, n = 20L))
  })
  output$DMvocanoparameters <- renderPlot({
    drawVolcano((iris[, 1:4]), c("a", "a", "b", "b"), "a", "b")
  })
  
  #################################
  # ViolinPlot
  #################################
  output$DMviolintable <- renderRHandsontable({
    rhandsontable(head(iris[, c(1, 5)], n = 20L))
  })
  output$DMviolinparameters <- renderPlot({
    data <- iris[, 4]
    sample <- iris[, 5]
    drawviolin(data, sample)
  })
  
  #################################
  # Radar
  #################################
  output$DMradartable <- renderRHandsontable({
    rhandsontable(head(iris[, c(1, 2)], n = 20L))
  })
  output$DMradarparameters <- renderCanvasXpress({
    data <- t(iris[, 3:4])
    drawradar(data)
  })
  
  #################################
  # feature selection
  #################################
  feature_sel_prot <- eventReactive(input$feature_do,
                                    {
                                      if (!is.null(isolate(input$DMprotM))) {
                                        if (isolate(input$DMprotM) == "original") {
                                          protM <- isolate(readProteinM())
                                        }
                                        #if(length(isolate(input$DManno))==1){
                                        label = isolate(input$DManno)
                                        #}
                                        rownames(protM) <-
                                          protM[, 1]
                                        protM <- protM[,-1]
                                        protM <- t(protM)
                                        sample_names <-
                                          rownames(protM)
                                        label_temp <-
                                          as.vector(getAnnoTable()[sample_names, label])
                                        
                                        labeled_protM <-
                                          cbind(label = label_temp, protM, stringsAsFactors = FALSE)
                                        fs_features <-
                                          colnames(protM)
                                        if (!is.null(input$featureSel_filter))
                                          fs_features <-
                                          featureFilter(labeled_protM,!is.na(match(
                                            c("nearZeoVar", "high_correlation"),
                                            input$featureSel_filter
                                          )), input$fs_missing_ratio)
                                        # if('random_forest' %in% input$featureSel_algorithm)
                                        #   use_rf=TRUE
                                        # if('lasso' %in% input$featureSel_algorithm)
                                        #   use_lasso = TRUE
                                        #nfeatures<-input$feature_num
                                        print(input$featureSel_algorithm)
                                        if (!is.null(input$featureSel_algorithm))
                                          fs_features <-
                                          featureSel(labeled_protM[, c("label", fs_features)], input$featureSel_algorithm)
                                        
                                        return(fs_features)
                                      }
                                    },
                                    ignoreNULL = T,
                                    ignoreInit = T)
  
  ####feature selection
  
  ####feature selection
  output$fs_summary <- renderText({
    paste(
      "After feature selection your matrix contains",
      
      length(feature_sel_prot()$features),
      "features:",
      paste(feature_sel_prot()$features, collapse = ",")
    )
  })
  output$fs_parameter <- renderPlot({
    plot(feature_sel_prot()$mod)
  })
  output$featureSelected <- DT::renderDataTable(DT::datatable({
    data.frame(feature_sel_prot()$features)
  }))
  
  
  #################################
  # ANNO
  #################################
  observeEvent(input$proteinlist, {
    output$anno_parameters1 <- renderPrint({
      print(paste0("Protein list: ", input$proteinlist))
      print(paste0(
        "Database: ",
        "Uniport, ",
        "StringDB, ",
        "KEGG, ",
        "GO, ",
        "Reactome, "
      ))
    })
  })
  output$anno_parameters2 <- renderText({
    
  })
  
  output$anno_table <- renderRHandsontable({
    DF = data.frame(
      Name = c("Uniport",
               "StringDB",
               "KEGG",
               "GO",
               "Reactome"),
      Link = c(
        "<a href='https://www.uniprot.org'>www.uniprot.org</a>",
        "<a href='https://string-db.org/'>string-db.org</a>",
        "<a href='https://www.kegg.jp'>www.kegg.jp</a>",
        "<a href='http://geneontology.org'>geneontology.org</a>",
        "<a href='https://reactome.org'>reactome.org</a>"
      ),
      desc = c(
        "Comprehensive protein sequence and functional information, including supporting data.",
        "Protein-Protein Interaction Networks.",
        "Kyoto Encyclopedia of Genes and Genomes; diagrams of signaling pathways.",
        "An ontology is a formal representation of a body of knowledge within a given domain. ",
        "Protein-specific information in the context of relevant cellular pathways."
      ),
      stringsAsFactors = FALSE
    )
    
    rhandsontable(
      DF,
      allowedTags = "<em><b><strong><a><big>",
      height = 500,
      rowHeaders = FALSE,
      readOnly = TRUE
    ) %>%
      hot_cols(colWidths = c(80, 200, 200)) %>%
      hot_col(1:2, renderer = "html") %>%
      hot_col(1:3, renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_cols(fixedColumnsLeft = 1, columnSorting = TRUE)
  })
  #################################
  # ML
  #################################
  observeEvent(input$mlsubmit,
               {
                 output$DMmlText <- renderPrint({
                   print(input$mlframework)
                   print(input$mlmethod)
                   print(input$mlptype)
                 })
                 #################################
                 # R Packages
                 #################################
                 if (input$mlframework == "R Packages")
                 {
                   #################################
                   # Decision Tree
                   #################################
                   if (input$mlmethod == "Decision Tree") {
                     dtree <- rpart(Species ~ ., data = iris, method = "class")
                     tree <-
                       prune(dtree, cp = dtree$cptable[which.min(dtree$cptable[, "xerror"]), "CP"])
                     output$DMmlPlot <- renderPlot({
                       rpart.plot(
                         tree,
                         branch = 0,
                         type = 0,
                         fallen.leaves = T,
                         cex = 1,
                         sub = "Demo"
                       )
                     })
                     output$DMmloutputText <- renderPrint({
                       printcp(tree)
                     })
                     output$DMmltables <- renderRHandsontable({
                       rhandsontable(head(iris[1:10,], n = 20L))
                     })
                   } else if (input$mlmethod == "Random Forest")
                   {
                     #################################
                     # Random Forest
                     #################################
                     model.forest <-
                       randomForest(Species ~ ., data = iris)
                     pre.forest <- predict(model.forest, iris)
                     
                     #obs_p_ran = data.frame(prob=pre.forest, obs=iris$Species)
                     ran_roc <-
                       roc(iris$Species, as.numeric(pre.forest))
                     
                     output$DMmlPlot <- renderPlot({
                       layout(matrix(c(1,2,3,0),2,2),widths = c(1,1),heights = c(1,1), F)
                       varImpPlot(model.forest, main = "variable importance")
                       plot(model.forest)
                       plot(
                         ran_roc,
                         print.auc = TRUE,
                         auc.polygon = TRUE,
                         grid = c(0.1, 0.2),
                         grid.col = c("green", "red"),
                         max.auc.polygon = TRUE,
                         auc.polygon.col = "skyblue",
                         print.thres = TRUE,
                         main = 'RF ROC,mtry=3,ntree=500'
                       )
                     })
                     
                     output$DMmloutputText <- renderPrint({
                       model.forest$importance
                       #
                       table(iris$Species, pre.forest, dnn = c("Obs", "Pre"))
                       
                     })
                     
                     output$DMmltables <- renderRHandsontable({
                       rhandsontable(head(iris[1:10,], n = 20L))
                     })
                     
                   } else if (input$mlmethod == "k-NearestNeighbor")
                   {
                     cat("k-NearestNeighbor comming soon!")
                   } else if (input$mlmethod == "Support Vector Machine")
                   {
                     cat("Support Vector Machine comming soon!")
                   } else if (input$mlmethod == "Artificial Neural Network")
                   {
                     cat("Artificial Neural Network comming soon!")
                   }
                   
                 } else if (input$mlframework == "Tensorflow")
                 {
                   #################################
                   # Tensorflow
                   #################################
                   cat("Tensorflow comming soon!")
                 } else if (input$mlframework == "MxNet")
                 {
                   #################################
                   # MxNet
                   #################################
                   cat("MxNet comming soon!")
                 }
                 
               })
}