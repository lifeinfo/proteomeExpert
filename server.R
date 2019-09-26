###label update
function(input, output, session) {
  observe({
    anno_name <- colnames(getAnnoTable())
    updateSelectInput(session,
                      "DManno2",
                      choices = c("None", anno_name),
                      selected = NULL)
    updateSelectInput(session,
                      "DPTR",
                      choices = c("None", anno_name),
                      selected = NULL)
    updateSelectInput(session,
                      "DPBR",
                      choices = c("None", anno_name),
                      selected = NULL)
    updateSelectInput(session,
                      "QCLabel",
                      choices = c("None", anno_name),
                      selected = NULL)
    
  })
  #################################
  # batch Design
  #################################
  observe({
    input$BDfile
    input$BDsep
    if (!is.null(input$BDfile)) {
      batchf <-
        read.table(
          input$BDfile$datapath,
          stringsAsFactors = F,
          sep = input$BDsep,
          header = T,
          encoding = "UTF-8",
          check.names = F,
          nrow = 1
        )
      BDcol_name <- colnames(batchf)
    }
    updateSelectInput(session,
                      "BDcol",
                      choices = c("None", BDcol_name),
                      selected = NULL)
    updateSelectInput(
      session,
      "BDnumeric_headers",
      choices = c("None", BDcol_name),
      selected = NULL
    )
  })
  batch_design_result <- eventReactive(input$BDdo, {
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',
                 value = 0,
                 {
                   incProgress(1 / 15)
                   col_weights <- strsplit(input$BDweight, ",")
                   col_weights <- as.numeric(unlist(col_weights))
                   result <-
                     batchGenerator(
                       input$BDfile$datapath,
                       input$BDcol,
                       input$BDnumeric_headers,
                       col_weights,
                       input$BDsize,
                       input$BDsep
                     )
                   incProgress(14 / 15, message = "Ready to finish!")
                 })
    return(result)
  },
  ignoreNULL = T,
  ignoreInit = T)
  
  ####show and download data
  output$BDresult <- DT::renderDataTable(DT::datatable({
    data.frame(batch_design_result())
  }))
  output$downloadBDresult <- downloadHandler(
    filename = function() {
      paste("BatchDesignResult", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(
        batch_design_result(),
        file,
        row.names = T,
        quote = F,
        na = "",
        sep = "\t"
      )
    }
  )
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
  DPdataprecessInput <- eventReactive(input$DPDo, {
    batch_factor <- input$DManno2
    if (!is.null(batch_factor) & batch_factor != "None") {
      sample_names <- colnames(readProteinM())[-1]
      batch_factor <-
        as.vector(getAnnoTable()[sample_names, batch_factor])
    }
    else
      batch_factor = NULL
    technical_col <- input$DPTR
    if (!is.null(technical_col) & technical_col != "None") {
      sample_names <- colnames(readProteinM())[-1]
      technical_col <-
        as.vector(getAnnoTable()[sample_names, technical_col])
    }
    else
      technical_col = NULL
    biological_col <- input$DPBR
    if (!is.null(biological_col) & biological_col != "None") {
      sample_names <- colnames(readProteinM())[-1]
      biological_col <-
        as.vector(getAnnoTable()[sample_names, biological_col])
    }
    else
      biological_col = NULL
    
    dataPreprocess(
      readProteinM(),
      input$DPmissingV,
      input$DPLog,
      input$DPnormaliztion,
      batch_factor,
      technical_col,
      input$DPTechnicalRepMethod,
      biological_col,
      input$DPBiologicalRep
    )
  })
  ####download data
  output$preprocessedprotM <- DT::renderDataTable(DT::datatable({
    myhead(data.frame(DPdataprecessInput()))
  }))
  output$downloadpreprocessedData <- downloadHandler(
    filename = function() {
      paste("PreProcessed", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      DPM <- data.frame(DPdataprecessInput())
      protein <- rownames(DPM)
      DPM <- cbind(protein, DPM)
      write.table(
        DPM,
        file,
        row.names = F,
        quote = F,
        na = "",
        sep = "\t"
      )
    }
  )
  ###pep2prot
  DdatasetInput <- eventReactive(input$process, {
    if (is.null(input$PeptideMatrix))
      "Please upload your files!"
    else{
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...',
                   value = 0,
                   {
                     incProgress(1 / 10)
                     prot_matrix <- auto_preprocess(
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
                     incProgress(9 / 10, message = "Ready to finish!")
                   })
      prot_matrix
    }
    
    
  }, ignoreNULL = FALSE)
  
  output$Dtable <- renderTable({
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
        na = "",
        sep = "\t"
      )
    }
  )
  #################################
  # pulseDIA preprocess
  #################################
  #
  #puslseDIAStatus
  #  initStatus 
  #  selected  user has select a DIAData file  to upload, but not sumbited.
  #  submited  DIAData file is submited.
  #  calculating  server is running the combining function
  #  cal_finished combining function is finished.
  pulseDIAStatus <- reactiveValues(status = "initStatus")
  observeEvent(input$pulseDiaFile, {
    if( !is.null(input$pulseDiaFile)){
      pulseDIAStatus$status = "selected"
      shinyjs::enable("submit_pulseDia_file")
    }
  })
  #output_file_name <- ""
  observeEvent(input$submit_pulseDia_file, {
    #pulseDIAStatus$status = "submited"
    
    input_name <-input$pulseDiaFile$datapath
    #output_file_name <<- tempfile("pulseDIACombined", fileext = ".txt")
    print(input_name)
    #print(output_file_name)
    shinyjs::disable("submit_pulseDia_file")
    
  #   #source(file.path("server", "pulse_dia_combine.R"),  local = TRUE)$value
  #   # sourced in global.R
    pulseDIAStatus$status = "calculating"
    result <<- pulseDIACombine(input_name)
    pulseDIAStatus$status = "cal_finished"
    #print(result)
  #   
  #   shinyjs::enable("downloadPulseDIAResult")
  #   output$tabPulseDIAcombined <- renderTable(
  #     
  #   )
    ##to do
    ##
    
  })
  output$ui <- renderUI({
    switch (pulseDIAStatus$status,
      "initStatus" = tags$h4("Please select a DIA file to upload"),
      "selected"   = tags$h4("Please click Submit button, then waiting "),
      "submited"   = tags$h4("calculate in progress ... Please waiting"),
      "calculating"   = tags$h4("calculate in progeress ... Please waiting"),
      "cal_finished"   = tags$div(
        tags$h4("calculate finished! Please download the result file"),
        downloadButton("downloadPulseDIAResult", label = "Download", class = "btn-primary")
      ),
      "nextTry" = tags$div(
        tags$h4("download finished! ,Please select a DIA file to upload"),
        shinyjs::disabled(
          downloadButton("downloadPulseDIAResult", label = "Download", class = "btn-primary")
        )
      ),
    )
  })
  output$downloadPulseDIAResult <- downloadHandler(
    filename = "result.txt",
    
    content <- function(file) {
      
      #input_name <-input$pulseDiaFile$datapath
      #print("abcceddd")
      #print(input_name)
      # result <- pulseDIACombine(input_name)
      write.table(result,file,sep="\t",col.names = T,row.names = F,quote = F)
      #file.copy(output_file_name, file)
      pulseDIAStatus$status = "nextTry"
      
    }
    #contentType = "text/tvs"
  )
  
  #################################
  # QC
  #################################
  
  QCdatasetInput <- eventReactive(input$QC, {
    print("missing value explore")
    if (class(readProteinM()) != "data.frame")
      return(NULL)
    else
      readProteinM()
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$QC, {
    qc_label <- input$QCLabel
    
    data <- readProteinM()
    col_name <- colnames(data)
    row_name <- as.matrix(data[, 1])
    row_name <- as.vector(row_name[, 1])
    data <- data.matrix(data)
    data <- data[,-1]
    colnames(data) <- col_name[2:length(col_name)]
    row.names(data) <- row_name
    
    data[is.na(data)] <- 0
    
    
    if (!is.null(qc_label) & qc_label != "None") {
      sample_names <- colnames(readProteinM())[-1]
      qc_label <- as.vector(getAnnoTable()[sample_names, qc_label])
    }
    #print(qc_label)
    
    #################################
    # Pearson Correlation
    #################################
    
    
    output$Qpcctable <- renderRHandsontable({
      if (input$reproducibility) {
        if (!is.null(data))
        {
          rhandsontable(head(data, n = 20L))
        }
      }
      
    })
    output$Qpccplot <- renderPlot({
      if (input$reproducibility) {
        if (!is.null(data))
        {
          drawcorrplot(data)
        }
      }
      
    })
    
    #################################
    # PCA
    #################################
    output$Qpcatable <- renderRHandsontable({
      if (input$qcPca) {
        if (!is.null(data))
        {
          rhandsontable(head(data, n = 20L))
        }
      }
    })
    output$Qpcaplot <- renderPlotly({
      if (input$qcPca) {
        if (!is.null(data))
        {
          p <- drawPCA(data, qc_label)
          ggplotly(p) %>% config(displaylogo = F)
        }
      }
    })
    #################################
    # T-sne
    #################################
    output$Qtsnetable <- renderRHandsontable({
      if (input$qctsne) {
        if (!is.null(data))
        {
          rhandsontable(head(data, n = 20L))
        }
      }
    })
    output$Qtsneplot <- renderPlotly({
      if (input$qctsne) {
        if (!is.null(data))
        {
          p <- drawTSNE(data, qc_label)
          ggplotly(p) %>% config(displaylogo = F)
        }
      }
      
    })
    
    #################################
    # umap
    #################################
    output$Qumaptable <- renderRHandsontable({
      if (input$qcUmap) {
        if (!is.null(data))
        {
          rhandsontable(head(data, n = 20L))
        }
      }
    })
    output$Qumapplot <- renderPlotly({
      if (input$qcUmap) {
        if (!is.null(data))
        {
          p <- drawUMAP(data, qc_label)
          ggplotly(p) %>% config(displaylogo = F)
        }
      }
      
    })
  })
  
  #################################
  # missing value explore
  #################################
  
  output$missingPlot <- renderPlot({
    if (input$MissingValueExplore_check & input$QC) {
      if (class(QCdatasetInput()) == "data.frame") {
        output$QMparameters <- renderText({
          "Results are showed below:"
        })
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
          sep = input$DCprotmSep,
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
          sep = input$DCsampleSep,
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
          "sample_info_individual_id",
          "select individual id/name",
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
          sep = input$DCindividualSep,
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
        
        actionButton("individual_info_annotation", "Submit", class = "btn-primary")
      )
    }
  })
  sampleInfoInput <- eventReactive(input$sample_info_annotation, {
    sampleInfo <-
      read.csv(
        input$sample_info$datapath,
        header = T,
        sep = input$DCsampleSep,
        check.names = F,
        encoding = "UTF-8"
      )
    sample_header <- colnames(sampleInfo)
    colnames(sampleInfo)[which(sample_header == input$sample_info_id)] <-
      "sampleId"
    if (input$sample_info_individual_id != 'select...')
      colnames(sampleInfo)[which(sample_header == input$sample_info_individual_id)] <-
      "individualId"
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
        read.csv(
          input$individual_info$datapath,
          header = T,
          sep = input$DCindividualSep,
          check.names = F,
          encoding = "UTF-8"
        )
      individual_header <- colnames(individualInfo)
      colnames(individualInfo)[which(individual_header == input$individual_info_id)] <-
        "individualId"
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
  output$DMprot_anno_Ui_fs <- renderUI({
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
  output$DMprot_anno_Ui_class <- renderUI({
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
  output$STprot_anno_Ui <- renderUI({
    anno_name <<- colnames(getAnnoTable())
    tagList(
      selectInput(
        'STanno',
        'Select Groups (Compare the two groups)',
        anno_name,
        multiple = F,
        selectize = TRUE
      )
    )
  })
  
  ####################################################
  #data mining
  ####################################################

  observeEvent(input$dmClustering, {
    qc_label1 <- input$DManno
    if (!is.null(qc_label1) & qc_label1 != "None") {
      sample_names <- colnames(readProteinM())[-1]
      qc_label <- as.vector(getAnnoTable()[sample_names, qc_label1])
    } else{
      return()
    }
    
    data <- readProteinM()
    col_name <- colnames(data)
    row_name <- as.matrix(data[, 1])
    row_name <- as.vector(row_name[, 1])
    data <- data.matrix(data)
    print(dim(data))
    trainData <- data[,-1]
    colnames(trainData) <- col_name[2:length(col_name)]
    row.names(trainData) <- row_name
    
    trainData[is.na(trainData)] <- 0

    #################################
    # Heatmap
    #################################
    # output$DMheatmaptable <- renderRHandsontable({
    #   if (input$dmheatmap) {
    #     if (!is.null(readProteinM()))
    #     {
    #       rhandsontable(head(trainData, n = 20L))
    #     }
    #   }
    # })
    output$DMheatmapparameters <- renderPlot({
      if (input$dmheatmap) {
        if (!is.null(readProteinM()))
        {
          drawheatmap(trainData, qc_label)
        }
      }
      
    })
    #################################
    # Radar
    #################################
    output$DMradartable <- renderRHandsontable({
      if (input$radarmap) {
        if (!is.null(readProteinM()))
        {
          rhandsontable(head(trainData, n = 20L))
        }
      }
    })
    output$DMradarparameters <- renderCanvasXpress({
      if (input$radarmap) {
        if (!is.null(readProteinM()))        {
          drawradar(trainData)
        }
      }
    })
  })

    #################################
    # ML
    #################################
  xgboost_classfier <- NULL
  observeEvent(input$mlsubmitTrain, {
    trainData <- NULL
    qc_label <- NULL
    qc_label1 <- input$DManno
    if (!is.null(qc_label1) & qc_label1 != "None") {
      sample_names <- colnames(readProteinM())[-1]
      print("qc_label")
      print(getAnnoTable())
      qc_label <- as.vector(getAnnoTable()[sample_names, qc_label1])
    } else{
      return()
    }
    
    data <- readProteinM()
    col_name <- colnames(data)
    row_name <- as.matrix(data[, 1])
    row_name <- as.vector(row_name[, 1])
    data <- data.matrix(data)
    trainData <<- data[,-1]
    colnames(trainData) <- col_name[2:length(col_name)]
    row.names(trainData) <- row_name
    
    trainData[is.na(trainData)] <- 0
    
    output$DMmlText <- renderPrint({
      #print(input$mlframework)
      print(input$mlmethod)
      print(unique(qc_label, fromLast = FALSE))
    })
    if (is.null(input$DManno))
      {
        return()
      }
      data <- as.data.frame(t(trainData))
      colnames(data)<-readProteinM()[,1]
      print("ML")
      print(qc_label)
      data$Label <- qc_label
      #print(head(data))
      #################################
      # Decision Tree
      #################################
      
      if (input$mlmethod == "Decision Tree") {
        print("Decision tree starting")
        if (!is.null(readProteinM()))
        {
          dtree <- rpart(Label ~ ., data = data, method = "class")
          #tree <- prune(dtree, cp = dtree$cptable[which.min(dtree$cptable[, "xerror"]), "CP"])
          tree <- dtree
          output$DMmlPlot <- renderImage({
            outfile <- tempfile(fileext='.png')
            png(outfile, width=400, height=400)
            rpart.plot(
              tree,
              box.palette = "auto",
              branch = 0,
              type = 0,
              fallen.leaves = T,
              cex = 1,
              sub = "Decision Tree"
            )
            dev.off()
            list(src = outfile,
                 alt = "Decision Tree Plot")
          }, deleteFile = TRUE)
          output$DMmloutputText <- renderPrint({
            printcp(tree)
          })
          output$DMmltables <- renderRHandsontable({
            rhandsontable(head(data, n = 20L))
          })
        }
      } else if (input$mlmethod == "Random Forest")
      {
        #################################
        # Random Forest
        #################################
        if (!is.null(readProteinM()))
        {
          #print(dim(data))
          #print(class(data))
          #print(head(data))
          data$Label <- as.factor(data$Label)
          model.forest <-
            randomForest(Label ~ ., data = data)
          pre.forest <- predict(model.forest, data)
          
          if (pre.forest)
          {
            ran_roc <-
              roc(data$Label, as.numeric(pre.forest))
          }
          
          
          output$DMmlPlot <- renderPlot({
            layout(matrix(c(1, 2, 3, 0), 2, 2),
                   widths = c(1, 1),
                   heights = c(1, 1),
                   F)
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
              main = 'RF ROC'
            )
          })
          
          output$DMmloutputText <- renderPrint({
            model.forest$importance
            #
            table(data$Label, pre.forest, dnn = c("Obs", "Pre"))
            
          })
          
          # output$DMmltables <- renderRHandsontable({
          #   rhandsontable(head(data, n = 20L))
          # })
        }
      } 
      else if (input$mlmethod == "XGBoost")
      {
        print("xgboost starting")
        if(is.null(input$protein_matrix)){
          #hint to upload train data
          output$DMmlText <- renderPrint({
            print("Please upload your training data from data console")
          })
          return()
        }
        #xgboost parameters
        #https://xgboost.readthedocs.io/en/latest/parameter.html
        params <- list(
          booster = input$xgb_xgbooster_type,
          objective='binary:logistic'
        )
        if(params$booster == "gbtree"){
          params$max_depth = as.integer(input$xgb_gbtree_max_depth)
          params$eta = 0.3
        }else if(params$booster == "gblinear"){
          params$feature_selector = input$xgb_gblinear_feature_selector
        }else if(params$booster == "dart"){
          #add 
        }
        buffer <<- vector('character')
        con    <<- textConnection('buffer', 'wr', local = TRUE)
        sink(con)
        
        xgboost_classfier <<- xgboost_classfier_training(data[,which(colnames(data)!="Label")],data$Label, parameters = params
                                                         , numRounds = as.integer(input$xgb_nrounds))
        sink()
        close(con)
 
        output$DMmlPlot <- renderPlot({
          importance_matrix <- xgb.importance(model = xgboost_classfier)
          imp_num<-nrow(importance_matrix)
          if(imp_num>50)
            imp_num<-50
          xgb.plot.importance(importance_matrix[1:imp_num,])
        })
        output$DMmloutputText <- renderPrint({
          print("train error")
          print(buffer)
        })
        
        # output$DMmltables <- renderRHandsontable({
        #   rhandsontable(data)
        # })
        
      }

    shinyjs::show(id = "mlPredictDiv")
  })
  
  observeEvent(input$mlTestFile, {
    
    shinyBS::updateButton(session, inputId = "mlsubmitPredict", label = "Predicting", style = "primary",  disabled = FALSE)
  })
  observeEvent(input$mlsubmitPredict, {
    result <- xgboost_classfier_predict(xgboost_classfier, input$mlTestFile$datapath)
    
    output$DMmloutputText <- renderPrint({
      print(result)
    })
    
  })
  #################################
  # feature selection
  #################################
  feature_sel_prot <- eventReactive(input$feature_do,
                                    {
                                      if (!is.null(isolate(input$DMprotM))) {
                                        if (isolate(input$DMprotM) == "uploadedProtMatrix") {
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
  #####download
  output$downloadfeatureSelData <- downloadHandler(
    filename = function() {
      paste("featureSelected", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      protM <- readProteinM()
      rownames(protM) <- as.vector(unlist(protM[1]))
      protM <- protM[feature_sel_prot()$features,]
      write.table(
        protM,
        file,
        row.names = F,
        quote = F,
        na = "",
        sep = "\t"
      )
    }
  )
  
  #################################
  # ANNO
  #################################
  observeEvent(input$annosubmit, {
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
    
    output$annouistring <- renderUI({
      if (is.null(input$proteinlist))
      {
        return()
      } else{
        if (length(input$proteinlist) < 1)
          return()
        img(src = paste0(
          "https://string-db.org/api/image/network?identifiers=",
          gsub(",", "%0d", input$proteinlist)
        ))
      }
    })
  })
  
  
  output$annouiuniport <- renderUI({
    if (is.null(input$proteinlist))
      return()
  })
  
  
  
  output$anno_table <- renderRHandsontable({
    DF = data.frame(
      Name = c("Uniport",
               "String",
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
  
}
