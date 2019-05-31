function(input, output) {
  output$txtout <- renderText({
    paste(input$n, input$m, sep = ", ")
  })
  output$table <- renderTable({
    head(iris, 4)
  })
  ###############################Power Analysis
  output$Pparameters <- renderText({
    paste(input$Pm, input$Pmu, input$Palpha, sep = ", ")
  })
  
  output$Ptable <- renderTable({
    head(iris, 4)
  })
  
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
    
    output$powerPlot <- renderPlot({
      mean_null = as.numeric(input$Pmu)
      mean_alt = as.numeric(input$Pmu0)
      sd = as.numeric(input$Psd)
      ap = as.numeric(input$Palpha) / as.numeric(input$Pm)
      z1 = qnorm(1 - ap / 2)
      zb = qnorm(1 - as.numeric(input$Pbeta))
      proN = 2 * (sd * (z1 + zb) / (mean_null - mean_alt)) ^ 2
      
      nc = c(0.25, 0.5, 0.75, 1, 1.25, 1.5) * proN
      NCP = (mean_null - mean_alt) ^ 2 / sd ^ 2 * (nc / 2)
      #        print(NCP)
      PW = pchisq(qchisq(ap, 1, lower.tail = F),
                  1,
                  ncp = NCP,
                  lower.tail = F)
      PWmat = matrix(PW, 1, length(PW))
      colnames(PWmat) = ceiling(nc)
      barplot(
        PWmat,
        beside = T,
        col = "grey",
        ylim = c(0, 1),
        border = F,
        ylab = "Statistical power",
        xlab = "Sample size"
      )
      abline(
        h = c(0.5, 1 - as.numeric(input$Pbeta)),
        col = c("blue", "red"),
        lty = 2
      )
    })
  })
  ###############################     data preprocess        ##############################
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
  ############################              QC             ##############################################################
  
  QCdatasetInput <- eventReactive(input$QC, {
    if (class(readProteinM()) != "data.frame")
      return(NULL)
    else
      readProteinM()
    
  }, ignoreNULL = FALSE)
  
  # output$QMparameters <- renderText({
  #   QCdatasetInput()
  # })
  ##############missing value explore
  
  output$missingPlot <- renderPlot({
    if (input$MissingValueExplore_check) {
      if (class(QCdatasetInput()) == "data.frame") {
        output$QMparameters <- renderText({
          "Results are showed below:"
        })
        source("missingValueExplore_zts.R", local = T)
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
      source("missingValueExplore_zts.R", local = T)
      missing_plot(readProteinM())
      dev.off()
    }
  )
  ############################              data console             ##############################################################
  
  ### for read protein matrix
  readProteinM <- reactive({
    if (!is.null(input$protein_matrix))
      prot <-
        read.table(
          input$protein_matrix$datapath,
          header = T,
          sep = "\t",
          check.names = F,
          encoding = "UTF-8"
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
}