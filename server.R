####global variable/functions
###label update
function(input, output, session) {
  observe({
      print('observe update')
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
  observe({
    print('observe update2')
    anno_name <- colnames(getAnnoTable2())
      getAnnoTable<-getAnnoTable2
      readProteinM<-readProteinM2

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
  
  setDataAnno<-function(readProt3,readProt2,readAnno3,readAnno2){
    if (class(readProt3) != "data.frame"){
      data0 <- readProt2
      anno0<-readAnno2
    }
    else{
      data0 <-readProt3
      anno0<-readAnno3
    }
    return(list(protM=data0,anno=anno0))
  }
  
  dataAnno <- reactive({
    setDataAnno(readProteinM(),
                           readProteinM2(),
                           getAnnoTable(),
                           getAnnoTable2())
  })
  ####end global
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
                   errors<-batch_design_check(input$BDfile$datapath,input$BDsep,T,input$BDcol)
                   if(!is.null(errors)){
                     showModal(modalDialog(
                       title = "Important message",errors))
                     stop()
                   }
                   col_weights <- strsplit(input$BDweight, ",")
                   col_weights <- as.numeric(unlist(col_weights))
                   result <-
                     batchGenerator(
                       input$BDfile$datapath,
                       input$BDcol,
                       input$BDnumeric_headers,
                       col_weights,
                       input$BDsize,
                       input$BDsep,
                       input$BD_tech_rep
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
        row.names = F,
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
    output$powerSize <- renderText({
      mean_null = as.numeric(input$Pmu)
      mean_alt = as.numeric(input$Pmu0)
      sd = as.numeric(input$Psd)
      ap = as.numeric(input$Palpha) / as.numeric(input$Pm)
      z1 = qnorm(1 - ap / 2)
      zb = qnorm(1 - as.numeric(input$Pbeta))
      proN = 2 * (sd * (z1 + zb) / (mean_null - mean_alt)) ^ 2
      
      pa.res<-paste0("Number of proteins: ", input$Pm,"\n","Alpha: ", input$Palpha,"\n","Power: ",
                     1 - as.numeric(input$Pbeta),"\n","Sample size required for cases (controls) is: ",
                     ceiling(proN))
      
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
      PWmat_data$Sample_size=as.numeric(PWmat_data$Sample_size)
      PWmat_data = PWmat_data[order(PWmat_data$Sample_size),]
      #print(PWmat_data$Sample_size)
      p <- ggplot() +
        geom_bar(
          data = PWmat_data,
          aes(x = Sample_size, y = Power),
          stat = "identity",
          fill = "#87CEFA"
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
        )+
        scale_x_continuous(breaks = PWmat_data$Sample_size)
      ggplotly(p) %>% config(displaylogo = F)
    })
  })
  ###with pilot experiment
  output$PowerD_anno_Ui <- renderUI({
    
    anno_name <- colnames(dataAnno()$anno)
    tagList(
      selectInput(
        'PowerDanno',
        'Select case/control column',
        anno_name,
        multiple = F,
        selectize = TRUE
      ),
      tags$div(title="Alpha is the significance level of the test, here the type I error rate.",
               textInput("Palpha2", "Alpha:", 0.05, width = "60%")),
      tags$div(title="Beta is the probability of accepting the null hypothesis even though the null hypothesis is false",
               textInput("Pbeta2", "Beta (Power=1-beta):", 0.2, width = "60%")),
      hr(),
      tags$h5("Click to process:"),
      actionButton("powerD_do", "Submit", class = "btn-primary")
    )
  })
  powerD2<- eventReactive(input$powerD_do, {
    data<-dataAnno()$protM
    anno<-dataAnno()$anno
    col_name <- colnames(data)
    row_name <- as.matrix(data[, 1])
    row_name <- as.vector(row_name[, 1])
    data <- data.matrix(data)
    data <- data[, -1]
    colnames(data) <- col_name[2:length(col_name)]
    row.names(data) <-row_name
    data<-log2(data)
    label2<-as.vector(unlist(anno[colnames(data),input$PowerDanno]))
    index.null<-label2==(unique(label2)[1])
    index.alt<-label2==(unique(label2)[2])
    mean_null = mean(data[,index.null],na.rm = T)
    mean_alt = mean(data[,index.alt],na.rm = T)
    sd<-sd(data,na.rm = T)
    nprots<-length(unique(rownames(data)))
    #
    ap = as.numeric(input$Palpha2) / as.numeric(nprots)
    z1 = qnorm(1 - ap / 2)
    zb = qnorm(1 - as.numeric(input$Pbeta2))
    proN = 2 * (sd * (z1 + zb) / (mean_null - mean_alt)) ^ 2
    print(nprots)
    cat(mean_null)
    print(mean_alt)
    print(sd)
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
    PWmat_data$Sample_size=as.numeric(PWmat_data$Sample_size)
    PWmat_data = PWmat_data[order(PWmat_data$Sample_size),]
    
      output$powerSize <- renderText({
        pa.res<-paste0("Number of proteins: ", nprots,"\n","Alpha: ", input$Palpha2,"\n","Power: ",
                       1 - as.numeric(input$Pbeta2),"\n","Sample size required for cases (controls) is: ",
                       ceiling(proN))
      })
      
    return(PWmat_data)
  })
  output$powerPlot <- renderPlotly({
    PWmat_data<-powerD2()
    p <- ggplot() +
      geom_bar(
        data = PWmat_data,
        aes(x = Sample_size, y = Power),
        stat = "identity",
        fill = "#87CEFA"
      ) +
      geom_hline(
        yintercept = 0.5,
        colour = 'red',
        size = 0.1,
        linetype = "dashed"
      ) +
      geom_hline(
        yintercept = 1 - as.numeric(input$Pbeta2),
        size = 0.1,
        linetype = "dashed"
      )+
      scale_x_continuous(breaks = PWmat_data$Sample_size)
    ggplotly(p) %>% config(displaylogo = F)
  })
  #################################
  # data preprocess
  #################################
  DPdataprecessInput <- eventReactive(input$DPDo, {
    withProgress(message = 'Checking in progress',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/10)
          data0 <- dataAnno()$protM
          anno0 <- dataAnno()$anno
    batch_factor <- input$DManno2
    if (!is.null(batch_factor) & batch_factor != "None") {
      sample_names <- colnames(data0)[-1]
      batch_factor <-
        as.vector(anno0[sample_names, batch_factor])
    }
    else
      batch_factor = NULL
    technical_col <- input$DPTR
    if (!is.null(technical_col) & technical_col != "None") {
      sample_names <- colnames(data0)[-1]
      technical_col <-
        as.vector(anno0[sample_names, technical_col])
    }
    else
      technical_col = NULL
    biological_col <- input$DPBR
    if (!is.null(biological_col) & biological_col != "None") {
      sample_names <- colnames(data0)[-1]
      biological_col <-
        as.vector(anno0[sample_names, biological_col])
    }
    else
      biological_col = NULL
    incProgress(2/10,"Preprocessing now")
    preProcessedData<-dataPreprocess(
      data0,
      input$DPmissingV,
      input$DPLog,
      input$DPnormaliztion,
      batch_factor,
      technical_col,
      input$DPTechnicalRepMethod,
      biological_col,
      input$DPBiologicalRep
    )
    incProgress(9/10,"Prepare to return")
                 })
    preProcessedData
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
  ##########################other tools
  ###pep2prot
  DdatasetInput <- eventReactive(input$process, {
    
    if (is.null(input$PeptideMatrix))
      "Please upload your files"
    else{
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...',
                   value = 0,
                   {
      errors<-protein_file_check(input$PeptideMatrix$datapath,input$Dpsep,input$Dpheader)
      if(!is.null(errors)){
        showModal(modalDialog(
          title = "Important message",errors))
        stop()
      }

                     incProgress(1 / 10)
                     prot_matrix <- try(auto_preprocess(
                       isolate(input$PeptideMatrix$datapath),
                       isolate(input$TechnicalReplicate$datapath),
                       isolate(input$BatchFile$datapath),
                       psep = isolate(input$Dpsep),
                       pheader = isolate(input$Dpheader),
                       tsep = isolate(input$Dtsep),
                       theader = isolate(input$Dtheader),
                       bsep = isolate(input$Dbsep),
                       bheader = isolate(input$Dbheader),
                       lr_top3 = isolate(input$lr_top3)
                     ),silent = T)
                     if("try-error" %in% class(prot_matrix)){
                       showModal(modalDialog(
                         title = "An error occur",
                         prot_matrix[1],
                         easyClose = TRUE
                       ))
                       stop("Error")
                     }
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
  # pulseDIA preprocess discarded20200512
  #################################
  #
  #puslseDIAStatus
  #  initStatus
  #  selected  user has select a DIAData file  to upload, but not sumbited.
  #  submited  DIAData file is submited.
  #  calculating  server is running the combining function
  #  cal_finished combining function is finished.
  # pulseDIAStatus <- reactiveValues(status = "initStatus")
  # observeEvent(input$pulseDia_part1, {
  #   if (!is.null(input$pulseDia_part1)) {
  #     pulseDIAStatus$status = "selected"
  #     shinyjs::enable("submit_pulseDia_file")
  #   }
  # })
  # pulseDIA_out<-eventReactive(input$submit_pulseDia_file, {
  #   if(!is.null(input$pulseDia_part1$datapath))
  #     parts<-input$pulseDia_part1$datapath
  #   if(!is.null(input$pulseDia_part2$datapath))
  #     parts<-list(parts,input$pulseDia_part2$datapath)
  #   if(!is.null(input$pulseDia_part3$datapath))
  #     parts<-list(parts,input$pulseDia_part3$datapath)
  #   if(!is.null(input$pulseDia_part4$datapath))
  #     parts<-list(parts,input$pulseDia_part4$datapath)
  #   if(!is.null(input$pulseDia_part5$datapath))
  #     parts<-list(parts,input$pulseDia_part5$datapath)
  #   if(!is.null(input$pulseDia_part6$datapath))
  #     parts<-list(parts,input$pulseDia_part6$datapath)
  # 
  #   pulseDIA_res<-ForpulseDIA(parts)
  #   return(pulseDIA_res)
  # }, ignoreNULL = FALSE)
  # 
  # output$pulsedia_ui <- renderUI({
  #   switch (
  #     pulseDIAStatus$status,
  #     "initStatus" = tags$h4("Please select a DIA file to upload"),
  #     "selected"   = tags$h4("Please click Submit button, then waiting "),
  #     "submited"   = tags$h4("calculate in progress ... Please waiting"),
  #     "calculating"   = tags$h4("calculate in progeress ... Please waiting"),
  #     "cal_finished"   = tags$div(
  #       tags$h4("calculate finished! Please download the result file"),
  #       downloadButton(
  #         "downloadPulseDIAResult",
  #         label = "Download",
  #         class = "btn-primary"
  #       )
  #     ),
  #     "nextTry" = tags$div(
  #       tags$h4("download finished! ,Please select a DIA file to upload"),
  #       shinyjs::disabled(
  #         downloadButton(
  #           "downloadPulseDIAResult",
  #           label = "Download",
  #           class = "btn-primary"
  #         )
  #       )
  #     ),
  #   )
  # })
  # output$downloadPulseDIAResult <- downloadHandler(filename = "result.txt",
  #                                                  content <- function(file) {
  #                                                    write.table(
  #                                                      pulseDIA_out(),
  #                                                      file,
  #                                                      sep = "\t",
  #                                                      col.names = T,
  #                                                      row.names = F,
  #                                                      quote = F
  #                                                    )
  #                                                    pulseDIAStatus$status = "nextTry"
  #                                                  })
  
  #################################
  # QC
  #################################
  
  QCdatasetInput <-
    eventReactive(input$QC, {
      print("missing value explore")
      if (class(readProteinM()) != "data.frame")
        return(NULL)
      else
        readProteinM()
      
    }, ignoreNULL = FALSE)
  
  observeEvent(input$QC, {
    qc_label <- input$QCLabel
    # if (class(readProteinM()) != "data.frame"){
    #   data0 <- readProteinM2()
    #   anno0<-getAnnoTable2()
    # }
    #   
    # else{
    #   data0 <- readProteinM()
    #   anno0<-getAnnoTable()
    # }
    #dataAnno<-setDataAnno(readProteinM(),readProteinM2(),getAnnoTable(),getAnnoTable2())
    data<-dataAnno()$protM
    anno<-dataAnno()$anno
    col_name <- colnames(data)
    row_name <- as.matrix(data[, 1])
    row_name <- as.vector(row_name[, 1])
    data <- data.matrix(data)
    data <- data[, -1]
    colnames(data) <- col_name[2:length(col_name)]
    row.names(data) <-row_name
    data.ori<-data
    data[is.na(data)] <-0
    
    
    if (!is.null(qc_label) &
        qc_label != "None") {
      sample_names <- colnames(dataAnno()$protM)[-1]
      qc_label <-
        as.vector(dataAnno()$anno[sample_names, qc_label])
    }
    #print(qc_label)
    
    #################################
    # Pearson Correlation
    #################################
    
    
    # output$Qpcctable <-
    #   renderRHandsontable({
    #     if (input$reproducibility) {
    #       if (!is.null(data))
    #       {
    #         rhandsontable(head(data, n = 20L))
    #       }
    #     }
    #     
    #   })
    output$Qpccplot <-
      renderPlot({
        if (input$reproducibility) {
          if (!is.null(data))
          {
            drawcorrplot(data)
          }
        }
        
      })
    output$QCMissingTable <-
      renderRHandsontable({
        rhandsontable(countNa(data.ori,qc_label))
      })
  })
  
  #################################
  # missing value explore
  #################################
  
  output$missingPlot <-
    renderPlot({
      if (input$MissingValueExplore_check & input$QC) {
        if (class(QCdatasetInput()) == "data.frame"|class(readProteinM2()) == "data.frame") {
          output$QMparameters <- renderText({
            "Results are showed below:"
          })
          if(class(readProteinM()) == "data.frame")
             missing_plot(readProteinM())
          else missing_plot(readProteinM2())
        }
        else if (is.null(readProteinM())&is.null(readProteinM2()))
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
  
  output$downloadMissingPlot <-
    downloadHandler(
      filename = function() {
        paste("missingValueExplore", Sys.time(), ".pdf", sep = "")
      },
      content = function(file) {
        pdf(file)
        if(class(readProteinM()) == "data.frame")
          missing_plot(readProteinM())
        else missing_plot(readProteinM2())
        
        dev.off()
      }
    )
  
  #################################
  # data console
  #################################
  ##two files

  readProteinM2 <-reactive({
    #print("two files")
    if (!is.null(input$protein_matrix2)){
      #prot <- getSampleInfo(input$protein_matrix$datapath,input$DCprotmSep)
      fileName = input$protein_matrix2$datapath
      len <- nchar(as.vector(fileName))
      last <- substring(fileName,len-3, len)
      if(identical(last, "xlsx") || identical(last, ".xls")){
        prot <- openxlsx::read.xlsx(fileName)
      }else{
        prot <-
          read.table(
            input$protein_matrix2$datapath,
            header = T,
            sep = input$DCprotmSep,
            check.names = F,
            encoding = "UTF-8",
            stringsAsFactors = F
          )
      }
      prot
    }
  })
 
  getAnnoTable2 <-
    #eventReactive(input$DC_anno,{
    reactive({
      if(!is.null(input$anno_info)){
        anno <-getSampleInfo(input$anno_info$datapath,input$DCsampleSep2)
        rownames(anno) <- anno[,1]
        #print(anno[1:3,1:2])
        anno
      }

    })
  if(!is.null(getAnnoTable2)){
    output$DC_annoShow <-
      DT::renderDataTable(DT::datatable({
        #anno_name<<-colnames(getAnnoTable())
        getAnnoTable2()
      })) 
  }

  ##three files
  #for read protein matrix
  readProteinM <-
    reactive({
      if (!is.null(input$protein_matrix)){
        #prot <- getSampleInfo(input$protein_matrix$datapath,input$DCprotmSep)
        fileName = input$protein_matrix$datapath
        len <- nchar(as.vector(fileName))
        last <- substring(fileName,len-3, len)
        if(identical(last, "xlsx") || identical(last, ".xls")){
          prot <- openxlsx::read.xlsx(fileName)
        }else{
          prot <-
            read.table(
              input$protein_matrix$datapath,
              header = T,
              sep = input$DCprotmSep,
              check.names = F,
              encoding = "UTF-8",
              stringsAsFactors = F
            )
        }
        
      }
      
    })
  ### for column annotation
  output$sampleUi <-
    renderUI({
      if (is.null(input$sample_info))
        "Please upload your files"
      else{
        #print(input$sample_info)
        sample_info <- getSampleInfo(input$sample_info$datapath,input$DCsampleSep)
        sample_header <<-
          colnames(sample_info)
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
  
  
  output$individualUi <-
    renderUI({
      if (is.null(input$individual_info))
        "Please upload your files"
      else{
        #print(input$sample_info)
        individual_info <- getSampleInfo(input$individual_info$datapath,input$DCindividualSep, 1)
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
  sampleInfoInput <-
    eventReactive(input$sample_info_annotation, {
      sampleInfo <- getSampleInfo(input$sample_info$datapath,input$DCsampleSep)
      sample_header <- colnames(sampleInfo)
      colnames(sampleInfo)[which(sample_header == input$sample_info_id)] <-
        "sampleId"
      if (input$sample_info_individual_id != 'select...')
        colnames(sampleInfo)[which(sample_header == input$sample_info_individual_id)] <-
        "individualId"
      sampleInfo
    }, ignoreNULL = T)
  
  sampleResInput <-
    eventReactive(input$sample_info_annotation, {
      sampleInfoInput()
    })
  output$sampleRes <-
    renderText({
      if (class(sampleResInput()) == "data.frame")
        paste("Successfully annotated the sample column!", Sys.time())
      else
        paste("Unsuccessfully annotated sample column", Sys.time())
    })
  individualInfoInput <-
    eventReactive(input$individual_info_annotation, {
      individualInfo <- getSampleInfo(input$individual_info$datapath,input$DCindividualSep)
      individual_header <-
        colnames(individualInfo)
      colnames(individualInfo)[which(individual_header == input$individual_info_id)] <-
        "individualId"
      individualInfo
    }, ignoreNULL = T)
  
  individualResInput <-
    eventReactive(input$individual_info_annotation, {
      individualInfoInput()
    })
  output$individualRes <-
    renderText({
      if (class(individualResInput()) == "data.frame")
        paste("Successfully annotated the individual column!", Sys.time())
      else
        paste("Unsuccessfully annotated individual column", Sys.time())
    })
  getAnnoTable <-
    eventReactive(input$DoAnnoTable, {
      anno <-
        merge(individualInfoInput(), sampleInfoInput(), by = 'individualId')
      rownames(anno) <-
        anno[, "sampleId"]
      anno
    })
  output$annoTable <-
    DT::renderDataTable(DT::datatable({
      #anno_name<<-colnames(getAnnoTable())
      getAnnoTable()
    }))
  
  output$prot_matrix2_preview <- renderTable({
    req(input$protein_matrix2)
    myhead(readProteinM2())
    })
  ###############################################################
  ##statistics
  observeEvent(input$stat_do,
               {
                 stat_label <- input$STanno
                 if (!is.null(stat_label) &
                     stat_label != "None") {
                   


                   sample_names <- colnames(dataAnno()$protM)[-1]
                   stat_label <-
                     as.vector(dataAnno()$anno[sample_names, stat_label])
                 } else{
                   return()
                 }
                 
                 prot_data <-dataAnno()$protM
                 col_name <-colnames(prot_data)
                 row_name <-
                   prot_data[, 1]
                 prot_data <-
                   prot_data[, -1]
                 colnames(prot_data) <-
                   col_name[2:length(col_name)]
                 row.names(prot_data) <-
                   row_name
                 prot_data[is.na(prot_data)] <-
                   0
                 get_stat_prot_anno <-
                   list(label = stat_label, data = prot_data)
                 ###########################
                 #t-test
                 output$ttest_download_ui <-
                   renderUI({
                     downloadButton("downloadttest", label = "Download", class = "btn-primary")
                   })
                 ###########################
                 
                 
                 
                 
               },
               ignoreNULL = TRUE,
               ignoreInit = T)
  output$STprot_anno_Ui <- renderUI({

    anno_name <- colnames(dataAnno()$anno)
    tagList(
      selectInput(
        'STanno',
        'Select type',
        anno_name,
        multiple = F,
        selectize = TRUE
      ),
      checkboxInput("ttest_check", "t test", TRUE),
      checkboxInput("Volcano_check", "Volcano Plot", FALSE),
      checkboxInput("Violin_check", "Violin Plot", TRUE),
      hr(),
      checkboxInput("radarmap", "Radar Map", FALSE),
      conditionalPanel(
        condition = "input.radarmap == true",
        selectInput(
          'rader_var',
          'Select proteins:',
          dataAnno()$protM[,1],
          multiple = T,
          selectize = TRUE
        )),
      hr(),
      tags$h5("Click to process:"),
      actionButton("stat_do", "Submit", class = "btn-primary")
    )
  })
  
  observeEvent(input$stat_do, {
    stat_label <- input$STanno
    if (!is.null(stat_label) & stat_label != "None") {
      sample_names <- colnames(dataAnno()$protM)[-1]
      stat_label <- as.vector(dataAnno()$anno[sample_names, stat_label])
    } else{
      return()
    }
    
    prot_data <- dataAnno()$protM
    col_name <- colnames(prot_data)
    row_name <- prot_data[, 1]
    prot_data <- prot_data[,-1]
    colnames(prot_data) <- col_name[2:length(col_name)]
    row.names(prot_data) <- row_name
    prot_data[is.na(prot_data)] <- 0
    get_stat_prot_anno<-list(label=stat_label,data=prot_data)
    ###########################
    #t-test
    if(input$ttest_check){
      label_vector<-unique(stat_label)
      output$ttest_groups_ui<-renderUI({
        tagList(
          selectInput(
            'ttest_group1',
            'select the first group',
            label_vector,
            multiple = F,
            selectize = TRUE
          ),
          selectInput(
            'ttest_group2',
            'select the second group',
            label_vector,
            multiple = F,
            selectize = TRUE
          )
        )
      })
      output$ttest_do_ui<-renderUI({
        actionButton("ttest_do", "Submit", class = "btn-primary")
      })
    }
    
    #volcano plot
    if(input$Volcano_check){
      label_vector<-unique(stat_label)
      output$volcano_ttest_groups_ui<-renderUI({
        tagList(
          selectInput(
            'volcano_ttest_group1',
            'select the first group',
            label_vector,
            multiple = F,
            selectize = TRUE
          ),
          selectInput(
            'volcano_ttest_group2',
            'select the second group',
            label_vector,
            multiple = F,
            selectize = TRUE
          )
        )
      })
      output$volcano_ttest_do_ui<-renderUI({
        actionButton("volcano_ttest_do", "Submit", class = "btn-primary")
      })
    }
    ###########################
    #violin
    if(input$Violin_check){
      output$DMviolin<-renderPlotly({
        ggplotly(drawviolin_cv(get_stat_prot_anno))
      }) 
    }
    else {
      output$DMviolin<-renderPlotly({
        NULL
      })
    }
    
    #################################
    # Radar
    #################################
    
    if (input$radarmap) {
      output$DMradarparameters <- renderCanvasXpress({
        if (!is.null(get_stat_prot_anno))        {
          if(is.null(input$rader_var)){
            drawradar(get_stat_prot_anno$data)
          }
          else {drawradar(get_stat_prot_anno$data[input$rader_var,])}
        }
        
      })
    }
    else  {
      output$DMradarparameters <- renderCanvasXpress({
        NULL
      })
    }
    
    
  }, ignoreNULL = TRUE, ignoreInit = T)
  ##ttest
  observeEvent(input$ttest_do,{
    stat_label <- input$STanno
    sample_names <- colnames(dataAnno()$protM)[-1]
    anno<-dataAnno()$anno
    stat_label <- as.vector(anno[sample_names, stat_label])
    prot_data <- dataAnno()$protM
    col_name <- colnames(prot_data)
    row_name <- prot_data[, 1]
    prot_data <- prot_data[,-1]
    colnames(prot_data) <- col_name[2:length(col_name)]
    row.names(prot_data) <- row_name
    prot_data[is.na(prot_data)] <- 0
    group1<-input$ttest_group1
    group2<-input$ttest_group2
    g1.data<-prot_data[,(stat_label==group1)]
    g2.data<-prot_data[,(stat_label==group2)]
    ttest_res<-apply_test(g1.data,g2.data,adj_method=input$adjP,alternative=input$t_test_alter,
                          paired=input$paried,var.equal=input$var.equal,conf.level=input$conf.level)
    ttest_res<-ttest_res[,-1]
    output$ttest_out <- renderRHandsontable({
      rhandsontable(ttest_res,height = 300,width = 300)
    })
    
    output$ttest_download_ui<-renderUI({
      downloadButton("downloadttest", label = "Download", class = "btn-primary")
    })
    output$downloadttest <- downloadHandler(
      filename = function() {
        paste("t_test_result", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(
          ttest_res,
          file,
          row.names = T,
          quote = F,
          na = ""
        )
      }
    )
  })
  ##vocano plot
  observeEvent(input$volcano_ttest_do,{
    stat_label <- input$STanno
    sample_names <- colnames(dataAnno()$protM)[-1]
    anno<-dataAnno()$anno
    stat_label <- as.vector(anno[sample_names, stat_label])
    prot_data <- dataAnno()$protM
    col_name <- colnames(prot_data)
    row_name <- prot_data[, 1]
    prot_data <- prot_data[,-1]
    colnames(prot_data) <- col_name[2:length(col_name)]
    row.names(prot_data) <- row_name
    prot_data[is.na(prot_data)] <- 0
    group1<-input$volcano_ttest_group1
    group2<-input$volcano_ttest_group2
    g1.data<-prot_data[,(stat_label==group1)]
    g2.data<-prot_data[,(stat_label==group2)]
    ttest_res<-apply_test(g1.data,g2.data,adj_method=input$volcano_adjP,isLog = input$volcano_isLog,alternative=input$volcano_t_test_alter,
                          paired=input$volcano_paried,var.equal=input$volcano_var.equal,conf.level=input$volcano_conf.level)
    
    output$volcano_plot<-renderPlot({
      myVolcano(ttest_res,input$volcano_adjp_threshold,input$volcano_fc)
    })
    # output$volcano_plot<-renderPlotly({
    #   ggplotly(myVolcano(ttest_res,input$volcano_adjp_threshold,input$volcano_fc))
    # })
    volcano_data<-myVolcanoData(ttest_res,input$volcano_adjp_threshold,input$volcano_fc)
    #print(volcano_data)
    output$volcano_ttest_out <- renderRHandsontable({
      rhandsontable(volcano_data,height = 300,width = 300)
    })
    
    output$volcano_ttest_download_ui<-renderUI({
      downloadButton("volcano_downloadttest", label = "Download", class = "btn-primary")
    })
    output$volcano_downloadttest <- downloadHandler(
      filename = function() {
        paste("volcano_t_test_result", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(
          volcano_data,
          file,
          row.names = T,
          quote = F,
          na = ""
        )
      }
    )
  })
  
  ####################################################
  #data mining
  ####################################################
  output$DMprot_anno_Ui <-
    renderUI({
      anno_name <<- colnames(dataAnno()$anno)
      tagList(
        selectInput('DMprotM', 'select matrix', protM_name, selectize = FALSE),
        selectInput(
          'DManno',
          'select types',
          anno_name,
          multiple = FALSE,
          selectize = TRUE
        )
      )
    })
  output$DMprot_anno_Ui_fs <-
    renderUI({
      
      anno_name <- colnames(dataAnno()$anno)
      tagList(
        selectInput('DMprotM', 'select matrix', protM_name, selectize = FALSE),
        selectInput(
          'DManno',
          'select types',
          anno_name,
          multiple = FALSE,
          selectize = TRUE
        )
      )
    })
  output$DMprot_anno_Ui_class <-
    renderUI({
      anno_name <- colnames(dataAnno()$anno)
      tagList(
        selectInput('DMprotM', 'select matrix', protM_name, selectize = FALSE),
        selectInput(
          'DManno',
          'select types',
          anno_name,
          multiple = FALSE,
          selectize = TRUE
        )
      )
    })
  ##
  observeEvent(input$dmClustering, {
    withProgress(message = 'Data prepare in progress',
                 detail = 'This may take a while...',
                 value = 1/10,
                 {
                   incProgress(2 / 10)
                   qc_label1 <- input$DManno
                   if (!is.null(qc_label1) &
                       qc_label1 != "None") {
                     sample_names <- colnames(dataAnno()$protM)[-1]
                     qc_label <-
                       as.vector(dataAnno()$anno[sample_names, qc_label1])
                   }
                   data <- dataAnno()$protM
                   col_name <- colnames(data)
                   row_name <- as.matrix(data[, 1])
                   row_name <- as.vector(row_name[, 1])
                   data <-
                     data.matrix(data)
                   trainData <-
                     data[, -1]
                   colnames(trainData) <-
                     col_name[2:length(col_name)]
                   row.names(trainData) <- row_name
                   if(input$DMclusertingLog!="none" & !is.null(input$DMclusertingLog)){
                     trainData<-log(trainData,as.numeric(input$DMclusertingLog))
                   }
                   trainData[is.na(trainData)] <-0
                   incProgress(9 / 10, message = "Ready to finish!")
                 })
    #################################
    # Heatmap
    #################################
    #print(qc_label)

                   output$DMheatmapparameters <-
                     renderPlot({
                       if (input$dmheatmap) {
                         if (!is.null(dataAnno()$protM))
                         {    withProgress(message = 'Calculation heatmap',
                                           detail = 'This may take a while...',
                                           value = 0,
                                           {
                                             incProgress(1 / 10)
                           p<-drawheatmap(trainData, qc_label)
                           incProgress(9 / 10, message = "Heatmap ready to finish!")
                                           })
                           print(p)
                           
                         }
                       }

                     })
                   

    #################################
    # PCA
    #################################
    # output$Qpcatable <- renderRHandsontable({
    #   if (input$qcPca) {
    #     if (!is.null(trainData))
    #     {
    #       rhandsontable(head(trainData, n = 20L))
    #     }
    #   }
    # })

                   output$Qpcaplot <-
                     renderPlotly({
                       if (input$qcPca) {
                         if (!is.null(trainData))
                         {    withProgress(message = 'Calculation PCA',
                                           detail = 'This may take a while...',
                                           value = 0,
                                           {
                                             incProgress(1 / 10)
                           p <- drawPCA(trainData, qc_label)
                           incProgress(9 / 10, message = "PCA ready to finish!")
                                           })
                           ggplotly(p) %>% config(displaylogo = F)
                           
                           }
                       }
                       
                     })

    #################################
    # T-sne
    #################################
    # output$Qtsnetable <- renderRHandsontable({
    #   if (input$qctsne) {
    #     if (!is.null(trainData))
    #     {
    #       rhandsontable(head(trainData, n = 20L))
    #     }
    #   }
    # })

                   output$Qtsneplot <-
                     renderPlotly({
                       if (input$qctsne) {
                         if (!is.null(trainData))
                         {    withProgress(message = 'Calculation t-sne',
                                           detail = 'This may take a while...',
                                           value = 0,
                                           {
                                             incProgress(1 / 10)
                           p <- drawTSNE(trainData, qc_label)
                           incProgress(9 / 10, message = "t-sne ready to finish!")
                                           })
                           ggplotly(p) %>% config(displaylogo = F)
                         }
                       }
                       
                     })

    #################################
    # umap
    #################################
    # output$Qumaptable <- renderRHandsontable({
    #   if (input$qcUmap) {
    #     if (!is.null(trainData))
    #     {
    #       rhandsontable(head(trainData, n = 20L))
    #     }
    #   }
    # })

                   output$Qumapplot <-
                     renderPlotly({
                       if (input$qcUmap) {
                         if (!is.null(trainData))
                         {    withProgress(message = 'Calculation umap',
                                           detail = 'This may take a while...',
                                           value = 0,
                                           {
                                             incProgress(1 / 10)
                           p <- drawUMAP(trainData, qc_label)
                           incProgress(9 / 10, message = "umap ready to finish!")
                           
                                           })
                           ggplotly(p) %>% config(displaylogo = F)
                         }
                       }
                     })

  })
  #################################
  # ML
  #################################
  observeEvent(input$mlmethod,{
    shinyjs::hide(id = "mlPredictDiv")
  })
  model.decisionTree_classifier <- NULL
  model.randomForest_classifier <- NULL
  model.xgboost_classifier      <- NULL
  
  trainYy <- NULL
  observeEvent(input$mlsubmitTrain, {
    withProgress(message = 'Data prepare in progress',
                 detail = 'This may take a while...',
                 value = 1/10,
                 {
                  
    if (is.null(input$protein_matrix & is.null(input$protein_matrix2))) {
      #hint to upload train data
      output$DMmlText <-
        renderPrint({
          print("Please upload your training data from data console")
        })
      return()
    }
    
    qc_label1 <- input$DManno
    if (!is.null(qc_label1) &  qc_label1 != "None") {
      sample_names <- colnames(dataAnno()$protM)[-1]
      qc_label <- as.vector(dataAnno()$anno[sample_names, qc_label1])
    } else{
      return()
    }
    
    # data <- readProteinM()
    # col_name <- colnames(data)
    # row_name <- as.matrix(data[, 1])
    # row_name <- as.vector(row_name[, 1])
    # data <-  data.matrix(data)
    # trainData <-  data[, -1]
    # colnames(trainData) <-  col_name[2:length(col_name)]
    # row.names(trainData) <- row_name
    # 
    # trainData[is.na(trainData)] <- 0
    
    output$DMmlText <-
      renderPrint({
        #print(input$mlframework)
        print(input$mlmethod)
        print(unique(qc_label, fromLast = FALSE))
      })
    if (is.null(input$DManno))
    {
      return()
    }
    if (is.null(input$protein_matrix) & is.null(input$protein_matrix2)) {
      #hint to upload train data
      output$DMmlText <-
        renderPrint({
          print("Please upload your training data from data console")
        })
      return()
    }
    #data <- as.data.frame(t(trainData))
    #colnames(data) <- readProteinM()[, 1]
    proteinData <-dataAnno()$protM
    data <- formatProteinMatrix(proteinData)
    
    incProgress(2 / 10,"Data is ready, caculating now!")
    #################################
    # Decision Tree
    #################################
    
    if (input$mlmethod == "Decision Tree") {
      print("Decision tree starting")
      if (!is.null(dataAnno()$protM))
      {
        data$Label <- as.factor(qc_label)
        ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
        trainData <- data[ind == 1,]
        handoutValidationData <- data[ind == 2,]
        model.decisionTree_classifier <<- rpart(Label ~ ., data = trainData, method = "class",
                                                minsplit = input$dt_minsplit, minbucket = input$dt_minbucket)
        #tree <- prune(dtree, cp = dtree$cptable[which.min(dtree$cptable[, "xerror"]), "CP"])
        #tree <- dtree
        
        #handout validation 
        #print((handoutValidationData$Label))
        dt.validation <- predict(model.decisionTree_classifier, handoutValidationData, type = "class")
        # print((dt.validation))
        aaa <- (as.data.frame(dt.validation))
        aaa$handoutValidationData.Label <-handoutValidationData$Label
        nrows <- nrow(aaa)
        ndiff <- 0
        for (curRow in 1:nrows) {
          if(aaa[curRow,1] != aaa[curRow,2]) ndiff <- ndiff + 1
        }
        output$DMmlPlot <-
          renderImage({
            outfile <- tempfile(fileext = '.png')
            png(outfile, width = 400, height = 400)
            rpart.plot(
              model.decisionTree_classifier,
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
        output$DMmloutputText <-
          renderPrint({
            printcp(model.decisionTree_classifier)
            
            cat("\nhandout validation result:\n")
            print(aaa)
            cat("\n error rate: " , ndiff, "/",
                nrows, " = ", ndiff/nrows, sep = "")
          })
        # output$DMmltables <-
        # renderRHandsontable({
        #   rhandsontable(head(data, n = 20L))
        # })
      }
    } else if (input$mlmethod == "Random Forest")
    {
      #################################
      # Random Forest
      #################################
      if (!is.null(dataAnno()$protM))
      {
        #print(dim(data))
        #print(class(data))
        #print(head(data))
        # data$Label <- as.factor(data$Label)
        # model.forest <-
        #   randomForest(Label ~ ., data = data)
        # pre.forest <-
        #   predict(model.forest, data)
        # 
        # if (pre.forest)
        # {
        #   ran_roc <-
        #     roc(data$Label, as.numeric(pre.forest))
        # }
        targetLabel <- as.factor(qc_label)
        ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
        trainData <- data[ind == 1,]
        trainDataLabel <-targetLabel[ind == 1]
        handoutValidationData <- data[ind == 2,]
        handoutValidationDataLabel <- targetLabel[ind == 2]
        rf_mtry <- input$rf_mtry
        if(rf_mtry == 0){
          model.randomForest_classifier <<- randomForest(x = trainData, y = trainDataLabel, prox=TRUE, 
                                                         importance=TRUE, ntree = input$rf_ntree)
        }else{
          model.randomForest_classifier <<- randomForest(x = data, y = targetLabel, prox=TRUE, 
                                                         importance=TRUE, ntree = input$rf_ntree,
                                                         mtry = rf_mtry)
        }
        
        forest.validtateion <- predict(model.randomForest_classifier, handoutValidationData)
        model.forest <- model.randomForest_classifier
        output$DMmlPlotforest <- renderPlot({
          plot(model.forest)
        })
        output$DMmlPlot <-
          renderPlot({
            # layout(matrix(c(1, 2, 3, 0), 2, 2),
            #        widths = c(1, 1),
            #        heights = c(1, 1),
            #        F)
            # layout(matrix(c(1,1), nrow = 2, ncol=1),
            #        respect = FALSE)
            # layout.show(n=2)
            varImpPlot(model.forest, main = "variable importance")
            #plot(model.forest)
            # plot(
            #   ran_roc,
            #   print.auc = TRUE,
            #   auc.polygon = TRUE,
            #   grid = c(0.1, 0.2),
            #   grid.col = c("green", "red"),
            #   max.auc.polygon = TRUE,
            #   auc.polygon.col = "skyblue",
            #   print.thres = TRUE,
            #   main = 'RF ROC'
            # )
          })
        
        output$DMmloutputText <-
          renderPrint({
            # imp <- importance(model.forest)
            # imp <- round(imp, digits = 3)
            # imp <- imp[, 1:3]
            # impvar <- imp[order(imp[, 3], decreasing=TRUE),]
            # if(length(impvar > 50)){
            #   print(head(impvar, n=50L))
            # }else{
            #   print(impvar)
            # }
            print(model.forest)
            #
            #Verification
            cat("\nhandout validationt:\n")
            table(observed = handoutValidationDataLabel, predict = forest.validtateion)
            
          })
        
        # output$DMmltables <- renderRHandsontable({
        #   rhandsontable(head(data, n = 20L))
        # })
      }
    }
    else if (input$mlmethod == "XGBoost")
    {
      print("xgboost starting")
      
      #xgboost parameters
      #https://xgboost.readthedocs.io/en/latest/parameter.html
      params <- list( "booster" = input$xgb_xgbooster_type,
                      "objective" = 'binary:logistic')
      if (params$booster == "gbtree") {
        params$max_depth = as.integer(input$xgb_gbtree_max_depth)
        params$eta = 0.3
      } else if (params$booster == "gblinear") {
        params$feature_selector = input$xgb_gblinear_feature_selector
      } else if (params$booster == "dart") {
        #add
      }
      
      targetLabel <- as.factor(qc_label)
      ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
      trainData <- data[ind == 1,]
      trainDataLabel <-targetLabel[ind == 1]
      handoutValidationData <- data[ind == 2,]
      handoutValidationDataLabel <- targetLabel[ind == 2]
      
      buffer <<- vector('character')
      con    <<- textConnection('buffer', 'wr', local = TRUE)
      sink(con)
      trainXx <- trainData
      trainYy <<- trainDataLabel
      model.xgboost_classifier <<-
        xgboost_classfier_training(
          trainX = trainXx,
          trainY = trainYy,
          parameters = params,
          numRounds = as.integer(input$xgb_nrounds)
        )
      sink()
      close(con)
      
      xgboost.validation <- xgboost_classfier_predict(model.xgboost_classifier, handoutValidationData)
      xgboost.validation <- formatXgbResult(xgboost.validation, handoutValidationDataLabel, row.names(handoutValidationData))
      
      xgboost.validation <- as.data.frame(xgboost.validation)
      xgboost.validation$observed <- handoutValidationDataLabel
      nrows <- nrow(xgboost.validation)
      nerror <- 0
      for (curRow in 1:nrows) {
        if(xgboost.validation[curRow, "predicted"] != xgboost.validation[curRow, "observed"] ) 
          nerror <- nerror +1
      }
      output$DMmlPlotforest <- renderPlot({
        model.xgboost_classifier
      })
      output$DMmlPlot <-
        renderPlot({
          importance_matrix <- xgb.importance(model = model.xgboost_classifier)
          imp_num <- nrow(importance_matrix)
          if (imp_num > 50)
            imp_num <- 50
          xgb.plot.importance(importance_matrix[1:imp_num, ])
        })
      output$DMmloutputText <-
        renderPrint({
          cat("train error\n")
          cat(buffer, sep = "\n")
          
          cat("\n handout validation:\n")
          print(xgboost.validation, digits = 3)
          cat("\nerror rate: ", nerror , "/", nrows, " = ", nerror/nrows , sep = "")
        })
      
      # output$DMmltables <- renderRHandsontable({
      #   rhandsontable(data)
      # })
      
    }
    incProgress(9 / 10, message = "Ready to finish!")})
    shinyjs::show(id = "mlPredictDiv")
  })
  
  observeEvent(input$mlTestFile, {
    shinyBS::updateButton(
      session,
      inputId = "mlsubmitPredict",
      label = "Predicting",
      style = "primary",
      disabled = FALSE
    )
  })
  observeEvent(input$mlsubmitPredict, {
    
    # testdata <- read.csv(file = input$mlTestFile$datapath, sep = input$testDataSep,
    #                      header = FALSE,stringsAsFactors = FALSE)
    # sampleNames <- testdata[1,]
    # sampleNames <- sampleNames[-1]
    # testdata <- testdata[-1,]
    # testdata <- t(testdata)
    # attrNames <- testdata[1,]
    # testdata <- testdata[-1,]
    # colnames(testdata) <- attrNames
    # testdata <-as.data.frame(testdata)
    
    # testdata2 <- read.csv(file = input$mlTestFile$datapath, sep = input$testDataSep,
    #                      header = TRUE,stringsAsFactors = FALSE)
    testdata2 <- getSampleInfo(input$mlTestFile$datapath, sep = input$testDataSep)
    testdata2 <- formatProteinMatrix(testdata2)
    sampleNames2 <- row.names(testdata2)
    if(input$mlmethod == "Decision Tree"){
      print("todo Decision Tree")
      #
      dt.pre <- predict(model.decisionTree_classifier, testdata2, type = "class")
      output$DMmloutputText <-
        renderPrint({
          cat("predict result:\n")
          print(as.data.frame(dt.pre))
        })
    }else if(input$mlmethod == "Random Forest"){
      print("to do Random Forest")
      #pre.forest <- predict(model.randomForest_classifier, testdata2, nodes=TRUE, type = "prob")
      pre.forest <- predict(model.randomForest_classifier, testdata2, type = "prob")
      output$DMmloutputText <-
        renderPrint({
          cat("predict result:\n")
          print(pre.forest)
        })
      
    }else if(input$mlmethod == "XGBoost"){
      result <- xgboost_classfier_predict(model.xgboost_classifier, testdata2)
      result <- formatXgbResult(result, trainYy, sampleNames2)
      #print(result)
      output$DMmloutputText <-
        renderPrint({
          cat("predict result:\n")
          print(as.table(result), digits = 3)
        })
    }
   
    
  })
  #################################
  # feature selection
  #################################
  feature_sel_prot <-
    eventReactive(input$feature_do,
                  {
                    withProgress(message = 'Calculation in progress',
                                 detail = 'This may take a while...',
                                 value = 0,
                                 {
                                   incProgress(1 / 15)
                                   if (!is.null(isolate(input$DMprotM))) {
                                     if (isolate(input$DMprotM) == "uploadedProtMatrix") {
                                       protM <- isolate(dataAnno()$protM)
                                     }
                                     #if(length(isolate(input$DManno))==1){
                                     label = isolate(input$DManno)
                                     #}
                                     rownames(protM) <-
                                       protM[, 1]
                                     protM <-
                                       protM[, -1]
                                     protM <-
                                       t(protM)
                                     sample_names <-
                                       rownames(protM)
                                     label_temp <-
                                       as.vector(dataAnno()$anno[sample_names, label])
                                     allowable_missing_ratio<-as.numeric(input$fs_missing_ratio)
                                     
                                     labeled_protM <-
                                       cbind(label = label_temp, protM, stringsAsFactors = FALSE)
                                     fs_features <-
                                       colnames(protM)
                                     if (!is.null(input$featureSel_filter))
                                       fs_features <-
                                       featureFilter(labeled_protM, !is.na(match(
                                         c("nearZeoVar", "high_correlation"),
                                         input$featureSel_filter
                                       )), allowable_missing_ratio)
                                     # if('random_forest' %in% input$featureSel_algorithm)
                                     #   use_rf=TRUE
                                     # if('lasso' %in% input$featureSel_algorithm)
                                     #   use_lasso = TRUE
                                     #nfeatures<-input$feature_num
                                     print(input$featureSel_algorithm)
                                     incProgress(2 / 15, message = "Initial feature selection!")
                                     if (!is.null(input$featureSel_algorithm))
                                       fs_features <-
                                       featureSel(labeled_protM[, c("label", fs_features)], input$featureSel_algorithm)
                                     
                                     return(fs_features)
                                   }
                                   incProgress(14 / 15, message = "Ready to finish!")
                                 })
                  },
                  ignoreNULL = T,
                  ignoreInit = T)
  
  ####feature selection
  
  ####feature selection
  output$fs_summary <-
    renderText({
      paste(
        "After feature selection your matrix contains",
        
        length(feature_sel_prot()$features),
        "features:",
        paste(feature_sel_prot()$features, collapse = ",")
      )
    })
  
  # output$fs_parameter <-
  #   renderPlot({
  #     plot(feature_sel_prot()$mod)
  #   })
  output$featureSelected <-
    DT::renderDataTable({
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...',
                   value = 0,
                   {
                     incProgress(1 / 15)
                     #DT::datatable({
                     print("Feature sel print")
                     features<-feature_sel_prot()$features
                     gene<-c()
                     desctription<-c()
                     i=0
                     for(f in features){i=i+1
                     incProgress(1/15+(11/15)*(i/length(features)), message = "Ready to finish!")
                     gene_desc<-getAnnoFromUniprot(f)
                     gene<-c(gene,gene_desc[1])
                     desctription<-c(desctription,gene_desc[2])
                     }
                     features<-linkUniprot(features)
                     incProgress(14 / 15, message = "Ready to finish!")
                   })
      data.frame(Proteins=features,Gene=gene,Desctription=desctription)
      #})
    }, escape = FALSE)
  #####download
  output$downloadfeatureSelData <-
    downloadHandler(
      filename = function() {
        paste("featureSelected", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        protM <- dataAnno()$protM
        rownames(protM) <-
          as.vector(unlist(protM[1]))
        protM <-
          protM[feature_sel_prot()$features, ]
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
      # print(paste0(
      #   "Database: ",
      #   "Uniport, ",
      #   "StringDB, ",
      #   "KEGG, ",
      #   "GO, ",
      #   "Reactome, "
      # ))
    })
    
    output$annouistring <-
      renderUI({
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
  
  
  output$annouiuniport <-
    renderUI({
      if (is.null(input$proteinlist))
        return()
    })
  
  
  
  output$anno_table <-
    renderRHandsontable({
      DF = data.frame(
        Name = c("UniProt",
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
  ####################################
  ###Help
  ####################################
  output$downlaod_test_pep <- downloadHandler(
    filename ="test_pep.txt",
    content = function(file) {file.copy("demo/peptides.txt",file)}
  )
  output$downlaod_test_technical <- downloadHandler(
    filename ="technical_replicas.txt",
    content = function(file) {file.copy("demo/technical_replicas.txt",file)}
  )
  output$downlaod_test_batch <- downloadHandler(
    filename ="test_batch.txt",
    content = function(file) {file.copy("demo/batch_file.txt",file)}
  )
  output$downlaod_test_prot <- downloadHandler(
    filename ="test_prot.txt",
    content = function(file) {file.copy("demo/prot.txt",file)}
  )
  output$downlaod_test_sample <- downloadHandler(
    filename ="test_sampleInfo.csv",
    content = function(file) {file.copy("demo/sampleInfo.csv",file)}
  )
  output$downlaod_test_individual <- downloadHandler(
    filename ="test_individual.csv",
    content = function(file) {file.copy("demo/IndividualInfo.csv",file)}
  )
  output$downlaod_test_batchDesign <- downloadHandler(
    filename ="test_batchDesign.csv",
    content = function(file) {file.copy("demo/batch_design.csv",file)}
  )
  output$downlaod_test_pulseDIA <- downloadHandler(
    filename ="test_pulseDIA.txt",
    content = function(file) {file.copy("demo/pulse_dia.txt",file)}
  )}