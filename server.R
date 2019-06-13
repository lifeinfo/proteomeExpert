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
      #print(PWmat)
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
  ############################              data console       #####################################################
  
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
  getAnnoTable <- eventReactive(input$DoAnnoTable, {
    anno <-
      merge(individualInfoInput(), sampleInfoInput(), by = 'individualId')
  })
  output$annoTable <- DT::renderDataTable(DT::datatable({
    #anno_name<<-colnames(getAnnoTable())
    getAnnoTable()
  }))
  # observeEvent(input$DoAnnoTable, {
  #   anno_name<<-colnames(getAnnoTable())
  # })
  output$DMprot_anno_Ui <- renderUI({
    anno_name<<-colnames(getAnnoTable())
    tagList(
      selectInput('DMprotM', 'select matrix', protM_name, selectize=FALSE),
      selectInput('DManno', 'select types', anno_name, multiple=TRUE, selectize=TRUE)
    )
    })
  ############################################################ ANNO #######################################
  observeEvent(input$proteinlist, {
    output$anno_parameters1 <- renderPrint({
      print(paste0("Protein list: ", input$proteinlist))
      print(paste0("Organism: ", input$Organism))
      print(paste0("Database: ", input$Database))
    })
  })
  output$anno_parameters2 <- renderText({
    if (input$Database == "Uniport") {
      print("Uniport")
    } else if (input$Database == "String-db") {
      print("String-db")
    } else if (input$Database == "KEGG") {
      print("KEGG")
    } else if (input$Database == "GO") {
      print("GO")
    } else if (input$Database == "Reactome") {
      print("Reactome")
    } else{
      print("Null")
      
    }
  })
  
  #output$anno_table <- DT::renderDataTable(DT::datatable({
  #  iris
  #}))
  
  output$anno_table <- renderRHandsontable({
    #DF = iris
    #if (!is.null(DF)) {
    #  rhandsontable(
    #    DF,
    #    stretchH = "all",
    #    contextMenu = TRUE,
    #    maxRows = 20,
    #    autoWrapRow = TRUE,
    #    dropdownMenu = TRUE,
    #    filters = TRUE,
    #    exportFile = TRUE,
    #    manualRowMove = TRUE,
    #    manualColumnMove = TRUE,
    #    colHeaders = colnames(iris),
    #    search = TRUE
    #  ) %>% hot_cols(columnSorting = TRUE)
    #}
    DF = data.frame(
      protein = c(
        "<a href='https://www.uniprot.org/uniprot/P12345'>P12345</a>",
        "<a href='https://www.uniprot.org/uniprot/P12346'>P12346</a>",
        "<a href='https://www.uniprot.org/uniprot/P12347'>P12347</a>",
        "<a href='https://www.uniprot.org/uniprot/P12345'>P12345</a>",
        "<a href='https://www.uniprot.org/uniprot/P12346'>P12346</a>",
        "<a href='https://www.uniprot.org/uniprot/P12347'>P12347</a>",
        "<a href='https://www.uniprot.org/uniprot/P12345'>P12345</a>",
        "<a href='https://www.uniprot.org/uniprot/P12346'>P12346</a>",
        "<a href='https://www.uniprot.org/uniprot/P12347'>P12347</a>",
        "<a href='https://www.uniprot.org/uniprot/P12345'>P12345</a>",
        "<a href='https://www.uniprot.org/uniprot/P12346'>P12346</a>",
        "<a href='https://www.uniprot.org/uniprot/P12347'>P12347</a>"
      ),
      database = c(
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>",
        "<a href='https://www.uniprot.org'>uniport</a>"
      ),
      desc = c(
        "Catalyzes the irreversible transamination of the L-tryptophan metabolite L-kynurenine to form kynurenic acid (KA). Plays a key role in amino acid metabolism. Important for metabolite exchange between mitochondria and cytosol. Facilitates cellular uptake of long-chain free fatty acids (By similarity).",
        "This book provides a developer-level introduction along with <b>more advanced</b> and useful features of JavaScript.",
        "<em>JavaScript: The Definitive Guide</em> provides a thorough description of the core <b>JavaScript</b> language and both the legacy and standard DOMs implemented in web browsers.",
        "Catalyzes the irreversible transamination of the L-tryptophan metabolite L-kynurenine to form kynurenic acid (KA). Plays a key role in amino acid metabolism. Important for metabolite exchange between mitochondria and cytosol. Facilitates cellular uptake of long-chain free fatty acids (By similarity).",
        "This book provides a developer-level introduction along with <b>more advanced</b> and useful features of JavaScript.",
        "<em>JavaScript: The Definitive Guide</em> provides a thorough description of the core <b>JavaScript</b> language and both the legacy and standard DOMs implemented in web browsers.",
        "Catalyzes the irreversible transamination of the L-tryptophan metabolite L-kynurenine to form kynurenic acid (KA). Plays a key role in amino acid metabolism. Important for metabolite exchange between mitochondria and cytosol. Facilitates cellular uptake of long-chain free fatty acids (By similarity).",
        "This book provides a developer-level introduction along with <b>more advanced</b> and useful features of JavaScript.",
        "<em>JavaScript: The Definitive Guide</em> provides a thorough description of the core <b>JavaScript</b> language and both the legacy and standard DOMs implemented in web browsers.",
        "Catalyzes the irreversible transamination of the L-tryptophan metabolite L-kynurenine to form kynurenic acid (KA). Plays a key role in amino acid metabolism. Important for metabolite exchange between mitochondria and cytosol. Facilitates cellular uptake of long-chain free fatty acids (By similarity).",
        "This book provides a developer-level introduction along with <b>more advanced</b> and useful features of JavaScript.",
        "<em>JavaScript: The Definitive Guide</em> provides a thorough description of the core <b>JavaScript</b> language and both the legacy and standard DOMs implemented in web browsers."
      ),
      comments = c(
        "Aspartate aminotransferase, mitochondrial",
        "Aspartate aminotransferase, mitochondrial",
        "Aspartate aminotransferase, mitochondrial",
        "Aspartate aminotransferase, mitochondrial",
        "Aspartate aminotransferase, mitochondrial",
        "Aspartate aminotransferase, mitochondrial",
        "Aspartate aminotransferase, mitochondrial",
        "Aspartate aminotransferase, mitochondrial",
        "Aspartate aminotransferase, mitochondrial",
        "Aspartate aminotransferase, mitochondrial",
        "Aspartate aminotransferase, mitochondrial",
        
        
        "Aspartate aminotransferase, mitochondrial"
      ), 
      cover = c(
        "http://geneontology.org/assets/go-logo.large.png",
        "http://ecx.images-amazon.com/images/I/51gdVAEfPUL._SL50_.jpg",
        "http://ecx.images-amazon.com/images/I/51VFNL4T7kL._SL50_.jpg",
        "http://geneontology.org/assets/go-logo.large.png",
        "http://ecx.images-amazon.com/images/I/51gdVAEfPUL._SL50_.jpg",
        "http://ecx.images-amazon.com/images/I/51VFNL4T7kL._SL50_.jpg",
        "https://www.uniprot.org/images/logos/Logo_medium.png",
        "http://ecx.images-amazon.com/images/I/51gdVAEfPUL._SL50_.jpg",
        "http://ecx.images-amazon.com/images/I/51VFNL4T7kL._SL50_.jpg",
        "https://www.uniprot.org/images/logos/Logo_medium.png",
        "http://ecx.images-amazon.com/images/I/51gdVAEfPUL._SL50_.jpg",
        "http://ecx.images-amazon.com/images/I/51VFNL4T7kL._SL50_.jpg"
      ),
      stringsAsFactors = FALSE
    )
    
    rhandsontable(DF, allowedTags = "<em><b><strong><a><big>", 
                  height = 500, rowHeaders = FALSE, readOnly = TRUE) %>%
      hot_cols(colWidths = c(80, 80, 200, 200, 80)) %>%
      hot_col(1:2, renderer = "html") %>%
      hot_col(1:4, renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_col(5, renderer = "
    function(instance, td, row, col, prop, value, cellProperties) {
      var escaped = Handsontable.helper.stringify(value),
        img;
  
      if (escaped.indexOf('http') === 0) {
        img = document.createElement('IMG');
        img.src = value;
        img.width = 80;
  
        Handsontable.dom.addEvent(img, 'mousedown', function (e){
          e.preventDefault(); // prevent selection quirk
        });
  
        Handsontable.dom.empty(td);
        td.appendChild(img);
      }
      else {
        // render as text
        Handsontable.renderers.TextRenderer.apply(this, arguments);
      }
  
      return td;
    }") %>% hot_cols(fixedColumnsLeft = 1, columnSorting = TRUE)
  })
  ############################################ ML #########################################
  
}