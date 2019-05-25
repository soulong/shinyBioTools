
module_rtPCR <- function(input, output, session) {
  # get basical upload file info, read data, ruturn data and relational info
  data_info <- reactive({
    skip_row <- ifelse(input$equip=="Quantstudio_5", 47, 42)
    infile <- input$userfile
    if (is.null(input$userfile))
      return()
    # real_file <- 'C:\\Users\\Hao He\\Desktop\\test.xlsx' get real path that
    # can be recognize by read_EXCEL for other read function like read_csv, this
    # doesn't needed
    file.rename(infile$datapath, paste(infile$datapath, ".xls", sep=""))
    real_file <- paste(infile$datapath, ".xls", sep="")
    # read data once file is uploaded
    data_all <- readxl::read_excel(real_file, sheet="Results", skip=skip_row, na="NA")
    # fectch only Well, Samples, Targets and CT
    data_all <- data_all[, c(1, 4, 5, 15)]
    # remove excel tail additional rows which was added by exported setup
    data_all <- na.omit(data_all)
    colnames(data_all) <- c("Well", "Sample", "Target", "CT")
    data_all$Well <- as.character(data_all$Well)
    # get all unique sample names
    all_sample_names <- unique(data_all$Sample)
    # get all unique targets names
    all_target_names <- unique(data_all$Target)
    return(list(data_all, all_sample_names, all_target_names, real_file))
  })

  data_expr <- eventReactive(input$apply, {
    skip_row <- ifelse(input$equip=="Quantstudio_5", 47, 42)
    print("analysis now")
    data_cal <- shinyBioTools::rtPCR(data_info()[[1]], input$sample, input$target, input$na.do,
                         data_info()[[4]], skip_row)
    print("get result")
    return(data_cal)
  })

  # update selected box value once file was uploaded
  observe({
    updateSelectInput(session, "sample", choices = data_info()[[2]])
  })
  observe({
    updateSelectInput(session, "target", choices = data_info()[[3]])
  })


  # plot facet and combined plot of calculated expr
  facet_plot <- reactive({
    ggplot(data = data_expr()[[1]], aes(x = Samples, y = Values, fill = Targets, color=Targets)) +
      facet_wrap(~Targets, scales='free_y') +
      theme_classic() +
      geom_bar(position = "identity", stat = "identity") + # width=input$barwidth
      geom_errorbar(aes(ymin = Values, ymax = Values + SDs), position = "identity", width=0.5) +
      labs(title='', x='', y='Relative mRNA expression') +
      theme(axis.text = element_text(size = input$size_facet), axis.title.y = element_text(size = input$size_facet)) +
      theme(axis.text.x = element_text(angle = input$xangle_facet, vjust = 0.5)) +
      theme(legend.position = 'none')
  })
  combine_plot <- reactive({
    ggplot(data = data_expr()[[1]], aes(x = Samples, y = Values, fill = Targets, color=Targets)) +
      geom_bar(position = "dodge", stat = "identity") + # width=input$barwidth
      theme_classic() +
      geom_errorbar(aes(ymin = Values, ymax = Values + SDs), position='dodge',width=0.5) +
      labs(title='', x='', y='Relative mRNA expression') +
      theme(axis.text = element_text(size = input$size_combine), axis.title.y = element_text(size = input$size_combine)) +
      theme(axis.text.x = element_text(angle = input$xangle_combine, vjust = 0.5)) +
      theme(legend.position='top', legend.title = element_blank(), legend.text = element_text(size=input$legendsize_combine))
  })

  # render plot to UI element
  output$plot_facet <- renderPlot({facet_plot()})
  output$plot_facet_ui <- renderUI({
    plotOutput(session$ns("plot_facet"),
               width = paste0(input$plotwidth_facet,'px'),
               height = paste0(input$plotheight_facet,'px'))
  })

  output$plot_combine <- renderPlot({combine_plot()})
  output$plot_combine_ui <- renderUI({
    plotOutput(session$ns("plot_combine"),
               width = paste0(input$plotwidth_combine,'px'),
               height = paste0(input$plotheight_combine,'px'))
  })


  # plot facet and combined plot of calculated melting curve
  mc_plot_1 <- reactive({
    ggplot(data_expr()[[4]], aes(x = Temperature, y = Derivative, color = Well)) +
      facet_wrap(~Target, scales = "free_y") +
      geom_line(stat = "identity", show.legend = FALSE) +
      labs(x = "Temperature", y = "Derivative") +
      ggthemes::theme_tufte()
  })
  mc_plot_2 <- reactive({
    if (is.null(data_expr()))
      return()
    ggplot(data_expr()[[4]], aes(x = Temperature, y = Derivative, color = Well)) +
      facet_wrap(~Target + Sample, scales = "free_y") +
      geom_line(stat = "identity", show.legend = FALSE) +
      labs(x = "Temperature", y = "Derivative") +
      ggthemes::theme_tufte()
  })

  output$plot_mc_1 <- renderPlot({mc_plot_1()})
  output$plot_mc_2 <- renderPlot({mc_plot_2()})

  output$table_summary <- renderTable({
    if (is.null(data_expr()))
      return()
    data_expr()[[1]]
  })

  output$na_sample <- renderPrint({
    if (is.null(data_expr()))
      return()
    data_expr()[[2]]
  })

  output$na_target <- renderPrint({
    if (is.null(data_expr()))
      return()
    data_expr()[[3]]
  })

  # hand download buttom, raw data and calculated data will be downloaded
  output$download_plot <- downloadHandler(filename="plot.pdf", content = function(file) {
    pdf(file, width = (input$plotwidth_facet/50), height = (input$plotheight_facet/50))
    print(facet_plot())
    print(combine_plot())
    print(mc_plot_1())
    print(mc_plot_2())
    dev.off()
  })

}
