  # ---------------------------------------------------------------------------
  # Example data and reset
  # ---------------------------------------------------------------------------
  example_on <- reactiveVal(FALSE)
  
  # Uploaded files, or the bundled example when it has been loaded
  files_reactive <- reactive({
    if (isTRUE(example_on())) {
      data.frame(name = "1114695724.csv",
                 datapath = file.path(main_dir, "1114695724.csv"),
                 stringsAsFactors = FALSE)
    } else {
      input$csvs
    }
  })
  
  observeEvent(input$load_example, {
    example_on(TRUE)
    updateRadioButtons(session, "mapped_input", selected = "FALSE")
    updateRadioButtons(session, "plate_format", selected = "1536")
    updateSelectInput(session, "stage", selected = "Primary")
    showNotification("Example data loaded (one 1536-well plate).", type = "message")
  })
  
  # Uploading real files switches the example off; clearing files does too
  observeEvent(input$csvs, example_on(FALSE))
  observeEvent(input$reset, example_on(FALSE))
  
  observeEvent(input$reset_defaults, {
    updateTextInput(session, "target", value = "Target_X")
    updateTextInput(session, "run", value = "01")
    updateRadioButtons(session, "mapped_input", selected = "FALSE")
    updateRadioButtons(session, "plate_format", selected = "1536")
    updateRadioButtons(session, "channel_flip", selected = "FALSE")
    updateRadioButtons(session, "assay_direction", selected = "Inhibitor")
    updateRadioButtons(session, "scale_scope", selected = "Platewise")
    updateRadioButtons(session, "col_to_penalise", selected = "ratio")
    updateSelectInput(session, "stage", selected = "Primary")
    updateSelectInput(session, "Ch1_Norm", selected = "percent- Compound Centre, Positive Control Scale")
    updateSelectInput(session, "Ch2_Norm", selected = "RZscore- Positive Control Centre and Scale")
    updateNumericInput(session, "hit_threshold", value = -30)
    updateNumericInput(session, "k", value = 1)
    updateCheckboxInput(session, "advanced", value = FALSE)
    advanced_values$power <- 2
    advanced_values$offset <- 0
    advanced_values$zero_limit <- FALSE
    updateSelectizeInput(session, "excluded_plates", selected = character(0))
    showNotification("Settings reset to defaults.", type = "message")
  })
  
  
  output$tbl = renderDT(
    df_reactive(), options = list(lengthChange = FALSE))
  
  output$sum_stats_plates = 
    renderDT(penalise_reactive()$plates_wide %>%
               mutate(`Plate ID`=as.factor(`Plate ID`),
                      feature=as.factor(feature)) %>%
               DT::datatable(
                 escape=F,
                 filter="top",
                 extensions='Buttons',
                 options=list(
                   dom='Bftrip',
                   rowCallback = htmlwidgets::JS(
                     sci.format.multiple.columns(
                       find.columns(
                         colnames =names(penalise_reactive()$plates_wide),
                         col = "RZ|median|mad|mean|sd|SB|Zf|Diff|CV"))),
                   buttons= list('colvis')
                 )
               ))
  
  output$sum_stats_run = renderDT(
    DT::datatable(penalise_reactive()$run_summary,
                  escape=F,
                  filter="top",
                  extensions='Buttons',
                  options=list(
                    dom='Bftrip',
                    rowCallback = htmlwidgets::JS(
                      sci.format.multiple.columns(
                        find.columns(
                          colnames = names(penalise_reactive()$run_summary),
                          col = "RZ|median|mad|mean|sd|SB|Zf|Diff|CV"))),
                    buttons= list('colvis')
                  )
    ))
  
  
  output$corr_tbl = renderDT(
    DT::datatable(penalise_reactive()$df_export_gd,
                  escape=F,
                  filter="top"
    ))
  
  
  
  output$corr_tbl_all = renderDT(
    penalise_reactive()$df_normw %>% 
      mutate(Ch1= round(Ch1),
             Ch2= round(Ch2),
             ratio=round(ratio),
             d=round(d,2),
             Penalised_value=round(Penalised_value)) %>% DT::datatable(
               escape=F,
               filter="top",
               extensions='Buttons',
               options=list(
                 dom='Bftrip')
             ))
  
  
  output$equation <- renderUI({
    equation<-equation()
    equation<- paste0(equation,"\ Where d is the distance of a point from the theoretical line connecting the median of the two controls")  
    print(equation)
    withMathJax(equation)
  })
  
  
  output$plate_map_plot<-renderPlot({
    apply_hp_theme(plot_plate(plate_map())+labs(title=paste("Experiment name",input$target,
                                             input$stage,
                                             input$run,
                                             paste0("X_",norm_settings_reactive()$Ch1_norm),
                                             paste0("Y_",norm_settings_reactive()$Ch2_norm),
                                             input$scale_scope,
                                             input$col_to_penalise,
                                             paste0("k",input$k),
                                             paste("d^",advanced_values$power),
                                             paste("offset",advanced_values$offset),
                                             paste("ZeroLim",advanced_values$zero_limit),
                                             paste0("Flip",input$channel_flip),sep="_")), input$dark_mode)
  })
  
  output$hit_tbl = renderDT(
    penalise_reactive()$hit_tbl)
  
  output$hit_tbl_tags = renderDT(
    penalise_reactive()$hit_tbl_tags)
  
  output$k_plot  <- renderPlotly({
    if (input$zoom) {ggplotly(penalise_reactive()$k_plot+coord_cartesian(xlim=penalise_reactive()$zoom_Ch1,ylim=penalise_reactive()$zoom_Ch2))}
    else {ggplotly(penalise_reactive()$k_plot)}
  })
  
  output$k2_plot  <- renderPlotly({
    if (input$zoom) {ggplotly(penalise_reactive()$k2_plot+coord_cartesian(xlim=penalise_reactive()$zoom_ratio,ylim=penalise_reactive()$zoom_Ch2))}
    else {ggplotly(penalise_reactive()$k2_plot)}
  })
  
  
  output$k_plot_raw  <- renderPlotly({
    if (input$zoom) {ggplotly(penalise_reactive()$k_plot_raw+coord_cartesian(xlim=penalise_reactive()$zoom_Ch1_raw,ylim=penalise_reactive()$zoom_Ch2_raw))}
    else {ggplotly(penalise_reactive()$k_plot_raw)}
  })
  
  output$d_dens  <- renderPlot({
    apply_hp_theme(penalise_reactive()$d_dens_plot, input$dark_mode)
    
  }, res = 96)
  
  output$norm_plot  <- renderPlot({
    apply_hp_theme(penalise_reactive()$df_normw_plot, input$dark_mode)
  }, res = 96)
  
  output$norm_plotly  <- renderPlotly({
    ggplotly(penalise_reactive()$df_normw_plot)
  })
  
  output$raw_ratio_plotly  <- renderPlotly({
    ggplotly(penalise_reactive()$df_export_plot_ratio)
  })
  output$raw_penalised_plotly  <- renderPlotly({
    ggplotly(penalise_reactive()$df_export_plot_penalised)
  })
  
  output$norm_constants_plotly  <- renderPlotly({
    ggplotly(penalise_reactive()$norm_plots)
  })
  
  output$done_out<-renderText({ penalise_reactive()$Done })
  
  output$downloadZip <- downloadHandler(
    filename = function() {
      paste("SessionZip", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Simply copy the pre-made ZIP file to the target file location
      file.copy(zip_file_path, file)
    })
  
