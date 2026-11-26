  # ---------------------------------------------------------------------------
  # Hit candidates
  # ---------------------------------------------------------------------------
  hit_candidates_reactive <- reactive({
    get_hit_candidates(penalise_reactive()$df_normw)
  })
  
  output$hit_summary_ui <- renderUI({
    s <- summarise_hits(penalise_reactive()$df_normw)
    box <- function(value, label) {
      column(3, wellPanel(style = "text-align:center;", tags$h3(value), tags$div(label)))
    }
    fluidRow(
      box(format(s$n_compounds, big.mark = ","), "Compounds tested"),
      box(s$n_classical, "Hits without penalty"),
      box(s$n_penalised, "Hits after penalty"),
      box(paste0(s$n_deprioritised, " (", s$pct_removed, "%)"), "Deprioritised by penalty")
    )
  })
  
  output$hit_candidates_tbl <- renderDT({
    hit_candidates_reactive() %>%
      filter(Hit_Class %in% input$hit_class_filter) %>%
      mutate(across(any_of(c("Ch1", "Ch2", "ratio", "d", "Penalised_value", "Penalised_value_raw")),
                    ~ round(.x, 2))) %>%
      DT::datatable(filter = "top", rownames = FALSE, extensions = "Buttons",
                    options = list(dom = "Bftrip", buttons = list("colvis"), pageLength = 15))
  })
  
  output$download_hits <- downloadHandler(
    filename = function() paste0(exp_reactive(), "_hit_candidates.csv"),
    content = function(file) readr::write_csv(hit_candidates_reactive(), file)
  )
  
  output$threshold_sens_plot <- renderPlotly({
    sens <- threshold_sensitivity(penalise_reactive()$df_normw, input$col_to_penalise)
    p <- ggplot(sens, aes(threshold, n_hits, colour = Method,
                          text = paste0(Method, "<br>threshold: ", threshold,
                                        "<br>hits: ", n_hits, " (", round(hit_rate_pct, 2), "%)"))) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = input$hit_threshold, linetype = "dashed") +
      scale_colour_brewer(palette = "Set2") +
      labs(title = "Compound hits vs. hit threshold", x = "Hit threshold", y = "Number of compound hits") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  # ---------------------------------------------------------------------------
  # Plate QC
  # ---------------------------------------------------------------------------
  output$plate_qc_plot <- renderPlotly({
    req(input$qc_metric, input$qc_feature, input$qc_cutoff)
    ggplotly(plot_plate_qc(penalise_reactive()$plates_qc, input$qc_metric, input$qc_feature, input$qc_cutoff),
             tooltip = "text")
  })
  
  output$control_trend_plot <- renderPlotly({
    req(input$qc_feature)
    ggplotly(plot_control_trend(penalise_reactive()$plates_qc, input$qc_feature), tooltip = "text")
  })
  
  output$plate_hit_rate_plot <- renderPlotly({
    d <- plate_hit_rates(penalise_reactive()$df_normw) %>%
      mutate(`Plate ID` = factor(`Plate ID`, levels = unique(`Plate ID`)))
    p <- ggplot(d, aes(`Plate ID`, hit_rate_pct,
                       text = paste0("Plate: ", `Plate ID`, "<br>Hit rate: ", round(hit_rate_pct, 2), "%"))) +
      geom_col(fill = "#66C2A5") +
      geom_hline(yintercept = median(d$hit_rate_pct), linetype = "dashed") +
      labs(title = "Hit rate per plate (dashed = median)", x = "Plate ID", y = "Hit rate (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    ggplotly(p, tooltip = "text")
  })
  
  output$heatmap_controls <- renderUI({
    d <- penalise_reactive()$df_normw
    fluidRow(
      column(6, selectInput("hm_plate", "Plate", choices = sort(unique(d$`Plate ID`)))),
      column(6, selectInput("hm_measure", "Heatmap measure",
                            choices = c("Penalised_value", "Ch1", "Ch2", "ratio", "d")))
    )
  })
  
  output$plate_heatmap <- renderPlot({
    req(input$hm_plate, input$hm_measure)
    apply_hp_theme(plot_plate_heatmap(penalise_reactive()$df_normw, input$hm_plate, input$hm_measure), input$dark_mode)
  }, res = 96)
  
  # ---------------------------------------------------------------------------
  # Save / load settings
  # ---------------------------------------------------------------------------
  collect_settings <- function() {
    list(
      target = input$target, mapped_input = as.character(input$mapped_input),
      plate_format = input$plate_format, stage = input$stage, run = input$run,
      channel_flip = as.character(input$channel_flip), assay_direction = input$assay_direction,
      Ch1_Norm = input$Ch1_Norm, Ch2_Norm = input$Ch2_Norm, scale_scope = input$scale_scope,
      col_to_penalise = input$col_to_penalise, hit_threshold = input$hit_threshold, k = input$k,
      power = advanced_values$power, offset = advanced_values$offset,
      zero_limit = as.character(advanced_values$zero_limit),
      excluded_plates = as.character(input$excluded_plates)
    )
  }
  
  output$save_settings <- downloadHandler(
    filename = function() paste0("HitPriority_settings_", input$target, ".json"),
    content = function(file) jsonlite::write_json(collect_settings(), file, auto_unbox = TRUE, pretty = TRUE)
  )
  
  pending_adv <- reactiveVal(NULL)
  
  # Advanced inputs only exist once the advanced panel is shown, so apply them when they appear
  observe({
    p <- pending_adv()
    req(p, input$power, input$offset, input$zero_limit)
    updateSliderInput(session, "power", value = p$power)
    updateNumericInput(session, "offset", value = p$offset)
    updateRadioButtons(session, "zero_limit", selected = p$zero_limit)
    pending_adv(NULL)
  })
  
  observeEvent(input$load_settings, {
    s <- tryCatch(jsonlite::fromJSON(input$load_settings$datapath), error = function(e) NULL)
    if (is.null(s)) {
      showNotification("Could not read the settings file.", type = "error")
      return()
    }
    if (!is.null(s$target)) updateTextInput(session, "target", value = s$target)
    if (!is.null(s$run)) updateTextInput(session, "run", value = s$run)
    for (id in c("mapped_input", "plate_format", "channel_flip", "assay_direction", "scale_scope", "col_to_penalise")) {
      if (!is.null(s[[id]])) updateRadioButtons(session, id, selected = s[[id]])
    }
    for (id in c("stage", "Ch1_Norm", "Ch2_Norm")) {
      if (!is.null(s[[id]])) updateSelectInput(session, id, selected = s[[id]])
    }
    if (!is.null(s$hit_threshold)) updateNumericInput(session, "hit_threshold", value = s$hit_threshold)
    if (!is.null(s$k)) updateNumericInput(session, "k", value = s$k)
    if (!is.null(s$power) || !is.null(s$offset) || !is.null(s$zero_limit)) {
      updateCheckboxInput(session, "advanced", value = TRUE)
      pending_adv(list(power = s$power %||% 2, offset = s$offset %||% 0,
                       zero_limit = s$zero_limit %||% "FALSE"))
    }
    if (length(s$excluded_plates) > 0) {
      ids <- as.character(unlist(s$excluded_plates))
      updateSelectizeInput(session, "excluded_plates", choices = ids, selected = ids)
    }
    showNotification("Settings loaded.", type = "message")
  })
  
  
  # ---------------------------------------------------------------------------
  # Dark mode
  # ---------------------------------------------------------------------------
  observeEvent(input$dark_mode, {
    session$sendCustomMessage("toggleDark", isTRUE(input$dark_mode))
  })
  
  # ---------------------------------------------------------------------------
  # Automatic k suggestion
  # ---------------------------------------------------------------------------
  observe({
    d <- tryCatch(penalise_reactive()$df_normw, error = function(e) NULL)
    req(d)
    tags <- setdiff(levels(d$`Well Tags`), c("Compound", "Neutral Control", "Positive Control"))
    updateSelectizeInput(session, "k_keep_tags", choices = tags, selected = isolate(input$k_keep_tags))
    updateSelectizeInput(session, "k_remove_tags", choices = tags, selected = isolate(input$k_remove_tags))
  })
  
  k_suggest_reactive <- reactive({
    suggest_k(penalise_reactive()$df_normw, input$col_to_penalise, input$hit_threshold,
              offset = advanced_values$offset, power = advanced_values$power,
              zero_limit = advanced_values$zero_limit,
              keep_tags = input$k_keep_tags, remove_tags = input$k_remove_tags,
              min_keep_pct = input$k_min_keep, min_remove_pct = input$k_min_remove)
  })
  
  output$k_suggestion_ui <- renderUI({
    r <- k_suggest_reactive()
    if (is.na(r$k)) {
      tagList(tags$h4("No k between 0 and 10 meets the criteria"),
              tags$div("Relax the targets or check the selected tags."))
    } else {
      tagList(tags$h3(paste("Suggested k =", r$k)), tags$div(r$method))
    }
  })
  
  observeEvent(input$apply_k, {
    k <- k_suggest_reactive()$k
    if (is.na(k)) {
      showNotification("No suggested k to apply.", type = "warning")
    } else {
      updateNumericInput(session, "k", value = k)
    }
  })
  
  output$k_suggest_plot <- renderPlotly({
    r <- k_suggest_reactive()
    long <- r$table %>%
      tidyr::pivot_longer(c(compound_hits_retained_pct, keep_retained_pct, artefact_removed_pct),
                          names_to = "Measure", values_to = "pct", values_drop_na = TRUE)
    p <- ggplot(long, aes(k, pct, colour = Measure)) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = input$k, linetype = "dashed") +
      scale_colour_brewer(palette = "Set2") +
      labs(title = "Effect of penalty strength k", x = "k", y = "Percent") +
      theme_minimal()
    if (!is.na(r$k)) p <- p + geom_vline(xintercept = r$k, colour = "#E7298A")
    ggplotly(p)
  })
  
  output$k_suggest_tbl <- renderDT({
    k_suggest_reactive()$table %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      DT::datatable(rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })
  
  # ---------------------------------------------------------------------------
  # Excel report
  # ---------------------------------------------------------------------------
  output$download_xlsx <- downloadHandler(
    filename = function() paste0(exp_reactive(), "_report.xlsx"),
    content = function(file) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' is required for Excel reports. Install it with install.packages('openxlsx').")
      }
      r <- penalise_reactive()
      s <- summarise_hits(r$df_normw)
      settings <- collect_settings()
      build_excel_report(file, list(
        Summary = data.frame(
          Metric = c("Compounds tested", "Hits without penalty", "Hits after penalty",
                     "Deprioritised by penalty", "% of non-penalised hits removed"),
          Value = c(s$n_compounds, s$n_classical, s$n_penalised, s$n_deprioritised, s$pct_removed)),
        Settings = data.frame(Parameter = names(settings),
                              Value = vapply(settings, function(x) paste(x, collapse = ", "), character(1)),
                              row.names = NULL),
        `Hit candidates` = get_hit_candidates(r$df_normw),
        `Hit rate by type` = r$hit_tbl,
        `Hit rate by tag` = r$hit_tbl_tags,
        `Threshold sensitivity` = threshold_sensitivity(r$df_normw, input$col_to_penalise),
        `Plate QC` = dplyr::select(dplyr::ungroup(r$plates_qc), feature, `Plate ID`,
                                   dplyr::any_of(c("rSB", "SB", "Zf", "Zfc", "rZf", "DiffZ")), dplyr::everything()),
        `Edge effect` = edge_reactive(),
        `Run health` = data.frame(Warning = c(run_health_reactive(),
                                              if (length(run_health_reactive()) == 0) "No run-level warnings")),
        `Run summary` = r$run_summary,
        `k suggestion` = k_suggest_reactive()$table
      ))
    }
  )
  
  
  # ---------------------------------------------------------------------------
  # Dark-aware ggplotly (shadows plotly::ggplotly inside the server)
  # ---------------------------------------------------------------------------
  ggplotly <- function(p, ...) plotly::ggplotly(apply_hp_theme(p, input$dark_mode), ...)
  
