  # ---------------------------------------------------------------------------
  # Edge effects and run health
  # ---------------------------------------------------------------------------
  edge_reactive <- reactive({
    req(input$edge_threshold)
    edge_effect_scores(penalise_reactive()$df_normw, input$col_to_penalise, input$edge_threshold)
  })
  
  output$edge_tbl <- renderDT({
    edge_reactive() %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      DT::datatable(rownames = FALSE, filter = "top", options = list(pageLength = 10))
  })
  
  run_health_reactive <- reactive({
    r <- penalise_reactive()
    run_health_messages(r$plates_qc, edge_reactive(), summarise_hits(r$df_normw),
                        excluded = input$excluded_plates)
  })
  
  output$run_health_ui <- renderUI({
    msgs <- run_health_reactive()
    if (length(msgs) == 0) {
      tags$div(class = "alert alert-success", "Run health: no warnings.")
    } else {
      tags$div(class = "alert alert-warning",
               tags$strong("Run health warnings"),
               tags$ul(lapply(msgs, tags$li)))
    }
  })
  
  
  # ---------------------------------------------------------------------------
  # Plate exclusion
  # ---------------------------------------------------------------------------
  observeEvent(input$csvs, {
    if (!as.logical(input$mapped_input)) {
      ids <- sub("\\.csv$", "", input$csvs$name)
      updateSelectizeInput(session, "excluded_plates", choices = ids, selected = isolate(input$excluded_plates))
    }
  })
  
  observeEvent(input$exclude_flagged, {
    q <- tryCatch(penalise_reactive()$plates_qc, error = function(e) NULL)
    if (is.null(q)) {
      showNotification("Load data first.", type = "warning")
      return()
    }
    z <- dplyr::ungroup(q) %>% dplyr::filter(feature == "Ch1")
    flagged <- as.character(z$`Plate ID`[!is.finite(z$Zf) | z$Zf < ZF_DEFAULT_CUTOFF])
    if (length(flagged) == 0) {
      showNotification(paste0("No plates fail the Z' cutoff (", ZF_DEFAULT_CUTOFF, ")."), type = "message")
      return()
    }
    all_ids <- union(as.character(input$excluded_plates), flagged)
    updateSelectizeInput(session, "excluded_plates", choices = all_ids, selected = all_ids)
    showNotification(paste("Excluded", length(flagged), "plate(s) with Z' below", ZF_DEFAULT_CUTOFF), type = "message")
  })
  
  
