# Server logic for the Visuals tab. Sourced inside server() with local = TRUE.

viz_base <- reactive({
  req(penalise_reactive())
  penalise_reactive()$df_normw
})

output$viz_plot <- renderPlotly({
  req(input$viz_type)
  d <- viz_base()
  cmp <- d %>% dplyr::filter(is.na(Masked), `Well Type` == "Compound")
  type <- input$viz_type

  if (type == "bar_class") {
    cnt <- hit_class_counts(d)
    p <- ggplot(cnt, aes(Hit_Class, n, fill = Hit_Class, text = paste0(Hit_Class, ": ", n))) +
      geom_col() + labs(title = "ਹਿੱਟ ਕਲਾਸ ਅਨੁਸਾਰ ਗਿਣਤੀ", x = NULL, y = "ਗਿਣਤੀ") +
      theme_minimal() + theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))
    return(ggplotly(apply_hp_theme(p, input$dark_mode), tooltip = "text"))
  }

  if (type == "bar_plate") {
    r <- plate_hit_rates(d) %>% dplyr::mutate(`Plate ID` = factor(`Plate ID`, levels = unique(`Plate ID`)))
    p <- ggplot(r, aes(`Plate ID`, hit_rate_pct, text = paste0("Plate ", `Plate ID`, ": ", round(hit_rate_pct, 2), "%"))) +
      geom_col(fill = "#66C2A5") + labs(title = "ਹਰ ਪਲੇਟ ਦੀ ਹਿੱਟ ਦਰ (%)", x = "Plate ID", y = "%") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
    return(ggplotly(apply_hp_theme(p, input$dark_mode), tooltip = "text"))
  }

  if (type == "scatter2d") {
    p <- ggplot(cmp, aes(Ch1, Ch2, colour = Penalised_Hit, text = paste0("Row ", Row, " Col ", Column))) +
      geom_point(alpha = 0.6) + labs(title = "Ch1 ਬਨਾਮ Ch2", colour = "ਪੈਨਲਟੀ ਹਿੱਟ") + theme_minimal()
    return(ggplotly(apply_hp_theme(p, input$dark_mode), tooltip = "text"))
  }

  if (type == "scatter3d") {
    plot_ly(cmp, x = ~Ch1, y = ~Ch2, z = ~Penalised_value, color = ~Penalised_Hit,
            type = "scatter3d", mode = "markers", marker = list(size = 3)) %>%
      layout(title = "3D: Ch1, Ch2, ਪੈਨਲਟੀ ਮੁੱਲ")
  } else if (type == "surface3d") {
    pl <- unique(as.character(d$`Plate ID`))[1]
    m <- plate_matrix(d, pl, "Penalised_value")
    plot_ly(z = m, type = "surface") %>%
      layout(title = paste("3D ਸਤਹ: ਪਲੇਟ", pl),
             scene = list(xaxis = list(title = "Column"), yaxis = list(title = "Row"),
                          zaxis = list(title = "Penalised_value")))
  } else if (type == "simulation") {
    sim <- simulate_hit_noise(cmp$Penalised_value, input$hit_threshold,
                              noise_sd = input$viz_noise, n_iter = input$viz_iter)
    plot_ly(sim, x = ~hit_rate_pct, type = "histogram", nbinsx = 40) %>%
      layout(title = "ਸਿਮੂਲੇਸ਼ਨ: ਹਿੱਟ ਦਰ ਦੀ ਵੰਡ (%)",
             xaxis = list(title = "ਹਿੱਟ ਦਰ (%)"), yaxis = list(title = "ਗਿਣਤੀ"))
  } else if (type == "box") {
    w <- well_type_distribution(d, "Penalised_value")
    p <- ggplot(w, aes(`Well Type`, value, fill = `Well Type`)) +
      geom_boxplot(outlier.alpha = 0.3) + labs(title = "ਕਿਸਮ ਅਨੁਸਾਰ ਪੈਨਲਟੀ ਮੁੱਲ", x = NULL, y = "ਪੈਨਲਟੀ ਮੁੱਲ") +
      theme_minimal() + theme(legend.position = "none")
    ggplotly(apply_hp_theme(p, input$dark_mode))
  } else if (type == "heat_all") {
    g <- all_plates_grid(d, "Penalised_value")
    p <- ggplot(g, aes(Column, factor(Row, levels = rev(sort(unique(Row)))), fill = value)) +
      geom_tile() + facet_wrap(~plate) + scale_fill_viridis_c(na.value = "grey90") +
      labs(title = "ਸਾਰੀਆਂ ਪਲੇਟਾਂ ਦਾ ਹੀਟਮੈਪ", x = "Column", y = "Row", fill = "ਪੈਨਲਟੀ") +
      theme_minimal()
    ggplotly(apply_hp_theme(p, input$dark_mode))
  } else if (type == "pipeline") {
    st <- pipeline_steps()
    st$x <- st$step
    st$y <- 1
    p <- ggplot(st, aes(x, y)) +
      geom_segment(aes(x = x + 0.35, xend = x + 0.65, y = 1, yend = 1),
                   data = st[-nrow(st), ], arrow = grid::arrow(length = grid::unit(0.15, "cm"))) +
      geom_label(aes(label = paste(step, label, sep = ". ")), size = 3.5, fill = "#E5F5F0") +
      xlim(0.5, nrow(st) + 0.5) + ylim(0.5, 1.5) +
      labs(title = "ਵਿਸ਼ਲੇਸ਼ਣ ਦਾ ਪ੍ਰਵਾਹ") + theme_void()
    ggplotly(apply_hp_theme(p, input$dark_mode))
  } else {
    g <- flow_field_grid()
    g$xend <- g$x + 0.05 * g$u
    g$yend <- g$y
    p <- ggplot(g) +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend, colour = u),
                   arrow = grid::arrow(length = grid::unit(0.1, "cm"))) +
      scale_colour_viridis_c() +
      labs(title = "ਚੈਨਲ ਵਹਾਅ ਖੇਤਰ (ਉਦਾਹਰਨ, CFD ਹੱਲ ਨਹੀਂ)", x = "ਲੰਬਾਈ", y = "ਚੌੜਾਈ") +
      theme_minimal()
    ggplotly(apply_hp_theme(p, input$dark_mode))
  }
})

output$viz_compare <- renderDT({
  comparison_table(viz_base(), input$col_to_penalise) %>%
    DT::datatable(rownames = FALSE, options = list(dom = "t"))
})
