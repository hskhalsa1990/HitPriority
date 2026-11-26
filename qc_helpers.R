# Plate-level QC plots and run-level QC checks.

# Default Z' cutoff used by the QC plot, the run health banner and the
# "Exclude plates failing Z'" button. Change it here only.
ZF_DEFAULT_CUTOFF <- 0.4

#' Bar chart of a plate quality metric (Zf, rZf, rSB, SB) per plate
plot_plate_qc <- function(plates_qc, metric = "Zf", feature_sel = "Ch1", cutoff = ZF_DEFAULT_CUTOFF) {
  d <- plates_qc %>%
    dplyr::ungroup() %>%
    dplyr::filter(feature == feature_sel) %>%
    dplyr::mutate(
      `Plate ID` = factor(`Plate ID`, levels = unique(`Plate ID`)),
      value  = .data[[metric]],
      status = ifelse(is.finite(value) & value >= cutoff, "Pass", "Review")
    )
  ggplot(d, aes(x = `Plate ID`, y = value, fill = status,
                text = paste0("Plate: ", `Plate ID`, "<br>", metric, ": ", round(value, 2)))) +
    geom_col() +
    geom_hline(yintercept = cutoff, linetype = "dashed") +
    scale_fill_manual(values = c(Pass = "#66C2A5", Review = "#FC8D62")) +
    labs(title = paste(metric, "per plate -", feature_sel), x = "Plate ID", y = metric, fill = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

#' Heatmap of one plate (useful for spotting edge effects and dispensing patterns)
plot_plate_heatmap <- function(df_normw, plate_id, measure = "Penalised_value") {
  d <- df_normw %>% dplyr::filter(as.character(`Plate ID`) == as.character(plate_id))
  lims <- stats::quantile(d[[measure]], c(0.01, 0.99), na.rm = TRUE)
  ggplot(d, aes(x = Column, y = factor(Row, levels = rev(sort(unique(Row)))), fill = .data[[measure]])) +
    geom_tile(colour = "grey85") +
    scale_fill_viridis_c(limits = lims, oob = scales::squish, na.value = "grey90") +
    scale_x_continuous(expand = c(0, 0)) +
    labs(title = paste("Plate", plate_id, "-", measure), x = "Column", y = "Row", fill = measure) +
    theme_minimal()
}

#' Edge-effect score per plate
#'
#' Compares the median of the outermost compound rows/columns with the interior median,
#' in units of the interior MAD. Positive = edge wells read higher than interior wells.
edge_effect_scores <- function(df_normw, measure = "Ch1", threshold = 0.5) {
  df_normw %>%
    dplyr::filter(is.na(Masked), `Well Type` == "Compound") %>%
    dplyr::group_by(`Plate ID`) %>%
    dplyr::mutate(
      .rk = dplyr::dense_rank(Row),
      .ck = dplyr::dense_rank(Column),
      .edge = .rk %in% c(1, max(.rk)) | .ck %in% c(1, max(.ck))
    ) %>%
    dplyr::summarise(
      n_edge = sum(.edge),
      n_interior = sum(!.edge),
      median_edge = stats::median(.data[[measure]][.edge], na.rm = TRUE),
      median_interior = stats::median(.data[[measure]][!.edge], na.rm = TRUE),
      mad_interior = stats::mad(.data[[measure]][!.edge], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      edge_score = (median_edge - median_interior) / mad_interior,
      Edge_Flag = dplyr::case_when(
        !is.finite(edge_score) ~ "n/a",
        abs(edge_score) > threshold ~ "Review",
        TRUE ~ "OK"
      )
    )
}

#' Plain-language warnings about the run
run_health_messages <- function(plates_qc, edge_tbl, hit_summary, zf_cutoff = ZF_DEFAULT_CUTOFF, excluded = NULL) {
  msgs <- character(0)
  z <- dplyr::ungroup(plates_qc) %>% dplyr::filter(feature == "Ch1")
  bad <- z$`Plate ID`[!is.finite(z$Zf) | z$Zf < zf_cutoff]
  if (length(bad) > 0) {
    msgs <- c(msgs, paste0(length(bad), " plate(s) with Z' below ", zf_cutoff, ": ",
                           paste(utils::head(bad, 10), collapse = ", "), if (length(bad) > 10) ", ..."))
  }
  e <- edge_tbl$`Plate ID`[edge_tbl$Edge_Flag == "Review"]
  if (length(e) > 0) {
    msgs <- c(msgs, paste0(length(e), " plate(s) with a possible edge effect: ",
                           paste(utils::head(e, 10), collapse = ", "), if (length(e) > 10) ", ..."))
  }
  if (hit_summary$n_compounds > 0) {
    rate <- 100 * hit_summary$n_penalised / hit_summary$n_compounds
    if (rate > 5) msgs <- c(msgs, paste0("Hit rate after penalty is ", round(rate, 1), "% - unusually high; check the threshold and normalisation."))
  }
  if (hit_summary$pct_removed > 50) {
    msgs <- c(msgs, paste0("The penalty removes ", hit_summary$pct_removed, "% of non-penalised hits - check k and the normalisation."))
  }
  if (length(excluded) > 0) {
    msgs <- c(msgs, paste0(length(excluded), " plate(s) manually excluded: ",
                           paste(utils::head(excluded, 10), collapse = ", "),
                           if (length(excluded) > 10) ", ..."))
  }
  msgs
}

#' Dark theme for ggplot objects (used when dark mode is on)
hp_dark_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#25282d", colour = NA),
    panel.background = element_rect(fill = "#25282d", colour = NA),
    panel.grid.major = element_line(colour = "#3a3f46"),
    panel.grid.minor = element_line(colour = "#30343a"),
    text = element_text(colour = "#e4e6ea"),
    axis.text = element_text(colour = "#c9ced6"),
    legend.background = element_rect(fill = "#25282d", colour = NA),
    legend.key = element_rect(fill = "#25282d", colour = NA),
    strip.background = element_rect(fill = "#3a3f46"),
    strip.text = element_text(colour = "#e4e6ea")
  )
}

apply_hp_theme <- function(p, dark) {
  if (isTRUE(dark)) p + hp_dark_theme() else p
}

#' Median of the Neutral and Positive controls across plates (spots drift / bad plates)
plot_control_trend <- function(plates_qc, feature_sel = "Ch1") {
  d <- plates_qc %>%
    dplyr::ungroup() %>%
    dplyr::filter(feature == feature_sel) %>%
    dplyr::mutate(plate_order = dplyr::row_number()) %>%
    tidyr::pivot_longer(c(`median_Neutral Control`, `median_Positive Control`),
                        names_to = "Control", values_to = "median") %>%
    dplyr::mutate(Control = sub("median_", "", Control))
  ggplot(d, aes(plate_order, median, colour = Control, group = Control,
                text = paste0("Plate: ", `Plate ID`, "<br>", Control, ": ", signif(median, 4)))) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = c("Neutral Control" = "#E69F00", "Positive Control" = "#8DA0CB")) +
    labs(title = paste("Control medians across plates -", feature_sel),
         x = "Plate order", y = "Median (raw)") +
    theme_minimal()
}
