# Helpers for the Visuals tab: 3D views, simulation, flow field, bars and comparison table.
# All functions are pure (no Shiny) so they can be unit tested.

#' Monte Carlo: how many compound hits survive measurement noise
#'
#' Adds Gaussian noise (sd = noise_sd) to each compound's value n_iter times and
#' reports the mean and 5-95% range of the hit rate at the given threshold.
simulate_hit_noise <- function(values, threshold, noise_sd = 5, n_iter = 500, seed = 1) {
  v <- values[is.finite(values)]
  if (length(v) == 0) return(data.frame(iter = integer(0), hit_rate_pct = numeric(0)))
  set.seed(seed)
  rates <- vapply(seq_len(n_iter), function(i) {
    noisy <- v + stats::rnorm(length(v), 0, noise_sd)
    100 * mean(noisy < threshold)
  }, numeric(1))
  data.frame(iter = seq_len(n_iter), hit_rate_pct = rates)
}

#' Summary of a simulation run
simulate_summary <- function(sim) {
  c(mean = mean(sim$hit_rate_pct),
    p05 = unname(stats::quantile(sim$hit_rate_pct, 0.05)),
    p95 = unname(stats::quantile(sim$hit_rate_pct, 0.95)))
}

#' Illustrative 2D channel-flow field (Poiseuille profile), for an educational CFD-style view.
#' This is NOT a CFD solver; it shows the shape of a parabolic flow in a channel.
flow_field_grid <- function(nx = 12, ny = 8, u_max = 1) {
  x <- seq(0, 1, length.out = nx)
  y <- seq(0, 1, length.out = ny)
  g <- expand.grid(x = x, y = y)
  g$u <- u_max * 4 * g$y * (1 - g$y)
  g$v <- 0
  g
}

#' Bar data: compounds per hit class
hit_class_counts <- function(df_normw) {
  d <- df_normw %>%
    dplyr::filter(is.na(Masked), `Well Type` == "Compound") %>%
    classify_hits()
  d %>% dplyr::count(Hit_Class, name = "n")
}

#' Comparison table: one row per method with the main hit numbers
comparison_table <- function(df_normw, col_to_penalise) {
  s <- summarise_hits(df_normw)
  data.frame(
    Measure = c("Compounds tested", "Hits without penalty", "Hits after penalty",
                "Deprioritised by penalty (%)", "Measure used for classical hits"),
    Value = c(s$n_compounds, s$n_classical, s$n_penalised,
              s$pct_removed, col_to_penalise),
    stringsAsFactors = FALSE
  )
}

#' Plate matrix (Row x Column) for a 3D surface
plate_matrix <- function(df_normw, plate_id, measure = "Penalised_value") {
  d <- df_normw %>% dplyr::filter(as.character(`Plate ID`) == as.character(plate_id))
  rows <- sort(unique(d$Row))
  cols <- sort(unique(d$Column))
  m <- matrix(NA_real_, nrow = length(rows), ncol = length(cols), dimnames = list(rows, cols))
  idx <- cbind(match(d$Row, rows), match(d$Column, cols))
  m[idx] <- d[[measure]]
  m
}

#' Steps of the HitPriority analysis pipeline (for the process diagram)
pipeline_steps <- function() {
  data.frame(
    step = 1:6,
    label = c("Raw plate CSV", "Normalisation", "Penalty (k)", "Hit calling", "QC checks", "Hit candidates"),
    stringsAsFactors = FALSE
  )
}

#' Distribution of a measure by well type (box plot data)
well_type_distribution <- function(df_normw, measure = "Penalised_value") {
  df_normw %>%
    dplyr::filter(is.na(Masked)) %>%
    dplyr::select(`Well Type`, value = dplyr::all_of(measure)) %>%
    dplyr::filter(is.finite(value))
}

#' Plate heatmap grid for all plates at once (Row x Column, faceted by plate)
all_plates_grid <- function(df_normw, measure = "Penalised_value") {
  df_normw %>%
    dplyr::filter(is.na(Masked)) %>%
    dplyr::transmute(plate = as.character(`Plate ID`), Row, Column, value = .data[[measure]])
}
