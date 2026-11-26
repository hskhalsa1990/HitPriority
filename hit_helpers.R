# Helpers for hit classification, hit lists and threshold sensitivity.
# Expects the `df_normw` table produced inside penalise_reactive() in app.R.

#' Classify every well by how the penalty changed its hit status
classify_hits <- function(df_normw) {
  df_normw %>%
    dplyr::mutate(
      .pen = dplyr::coalesce(Penalised_Hit, FALSE),
      .cls = dplyr::coalesce(Classical_Hit, FALSE),
      Hit_Class = dplyr::case_when(
        .pen & .cls   ~ "Confirmed hit",
        !.pen & .cls  ~ "Deprioritised (likely artefact)",
        .pen & !.cls  ~ "Penalised-only hit",
        TRUE          ~ "Inactive"
      )
    ) %>%
    dplyr::select(-.pen, -.cls)
}

#' Ranked table of all compounds that are a hit with or without the penalty
get_hit_candidates <- function(df_normw) {
  df_normw %>%
    dplyr::filter(is.na(Masked), `Well Type` == "Compound") %>%
    classify_hits() %>%
    dplyr::filter(Hit_Class != "Inactive") %>%
    dplyr::arrange(Penalised_value) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::select(Rank, dplyr::any_of(c("Plate ID", "Row", "Column", "Well Tags",
                                        "Ch1", "Ch2", "ratio", "d",
                                        "Penalised_value", "Penalised_value_raw",
                                        "Classical_Hit", "Penalised_Hit", "Hit_Class")))
}

#' Headline numbers for the Hit candidates tab
summarise_hits <- function(df_normw) {
  d <- df_normw %>%
    dplyr::filter(is.na(Masked), `Well Type` == "Compound") %>%
    classify_hits()
  n_cls <- sum(d$Hit_Class %in% c("Confirmed hit", "Deprioritised (likely artefact)"))
  n_dep <- sum(d$Hit_Class == "Deprioritised (likely artefact)")
  list(
    n_compounds     = nrow(d),
    n_classical     = n_cls,
    n_penalised     = sum(d$Hit_Class %in% c("Confirmed hit", "Penalised-only hit")),
    n_deprioritised = n_dep,
    pct_removed     = if (n_cls > 0) round(100 * n_dep / n_cls, 1) else 0
  )
}

#' Number of compound hits as the hit threshold is varied
threshold_sensitivity <- function(df_normw, col_to_penalise, thresholds = seq(-100, 0, by = 5)) {
  d <- df_normw %>% dplyr::filter(is.na(Masked), `Well Type` == "Compound")
  n <- nrow(d)
  pen <- d$Penalised_value
  cls <- d[[col_to_penalise]]
  data.frame(
    threshold = rep(thresholds, 2),
    Method    = rep(c("Penalised", "Non-penalised"), each = length(thresholds)),
    n_hits    = c(vapply(thresholds, function(t) sum(pen < t, na.rm = TRUE), numeric(1)),
                  vapply(thresholds, function(t) sum(cls < t, na.rm = TRUE), numeric(1)))
  ) %>%
    dplyr::mutate(hit_rate_pct = if (n > 0) 100 * n_hits / n else NA_real_)
}

#' Compound hit rate (after penalty) per plate
plate_hit_rates <- function(df_normw) {
  df_normw %>%
    dplyr::filter(is.na(Masked), `Well Type` == "Compound") %>%
    dplyr::group_by(`Plate ID`) %>%
    dplyr::summarise(
      n_compounds = dplyr::n(),
      n_hits = sum(Penalised_Hit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(hit_rate_pct = 100 * n_hits / n_compounds)
}
