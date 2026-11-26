#' Suggest a penalty strength (k)
#'
#' Scans a grid of k values and reports, for each one, how many compound hits remain,
#' how many labelled tool compounds are retained and how many labelled artefacts are removed.
#' With labelled wells (Well Tags) the suggestion is the lowest k meeting both targets.
#' Without labels a plateau heuristic is used: the lowest k > 0 beyond which a further step
#' removes less than 1% of the non-penalised hits.
suggest_k <- function(df_normw, col_to_penalise, hit_threshold,
                      offset = 0, power = 2, zero_limit = FALSE,
                      keep_tags = NULL, remove_tags = NULL,
                      min_keep_pct = 100, min_remove_pct = 100,
                      k_grid = c(0, 0.125, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 6, 8, 10)) {
  x <- df_normw[is.na(df_normw$Masked), , drop = FALSE]
  base <- x[[col_to_penalise]]
  excess <- pmax(x$d - offset, 0)^power
  tags <- as.character(x$`Well Tags`)
  is_comp <- x$`Well Type` == "Compound"
  limit_zero <- identical(as.character(zero_limit), "TRUE")
  
  hit_at <- function(k) {
    v <- base + k * excess
    if (limit_zero) v[!is.na(v) & v > 0] <- 0
    v < hit_threshold
  }
  hit_pct <- function(h, idx) if (any(idx)) 100 * mean(h[idx], na.rm = TRUE) else NA_real_
  
  res <- do.call(rbind, lapply(k_grid, function(k) {
    h <- hit_at(k)
    artefact_hit_pct <- hit_pct(h, tags %in% remove_tags)
    data.frame(
      k = k,
      compound_hits = sum(h[is_comp], na.rm = TRUE),
      keep_retained_pct = hit_pct(h, tags %in% keep_tags),
      artefact_removed_pct = if (is.na(artefact_hit_pct)) NA_real_ else 100 - artefact_hit_pct
    )
  }))
  res$compound_hits_retained_pct <- 100 * res$compound_hits / max(res$compound_hits[1], 1)
  
  if (length(keep_tags) > 0 || length(remove_tags) > 0) {
    ok <- rep(TRUE, nrow(res))
    if (length(keep_tags) > 0) ok <- ok & (res$keep_retained_pct >= min_keep_pct)
    if (length(remove_tags) > 0) ok <- ok & (res$artefact_removed_pct >= min_remove_pct)
    ok[is.na(ok)] <- FALSE
    k_sug <- if (any(ok)) res$k[which(ok)[1]] else NA_real_
    method <- "Based on the labelled tool compounds / artefacts you selected"
  } else {
    n <- nrow(res)
    r <- res$compound_hits_retained_pct / 100
    step_loss <- c(r[-n] - r[-1], NA)
    cand <- which(res$k > 0 & !is.na(step_loss) & step_loss < 0.01)
    k_sug <- if (length(cand) > 0) res$k[cand[1]] else NA_real_
    method <- "Plateau heuristic (no labelled wells selected) - treat as a starting point only"
  }
  list(table = res, k = k_sug, method = method)
}
