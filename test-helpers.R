# Run from the project root with: testthat::test_dir(".", filter = "^test-")
library(testthat)
library(tidyverse)
root <- "."
source(file.path(root, "hit_helpers.R"))
source(file.path(root, "k_suggest.R"))
source(file.path(root, "qc_helpers.R"))
source(file.path(root, "excel_report.R"))

hit_df <- tibble::tibble(
  `Plate ID` = 1L, Row = 1:4, Column = 1L, `Well Type` = "Compound", Masked = NA,
  `Well Tags` = "Compound",
  Penalised_value = c(-80, -10, -40, 5), ratio = c(-80, -60, -40, 5), d = c(0, 10, 0, 0),
  Penalised_Hit = c(TRUE, FALSE, TRUE, FALSE), Classical_Hit = c(TRUE, TRUE, TRUE, FALSE)
)

test_that("classify_hits labels wells correctly", {
  expect_equal(classify_hits(hit_df)$Hit_Class,
               c("Confirmed hit", "Deprioritised (likely artefact)", "Confirmed hit", "Inactive"))
})

test_that("summarise_hits counts correctly", {
  s <- summarise_hits(hit_df)
  expect_equal(s$n_compounds, 4)
  expect_equal(s$n_classical, 3)
  expect_equal(s$n_penalised, 2)
  expect_equal(s$n_deprioritised, 1)
  expect_equal(s$pct_removed, 33.3)
})

test_that("threshold_sensitivity counts hits per threshold", {
  sens <- threshold_sensitivity(hit_df, "ratio", thresholds = c(-50, 0))
  expect_equal(sens$n_hits, c(1, 3, 2, 3))
})

test_that("plate_hit_rates gives per-plate rate", {
  r <- plate_hit_rates(hit_df)
  expect_equal(r$hit_rate_pct, 50)
})

test_that("suggest_k finds lowest k removing the artefact and keeping the tool compound", {
  x <- tibble::tibble(
    Masked = NA, `Well Type` = "Compound", `Well Tags` = c("tool", "quench"),
    ratio = c(-60, -60), d = c(0, 10)
  )
  r <- suggest_k(x, "ratio", -30, offset = 0, power = 2,
                 keep_tags = "tool", remove_tags = "quench")
  expect_equal(r$k, 0.5)
})

test_that("edge_effect_scores flags an edge effect", {
  val <- rep(10, 16)
  d <- tibble::tibble(
    `Plate ID` = 1L, Row = rep(1:4, each = 4), Column = rep(1:4, 4),
    `Well Type` = "Compound", Masked = NA
  )
  interior <- d$Row %in% 2:3 & d$Column %in% 2:3
  val[interior] <- c(0, 1, 0, 1)
  d$Ch1 <- val
  s <- edge_effect_scores(d, "Ch1", 0.5)
  expect_equal(s$n_edge, 12)
  expect_equal(s$Edge_Flag, "Review")
})

test_that("run_health_messages mentions excluded plates", {
  q <- tibble::tibble(feature = "Ch1", `Plate ID` = 1L, Zf = 0.8)
  e <- tibble::tibble(`Plate ID` = 1L, Edge_Flag = "OK")
  hs <- list(n_compounds = 100, n_penalised = 1, pct_removed = 0)
  msgs <- run_health_messages(q, e, hs, excluded = c("5", "6"))
  expect_true(any(grepl("2 plate\\(s\\) manually excluded", msgs)))
})

test_that("the default Z' cutoff is shared by the QC plot and run health", {
  expect_equal(ZF_DEFAULT_CUTOFF, 0.4)
  q <- tibble::tibble(feature = "Ch1", `Plate ID` = c(1L, 2L), Zf = c(0.3, 0.9))
  e <- tibble::tibble(`Plate ID` = c(1L, 2L), Edge_Flag = c("OK", "OK"))
  hs <- list(n_compounds = 100, n_penalised = 1, pct_removed = 0)
  msgs <- run_health_messages(q, e, hs)
  expect_true(any(grepl("1 plate\\(s\\) with Z' below 0.4", msgs)))
})

test_that("classify_hits treats missing hit flags as FALSE", {
  d <- tibble::tibble(
    `Plate ID` = 1L, Row = 1L, Column = 1L, `Well Type` = "Compound", Masked = NA,
    Penalised_Hit = NA, Classical_Hit = TRUE
  )
  expect_equal(classify_hits(d)$Hit_Class, "Deprioritised (likely artefact)")
})

test_that("get_hit_candidates excludes masked wells and non-compounds", {
  d <- tibble::tibble(
    `Plate ID` = 1L, Row = 1:3, Column = 1L,
    `Well Type` = c("Compound", "Compound", "Neutral Control"),
    Masked = c(NA, "x", NA),
    Penalised_value = c(-50, -50, -50), Classical_Hit = TRUE, Penalised_Hit = TRUE
  )
  out <- get_hit_candidates(d)
  expect_equal(nrow(out), 1)
  expect_equal(out$Rank, 1L)
})

test_that("build_excel_report writes a workbook with one sheet per table", {
  skip_if_not_installed("openxlsx")
  path <- tempfile(fileext = ".xlsx")
  build_excel_report(path, list(Summary = data.frame(a = 1:2), `Hit candidates` = data.frame(b = "x")))
  expect_true(file.exists(path))
  expect_equal(openxlsx::getSheetNames(path), c("Summary", "Hit candidates"))
})

test_that("threshold_sensitivity reports hit rate as a percentage of compounds", {
  sens <- threshold_sensitivity(hit_df, "ratio", thresholds = c(-50))
  expect_equal(sens$hit_rate_pct, c(25, 75))
})

test_that("suggest_k uses the plateau heuristic when no wells are labelled", {
  x <- tibble::tibble(
    Masked = NA, `Well Type` = "Compound", `Well Tags` = "Compound",
    ratio = c(-60, -40, -20, 5), d = c(0, 5, 10, 15)
  )
  r <- suggest_k(x, "ratio", -30)
  expect_true(grepl("Plateau", r$method))
  expect_true(is.data.frame(r$table))
})

test_that("calc_point_line2 gives the perpendicular distance to a line", {
  source(file.path(root, "calc_distances.R"))
  expect_equal(calc_point_line2(c(0, 0), c(1, 0), 0.5, 2), 2)
})

test_that("calc_point_line2_dt matches the scalar version", {
  source(file.path(root, "calc_distances.R"))
  dt <- data.frame(px = c(0.5, 3), py = c(2, -1))
  expect_equal(calc_point_line2_dt(c(0, 0), c(1, 0), dt, "px", "py"), c(2, 1))
})

test_that("app is split into UI and server files", {
  expect_true(file.exists(file.path(root, "ui.R")))
  expect_true(file.exists(file.path(root, "server.R")))
  for (f in c("core.R", "results.R", "qc.R", "session.R")) {
    expect_true(file.exists(file.path(root, f)))
  }
})

test_that("every split R file parses", {
  files <- list.files(root, pattern = "\\.R$", full.names = TRUE)
  for (f in c(files, file.path(root, "app.R"))) {
    expect_no_error(parse(file = f))
  }
})
