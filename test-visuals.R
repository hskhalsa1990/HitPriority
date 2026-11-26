root <- "."
source(file.path(root, "hit_helpers.R"))
source(file.path(root, "viz_helpers.R"))

test_that("simulate_hit_noise is reproducible and within 0-100%", {
  v <- c(-80, -60, -20, 0, 10)
  a <- simulate_hit_noise(v, -50, noise_sd = 5, n_iter = 50, seed = 7)
  b <- simulate_hit_noise(v, -50, noise_sd = 5, n_iter = 50, seed = 7)
  expect_equal(a, b)
  expect_true(all(a$hit_rate_pct >= 0 & a$hit_rate_pct <= 100))
  expect_equal(nrow(a), 50)
})

test_that("zero noise gives the exact hit rate", {
  sim <- simulate_hit_noise(c(-80, -60, -20, 0), -50, noise_sd = 0, n_iter = 3)
  expect_equal(sim$hit_rate_pct, c(50, 50, 50))
})

test_that("flow field is zero at the walls and maximal at the centre", {
  g <- flow_field_grid(nx = 3, ny = 3, u_max = 1)
  expect_equal(g$u[g$y == 0], c(0, 0, 0))
  expect_equal(max(g$u), 1)
})

test_that("plate_matrix places values at the right Row/Column", {
  d <- tibble::tibble(`Plate ID` = 1L, Row = c(1L, 2L), Column = c(2L, 1L),
                      `Well Type` = "Compound", Masked = NA, Penalised_value = c(5, 7))
  m <- plate_matrix(d, 1)
  expect_equal(m["1", "2"], 5)
  expect_equal(m["2", "1"], 7)
})

test_that("comparison_table has one row per measure", {
  d <- tibble::tibble(`Plate ID` = 1L, Row = 1:2, Column = 1L, `Well Type` = "Compound",
                      Masked = NA, Penalised_Hit = c(TRUE, FALSE), Classical_Hit = c(TRUE, TRUE))
  ct <- comparison_table(d, "ratio")
  expect_equal(nrow(ct), 5)
})

test_that("pipeline_steps lists six numbered steps in order", {
  p <- pipeline_steps()
  expect_equal(nrow(p), 6)
  expect_equal(p$step, 1:6)
})

test_that("well_type_distribution drops masked and non-finite values", {
  d <- tibble::tibble(`Well Type` = c("Compound", "Compound", "Neutral Control"),
                      Masked = c(NA, "x", NA), Penalised_value = c(1, 2, NA))
  w <- well_type_distribution(d)
  expect_equal(nrow(w), 1)
  expect_equal(w$value, 1)
})

test_that("all_plates_grid keeps plate labels as text", {
  d <- tibble::tibble(`Plate ID` = c(1L, 2L), Row = 1L, Column = 1L, Masked = NA, Penalised_value = c(3, 4))
  g <- all_plates_grid(d)
  expect_equal(g$plate, c("1", "2"))
})
