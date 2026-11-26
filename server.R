server <- function(input, output, session) {
  source(file.path(main_dir, "core.R"), local = TRUE)
  source(file.path(main_dir, "results.R"), local = TRUE)
  source(file.path(main_dir, "qc.R"), local = TRUE)
  source(file.path(main_dir, "session.R"), local = TRUE)
  source(file.path(main_dir, "visuals.R"), local = TRUE)
}
