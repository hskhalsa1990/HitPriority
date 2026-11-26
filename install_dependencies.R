# Installs the R packages HitPriority needs (run once).
pkgs <- c("shiny", "shinyBS", "shinyWidgets", "rlang", "data.table", "tidyverse",
          "readxl", "LearnGeom", "scales", "DT", "patchwork", "plotly", "knitr",
          "kableExtra", "here", "rmarkdown", "zip", "jsonlite", "openxlsx", "testthat")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)
message("All dependencies installed.")
