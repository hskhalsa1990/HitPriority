#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(shinyWidgets)
library(rlang)
library(data.table)
library(tidyverse)
library(readxl)
library(LearnGeom)
library(scales)
library(DT)
library(patchwork)
library(plotly)
library(knitr)
library(kableExtra)




# Setting paths:

main_dir<-here::here()

# Output location: set HITPRIORITY_OUT to override. Falls back to the original
# shared scratch path if it exists, otherwise to a temporary folder.
default_out <- "/wscratch/os_images/FRETCorr"
out_path <- Sys.getenv(
  "HITPRIORITY_OUT",
  unset = if (dir.exists(dirname(default_out))) default_out else file.path(tempdir(), "HitPriority_out")
)
# Path for the pre-made ZIP file
zip_file_path <- file.path(out_path, "SessionZip.zip")

if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)




ctrl_cols <- c("Compound"="#66C2A5",
               "Neutral Control"="#E69F00",
               "Positive Control"="#8DA0CB"
)



set2_c <- c(
  "#66C2A5", "#E69F00", "#8DA0CB", "#E78AC3", "#FC8D62",
  "#E5C494", "#A6D854", "#999999", "#000000", 
  # Additional distinct colors
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", 
  "#E6AB02", "#A6761D", "#666666", "#B3E2CD", "#FDCDAC",
  "#CBD5E8", "#F4CAE4", "#F6E8C3", "#FFF2AE", "#F1E2CC",
  "#B3B3B3", "#B2182B", "#2166AC", "#8856A7", "#006D2C",
  "#FEB24C"
)


source(file.path(main_dir, "calc_distances.R"))
source(file.path(main_dir, "process_csv_matrices.R"))
source(file.path(main_dir, "DT_format_sci.R"))
source(file.path(main_dir, "calc_norm_rmsd.R"))
source(file.path(main_dir, "plot_plate.R"))
source(file.path(main_dir, "system_helpers.R"))
source(file.path(main_dir, "hit_helpers.R"))
source(file.path(main_dir, "qc_helpers.R"))
source(file.path(main_dir, "k_suggest.R"))
source(file.path(main_dir, "excel_report.R"))
source(file.path(main_dir, "viz_helpers.R"))
source(file.path(main_dir, "visuals_tab.R"), local = TRUE)




dark_css <- "
body.dark-mode { background:#1b1d21; color:#e4e6ea; }
body.dark-mode a { color:#8ec5ff; }
body.dark-mode .well { background:#25282d; border-color:#3a3f46; }
body.dark-mode .form-control, body.dark-mode .selectize-input, body.dark-mode .selectize-dropdown,
body.dark-mode .selectize-input.full { background:#2f343a; color:#e4e6ea; border-color:#4a5058; }
body.dark-mode .selectize-dropdown .active { background:#3d444c; color:#fff; }
body.dark-mode .nav-tabs { border-bottom-color:#3a3f46; }
body.dark-mode .nav-tabs > li > a { color:#c9ced6; }
body.dark-mode .nav-tabs > li.active > a, body.dark-mode .nav-tabs > li.active > a:hover,
body.dark-mode .nav-tabs > li.active > a:focus { background:#25282d; color:#fff; border-color:#3a3f46; }
body.dark-mode .nav-tabs > li > a:hover { background:#2f343a; border-color:#3a3f46; }
body.dark-mode .popover { background:#2f343a; color:#e4e6ea; }
body.dark-mode .popover-title { background:#3a3f46; color:#fff; }
body.dark-mode table.dataTable, body.dark-mode table.dataTable tbody tr,
body.dark-mode table.dataTable tbody td { background-color:#25282d !important; color:#e4e6ea; }
body.dark-mode table.dataTable tbody tr:nth-child(even) td { background-color:#2b2f35 !important; }
body.dark-mode table.dataTable thead th, body.dark-mode table.dataTable thead td { border-color:#3a3f46; color:#e4e6ea; }
body.dark-mode .dataTables_wrapper, body.dark-mode .dataTables_info,
body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button { color:#e4e6ea !important; }
body.dark-mode .dataTables_wrapper input, body.dark-mode .dataTables_wrapper select { background:#2f343a; color:#e4e6ea; }
body.dark-mode .btn-default { background:#3a3f46; color:#e4e6ea; border-color:#4a5058; }
"

dark_js <- "
Shiny.addCustomMessageHandler('toggleDark', function(on) {
  document.body.classList.toggle('dark-mode', on);
  try { localStorage.setItem('hp_dark', on ? '1' : '0'); } catch (e) {}
});
$(document).on('shiny:connected', function() {
  var d = null;
  try { d = localStorage.getItem('hp_dark'); } catch (e) {}
  if (d === '1') { $('#dark_mode').prop('checked', true).trigger('change'); }
});
"

# Define UI for application
# UI and server are split into R/ui and R/server (sourced into this environment)
source(file.path(main_dir, "ui.R"), local = TRUE)
source(file.path(main_dir, "server.R"), local = TRUE)






# Run the application 
shinyApp(ui = ui, server = server)


