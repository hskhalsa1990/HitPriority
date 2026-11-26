ui <- fluidPage(
  withMathJax(), # Include MathJax
  
  # Application title
  tags$head(tags$style(HTML(dark_css)), tags$script(HTML(dark_js))),
  titlePanel("Hit Priority App"),
  checkboxInput("dark_mode", "ਗੂੜ੍ਹਾ ਮੋਡ", value = FALSE),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout( 
    sidebarPanel(width=4,
                 "Plate mapping and naming inputs",
                 wellPanel(
                   #Users can load multiple  files,
                   textInput("target", label="Input Target Name", value = "Target_X"),
                   radioButtons("mapped_input",
                                label =tags$span("ਪਹਿਲਾਂ ਤੋਂ ਮੈਪ ਕੀਤੀ ਫਾਈਲ?", bsButton("mapped_input_i", label = "", icon = icon("info"), style = "info", size = "extra-small")),
                                c(TRUE, FALSE),
                                selected = FALSE,
                                inline = TRUE),
                   bsPopover(
                     id = "mapped_input_i",
                     title = "Using a pre-mapped file",
                     paste0("The Default (FALSE) assumes standard HTS ",
                            a("platemap ", href = "PlateMaps.html", target = "_blank"),
                            " and reads acquired as matrices separated by one empty line in csv files without headers",
                            a("(example)", href = "1114695724.csv", target = "_blank"),
                            ". If your data is not organised like this or if you would like to add custom Well Tags to colour points by in the penalty strength plots, you can supply a premapped file of Ch1 and Ch2 readings.",
                            "The file should be a .csv file organised according to the following ",
                            a("schema. ", href = "premapped.html", target = "_blank"),
                            "Example file can be found ",
                            a("here.", href = "Premapped_Example.csv", target = "_blank")
                     ),
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
                   ),
                   radioButtons("plate_format", 
                                label= "Select Plate format",
                                c("384","1536"),
                                selected = "1536",
                                inline = TRUE),
                   selectInput(
                     inputId="stage",
                     label = tags$span("ਸਕ੍ਰੀਨਿੰਗ ਪੜਾਅ ਚੁਣੋ", bsButton("stage_i", label = "", icon = icon("info"), style = "info", size = "extra-small")),
                     c("Validation", "Primary", "Confirmation", "XC50_Round1", "XC50_Round2"),
                     selected = "Primary"
                   ),
                   bsPopover(
                     id = "stage_i",
                     title = "Stage Information",
                     content = paste0(
                       "The `Stage` alongside the `Plate format` (1536 or 384) defines the default map used. If you are using a `Pre-mapped file`, the Stage only serves as an additional label to the experiment title."
                       
                     ),
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
                   ),
                   textInput("run", label="Input Run Name", value = "01"),
                   radioButtons("channel_flip",
                                label =  tags$span("Is the Donor Channel the first matrix in the csv?", bsButton("channel_flip_i", label = "", icon = icon("info"), style = "info", size = "extra-small")),
                                c("Donor First"="TRUE", "Acceptor First"="FALSE"),
                                selected = "FALSE",
                                inline = TRUE),
                   bsPopover(
                     id = "channel_flip_i",
                     title = "Channel Order in Matrix",
                     content = paste0(
                       "Appliclable when using the standard HTS maps and FRET reads acquired as matrix. The default setting (`Acceptor First`) assumes that the first matrix in the csv files is the Acceptor Channel, and the second is the Donor Channel. If the channels are flipped and you have acquired the Donor first - select `Donor First` "
                     ),
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
                   ),
                 ),
                 "App settings",
                 wellPanel(
                   #fileInput("barcodes", "Upload Plate barcodes"),
                   radioButtons("assay_direction",
                                label = "Choose Positive Control Direction",
                                c("Inhibitor", "Activator"),
                                selected = "Inhibitor",
                                inline = TRUE),
                   selectInput("Ch1_Norm",
                               label =tags$span("Ch1 (X-axis) Normalisation", bsButton("Ch1_Norm_i", label = "", icon = icon("info"), style = "info", size = "extra-small")),
                               c("percent- Compound Centre, Positive Control Scale","percent- Neutral Centre, Positive Control Scale","RZscore- Compound Centre and Scale",
                                 "RZscore- Positive Control Centre and Scale","RZscore- Neutral Centre and Scale", "percent- Neutral Centre and Scale"#,
                                 #"Other"
                               )
                               ,selected="percent- Compound Centre, Positive Control Scale"),
                   bsPopover(
                     id = "Ch1_Norm_i",
                     title = "Normalisation Information",
                     content = paste0(
                       "Normalisation calculation for Ch1 and Ch2. The ratio normalisation strategy matches the one selected for Ch1. Use the `QC check` and `Summary Stats` tabs to guide your selection. For more information on mathematical background see: ",
                       a("Normalisation equations", href = "normalisation.html", target = "_blank")
                     ),
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
                   ),
                   selectInput("Ch2_Norm", "Ch2 (Y-axis) Normalisation",
                               c("RZscore- Compound Centre and Scale",
                                 "RZscore- Positive Control Centre and Scale",
                                 "RZscore- Neutral Centre and Scale",
                                 "percent- Neutral Centre and Scale"#,
                                 #"percent- Compound Centre, Positive Control Scale",
                                 #"percent- Neutral Centre, Positive Control Scale",
                                 #"Other"
                               )
                               ,selected="RZscore- Positive Control Centre and Scale"),
                   radioButtons("scale_scope",
                                label = tags$span("Choose Scaling Scope", bsButton("scale_scope_i", label = "", icon = icon("info"), style = "info", size = "extra-small")),
                                c("Platewise", "Runwise"),
                                selected = "Platewise",
                                inline = TRUE),
                   bsPopover(
                     id = "scale_scope_i",
                     title = "Scaling Scope",
                     content = paste0(
                       "Platewise uses the medians and MAD of each plate to normalise values within the plate. Runwise uses the median and MAD of the entire run (all files) to normalise values across the whole run."
                     ),
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
                   ),
                   radioButtons("col_to_penalise",
                                label = tags$span("ਪੈਨਲਟੀ ਲਈ ਮਾਪ", bsButton("col_to_penalise_i", label = "", icon = icon("info"), style = "info", size = "extra-small")),
                                c("ratio", "Ch1"),
                                selected = "ratio",
                                inline = TRUE),
                   bsPopover(
                     id = "col_to_penalise_i",
                     title = "ਪੈਨਲਟੀ ਲਈ ਮਾਪ",
                     content = paste0(
                       "Should the penalty be applied to Ch1 or the ratio of Ch1/Ch2"
                     ),
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
                   ),
                   numericInput("hit_threshold",
                                label =  tags$span("ਹਿੱਟ ਥ੍ਰੈਸ਼ਹੋਲਡ", bsButton("hit_threshold_i", label = "", icon = icon("info"), style = "info", size = "extra-small")),
                                min = -100, max = 100, value = -30,step = 10
                   ),
                   bsPopover(
                     id = "hit_threshold_i",
                     title = "Hit Threshold",
                     content = paste0(
                       "Actives are defined as compounds with penalised value lower than this. Use this to visually inspect the effect of penalty strength. Has no effect on exported data"
                     ),
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
                   ),
                   uiOutput("sidebar_equation"), 
                   numericInput("k", label =  tags$span("ਪੈਨਲਟੀ ਦੀ ਤਾਕਤ", bsButton("k_i", label = "", icon = icon("info"), style = "info", size = "extra-small")),
                                value = 1,
                                min = 0,
                                max = 10,
                                step = 0.125
                                
                                
                   ),
                   bsPopover(
                     id = "k_i",
                     title = "Strength of penalty (k)",
                     content = paste0(
                       "The strength of the penalty can be controlled by the parameter `k` (see equation above and `Penalty strength plots` tab). If labelled examples for tool compounds or quenchers are available, choose the lowest value for `k` which preserves all known tool compounds and eliminates apparent fluorescent compounds and known quenchers."
                     ),
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
                   ),
                   checkboxInput("advanced", label =  tags$span("ਐਡਵਾਂਸਡ ਸੈਟਿੰਗਾਂ ਦਿਖਾਓ", bsButton("advanced_i", label = "", icon = icon("info"), style = "info", size = "extra-small")), value = FALSE),
                   bsPopover(
                     id = "advanced_i",
                     title = "Advanced Settings",
                     content = paste0(
                       "Explore more advanced strategies of controlling for artefacts. Only use these if changing the strength of penalty does not achieve appropriate deprioritisation of artefacts."
                     ),
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
                   ),
                   uiOutput("advanced_ui"), # Placeholder for advanced inputs
                 ),
                 "Input Files",
                 wellPanel(style = "background-color: hsla(0, 0%, 90%, 0.5);  /* Grey with 90% transparency */",
                           fileInput("csvs", "Select csv files",multiple=T),
                           actionButton("load_example", "ਉਦਾਹਰਨ ਡਾਟਾ ਲੋਡ ਕਰੋ"),
                           tags$hr(),
                           selectizeInput("excluded_plates", "Exclude plates (Plate IDs)", choices = NULL, multiple = TRUE,
                                          options = list(create = TRUE, placeholder = "type or pick Plate IDs")),
                           actionButton("exclude_flagged", "Z' ਵਿੱਚ ਫੇਲ੍ਹ ਪਲੇਟਾਂ ਬਾਹਰ ਕੱਢੋ (Ch1)")
                 ),
                 "Reproducibility",
                 wellPanel(
                   downloadButton("save_settings", "ਸੈਟਿੰਗਾਂ ਸੇਵ ਕਰੋ (JSON)"),
                   fileInput("load_settings", "Load settings (JSON)", accept = ".json"),
                   actionButton("reset_defaults", "ਡਿਫਾਲਟ ਤੇ ਰੀਸੈੱਟ ਕਰੋ")
                 ),
                 "Export",
                 radioButtons("Export_files",
                              label = "Are you ready to export files?",
                              c("Yes"="TRUE", "No, working on it"="FALSE"),
                              selected = "FALSE",
                              inline = TRUE),
                 downloadButton("downloadZip", "ZIP ਡਾਊਨਲੋਡ"),
                 downloadButton("download_xlsx", "Excel ਰਿਪੋਰਟ ਡਾਊਨਲੋਡ")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      withMathJax(),
      tabsetPanel(
        tabPanel("ਮੌਜੂਦਾ ਨਕਸ਼ਾ",
                 plotOutput("plate_map_plot")),
        tabPanel("ਇਨਪੁੱਟ CSV",
                 DT::DTOutput('tbl')),
        tabPanel("ਪੈਨਲਟੀ ਆਉਟਪੁੱਟ",
                 DT::DTOutput('corr_tbl')),
        tabPanel("ਪੈਨਲਟੀ ਆਉਟਪੁੱਟ (ਸਾਰੇ)",
                 DT::DTOutput('corr_tbl_all')),
        tabPanel("ਸਾਰ ਅੰਕੜੇ",
                 plotly::plotlyOutput("norm_constants_plotly"),
                 DT::DTOutput('sum_stats_plates'),
                 DT::DTOutput('sum_stats_run')),
        tabPanel("QC ਜਾਂਚ",
                 DTOutput('hit_tbl'),
                 DTOutput('hit_tbl_tags')),
        tabPanel("ਹਿੱਟ ਉਮੀਦਵਾਰ",
                 uiOutput("run_health_ui"),
                 uiOutput("hit_summary_ui"),
                 downloadButton("download_hits", "ਹਿੱਟ ਸੂਚੀ ਡਾਊਨਲੋਡ ਕਰੋ (CSV)"),
                 checkboxGroupInput("hit_class_filter", "Show in table",
                                    choices = c("Confirmed hit", "Deprioritised (likely artefact)", "Penalised-only hit"),
                                    selected = c("Confirmed hit", "Deprioritised (likely artefact)", "Penalised-only hit"),
                                    inline = TRUE),
                 DT::DTOutput("hit_candidates_tbl"),
                 plotly::plotlyOutput("threshold_sens_plot")),
        tabPanel("ਪਲੇਟ QC",
                 fluidRow(
                   column(4, selectInput("qc_metric", "Metric", c("Zf", "rZf", "rSB", "SB"), selected = "Zf")),
                   column(4, selectInput("qc_feature", "Channel", c("Ch1", "Ch2", "ratio"), selected = "Ch1")),
                   column(4, numericInput("qc_cutoff", "Flag plates below", value = ZF_DEFAULT_CUTOFF, step = 0.1))
                 ),
                 plotly::plotlyOutput("plate_qc_plot"),
                 plotly::plotlyOutput("plate_hit_rate_plot"),
                 plotly::plotlyOutput("control_trend_plot"),
                 tags$hr(),
                 h4("ਕਿਨਾਰੇ ਪ੍ਰਭਾਵ ਦੀ ਜਾਂਚ"),
                 numericInput("edge_threshold", "Flag plates with |edge score| above (MAD units)", value = 0.5, min = 0, step = 0.1),
                 DT::DTOutput("edge_tbl"),
                 tags$hr(),
                 uiOutput("heatmap_controls"),
                 plotOutput("plate_heatmap", height = "450px")),
        tabPanel("k ਸੁਝਾਅ",
                 p("Pick Well Tags for tool compounds that must stay active and for artefacts / quenchers that must be removed. The app suggests the lowest k that meets both targets. Without labels a plateau heuristic is used."),
                 fluidRow(
                   column(6, selectizeInput("k_keep_tags", "Tool compounds to retain", choices = NULL, multiple = TRUE)),
                   column(6, selectizeInput("k_remove_tags", "Artefacts / quenchers to remove", choices = NULL, multiple = TRUE))
                 ),
                 fluidRow(
                   column(6, numericInput("k_min_keep", "Min % tool compounds retained", value = 100, min = 0, max = 100, step = 5)),
                   column(6, numericInput("k_min_remove", "Min % artefacts removed", value = 100, min = 0, max = 100, step = 5))
                 ),
                 uiOutput("k_suggestion_ui"),
                 actionButton("apply_k", "ਸੁਝਾਇਆ k ਲਾਗੂ ਕਰੋ"),
                 plotly::plotlyOutput("k_suggest_plot"),
                 DT::DTOutput("k_suggest_tbl")),
        tabPanel("ਪੈਨਲਟੀ ਤਾਕਤ ਚਾਰਟ",
                 uiOutput("equation"), # Use uiOutput to render the equation,
                 plotOutput("d_dens"),
                 checkboxInput("zoom", "ਜ਼ੂਮ", value = FALSE, width = NULL),
                 plotly::plotlyOutput("k_plot"),
                 plotly::plotlyOutput("k2_plot"),
                 plotly::plotlyOutput("k_plot_raw")),
        tabPanel("ਨੌਰਮ ਚਾਰਟ",
                 plotOutput("norm_plot"),
                 plotly::plotlyOutput("norm_plotly"),
                 textOutput("done_out")),
        tabPanel("ਕੱਚੇ ਚਾਰਟ",
                 plotly::plotlyOutput("raw_ratio_plotly"),
                 plotly::plotlyOutput("raw_penalised_plotly")),
        visuals_tab,
        tabPanel("ਮਦਦ",
                 tags$h4("ਚੀਜ਼ਾਂ ਕਿੱਥੇ ਲੱਭਣੀਆਂ ਹਨ"),
                 tags$ul(
                   tags$li(tags$b("Hit candidates"), " - run health warnings, headline numbers, ranked hit list (filterable), threshold sensitivity."),
                   tags$li(tags$b("Plate QC"), " - Z'/S:B per plate, hit rate per plate, control drift, edge-effect check and per-plate heatmaps."),
                   tags$li(tags$b("k suggestion"), " - suggests the lowest penalty strength that keeps tool compounds and removes artefacts (select them by Well Tags)."),
                   tags$li(tags$b("Save / Load settings"), " (sidebar) - reproduce a run; export also writes a settings.json."),
                   tags$li(tags$b("Download Excel report"), " - all key tables in one workbook (needs the openxlsx package)."),
                   tags$li(tags$b("ਗੂੜ੍ਹਾ ਮੋਡ"), " - checkbox below the title."),
                   tags$li(tags$b("ਉਦਾਹਰਨ ਡਾਟਾ ਲੋਡ ਕਰੋ"), " - loads a bundled single-plate 1536 example so you can explore every tab."),
                   tags$li(tags$b("ਡਿਫਾਲਟ ਤੇ ਰੀਸੈੱਟ ਕਰੋ"), " - restores all settings.")
                 )),
        tabPanel("ਡਾਇਗਨੌਸਟਿਕਸ",
                 textOutput("exp_out"),
                 textOutput("csvs_out"),
                 textOutput("barcodes_out"),
                 textOutput("Ch1_Norm_out"),
                 textOutput("Ch2_Norm_out"),
                 textOutput("scale_scope_out"),
                 textOutput("channel_flip_out"),
                 textOutput("hit_threshold_out"),
                 textOutput("k_out"),
                 textOutput("Export_files_out"),
                 verbatimTextOutput("sessionInfo"),
                 actionButton("reset", "ਸਾਰੀਆਂ ਫਾਈਲਾਂ ਹਟਾਓ"))
        
      )
    )
  )
)
# Define server logic 
