# UI for the Visuals tab (inserted into the main tabsetPanel in ui.R)
visuals_tab <- tabPanel(
  "ਵਿਜ਼ੂਅਲ ਅਤੇ ਸਿਮੂਲੇਸ਼ਨ",
  br(),
  fluidRow(
    column(4, selectInput("viz_type", "ਚਾਰਟ ਦੀ ਕਿਸਮ",
      choices = c(
        "ਬਾਰ: ਹਿੱਟ ਕਲਾਸ ਅਨੁਸਾਰ ਗਿਣਤੀ" = "bar_class",
        "ਬਾਰ: ਹਰ ਪਲੇਟ ਦੀ ਹਿੱਟ ਦਰ" = "bar_plate",
        "ਗ੍ਰਾਫ਼: Ch1 ਬਨਾਮ Ch2 (2D)" = "scatter2d",
        "3D ਸਕੈਟਰ: Ch1, Ch2, ਪੈਨਲਟੀ ਮੁੱਲ" = "scatter3d",
        "3D ਸਤਹ: ਇੱਕ ਪਲੇਟ ਦਾ ਨਕਸ਼ਾ" = "surface3d",
        "ਸਿਮੂਲੇਸ਼ਨ: ਸ਼ੋਰ ਹੇਠ ਹਿੱਟ ਦਰ" = "simulation",
        "ਬਾਕਸ ਪਲਾਟ: ਕਿਸਮ ਅਨੁਸਾਰ ਵੰਡ" = "box",
        "ਹੀਟਮੈਪ: ਸਾਰੀਆਂ ਪਲੇਟਾਂ" = "heat_all",
        "ਪ੍ਰਕਿਰਿਆ ਡਾਇਗ੍ਰਾਮ: ਵਿਸ਼ਲੇਸ਼ਣ ਦੇ ਕਦਮ" = "pipeline",
        "CFD-ਸ਼ੈਲੀ: ਚੈਨਲ ਵਹਾਅ ਖੇਤਰ (ਉਦਾਹਰਨ)" = "flow"
      ))),
    column(4, numericInput("viz_noise", "ਸਿਮੂਲੇਸ਼ਨ ਲਈ ਸ਼ੋਰ (sd)", value = 5, min = 0, step = 1)),
    column(4, numericInput("viz_iter", "ਸਿਮੂਲੇਸ਼ਨ ਦੁਹਰਾਅ", value = 500, min = 50, step = 50))
  ),
  plotlyOutput("viz_plot", height = "520px"),
  tags$hr(),
  tags$h4("ਤੁਲਨਾ ਸਾਰਣੀ"),
  DT::DTOutput("viz_compare")
)
