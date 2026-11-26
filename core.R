  
  # Initialise advanced input values
  advanced_values <- reactiveValues(
    power = 2,
    offset = 0,
    zero_limit = FALSE
  )
  
  
  
  # Render advanced UI conditionally
  output$advanced_ui <- renderUI({
    if (input$advanced) {
      tagList(
        sliderInput(
          "power",
          label = tags$span(
            "Power degree of penalty:", 
            bsButton("power_i", label = "", icon = icon("info"), style = "info", size = "extra-small")
          ),
          min = 1, max = 2, value = 2, step = 1    # Set default 2 directly (do not use advanced_values)
        ),
        bsPopover(
          id = "power_i",
          title = "d^power",
          content = paste0(
            "Power to raise d (distance from theoretical fit line) by. See equation above. Recommended default value is 2, to preferentially penalise large values of d."
          ),
          placement = "right",
          trigger = "click",
          options = list(container = "body")
        ),
        numericInput(
          inputId = "offset",
          label = tags$span(
            "Minimal distance to penalise:",
            bsButton("offset_i", label = "", icon = icon("info"), style = "info", size = "extra-small")
          ),
          value = 0,
          min = 0,
          max = 100,
          step = 1
        ),
        bsPopover(
          id = "offset_i",
          title = "Threshold for no penalty",
          content = paste0(
            "Optional argument to only penalise values larger than the `Minimal distance to penalise` (offset). For values smaller than the offset no penalty is applied. Values larger than the offset are penalised according to the settings above but with the offset subtracted d_apparent=(d-offset)"
          ),
          placement = "right",
          trigger = "click",
          options = list(container = "body")
        ),
        radioButtons("zero_limit",
                     label = tags$span(
                       "Limit Max Penalised Value to Zero",
                       bsButton("zero_limit_i", label = "", icon = icon("info"), style = "info", size = "extra-small")
                     ),
                     c(TRUE, FALSE),
                     selected = FALSE,
                     inline = TRUE
        ),
        bsPopover(
          id = "zero_limit_i",
          title = "Zero Limit",
          content = paste0(
            "All penalised values > 0  will be set to exactly 0."
          ),
          placement = "right",
          trigger = "click",
          options = list(container = "body")
        )
      )
    }
  })
  
  
  # Update advanced_values when inputs change
  observeEvent(input$power, {
    advanced_values$power <- input$power
  })
  
  observeEvent(input$offset, {
    advanced_values$offset <- input$offset
  })
  
  observeEvent(input$zero_limit, {
    advanced_values$zero_limit <- input$zero_limit
  })
  
  observeEvent(input$reset, {
    unlink(out_path,recursive = T)
    dir.create(out_path, recursive = TRUE)
    chmod_all(out_path)
  })  
  
  
  output$sidebar_equation <- renderUI({
    tagList(
      tags$head(
        tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
      ),
      tags$span(
        tags$span(
          tags$i(class = "fa fa-info-circle"),
          `data-toggle` = "tooltip",
          `data-placement` = "right",
          title = "d- distance of each point from the theoretical line connecting the median of the Neutral and Positive Controls. See magenta line in the `Penalty strength plots` tab",
          
          # Custom styling to match bsButton
          style = "cursor: pointer; color: white; background-color: #5bc0de; border-radius: 4px; padding: 2px 6px; font-size: 12px;"
        ),
        withMathJax(equation())
      ),
      tags$script(HTML("$('[data-toggle=\"tooltip\"]').tooltip();")) # Initialise tooltips
    )
  })
  
  # Attach the popover in the server after the UI is rendered
  observe({
    addPopover(
      session = session,
      id = "d_i",
      title = "d",
      content = paste0(
        "d- distance from theoretical fit line connecting Neutral and Positive Controls"
      ),
      placement = "right",
      trigger = "click",
      options = list(container = "body")
    )
  })
  
  norm_settings_reactive <- reactive({
    Ch2_norm <- case_when(
      input$Ch2_Norm == "percent- Compound Centre, Positive Control Scale" ~ "two_point",
      input$Ch2_Norm == "percent- Neutral Centre, Positive Control Scale" ~ "two_point_n",
      input$Ch2_Norm == "RZscore- Compound Centre and Scale" ~ "znorm_c",
      input$Ch2_Norm == "RZscore- Positive Control Centre and Scale" ~ "znorm_i",
      input$Ch2_Norm == "RZscore- Neutral Centre and Scale" ~ "znorm_n",
      input$Ch2_Norm == "percent- Neutral Centre and Scale" ~ "one_point_n",
      TRUE ~ NA_character_  # Default case to handle unexpected inputs
    )
    
    Ch1_norm <- case_when(
      input$Ch1_Norm == "percent- Compound Centre, Positive Control Scale" ~ "two_point",
      input$Ch1_Norm == "percent- Neutral Centre, Positive Control Scale" ~ "two_point_n",
      input$Ch1_Norm == "RZscore- Compound Centre and Scale" ~ "znorm_c",
      input$Ch1_Norm == "RZscore- Positive Control Centre and Scale" ~ "znorm_i",
      input$Ch1_Norm == "RZscore- Neutral Centre and Scale" ~ "znorm_n",
      input$Ch1_Norm == "percent- Neutral Centre and Scale" ~ "one_point_n",
      TRUE ~ NA_character_  # Default case to handle unexpected inputs
    )
    
    list(Ch2_norm = Ch2_norm, Ch1_norm = Ch1_norm)
  })
  
  
  
  exp_reactive<-reactive({
    paste(input$target,
          input$stage,
          input$run,
          paste0("X_",norm_settings_reactive()$Ch1_norm),
          paste0("Y_",norm_settings_reactive()$Ch2_norm),
          input$scale_scope,
          input$col_to_penalise,
          paste0("k",input$k),
          paste0(advanced_values$power),
          paste0(advanced_values$offset),
          paste0(substr(advanced_values$zero_limit,1,1)),
          paste0(substr(input$channel_flip,1,1)),sep="_")
  })
  
  
  
  
  
  output$exp_out<-renderText({ exp_reactive() })
  output$csvs_out<-renderText({ files_reactive()$datapath })
  output$barcodes_out<-renderText({ input$barcodes })
  output$Ch1_Norm_out<-renderText({ input$Ch1_Norm })
  output$Ch2_Norm_out<-renderText({ input$Ch2_Norm })
  output$scale_scope_out<-renderText({ input$scale_scope })
  output$channel_flip_out<-renderText({ input$channel_flip })
  output$hit_threshold_out<-renderText({ input$hit_threshold })
  output$k_out<-renderText({ input$k })
  output$Export_files_out<-renderText({ input$Export_files })
  output$sessionInfo <- renderPrint({
    capture.output(list(sessionInfo()))
  })
  
  plate_map<- reactive({
    platemap_file<- case_when(input$stage %in% c("Validation","Primary","Confirmation") & input$plate_format =="1536" ~"HTS_1536_primary.csv",
                              input$stage %in% c("XC50_Round1","XC50_Round2")& input$plate_format =="1536" ~"1536_10pt_sing_2013_HTS.csv",
                              input$stage %in% c("Validation","Primary","Confirmation") & input$plate_format =="384" ~"HTS_384_primary.csv",
                              input$stage %in% c("XC50_Round1","XC50_Round2") & input$plate_format =="384" ~"384_HTS_CR.csv",
                              TRUE~"HTS_1536_primary.csv")
    
    plate_map<-read_csv(file.path(main_dir, platemap_file))
    print(platemap_file)
    print(plate_map)
    
  })
  
  
  equation <- reactive({
    col_to_penalise <- input$col_to_penalise
    offset <- advanced_values$offset
    power <- advanced_values$power
    k <- input$k
    
    # Simplify the equation if power is 1
    if (power == 1) {
      d_power=""
    } else {
      d_power<-paste0("^{", power, "}")
    }
    
    if (k==1) {
      k_statement<-""
    } else {
      k_statement <- paste0(k,"*")
    }
    
    if (offset==0){
      
      equation <- paste0(
        "\\[",
        col_to_penalise, "_{penalised} = ", col_to_penalise, " +",k_statement, "d",d_power,
        "\\]"
      )
    } else {
      equation <- paste0(
        "\\[",
        col_to_penalise, "_{penalised} = \\begin{cases}",
        col_to_penalise, ", & \\text{if } d \\leq ", offset, "\\\\",
        col_to_penalise, " +",k_statement ,"(d - ", offset, ")",d_power, ", & \\text{otherwise}",
        "\\end{cases}",
        "\\]"
      )
      
    }
    
  })
  
  
  
  
  df_reactive<-reactive({
    req(files_reactive())
    if (input$mapped_input==TRUE) {
      dex <- process_mapped_input(files_reactive()$datapath)
    } else {
      
      
      csv_tables <- process_csv_matrices(files_reactive()$datapath)
      ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      names(csv_tables)<-str_remove(files_reactive()$name,".csv")
      df<-rbindlist(csv_tables,idcol = "Plate ID")
      
      df<-left_join(df,plate_map()) %>% mutate(`Plate ID`=as.integer(`Plate ID`))
      # 
      # When Donor is the first channel - flip
      
      if (as.logical(input$channel_flip)) {
        df<-rename(df,
                   Ch1=Ch2,
                   Ch2=Ch1)
      }
      
      dex <- mutate(df,ratio=Ch1/Ch2) %>%
        mutate(`Plate ID`=as.integer(`Plate ID`)) %>%
        #left_join(masking_df) %>%
        mutate(Masked=NA,
               `Well Tags`=NA) %>%
        select(`Plate ID`, Row, Column,`Well Type`,everything())
    }
    dex <- mutate(dex,`Well Type`= ifelse(`Well Type`=="Sample", "Compound",`Well Type`))
    dex <- mutate(dex,`Well Type`= ifelse(`Well Type`=="Inhibitor Control", "Positive Control",`Well Type`))
    dex <- mutate(dex,`Well Type`= ifelse(`Well Type`=="Stimulator Control", "Positive Control",`Well Type`))
    # Manually excluded plates are masked (all downstream summaries ignore masked wells)
    if (!"Masked" %in% names(dex)) dex$Masked <- NA
    excluded <- suppressWarnings(as.integer(input$excluded_plates))
    if (length(excluded) > 0) {
      dex <- mutate(dex, Masked = ifelse(`Plate ID` %in% excluded, TRUE, Masked))
    }
    if (!"Well Tags" %in% names(dex)) {dex <- mutate(dex,`Well Tags`=`Well Type`)
    } else {
      dex <- mutate(dex, `Well Tags`=ifelse(is.na(`Well Tags`),`Well Type`,`Well Tags`))
    }
    
    all_tags<- unique(dex$`Well Tags`)
    
    constant_tags<-c("Compound","Neutral Control","Positive Control")
    variable_tags<-setdiff(all_tags,constant_tags)
    dex$`Well Tags`<-factor(dex$`Well Tags`,levels=c(constant_tags,sort(variable_tags)))
    
    return(dex)
    
  })
  
  penalise_reactive<-reactive({
    print(paste(Sys.time(), " Starting calculations"))
    
    req(files_reactive())
    df_input<-df_reactive()
    validate(
      need(any(is.na(df_input$Masked)), "All plates are excluded - clear the exclusion list."),
      need(all(c("Neutral Control", "Positive Control") %in% df_input$`Well Type`),
           "The data must contain both 'Neutral Control' and 'Positive Control' wells. Check plate format, stage and pre-mapped settings."),
      need(all(c("Ch1", "Ch2") %in% names(df_input)),
           "Ch1/Ch2 readings not found. Check that the files match the selected input format (matrix vs pre-mapped).")
    )
    hit_threshold <- input$hit_threshold
    dexl <- pivot_longer(df_reactive(), cols = any_of(c("Ch1","Ch2","ratio")),names_to = "feature")
    
    plates<-dexl %>%
      filter(is.na(Masked)) %>%
      group_by(feature, `Well Type`,`Plate ID`) %>%
      summarise(mean=mean(value, na.rm=T),
                median=median(value, na.rm=T),
                sd=sd(value, na.rm=T),
                mad=mad(value, na.rm=T),
                CV=100*sd/mean,
                rCV=100*mad/median,
                n=n())
    
    plates_wide <- plates %>%
      select(feature,`Well Type`,`Plate ID`, mean,median,sd,mad,CV,rCV) %>%
      pivot_wider(names_from = `Well Type`,values_from = mean:rCV)
    
    plates_wide<- plates_wide %>%
      mutate(rSB=`median_Neutral Control`/`median_Positive Control`,
             SB=`mean_Neutral Control`/`mean_Positive Control`,
             Zf=1-(3*(`sd_Neutral Control`+`sd_Positive Control`)/abs(`mean_Neutral Control`-`mean_Positive Control`)),
             Zfc=1-(3*(`sd_Neutral Control`+`sd_Positive Control`)/(`mean_Neutral Control`-`mean_Positive Control`)),
             rZf=1-(3*(`mad_Neutral Control`+`mad_Positive Control`)/abs(`median_Neutral Control`-`median_Positive Control`)),
             DiffZ=abs(rZf-Zf))
    
    
    
    
    fa<-filter(plates_wide,feature=="Ch1")
    
    
    #Plates which are suspect and need checking
    to_check<-fa %>%
      filter(Zf<ZF_DEFAULT_CUTOFF | rZf<ZF_DEFAULT_CUTOFF | rZf>1 | Zf>1)
    
    
    ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    run_summary<-dexl %>%
      filter(is.na(Masked)) %>%
      group_by(feature, `Well Type`) %>%
      summarise(mean=mean(value, na.rm=T),
                median=median(value, na.rm=T),
                sd=sd(value, na.rm=T),
                mad=mad(value, na.rm=T),
                CV=100*sd/mean,
                rCV=100*mad/median,
                n=n()) %>%
      select(feature,`Well Type`, mean,median,sd,mad,CV,rCV) %>%
      pivot_wider(names_from = `Well Type`,values_from = mean:rCV)
    
    
    
    
    if (input$scale_scope=="Platewise") {
      df<-left_join(dexl,plates_wide)
      
      
      
    }  else {
      
      
      df<-left_join(dexl,run_summary)
      
    }
    
    df<-df %>%
      mutate(znorm_c=(value-median_Compound)/mad_Compound,
             znorm_i=(value-`median_Positive Control`)/`mad_Positive Control`,
             znorm_n=(value-`median_Neutral Control`)/`mad_Neutral Control`,
             two_point=-100*(value-median_Compound)/(`median_Positive Control`-median_Compound),
             two_point_n=-100*(value-`median_Neutral Control`)/(`median_Positive Control`-`median_Neutral Control`),
             one_point_n=100*(value-`median_Neutral Control`)/`median_Neutral Control`)
    
    
    
    
    df_normalisation_table<-tribble(~"Normalisation",~"Short_Name",~"Center",~"Scale",~"Span",
                                    "percent- Compound Centre, Positive Control Scale","two_point","median_Compound","median_Positive Control",100,
                                    "percent- Neutral Centre, Positive Control Scale","two_point_n","median_Neutral Control","median_Positive Control",100,
                                    "percent- Neutral Centre and Scale","one_point_n","median_Neutral Control","median_Neutral Control",100,
                                    "RZscore- Compound Centre and Scale","znorm_c","median_Compound","mad_Compound",1,
                                    "RZscore- Positive Centre and Scale","znorm_i","median_Positive Control","mad_Positive Control",1,
                                    "RZscore- Neutral Centre and Scale","znorm_n","median_Neutral Control","mad_Neutral Control",1)
    
    
    i<-1
    run_list<-list()
    plates_list<-list()
    for (i in 1:nrow(df_normalisation_table)){
      
      Center<-df_normalisation_table$Center[i]
      Scale <- df_normalisation_table$Scale[i]
      
      #print(Center)
      #print(Scale)
      
      plates_wide_s<-select(plates_wide,feature, `Plate ID`, 
                            Center_value=!!sym(Center),
                            Scale_value=!!sym(Scale))
      
      run_summary_s<-select(run_summary,feature, 
                            Center_value=!!sym(Center),
                            Scale_value=!!sym(Scale))
      plates_wide_s$Normalisation <- df_normalisation_table$Normalisation[i]
      run_summary_s$Normalisation <- df_normalisation_table$Normalisation[i]
      plates_list[[i]]<-plates_wide_s
      run_list[[i]]<-run_summary_s
    }
    
    runwise_norm<-rbindlist(run_list) %>% distinct() %>% left_join(df_normalisation_table)
    platewise_norm<-rbindlist(plates_list) %>% distinct()%>% left_join(df_normalisation_table)
    
    
    
    
    norm_plots<- ggplot(platewise_norm,aes(x = Center_value,y=Scale_value,colour=Normalisation))+geom_point(alpha=1,aes(label4=`Plate ID`))+facet_wrap(~feature,scale="free")+
      geom_point(data = runwise_norm,size=6,alpha=0.4)+scale_color_brewer(palette = "Set2")
    
    ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    # Assume these variables are set programmatically
    Ch1_norm <- norm_settings_reactive()$Ch1_norm # Assign the name of the column for Ch1
    Ch2_norm <- norm_settings_reactive()$Ch2_norm # Assign the name of the column for Ch2
    value_col <- "value" # Assign the name of the column for value
    
    
    
    # Use mutate with case_when and programmatically set column names
    df_norm <- df %>%
      mutate(norm = case_when(
        feature == "Ch2" ~ !!sym(Ch2_norm),
        feature == "Ch1" ~ !!sym(Ch1_norm),
        feature == "pct_effect" ~ !!sym(value_col),
        feature == "rzscore" ~ !!sym(value_col),
        feature == "ratio" ~ !!sym(Ch1_norm),
        TRUE ~ !!sym(Ch1_norm)
      ))
    
    # Print the updated data frame
    #print(df_norm)
    
    
    df_normw <- pivot_wider(df_norm,id_cols = `Plate ID`:`Well Tags`,names_from = feature,values_from = norm)
    
    
    
    df_normws<-pivot_longer(df_normw, Ch1:ratio,names_to = "feature")%>%
      group_by(feature, `Well Type`) %>%
      summarise( median=median(value, na.rm=T),
                 mad=mad(value, na.rm=T)) %>%
      pivot_wider(names_from = `Well Type`,values_from = median:mad)
    
    
    neutral <- c(df_normws [df_normws$feature=="Ch1","median_Neutral Control"] %>% simplify(),df_normws [df_normws$feature=="Ch2","median_Neutral Control"] %>% simplify())
    inh <- c(df_normws [df_normws$feature=="Ch1","median_Positive Control"] %>% simplify(),df_normws [df_normws$feature=="Ch2","median_Positive Control"] %>% simplify())
    
    line_frame<- data.frame(rbind(neutral,inh))
    names(line_frame)<-c("X","Y")
    # Extract points
    x1 <- line_frame["neutral", "X"]
    y1 <- line_frame["neutral", "Y"]
    x2 <- line_frame["inh", "X"]
    y2 <- line_frame["inh", "Y"]
    
    slope <- (y2 - y1) / (x2 - x1)
    intercept <- y1 - slope * x1
    
    
    
    calc_distance_vec <- Vectorize(calc_point_line2, c("px", "py"))
    
    
    
    df_normw$d <- calc_point_line2_dt(neutral,inh, df_normw, "Ch1", "Ch2")
    
    df_normw <- df_normw %>%
      mutate(
        #d=calc_distance_vec(neutral,inh,Ch1,Ch2),
        Penalised_value=!!sym(input$col_to_penalise) + input$k * pmax(d-advanced_values$offset,0) ^ advanced_values$power,
        Penalised_value=ifelse(advanced_values$zero_limit==TRUE & Penalised_value>0  , 0 , Penalised_value))
    
    
    df_ratio <- filter(df,feature=="ratio")
    
    df_ratio <- left_join(df_ratio, select(df_normw, `Plate ID`:`Column`,d,Penalised_value))
    
    df_ratio<- mutate(df_ratio,
                      Penalised_value_raw =  ((`median_Positive Control` - median_Compound) * Penalised_value / -100) + median_Compound)
    
    
    
    df_normw<-left_join(df_normw,select(df_ratio,`Plate ID`:`Column`,Penalised_value_raw))
    
    df_normw <- df_normw %>%
      mutate(
        Penalised_Hit=Penalised_value<hit_threshold,
        Classical_Hit=!!sym(input$col_to_penalise)<hit_threshold)
    
    
    d_dens_plot <- df_normw %>%
      filter(is.na(Masked)) %>%
      #filter(ratio < hit_threshold) %>%
      ggplot(aes(d,fill=ratio < hit_threshold))+
      geom_density(alpha=0.7)+
      scale_fill_brewer(palette = "Accent")+
      facet_wrap(~`Well Tags`,scale="free")+labs(title="Density plot for d", fill="Non-penalised Hit")+labs(fill="Active (no penalty applied)")
    
    
    hit_tbl<-df_normw %>%
      filter(is.na(Masked)) %>%
      group_by(`Well Type`) %>%
      summarise(n_wells=n(),
                n_active=sum(`Penalised_Hit`),
                pct_active=signif(100*n_active/n_wells,2),
                n_active_nonpenalised=sum(`Classical_Hit`),
                pct_active_nonpenalised=signif(100*n_active_nonpenalised/n_wells,2)) %>%
      arrange(`Well Type`)
    
    hit_tbl_tags<-df_normw %>%
      filter(is.na(Masked)) %>%
      group_by(`Well Tags`) %>%
      summarise(n_wells=n(),
                n_active=sum(`Penalised_Hit`),
                pct_active=signif(100*n_active/n_wells,2),
                n_active_nonpenalised=sum(`Classical_Hit`),
                pct_active_nonpenalised=signif(100*n_active_nonpenalised/n_wells,2)) %>%
      arrange(`Well Tags`)
    #browser() 
    
    
    df_export<- left_join(df_reactive(),select(df_normw,`Plate ID`:`Column`,d,Penalised_value,Penalised_value_raw))
    df_export_gd <- select(df_export,
                           #file,
                           "Barcode"=`Plate ID`,
                           Row,
                           Column,
                           Ch1,
                           Ch2,
                           ratio,
                           d,
                           Penalised_value,
                           Penalised_value_raw) %>%
      mutate(d=round(d,3),
             Penalised_value=round(Penalised_value,3))
    
    if (input$assay_direction=="Activator"){
      df_export_gd <- df_export_gd %>%
        mutate(Penalised_value=(-1)*Penalised_value )
    }
    
    
    df_export_gd <- data.table(df_export_gd)
    df_export_gd[, (names(df_export_gd)) := lapply(.SD, function(col) {
      if (is.numeric(col)) {
        format(col, scientific = FALSE)
      } else {
        col
      }
    })]
    
    df_export_gd_list<-split(df_export_gd,df_export_gd$Barcode)
    
    x<-1
    out_dir<-file.path(out_path, exp_reactive())
    if(as.logical(input$Export_files)){
      if (dir.exists(out_dir)) {unlink(out_dir,recursive = T)
      }
      dir.create(out_dir,recursive = T)
      # Written next to the report (not into the per-plate CSV folder)
      readr::write_csv(get_hit_candidates(df_normw), file.path(out_path, paste0(exp_reactive(), "_hit_candidates.csv")))
      jsonlite::write_json(collect_settings(), file.path(out_path, paste0(exp_reactive(), "_settings.json")),
                           auto_unbox = TRUE, pretty = TRUE)
      
      
      permission <- "777"  # Replace with the desired permission value
      
      # Execute the chmod command using system()
      
      lapply(seq_along(df_export_gd_list), function(x){
        
        filename<-paste0(names(df_export_gd_list[x]),".csv")
        if (file.exists(file.path( out_dir,filename))) file.remove(file.path( out_dir,filename))
        
        chunk<-df_export_gd_list[[x]] %>% select(-Barcode)
        
        chunk_long<-pivot_longer(chunk,Ch1:Penalised_value_raw)
        chunk_long$name<-factor(chunk_long$name,levels = c("Ch1","Ch2","ratio","d","Penalised_value","Penalised_value_raw"))
        chunk_wide<- chunk_long  %>% pivot_wider(names_from = Column,values_from = value) %>% arrange(name,Row)
        
        
        chunk_wide_split<-split(chunk_wide,chunk_wide$name)
        y<-chunk_wide_split[[1]]
        lapply(chunk_wide_split,function(y){
          y <-y %>% select(-Row, - name)
          
          write.table(y,file.path( out_dir,filename),sep = ",",quote = FALSE,row.names = FALSE,col.names = FALSE,append = T)
          write_lines("",file.path( out_dir,filename),append = T)
        })
        
        
        
      })
      chmod_all(out_dir, permission)
      
    }
    
    quality_raw_data<- df_reactive() %>%
      filter(is.na(Masked))%>%
      filter(!`Plate ID` %in% to_check$`Plate ID`) 
    
    all_possible_hits<-filter(df_normw,(is.na(`Masked`) & `Well Type`=="Compound" & (Penalised_Hit  | Classical_Hit | `Well Tags` != "Compound")))
    
    rest_of_deck<-anti_join(filter(df_normw,is.na(`Masked`)),all_possible_hits) 
    
    if (nrow(rest_of_deck)>1000) {
      cat("N rows=", nrow(rest_of_deck),"\n")
      cat("Sampling 1000 points")
      rest_of_deck<-slice_sample(rest_of_deck, n = 1000)
    }
    
    sample_wells<-rbind(all_possible_hits,rest_of_deck)
    
    
    
    
    df_wide <- select(sample_wells, `Plate ID`, Row, Column) %>%
      distinct() %>%
      left_join(df) %>%
      pivot_wider(id_cols = `Plate ID`:`Well Tags`,names_from = feature,values_from = c("value","znorm_i","two_point")) %>%
      select(-znorm_i_Ch1,
             -znorm_i_ratio) %>%
      left_join(select(df_normw,`Plate ID`,`Row`,Column,`d`, `Penalised_value`)) %>%
      mutate(value_Ch1=round(value_Ch1,2),
             value_Ch2=round(value_Ch2,2),
             d=round(d,2),
             two_point_ratio=round(two_point_ratio,2),
             Penalised_value=round(Penalised_value,2))
    
    
    #
    
    df_export_plot_penalised<-df_wide %>%
      ggplot(aes(colour=Penalised_value),alpha=0.01)+
      geom_point(aes(value_Ch1,value_Ch2,label=d,label2=two_point_ratio, label3=`Plate ID`))+
      scale_colour_viridis_c(limits = c(-100, 3), oob = scales::squish)
    
    
    df_export_plot_ratio<-df_wide %>%
      ggplot(aes(value_Ch1,value_Ch2,colour=two_point_ratio,label=d,label3=`Plate ID`),alpha=0.01)+
      geom_point()+
      scale_colour_viridis_c(limits = c(-100, 3), oob = scales::squish)
    
    
    plots_both_penalty<- df_export_plot_penalised+df_export_plot_ratio
    
    
    
    
    
    
    
    ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    sample_wells_plot_df<- sample_wells %>%
      mutate(Ch1=round(Ch1,2),
             Ch2=round(Ch2,2),
             d=round(d,2),
             ratio=round(ratio,2),
             Penalised_value=round(!!sym(input$col_to_penalise) + input$k * pmax(d-advanced_values$offset,0) ^advanced_values$power,2),
             Penalised_value=ifelse(advanced_values$zero_limit==TRUE & Penalised_value>0  , 0 , Penalised_value))
    
    df_normw_plot<-sample_wells_plot_df %>%
      ggplot(aes(colour=Penalised_value))+
      geom_point(aes(Ch1,Ch2,label=d,label2=ratio),alpha=0.5)+
      geom_line(data = line_frame,aes(x=X,y=Y,colour=-200),alpha=1,linewidth=1.3,arrow=arrow(angle = 30,
                                                                                             ends = "first", type = "open"))+
      labs(title="Normalised penalised data",colour=paste0("ratio + ",input$k,"*d^",advanced_values$power),y="Ch2 Z Score",x="Ch1 Percent effect")+
      scale_colour_viridis_c(limits = c(-100, 3), oob = scales::squish)+
      coord_cartesian(xlim=c(-120,100),ylim=c(-20,80))
    
    
    
    ## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
    print(paste(Sys.time(),"Starting grid creation."))
    positive_score_df <-filter(df_normw, is.na(Masked),  Penalised_value< 50 ) %>% select(`Plate ID`, `Row`, `Column`) %>% left_join(df_input) 
    
    Ch1_fine_min <-  positive_score_df %>% pull(Ch1) %>% min(na.rm = T)
    Ch1_fine_min <- max(c(Ch1_fine_min,0))
    
    Ch2_fine_min <-  positive_score_df %>% pull(Ch2) %>% min(na.rm = T)
    Ch1_fine_min<-max(Ch1_fine_min,0)
    Ch1_fine_max <- filter(positive_score_df, is.na(Masked)) %>% pull(Ch1) %>% max(na.rm = T)
    Ch2_fine_max <- filter(positive_score_df, is.na(Masked)) %>% pull(Ch2) %>% max(na.rm = T)
    
    
    
    Ch1_min <- filter(df_input, is.na(Masked)) %>% pull(Ch1) %>% min(na.rm = T)
    Ch1_min <- max(c(Ch1_min,0))
    Ch2_min <- filter(df_input, is.na(Masked)) %>% pull(Ch2) %>% min(na.rm = T)
    Ch2_min<-max(Ch2_min,0)
    
    Ch1_max <- filter(df_input, is.na(Masked)) %>% pull(Ch1) %>% max(na.rm = T)
    Ch2_max <- filter(df_input, is.na(Masked)) %>% pull(Ch2) %>% max(na.rm = T)
    
    
    grid_fine <- as.data.table(expand.grid(Ch1 = seq(Ch1_fine_min, Ch1_fine_max, length.out = 500),
                                           Ch2 = seq(Ch2_fine_min, Ch2_fine_max, length.out = 500)))
    
    grid_fine$Density <- "Dense"
    grid_sparse <- as.data.table(expand.grid(Ch1 = seq(Ch1_min, Ch1_max, length.out = 500),
                                             Ch2 = seq(Ch2_min, Ch2_max, length.out = 500)))
    grid_sparse$Density <- "Sparse"
    grid<- rbind(grid_fine, grid_sparse) %>% arrange(Ch2,Ch2)
    
    
    grid[, `:=`(ratio = Ch1 / Ch2, id = .I)] # .I is data.table's row number
    
    
    gridl <- melt(grid, id.vars = c("id","Density"), measure.vars = c("Ch1", "Ch2", "ratio"),
                  variable.name = "feature", value.name = "value")
    
    # 4. tibble creation and left_join with runwise_norm
    selected_norm <- data.table(feature = c("Ch1", "Ch2", "ratio"),
                                Short_Name = c(Ch1_norm, Ch2_norm, Ch1_norm))
    
    selected_norm <- merge(selected_norm, runwise_norm, by = c("feature", "Short_Name"), all.x = TRUE)
    gridl <- merge(gridl, selected_norm, by = c("feature"), all.x = TRUE)
    
    
    
    
    
    
    
    gridl[, norm_value := fifelse(
      Normalisation %in% c("percent- Compound Centre, Positive Control Scale", "percent- Neutral Centre, Positive Control Scale"),
      -100 * (value - Center_value) / (Scale_value - Center_value),
      fifelse(
        Normalisation == "percent- Neutral Centre and Scale",
        100 * (value - Center_value) / Scale_value,
        fifelse(
          grepl("RZscore-", Normalisation),
          (value - Center_value) / Scale_value,
          NA_real_  # or another action/value if needed
        )
      )
    )]
    
    
    
    grid_norm <- gridl[
      , .(id, Density, feature, norm_value)][      # Select specific columns
        , dcast(.SD, id+Density ~ feature, value.var = "norm_value")  # Pivot wider
      ]
    
    grid_norm <- gridl[
      , .(id, Density, feature, norm_value)  # Select specific columns
    ][
      , dcast(.SD, id + Density ~ feature, value.var = "norm_value")  # Pivot wider
    ]
    
    
    
    grid_norm$d <- calc_point_line2_dt(neutral,inh, grid_norm, "Ch1", "Ch2")
    
    grid_norm[, `:=`(
      Penalised_value = get(input$col_to_penalise) + input$k * pmax(d - advanced_values$offset, 0) ^ advanced_values$power,
      Penalised_value_double_k = get(input$col_to_penalise) + 2 * input$k * pmax(d - advanced_values$offset, 0) ^ advanced_values$power,
      Penalised_value_half_k = get(input$col_to_penalise) + 0.5 * input$k * pmax(d - advanced_values$offset, 0) ^ advanced_values$power
    )]
    
    
    grid_norm <- merge(
      grid_norm,
      grid[, .(id, Ch1_raw = Ch1, Ch2_raw = Ch2)], 
      by = "id", 
      all.x = TRUE
    )
    
    #lookup table.
    k_lookup <- data.table(
      name = c("Penalised_value", "Penalised_value_double_k", "Penalised_value_half_k"),
      multiplier = c(1,2,0.5)
    )
    
    grid_norml <- melt(
      grid_norm,
      measure.vars = patterns("^Penalised_value"),
      variable.name = "name",
      value.name = "value"
    )
    
    
    
    grid_norml <- merge(grid_norml, k_lookup, by = "name", all.x = TRUE)
    
    
    grid_norml[ , k := multiplier * input$k]
    grid_norml[is.na(k) , k := NA_real_]
    
    
    
    grid_norml[, multiplier := NULL]
    
    
    
    
    
    threshold<-0.1
    grid_norm_closest<-filter(grid_norml, value < (hit_threshold)+threshold  & value > (hit_threshold)-threshold)
    grid_norm_sel_col_closest<-filter(grid_norm,!!sym(input$col_to_penalise) < (hit_threshold)+threshold  & !!sym(input$col_to_penalise) > (hit_threshold)-threshold)
    
    
    
    grid_norm_sel_col_annotation <-arrange(grid_norm_sel_col_closest,Ch1)
    
    grid_norm_sel_col_annotation <- grid_norm_sel_col_annotation[floor(nrow(grid_norm_sel_col_annotation)/2),]
    
    grid_norm_closest_mins<-grid_norm_closest %>%
      group_by(name) %>%
      slice_min(Ch1,with_ties = F,na_rm = T,n=1)
    
    
    print(paste(Sys.time(),"Selected annotation data frames"))
    grid_norm_dense<-filter(grid_norm,Density=="Dense")
    zoom_Ch1 <- c(min(c(grid_norm_dense$Ch1,grid_norm_closest$Ch1)),max(c(grid_norm_dense$Ch1,grid_norm_closest$Ch1)))
    zoom_Ch2 <- c(min(c(grid_norm_dense$Ch2,grid_norm_closest$Ch2)),max(c(grid_norm_dense$Ch2,grid_norm_closest$Ch2)))
    zoom_ratio<-c(min(c(grid_norm_dense$ratio,grid_norm_closest$ratio)),max(c(grid_norm_dense$ratio,grid_norm_closest$ratio)))
    
    zoom_Ch1_raw<-c(min(grid[grid$Density=="Dense",Ch1]),max(grid[grid$Density=="Dense",Ch1]))
    zoom_Ch2_raw<-c(min(grid[grid$Density=="Dense",Ch2]),max(grid[grid$Density=="Dense",Ch2]))
    
    col_to_penalise <- input$col_to_penalise
    
    grid_norm_closest <- grid_norm_closest %>%
      mutate(legend_group = paste0("k = ", k))
    
    grid_norm_closest_mins <- grid_norm_closest_mins %>%
      mutate(legend_group = paste0("k = ", k))
    
    df_wide$`Well Tags` <- as.factor(df_wide$`Well Tags`)
    
    # Get the unique Well Tag levels and the "k = x" label
    well_tag_levels <- levels(df_wide$`Well Tags`)
    print(well_tag_levels)
    grid_norm_closest$legend_group <- as.factor(grid_norm_closest$legend_group)
    k_labels <- levels(grid_norm_closest$legend_group) # Assuming k is constant for the path
    
    # Create the desired order: Well Tags first, then "k = "
    desired_levels <- c(well_tag_levels, k_labels)
    print(desired_levels)
    # Reorder the factor levels
    df_wide$`Well Tags` <- factor(df_wide$`Well Tags`, levels = desired_levels)
    
    names(set2_c)<-desired_levels
    
    
    
    k_plot<-ggplot() +
      geom_point(data=sample_wells_plot_df,alpha=0.5,aes(x = Ch1, y = Ch2,label1=ratio,label2=d,label3=Penalised_value,label4=`Plate ID`, colour=`Well Tags`))+
      geom_point(aes(x = neutral[1], y =neutral[2]), color = "#E7298A", size = 1) +
      geom_point(aes(x = inh[1], y = inh[2]), color = "#E7298A", size = 1) +
      geom_line(data = line_frame,aes(x=X,y=Y),alpha=0.7,colour="#E7298A",linewidth=0.3,arrow=arrow(angle = 30,
                                                                                                    ends = "first", type = "open"))+
      scale_color_manual(values= set2_c,breaks = desired_levels)+
      labs(title=paste0("Activity Decision boundaries in normalised Ch1 vs Ch2 space"))
    
    k2_plot<-ggplot() +
      geom_point(data=sample_wells_plot_df,alpha=0.5,aes(x = ratio, y = Ch2,label1=ratio,label2=d,label3=Penalised_value,label4=`Plate ID`, colour=`Well Tags`))+
      scale_color_manual(values= set2_c,breaks = desired_levels)+
      labs(title=paste0("Activity Decision boundaries in normalised ratio vs Ch2 space ") )+coord_cartesian(xlim = c(min(sample_wells_plot_df$ratio), quantile(sample_wells_plot_df$ratio, 0.99)))
    
    
    
    k_plot_raw<-ggplot(grid_norm,aes(Ch1_raw,Ch2_raw)) +
      geom_point(data=df_wide,alpha=0.5,aes(x=value_Ch1,y=value_Ch2,label4=`Plate ID`, colour=`Well Tags`))+
      scale_color_manual(values= set2_c,breaks = desired_levels)+
      labs(title=paste("Activity Decision boundaries for raw Ch1 and Ch2 values"))
    
    
    if (nrow(grid_norm_sel_col_closest) != 0) {
      m_Ch1 <- lm(Ch1~Ch2,data=grid_norm_sel_col_closest)
      
      #Calculate reversed coefficients
      slope_Ch1 <- m_Ch1$coefficients["Ch2"]
      intercept_Ch1 <- m_Ch1$coefficients["(Intercept)"]
      
      
      reverse_slope_Ch1 <- 1 / slope_Ch1
      reverse_slope_Ch1 <- ifelse(is.na(reverse_slope_Ch1), 0, reverse_slope_Ch1)
      
      reverse_intercept_Ch1 <- - intercept_Ch1 / slope_Ch1
      
      
      
      m_ratio <- lm(ratio~Ch2,data=grid_norm_sel_col_closest)
      
      rmsd_m_ratio<- round(calculate_normalised_rmsd_median(m_ratio)) 
      rmsd_m_Ch1<-round(calculate_normalised_rmsd_median(m_Ch1))
      
      #Calculate reversed coefficients
      slope_ratio <- m_ratio$coefficients["Ch2"]
      intercept_ratio <- m_ratio$coefficients["(Intercept)"]
      
      reverse_slope_ratio <- 1 / slope_ratio
      reverse_intercept_ratio <- - intercept_ratio / slope_ratio
      
      grid_norm_closest <- grid_norm_closest %>%
        mutate(
          y_line = slope * ratio + intercept,
          above_line = Ch2 > y_line   # TRUE if above, FALSE if below
        )
      
      grid_norm_closest <- arrange(grid_norm_closest,legend_group,above_line,ratio,Ch2)
      above_line<- filter(grid_norm_closest,above_line)
      below_line<- filter(grid_norm_closest,!above_line)
      print(above_line)
      k2_plot<- k2_plot+ 
        #geom_path(data=grid_norm_closest,aes(x = ratio, y = Ch2,colour=legend_group))+
        geom_path(data=above_line,aes(x = ratio, y = Ch2,colour=legend_group))+
        geom_path(data=below_line,aes(x = ratio, y = Ch2,colour=legend_group))+
        geom_text(data = grid_norm_closest_mins, aes(x = ratio, y = Ch2, label = legend_group, colour=legend_group),nudge_x =1, nudge_y = 1,show.legend = F )+
        geom_text(data = grid_norm_sel_col_annotation, aes(x = ratio, y = Ch2, label = paste("non-penalised",col_to_penalise)),colour="black",nudge_x =1, nudge_y = max(grid_norm_closest_mins$Ch2,na.rm = T)/20,show.legend = F )
      
      if (rmsd_m_ratio > 0) {
        k2_plot <- k2_plot 
        
      } else { k2_plot<-k2_plot+   geom_abline(slope = reverse_slope_ratio,intercept = reverse_intercept_ratio,linetype="dashed",colour="black",linewidth=0.3)
      }
      
      k_plot<-k_plot +
        geom_path(data=above_line,aes(x = Ch1, y = Ch2,colour=legend_group))+
        geom_path(data=below_line,aes(x = Ch1, y = Ch2,colour=legend_group))+
        geom_text(data = grid_norm_closest_mins, aes(x = Ch1, y = Ch2, label = legend_group,colour=legend_group),nudge_x =1, nudge_y = 1,show.legend = F )+
        geom_text(data = grid_norm_sel_col_annotation, aes(x = Ch1, y = Ch2, label = paste("non-penalised",col_to_penalise)),colour="black",nudge_x =1, nudge_y = max(grid_norm_closest_mins$Ch2,na.rm = T)/20,show.legend = F )
      
      
      if (rmsd_m_Ch1 > 0) {
        k_plot <- k_plot +   geom_path(data=grid_norm_sel_col_closest,aes(x = Ch1, y = Ch2),linetype="dashed",colour="black",linewidth=0.3)
        
      } else { k_plot<-k_plot +   geom_abline(slope = reverse_slope_Ch1, intercept = reverse_intercept_Ch1,linetype="dashed",colour="black",linewidth=0.3)
      }
      
      k_plot_raw<- k_plot_raw +
        geom_path(data=grid_norm_sel_col_closest,linetype="dashed",colour="black",linewidth=0.3)+
        #geom_path(data=grid_norm_closest,aes(colour=legend_group))+
        geom_path(data=above_line,aes(colour=legend_group))+
        geom_path(data=below_line,aes(colour=legend_group))+
        geom_text(data = grid_norm_closest_mins, aes(x = Ch1_raw, y = Ch2_raw, label = legend_group,colour=legend_group), nudge_y = max(grid_norm_closest_mins$Ch2_raw,na.rm = T)/20, show.legend = F )+
        geom_text(data = grid_norm_sel_col_annotation, aes(x = Ch1_raw, y = Ch2_raw, label = paste("non-penalised",col_to_penalise)),colour="black",show.legend = F )
      
      
    }
    
    k_plot_zoom<-k_plot+coord_cartesian(xlim = zoom_Ch1,ylim = zoom_Ch2)
    k2_plot_zoom<-k2_plot+coord_cartesian(xlim = zoom_ratio,ylim = zoom_Ch2)
    k_plot_raw_zoom<-k_plot_raw+coord_cartesian(xlim=zoom_Ch1_raw,ylim=zoom_Ch2_raw)
    
    
    
    combined_plot <- k_plot_zoom / k2_plot_zoom / k_plot_raw_zoom
    
    
    
    if(as.logical(input$Export_files)){
      
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      tempReport <- file.path(out_path,paste0(exp_reactive(),"_report.Rmd"))
      tempStyle <- file.path(out_path,"report_style.docx")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      file.copy(file.path("report_style.docx"), tempStyle)
      
      
      
      #Prep a list of params for parametrized report
      report_params <- list(target= input$target,
                            stage= input$stage,
                            run= input$run,
                            Ch1_norm= norm_settings_reactive()$Ch1_norm,
                            Ch2_norm= norm_settings_reactive()$Ch2_norm,
                            scale_scope=  input$scale_scope,
                            col_to_penalise=  input$col_to_penalise,
                            equation=equation(),
                            zero_limit= advanced_values$zero_limit,
                            channel_flip= input$channel_flip,
                            hit_threshold=input$hit_threshold,
                            norm_plots= norm_plots,
                            run_summary= run_summary,
                            d_dens_plot= d_dens_plot,
                            combined_plot= combined_plot,
                            df_normw_plot= df_normw_plot,
                            df_export_plot_ratio= df_export_plot_ratio,
                            df_export_plot_penalised= df_export_plot_penalised,
                            zoom_Ch1_raw=zoom_Ch1_raw,
                            zoom_Ch2_raw=zoom_Ch2_raw,
                            hit_tbl_tags= hit_tbl_tags,
                            hit_tbl= hit_tbl,
                            plates_wide= plates_wide)
      
      # Save the list as an RDS file
      #save(report_params, file = "report_params.rda")
      
      rmarkdown::render(tempReport,
                        params =report_params ,
                        output_file = file.path(out_path,paste0(exp_reactive(),"_report.docx")),
                        
                        envir = new.env(parent = globalenv()))
      
      
      chmod_all(file.path(out_path,paste0(exp_reactive(),"_report.docx")), permission)
      
      file.remove(tempReport)
      file.remove(tempStyle)
      
      zip::zipr(zipfile = zip_file_path, files = list.files(out_path,recursive = T), root = out_path)
      
    }
    
    print(Sys.time())
    
    #browser()
    #save(list=ls(), file="env.rda")
    return(list(df_export_gd=df_export_gd,
                plates_qc=dplyr::ungroup(plates_wide),
                plates_wide=select(plates_wide,feature,`Plate ID`,median_Compound:`median_Positive Control`,mad_Compound:`mad_Positive Control`,rCV_Compound:`rCV_Positive Control`,rSB,rZf),
                run_summary=select(run_summary,feature,median_Compound:`median_Positive Control`,mad_Compound:`mad_Positive Control`,rCV_Compound:`rCV_Positive Control`),
                df_normw=df_normw,
                df=df,
                dexl=dexl,
                hit_tbl=hit_tbl,
                hit_tbl_tags=hit_tbl_tags,
                sample_wells=sample_wells,
                d_dens_plot=d_dens_plot,
                df_normw_plot=df_normw_plot,
                k_plot=k_plot,
                k_plot_raw=k_plot_raw,
                k2_plot=k2_plot,
                zoom_Ch1=zoom_Ch1,
                zoom_Ch2=zoom_Ch2,
                zoom_ratio=zoom_ratio,
                zoom_Ch1_raw=zoom_Ch1_raw,
                zoom_Ch2_raw=zoom_Ch2_raw,
                plots_both_penalty=plots_both_penalty,
                df_export_plot_ratio=df_export_plot_ratio,
                df_export_plot_penalised=df_export_plot_penalised,
                norm_plots=norm_plots,
                Done="Done"))
  })
  
  
  
  
  
  
  
  
  
  
