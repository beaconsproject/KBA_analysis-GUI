assessRepServer <- function(input, output, session, project, map, rv){
  
  # RENDER ASSESS REPRESENTATION UI
  output$assessRep <- renderUI({
    req(input$tabs == "tabKBA")
    tagList(
      div("Select reference area.", style = "font-size: 15px; font-weight: bold; margin-left: 15px; margin-top: 20px;"),
      if (is.null(rv$refarea_reactive())) {
        fileInput("upload_refarea", "Upload reference area shapefile", multiple = TRUE)
      } else {
        div(HTML('<i class="fa fa-thumb-tack" style="color:#d9534f; "></i>'), "Reference area already uploaded.", style = "font-size: 12px; margin-top: 20px; margin-left: 30px;")
      },
      div(style = "margin-top: 0px;", radioButtons("assessKBAs", "Assess representation using:", choices = c("Only KBAs", "Only PAs", "Both KBAs and PAs"))),
      actionButton("runRep", "Run representation analysis", icon = icon("image"), class = "btn-warning", style="width:250px"),
    )
  })
  
  observeEvent(input$upload_refarea, {
    rv$refarea_reactive(read_shp_from_upload(input$upload_refarea))
    st_write(rv$refarea_reactive(), dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "reference area", driver = "GPKG", append = TRUE)
  })
  
  # RENDER KBA FILTERING UI
  output$filterRep <- renderUI({
    req(rv$poly_reactive())
    
    tagList(
      div("Filter KBAs and/or PAs based on dissimilarity metrics (DMs), upstream area and PAs area",
          style = "font-size: 14px; font-weight: bold; margin-top: 20px; margin-left: 20px;"),
      div("DMs range from 0 to 1. 0 = low dissimilarity or high representation, 1 = high dissimilarity or low representation",
          style = "font-size: 12px; margin-left: 20px; margin-top: 20px;"),
      
      sliderInput("slideCMI", "CMI:", min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE),
      sliderInput("slideLED", "LED:", min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE),
      sliderInput("slideGPP", "GPP:", min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE),
      sliderInput("slideLCC", "LCC:", min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE),
      
      uiOutput("slidercrit5"),
      
      sliderInput("slideUP", "Maximum upstream area (sq.km):", min = 0, max = 100000, value = 25000, step = 1000, ticks = FALSE),
      sliderInput("slidePAs", "Minimum PAs area (sq.km):", min = 0, max = 10000, value = 5, step = 100, ticks = FALSE),
      
      actionButton("filterRep", "Apply filtering", icon = icon("filter"), class = "btn-primary", style = "width:250px"),
      
      div(style = "margin-top: 20px;", actionButton("downloadKBA", "Download Filtered KBAs", icon = icon("download"), class = "btn-warning", style = "width:250px")))
  })
  
  observe({
    req(rv$outdir())
    invalidateLater(2000)
    gpkg_path <- file.path(rv$outdir(), "output/KBA_analysis.gpkg")
    req(file.exists(gpkg_path))
    
    layers <- sf::st_layers(gpkg_path)$name
    
    # If protected areas do NOT exist → disable PA-dependent options
    if (!"protected_areas" %in% layers) {
      # Insert disabled attributes after UI is drawn
      session$sendCustomMessage( "disablePAchoices", list())
    } else{
      session$sendCustomMessage("enablePAchoices", list())
    }
  })
  ####################################################################################################
  ####################################################################################################
  # ASSESS REPRESENTATION 
  ####################################################################################################
  
  #########################################################
  #-UPDATE FREQUENCY TABLE AND MAX UPSTREAM SLIDER
  observeEvent(input$tabs, {
    req(input$tabs == "tabKBA")
    req(rv$layers_rv$catchments)
    
    # Initialize KBA/PAs freq table
    x <- tibble(
      Variables = c("KBAs", "PAs", "Filtered KBAs", "Filtered PAs"),
      Count = c(NA, NA, NA, NA))  
    
    layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    reduced_kba <- layers[grepl("^KBAs_reduced", layers)]
    if (length(reduced_kba) > 0) { 
      if(paste0("KBAs_reduced", input$set_grid) %in% reduced_kba){
        updateSelectInput(session = getDefaultReactiveDomain(), "KBAlayer", choices = reduced_kba, selected = isolate(paste0("KBAs_reduced", input$set_grid)))
        kba_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = paste0("KBAs_reduced", input$set_grid))
        x <- x %>% 
          mutate(Count = case_when(Variables == "KBAs" ~  nrow(kba_sf),
                                   TRUE ~ Count))
        #rv$kba_init_label("Potential KBAs (reduced)")
      }else{
        updateSelectInput(session = getDefaultReactiveDomain(), "KBAlayer", choices = reduced_kba, selected = isolate(reduced_kba[1]))
        kba_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = reduced_kba[1])
        x <- x %>% 
          mutate(Count = case_when(Variables == "KBAs" ~  nrow(kba_sf),
                                   TRUE ~ Count))
        #rv$kba_init_label("Potential KBAs (all)")
        
      }
      rv$kba_init_label("Potential KBAs")
    }
    
    if ("protected_areas" %in% layers) {
      pas_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas")
      x <- x %>% 
        mutate(Count = case_when(Variables == "PAs" ~  nrow(pas_sf),
                                 TRUE ~ Count))
    }
    
    # Generate Stat Tables    
    rv$outfreqkba(x) 
    
    output$outkbafreq <- renderTable({
      rv$outfreqkba()
    })
  })
  
  # Update LEAFLET
  observeEvent(input$KBAlayer, {
    if (input$tabs == "tabKBA") {
      
      layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
      layers <- layers_info$name
      # Initialize kba_sf and pas_sf as NULL
      kba_sf <- NULL
      if (!(input$KBAlayer=="No KBA generated")){
        kba_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = input$KBAlayer)
        rv$kba_sf_reactive(kba_sf)
      } 
      
      req(rv$kba_sf_reactive())
      x <- rv$outfreqkba()
      
      x <- x %>% 
        mutate(Count = case_when(Variables == "KBAs" ~  ifelse(!is.null(kba_sf), nrow(kba_sf), NA_integer_),
                                 TRUE ~ Count))
      rv$outfreqkba(x)
      
      output$outkbafreq <- renderTable({
        rv$outfreqkba()
      })
      
      output$slidercrit5 <- renderUI({
        # Check if criteria5() is NULL
        if (!is.null(rv$layers_rv$criteria5)) {
          # If criteria5 is NULL, render the sliderInput with disabled = TRUE
          div(style = "margin-top: -30px;", sliderInput("slidecrit5", label = paste0(rv$criteria5name(), ":"), min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE))
        }
      })
      
      legend <- c(rv$overlayGroups(), rv$kba_init_label())
      rv$overlayGroups(legend)
      
      kba_sf_4326 <- st_transform(kba_sf, 4326) %>% st_simplify(dTolerance = 0.001)
      leafletProxy("map") %>%
        clearControls() %>%
        clearGroup("Potential KBAs (reduced)") %>%
        clearGroup("Potential KBAs (all)") %>%
        clearGroup("Potential KBAs") %>%
        addPolygons(data=kba_sf_4326, fillColor='purple', color= "#000000", weight = 1,  group="Potential KBAs", options = leafletOptions(pane = "over")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                         overlayGroups = c(rv$overlayGroups(), "Potential KBAs"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Streams"))
      
      if(!is.null(rv$refarea_reactive())){
        req(rv$refarea_reactive())
        refarea <- rv$refarea_reactive() %>% st_transform(4326)
        legend <- c(rv$overlayGroups(), "Reference area")
        rv$overlayGroups(legend)
        
        leafletProxy("map") %>%
          clearControls() %>%
          addPolygons(data=refarea, color='#6b4b38', fill = F, weight=3,  group="Reference area", options = leafletOptions(pane = "ground")) %>%
          addLayersControl(position = "topright",
                           baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                           overlayGroups = c(rv$overlayGroups(), "Potential KBAs"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(c("Streams"))
      }
    }
  }, ignoreInit = TRUE)
  
  #########################################################
  #-RUN REPRESENTATION
  #########################################################
  observeEvent(input$runRep, {
    #Test on required objects
    
    if (is.null(rv$refarea_reactive()) || is.null(rv$layers_rv$streams) || is.null(rv$layers_rv$planreg) || is.null(rv$layers_rv$cmi) || is.null(rv$layers_rv$gpp) || is.null(rv$layers_rv$led) || is.null(rv$layers_rv$lcc)) {
      missing_layers <- c(
        if (is.null(rv$refarea_reactive())) "reference area",
        if (is.null(rv$layers_rv$streams)) "stream",
        if (is.null(rv$layers_rv$planreg)) "planning region",
        if (is.null(rv$layers_rv$cmi)) "CMI",
        if (is.null(rv$layers_rv$gpp)) "GPP",
        if (is.null(rv$layers_rv$led)) "LED",
        if (is.null(rv$layers_rv$lcc)) "LCC"
      )
      # Create the modal dialog
      showModal(modalDialog(
        title = "Missing Data",
        paste("The following layers are missing:", paste(missing_layers, collapse = ", "), ". Please upload missing spatial dataset or provide a csv tha contain access path in the Set input parameters step."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    req(rv$refarea_reactive())
    req(rv$layers_rv$catchments)
    req(input$assessKBAs)
    
    # Test on existing layers
    layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    
    if(input$assessKBAs == "Only KBAs" || input$assessKBAs == "Both KBAs and PAs"){
      if(is.null(rv$kba_sf_reactive())){
        showModal(modalDialog(
          title = "KBAs are missing from your gpkg.",
          "Please run Builder and calculate hydrology metrics prior to assess representation.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
    }
    
    if(input$assessKBAs == "Only PAs" || input$assessKBAs == "Both KBAs and PAs"){
      if (!("protected_areas" %in% layers)) {
        showModal(modalDialog(
          title = "Hydrology metrics were not calclulated on protected areas layers", "Make sure protected areas are uploaded and run the Evaluate PAs step",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
        return()
      }
    }
    #Start processing
    showModal(modalDialog(
      title = "Processing representation analysis",
      "Please wait...",
      footer = NULL
    ))
    
    if (!is.null(rv$layers_rv$criteria5)) {
      updated_grp <- c(rv$legendcrit(), rv$criteria5name())
      rv$legendcrit(updated_grp)
    }
    
    extent <- st_union(rv$refarea_reactive(), rv$layers_rv$planreg) %>%     
      st_as_sf() %>%
      st_make_valid()                
    
    plot_dir <- file.path(rv$outdir(), "output/plot",  input$KBAlayer)
    rv$plotDir(plot_dir)
    
    #Prep criteria
    if (!file.exists(file.path(rv$outdir(), "output/kba_cmi.tif"))) {
      showModal(modalDialog(
        title = "Processing representation analysis",
        "Extracting representation criterion: CMI...",
        footer = NULL
      ))
      r_crop <- crop(rv$layers_rv$cmi, vect(extent))
      r_mask <- mask(r_crop, vect(extent))
      cmi <- process_raster(r_mask, rv$refarea_reactive(), rv$outdir(), "kba_cmi", fact = 2, aggregation_fun = "mean")
      kba_cmi <- cmi$original
      cmi_4326 <- cmi$projected
    }else{
      kba_cmi <- rv$layers_rv$cmi
      cmi_4326 <- rast(file.path(rv$outdir(), "output/kba_cmi_4326.tif"))
    }
    if (!file.exists(file.path(rv$outdir(), "output/kba_led.tif"))) {
      showModal(modalDialog(
        title = "Processing representation analysis",
        "Extracting representation criterion: LED...",
        footer = NULL
      ))
      r_crop <- crop(rv$layers_rv$led, vect(extent))
      r_mask <- mask(r_crop, vect(extent))
      led <- process_raster(r_mask, rv$refarea_reactive(), rv$outdir(), "kba_led", fact = 4, aggregation_fun = "mean")
      kba_led <- led$original
      led_4326 <- led$projected
    } else{
      kba_led <- rv$layers_rv$led
      led_4326 <- rast(file.path(rv$outdir(), "output/kba_led_4326.tif"))
    }
    if (!file.exists(file.path(rv$outdir(), "output/kba_gpp.tif"))) {
      showModal(modalDialog(
        title = "Processing representation analysis",
        "Extracting representation criterion: GPP...",
        footer = NULL
      ))
      r_crop <- crop(rv$layers_rv$gpp, vect(extent))
      r_mask <- mask(r_crop, vect(extent))
      gpp <- process_raster(r_mask, rv$refarea_reactive(), rv$outdir(), "kba_gpp", fact = 4, aggregation_fun = "mean")
      kba_gpp <- gpp$original
      gpp_4326 <- gpp$projected
    } else{
      kba_gpp <- rv$layers_rv$gpp
      gpp_4326 <- rast(file.path(rv$outdir(), "output/kba_gpp_4326.tif"))
    }
    if (!file.exists(file.path(rv$outdir(), "output/kba_lcc.tif"))) {
      showModal(modalDialog(
        title = "Processing representation analysis",
        "Extracting representation criterion: LCC...",
        footer = NULL
      ))
      r_crop <- crop(rv$layers_rv$lcc, vect(extent))
      r_mask <- mask(r_crop, vect(extent))
      
      lcc <- process_raster(r_mask, rv$refarea_reactive(), rv$outdir(), "kba_lcc", fact = 40, aggregation_fun = "modal", ignored = c(15, 17))
      kba_lcc <- lcc$original
      lcc_4326 <- lcc$projected
    }else{
      kba_lcc <- rv$layers_rv$lcc
      lcc_4326 <- rast(file.path(rv$outdir(), "output/kba_lcc_4326.tif"))
    }
    
    if(!is.null(rv$layers_rv$criteria5)){
      if (!file.exists(file.path(rv$outdir(), "output", paste0(rv$criteria5name(), ".tif")))) {
        r_crop <- crop(rv$layers_rv$criteria5, vect(extent))
        r_mask <- mask(r_crop, vect(extent))
        
        crit5 <- process_raster(r_mask, rv$refarea_reactive(), rv$outdir(), rv$criteria5name(), fact = 4, aggregation_fun = "mean")
        kba_criteria5 <- crit5$original
        crit5_4326 <- crit5$projected
      }else{
        kba_criteria5 <- rast(file.path(rv$outdir(), "output", paste0(rv$criteria5name(), ".tif")))
        crit5_4326 <- rast(file.path(rv$outdir(), "output", paste0(rv$criteria5name(), "_4326.tif")))
      }
    }else{
      kba_criteria5 <- NULL
    } 
    
    # Access elements
    legend_data <- prep_legend(kba_cmi, kba_led, kba_gpp, kba_lcc, kba_criteria5)
    cmi_xpal <- legend_data$cmi_xpal
    led_xpal <- legend_data$led_xpal
    gpp_xpal <- legend_data$gpp_xpal
    lcc_labels <- legend_data$lcc_labels
    df_label <- legend_data$df_label
    lcc_cols <- legend_data$lcc_cols
    val.color <- legend_data$val.color
    led_val.color <- legend_data$led_val.color
    crit_xpal <- legend_data$crit_xpal
    labeller_function <- legend_data$labeller_function
    showModal(modalDialog(
      title = "Calculating dissimilarity metrics",
      "Please wait...",
      footer = NULL
    ))
    
    n_metrics <- 4L + !is.null(rv$layers_rv$criteria5)
    metric_progress <- make_metric_progress(n_metrics)
    
    if(input$assessKBAs == "Only KBAs" || input$assessKBAs == "Both KBAs and PAs"){  
      set_grid <- sub("^KBAs_reduced([^_]+)$", "\\1", input$KBAlayer) 
      kba_up <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "upstream_KBAs") %>%
        dplyr::select(network)
      if(!(paste0("repKBAs_reduced", set_grid) %in% layers)) {
        kba_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = input$KBAlayer)
        
        if(attr(kba_sf, "sf_column") != "geometry"){
          kba_sf$geometry <- kba_sf$geom
        }
        
        calculation_error <- shiny::withProgress(
          message = "Calculating dissimilarity metrics",
          detail = "Preparing calculation...",
          value = 0,
          {
            tryCatch({
              kba_sf$cmi <- calc_dissimilarity(reserves_sf = kba_sf, reserves_id = "network", reference_sf = rv$refarea_reactive(), raster_layer = kba_cmi, raster_type = "continuous", plot_out_dir = file.path(plot_dir, "cmi"), progress = metric_progress("CMI"))
              kba_sf$lcc <- calc_dissimilarity(reserves_sf = kba_sf, reserves_id = "network", reference_sf = rv$refarea_reactive(), raster_layer = kba_lcc, raster_type = "categorical", categorical_class_values = df_label$values,
                                               plot_out_dir = file.path(plot_dir, "lcc"), categorical_class_labels = df_label, progress = metric_progress("LCC"))
              kba_sf$led <- calc_dissimilarity(reserves_sf = kba_sf, reserves_id = "network", reference_sf = rv$refarea_reactive(), raster_layer = kba_led, raster_type = "continuous", plot_out_dir = file.path(plot_dir, "led"), progress = metric_progress("LED"))
              kba_sf$gpp <- calc_dissimilarity(reserves_sf = kba_sf, reserves_id = "network", reference_sf = rv$refarea_reactive(), raster_layer = kba_gpp, raster_type = "continuous", plot_out_dir = file.path(plot_dir, "gpp"), progress = metric_progress("GPP"))
              
              if (!is.null(rv$layers_rv$criteria5)) {
                criteria_name <- rv$criteria5name()
                kba_sf[[criteria_name]] <- calc_dissimilarity(reserves_sf = kba_sf, reserves_id = "network", reference_sf = rv$refarea_reactive(), raster_layer = kba_criteria5, raster_type = "continuous", plot_out_dir = file.path(plot_dir, criteria_name), progress = metric_progress(criteria_name))
              }
              NULL
            }, error = function(e) {
              e
            })
          }
        )
        
        if (inherits(calculation_error, "error")) {
          error_occurred <- TRUE
          showModal(modalDialog(
            title = "Error calculating dissimilarity",
            paste( "Possible issues may involve partially overlapping objects or a reserve", "being too small relative to the raster resolution. Code error:",
              calculation_error$message
            ),
            easyClose = FALSE,
            footer = modalButton("OK")
          ))
        }
 
        kba_sf <- kba_sf %>%
          dplyr::select(-any_of(c("group_id", "Area_PB")))
        st_write(kba_sf, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = paste0("repKBAs_reduced", set_grid), driver = "GPKG", append = FALSE)
        rv$kba_sf_reactive(kba_sf)
      }else{
        kba_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = paste0("repKBAs_reduced", set_grid))
        if(attr(kba_sf, "sf_column") != "geometry"){
          kba_sf$geometry <- kba_sf$geom
        }
        
        rv$kba_sf_reactive(kba_sf)
        kba_up <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "upstream_KBAs")
        rv$kba_upstream_reactive(kba_up)
      }
      updated_grp <- c(rv$overlayGroup(), "Potential KBAs")
      rv$overlayGroup(updated_grp)
    } 
    
    if(input$assessKBAs == "Only PAs" || input$assessKBAs == "Both KBAs and PAs"){
      
      pas_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas")
      pas_up <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas_upstream") %>%
        dplyr::select(network)
      ###
      if (!("repPAs" %in% layers)) {
        if(attr(pas_sf, "sf_column") != "geometry"){
          pas_sf$geometry <- pas_sf$geom
        }
        error_occurred <- FALSE
        calculation_error <- shiny::withProgress(
          message = "Calculating dissimilarity metrics on PAs",
          detail = "Preparing calculation...",
          value = 0,
          {
            tryCatch({
              pas_sf$cmi <- calc_dissimilarity(reserves_sf = pas_sf, reserves_id = "network", reference_sf = rv$refarea_reactive(), raster_layer = kba_cmi, raster_type = "continuous", plot_out_dir = file.path(plot_dir, "cmi"), progress = metric_progress("CMI"))
              pas_sf$lcc <- calc_dissimilarity(reserves_sf = pas_sf, reserves_id = "network", reference_sf = rv$refarea_reactive(), raster_layer = kba_lcc, raster_type = "categorical", categorical_class_values = df_label$values,
                                               plot_out_dir = file.path(plot_dir, "lcc"), categorical_class_labels = df_label, progress = metric_progress("LCC"))
              pas_sf$led <- calc_dissimilarity(reserves_sf = pas_sf, reserves_id = "network", reference_sf = rv$refarea_reactive(), raster_layer = kba_led, raster_type = "continuous", plot_out_dir = file.path(plot_dir, "led"), progress = metric_progress("LED"))
              pas_sf$gpp <- calc_dissimilarity(reserves_sf = pas_sf, reserves_id = "network", reference_sf = rv$refarea_reactive(), raster_layer = kba_gpp, raster_type = "continuous", plot_out_dir = file.path(plot_dir, "gpp"), progress = metric_progress("GPP"))
              
              if (!is.null(rv$layers_rv$criteria5)) {
                criteria_name <- rv$criteria5name()
                pas_sf[[criteria_name]] <- calc_dissimilarity(reserves_sf = pas_sf, reserves_id = "network", reference_sf = rv$refarea_reactive(), raster_layer = kba_criteria5, raster_type = "continuous", plot_out_dir = file.path(plot_dir, criteria_name), progress = metric_progress(criteria_name))
              }
              NULL
            }, error = function(e) {
              e
            })
          }
        )
        
        if (inherits(calculation_error, "error")) {
          error_occurred <- TRUE
          showModal(modalDialog(
            title = "Error calculating dissimilarity",
            paste( "Possible issues may involve partially overlapping objects or a reserve", "being too small relative to the raster resolution. Code error:",
                   calculation_error$message
            ),
            easyClose = FALSE,
            footer = modalButton("OK")
          ))
        } 
        st_write(pas_sf, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "repPAs", driver = "GPKG", append = FALSE)
        rv$pas_sf_reactive(pas_sf)
      }else{
        pas_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "repPAs")
        rv$pas_sf_reactive(pas_sf)
        if(attr(pas_sf, "sf_column") != "geometry"){
          pas_sf$geometry <- pas_sf$geom
        }
        pas_up <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas_upstream")
        rv$pas_upstream_reactive(pas_up)
      }
    }
    
    # Close the modal once processing is done
    removeModal()
    
    showModal(modalDialog(
      title = "Mapping results from the representation analysis...",
      "Please wait...",
      footer = NULL
    ))
    
    groups_to_remove <- c(rv$reactive_labelKBA(), rv$reactive_labelNET())
    
    # Remove these groups from overlayGroups()
    rv$overlayGroups(setdiff(rv$overlayGroups(), groups_to_remove))
    
    #Delete previous dynamic label if
    labelKBA <- rv$reactive_labelKBA()
    refarea <- rv$refarea_reactive() %>% st_transform(4326)
    
    leafletProxy("map") %>%
      clearControls() %>%
      clearGroup(rv$reactive_labelKBA()) %>%
      clearGroup(rv$reactive_labelNET()) %>%
      addTiles() %>%
      addPolygons(data=refarea, color='#6b4b38', fill = F, weight=3, group="Reference area", options = leafletOptions(pane = "ground")) %>%
      addRasterImage(lcc_4326, colors=lcc_cols, opacity = 1, group="LCC",  maxBytes = 10 * 1024 * 1024) %>%
      addRasterImage(led_4326, colors=led_val.color, opacity = 1, group="LED",  maxBytes = 10 * 1024 * 1024) %>%
      addRasterImage(gpp_4326, colors=val.color, opacity = 1, group="GPP",  maxBytes = 10 * 1024 * 1024) %>%
      addRasterImage(cmi_4326, colors=val.color, opacity = 1, group="CMI",  maxBytes = 10 * 1024 * 1024) %>%
      
      addLegend(pal = led_xpal, values = values(led_4326), opacity = 1, title = "LED",
                position = "bottomright", group="LED", labFormat = labeller_function)  %>%
      addLegend(pal = gpp_xpal, values = values(gpp_4326), opacity = 1, title = "GPP",
                position = "bottomright", group="GPP", labFormat = labeller_function)  %>%
      addLegend(pal = cmi_xpal, values = values(cmi_4326), opacity = 1, title = "CMI",
                position = "bottomright", group="CMI", labFormat = labeller_function)  %>%
      addLegend(colors = lcc_cols, label = lcc_labels,  position=c("bottomleft"), opacity = 1, title = "LCC",
                group="LCC") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                       overlayGroups = c(rv$overlayGroups(), rv$legendcrit()),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Streams"))
    
    if(!is.null(rv$layers_rv$criteria5)){
      leafletProxy("map") %>%
        addRasterImage(crit5_4326, colors=val.color, opacity = 1, group=rv$criteria5name()) %>%
        addLegend(pal = crit_xpal, values = values(crit5_4326), opacity = 1, title = rv$criteria5name(),
                  position = "bottomright", group=rv$criteria5name(), labFormat = labeller_function)  %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                         overlayGroups = c(rv$overlayGroups(), rv$legendcrit(), rv$criteria5name()),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("Streams"))
    }
    
    if(input$assessKBAs == "Only KBAs"){
      rv$upstream_reactive(kba_up)
      rv$poly_reactive(kba_sf)
      
      # Update max upstream slider
      max_value <- as.integer(max(kba_sf$up_km2, na.rm = TRUE))
      # Update the slider input with the max value
      updateSliderInput(
        session = getDefaultReactiveDomain(),
        inputId = "slideUP",
        max = max_value
      )
    }
    if(input$assessKBAs == "Only PAs"){
      rv$upstream_reactive(pas_up)
      pas_sf <- pas_sf %>%
        dplyr::select(-NAME, -intact_km2)
      rv$poly_reactive(pas_sf)
      
      # Update max upstream slider
      max_value <- as.integer(max(pas_sf$up_km2, na.rm = TRUE))
      # Update the slider input with the max value
      updateSliderInput(
        session = getDefaultReactiveDomain(),
        inputId = "slideUP",
        max = max_value
      )
    }
    if(input$assessKBAs == "Both KBAs and PAs"){
      
      pas_up <- pas_up %>%
        dplyr::select(network)
      kbapas_up <- rbind(kba_up, pas_up)
      rv$upstream_reactive(kbapas_up)
      pas_sf <- pas_sf %>%
        dplyr::select(-any_of(c("NAME", "intact_km2")))
      kba_sf <- kba_sf %>%
        dplyr::select(-any_of("group_id"))
      kbapas_sf <- rbind(kba_sf, pas_sf)
      st_write(kbapas_sf, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = paste0("repKBAPAs_reduced", set_grid), driver = "GPKG", append = FALSE)
      rv$poly_reactive(kbapas_sf)
      
      # Update max upstream slider
      max_value <- as.integer(max(kbapas_sf$up_km2, na.rm = TRUE))
      # Update the slider input with the max value
      updateSliderInput(
        session = getDefaultReactiveDomain(),
        inputId = "slideUP",
        max = max_value
      )
    }
    
    unique_kbas <- unique(rv$poly_reactive()$network)
    updateSelectInput(getDefaultReactiveDomain(), "KBA", choices = unique_kbas)
    
    # Close the modal once processing is done
    removeModal()
  })
  
  #######################################
  ### Render map, tables and plot based on select KBA/PA
  observeEvent(input$KBA, {
    req(input$KBA)
    req(rv$poly_reactive())
    
    poly_sf_4326 <- rv$poly_reactive() %>% st_transform(4326)
    # Filter the `sf` object to get the selected KBA based on the input value
    selected_polygon <- poly_sf_4326 %>%
      filter(network == input$KBA) #%>%
    
    selected_up <- rv$upstream_reactive() %>%
      filter(network == input$KBA) %>%
      st_transform(4326)  # Make sure it's in the correct coordinate system for Leaflet
    
    # Remove these groups from overlayGroups()
    rv$overlayGroups(setdiff(rv$overlayGroups(), rv$reactive_labelKBA()))
    legend <- c(rv$overlayGroups(), input$KBA, "Upstream")
    rv$overlayGroups(legend)
    
    #Delete previous dynamic label
    labelKBA <- rv$reactive_labelKBA()
    labelNET <- rv$reactive_labelNET()
    
    
    # Highlight the selected KBA on the map
    leafletProxy("map") %>%
      clearGroup(input$KBA) %>%
      clearGroup(labelKBA) %>%
      clearGroup(labelNET) %>%
      clearGroup("Upstream") %>%  # Clear previous highlight
      addPolygons(data = selected_polygon, color = "black",  fillColor = "#989898", fillOpacity = 0.8, weight = 2, group = input$KBA) %>%
      addPolygons(data = selected_up, color = "blue",  fillColor = "blue", fillOpacity = 0.2, weight = 2, group = "Upstream") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                       overlayGroups = c(rv$overlayGroups(), rv$legendcrit()), 
                       options = layersControlOptions(collapsed = TRUE)
      )
    
    rv$reactive_labelKBA(input$KBA)
    
    ####################################################################################################
    # Render summary table
    ####################################################################################################
    # Prepare the table for display
    if(is.null(rv$layers_rv$criteria5)){
      x <- tibble(
        Variables = c("Area km2", "AWI (%)", "Upstream area km2", "Upstream AWI (%)", 
                      "DCI", "CMI", "GPP", "LED", "LCC"),
        Values = NA)
    }else{
      x <- tibble(
        Variables = c("Area km2", "AWI (%)", "Upstream area km2", "Upstream AWI (%)", 
                      "DCI", "CMI", "GPP", "LED", "LCC", rv$criteria5name()),
        Values = NA)
    }
    
    x$Values[x$Variables == "Area km2"] <- as.integer(selected_polygon$area_km2)
    x$Values[x$Variables == "AWI (%)"] <- round(as.numeric(selected_polygon$AWI) * 100, 3)
    x$Values[x$Variables == "Upstream area km2"] <- as.integer(selected_polygon$up_km2)
    x$Values[x$Variables == "Upstream AWI (%)"] <- round(as.numeric(selected_polygon$up_AWI) * 100, 2)
    x$Values[x$Variables == "DCI"] <- round(selected_polygon$dci, 3)
    x$Values[x$Variables == "CMI"] <- selected_polygon$cmi
    x$Values[x$Variables == "GPP"] <- selected_polygon$gpp
    x$Values[x$Variables == "LED"] <- selected_polygon$led
    x$Values[x$Variables == "LCC"] <- selected_polygon$lcc
    
    if(!is.null(rv$layers_rv$criteria5)){
      x$Values[x$Variables == rv$criteria5name()] <- round(selected_polygon[[rv$criteria5name()]], 3)
    }
    
    formatted_x <- x %>%
      mutate(
        Values = case_when(
          Variables %in% c("Area km2", "Upstream area km2") ~ as.character(as.integer(Values)),  # No decimals
          Variables %in% c("AWI (%)", "Upstream AWI (%)") ~ formatC(as.numeric(Values), format = "f", digits = 2),  # 2 decimals
          TRUE ~ formatC(as.numeric(Values), format = "f", digits = 3)  # 3 decimals for others
        )
      )
    
    output$outkba <- renderTable({
      formatted_x
    }, digits = 0)  # digits is ignored since we manually formatted the values
    
    ####################################################################################################
    # Render Rep Analysis PLOT per KBA
    ####################################################################################################
    # Define a route to serve images from the external directory
    shiny::addResourcePath("image", rv$plotDir())
    
    output$images <- renderUI({
      image_box <- function(title, src, id) {
        tags$div(id = id, class = "image-box",
          tags$div(class = "image-box-header", tags$span(title),
            tags$button(type = "button", class = "image-expand-btn", onclick = paste0("toggleImageBox('", id, "')"), title = "Expand image", 
                        tags$i(class = "fa fa-expand"))),
          tags$div(class = "image-container", tags$img(src = src, class = "kba-image"))
        )
      }
      
      boxes <- list(image_box("CMI", paste0("image/cmi/", input$KBA, ".png"), "cmi_box"),
                    image_box("GPP", paste0("image/gpp/", input$KBA, ".png"), "gpp_box"),
                    image_box("LED", paste0("image/led/", input$KBA, ".png"), "led_box"),
                    image_box("LCC", paste0("image/lcc/", input$KBA, ".png"), "lcc_box")
                   )
      
      if (!is.null(rv$layers_rv$criteria5)) {
        boxes[[5]] <- image_box(rv$criteria5name(), paste0("image/", rv$criteria5name(), "/", input$KBA, ".png"), "criteria5_box")
      }
      
      tags$div(class = "image-box-container", boxes)
    })
  })
  
  ################################################################################################
  # Apply filtering on KBAs
  ################################################################################################  
  observeEvent(input$filterRep, {
    req(rv$layers_rv$catchments)
    req(rv$poly_reactive())
    
    filetred_sf <- rv$poly_reactive()
    if(!is.null(rv$layers_rv$criteria5)){
      filtered_sf_rep <- filter(filetred_sf, lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & !!sym(rv$criteria5name()) <= input$slidecrit5 & up_km2 <= input$slideUP)
    }else{
      filtered_sf_rep <- filter(filetred_sf, lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & up_km2 <= input$slideUP)
    }
    
    if(input$assessKBAs== "Only PAs" || input$assessKBAs == "Both KBAs and PAs"){
      filtered_sf_rep <- filtered_sf_rep %>%
        filter(str_starts(network, "KBA") | (str_starts(network, "PA") & area_km2 > input$slidePAs))
    }
    
    rv$filtered_rep(filtered_sf_rep)
    
    if(nrow(filtered_sf_rep)>0){
      showModal(modalDialog(
        title = "Processing",
        "Filter KBAs based on dissimilarity metrics threshold. Please wait...",
        footer = NULL
      ))
      # Extract unique KBA values for selectInput
      unique_kbas <- unique(filtered_sf_rep$network)
      
      # Update selectInput choices based on filtered KBA values
      updateSelectInput(getDefaultReactiveDomain(), "KBA", choices = unique_kbas)
      
      if(input$assessKBAs == "Only KBAs"){
        filtered_sf_4326 <- filtered_sf_rep %>% st_transform(4326)
        rv$filtered_kba(filtered_sf_4326)
        leafletProxy("map") %>%
          clearGroup('Potential KBAs') %>% 
          #clearGroup('Protected areas') %>% 
          addPolygons(data = filtered_sf_4326, color = 'purple', fillColor = "transparent", fillOpacity = 0, weight = 3,
                      layerId = filtered_sf_4326$network, popup = ~network, group = "Potential KBAs", 
                      options = leafletOptions(pane = "over")) %>%
          addLayersControl(position = "topright",
                           overlayGroups = c(rv$overlayGroups(), "Potential KBAs", rv$legendcrit()),
                           options = layersControlOptions(collapsed = TRUE)) %>%
          hideGroup(c("Streams"))
        
        # Update specific rows based on a condition or manually
        x <- rv$outfreqkba()
        x <- x %>% 
          mutate(Count = case_when(Variables == "Filtered KBAs" ~ ifelse(!is.null(filtered_sf_rep), nrow(filtered_sf_rep), NA_integer_),
                                   TRUE ~ Count)  # Keep existing values for other rows
          )
      }
      if(input$assessKBAs == "Only PAs"){
        filtered_sf_4326 <- filtered_sf_rep %>% st_transform(4326)
        rv$filtered_pas(filtered_sf_4326)
        leafletProxy("map") %>%
          clearGroup('Protected areas') %>% 
          clearGroup('Potential KBAs') %>% 
          addPolygons(data = filtered_sf_4326, color='#6b4b38', fillOpacity = 0.4, weight=2, layerId = filtered_sf_4326$network, popup = ~network, group = "Protected areas", 
                      options = leafletOptions(pane = "over")) %>%
          addLayersControl(position = "topright",
                           overlayGroups = c(rv$overlayGroups(), rv$legendcrit()),
                           options = layersControlOptions(collapsed = TRUE)) %>%
          hideGroup(c("Streams"))
        
        # Update specific rows based on a condition or manually
        x <- rv$outfreqkba()
        x <- x %>% 
          mutate(Count = case_when(Variables == "Filtered PAs" ~ ifelse(!is.null(filtered_sf_rep), nrow(filtered_sf_rep), NA_integer_),
                                   TRUE ~ Count)  # Keep existing values for other rows
          )
      }
      if(input$assessKBAs == "Both KBAs and PAs"){
        kba <- rv$kba_sf_reactive()[rv$kba_sf_reactive()$network %in% rv$filtered_sf_rep$network,]
        rv$filtered_kba(kba)
        kba_4326 <- kba %>% st_transform(4326)
        pas <- rv$pas_sf_reactive()[rv$pas_sf_reactive()$network %in% rv$filtered_sf_rep$network,]
        rv$filtered_pas(pas)
        pas_4326 <- pas %>% st_transform(4326)
        
        leafletProxy("map") %>%
          clearGroup('Protected areas') %>% 
          clearGroup('Potential KBAs') %>%
          addPolygons(data = kba_4326, color = 'black', fillColor = "transparent", fillOpacity = 0, weight = 2,
                      layerId = kba_4326$network, popup = ~network, group = "Potential KBAs", 
                      options = leafletOptions(pane = "over")) %>%
          addPolygons(data = pas_4326, color='#6b4b38', fillOpacity = 0.4, weight=2, layerId = pas_4326$network, popup = ~network, group = "Protected areas", 
                      options = leafletOptions(pane = "over")) %>%
          addLayersControl(position = "topright",
                           overlayGroups = c(rv$overlayGroups(), "Potential KBAs", rv$legendcrit()),
                           options = layersControlOptions(collapsed = TRUE)) %>%
          hideGroup(c("Streams"))
        
        # Update specific rows based on a condition or manually
        x <- rv$outfreqkba()
        x <- x %>% 
          mutate(Count = case_when(Variables == "Filtered KBAs" ~ ifelse(!is.null(kba), nrow(kba), NA_integer_),
                                   Variables == "Filtered PAs" ~ ifelse(!is.null(pas), nrow(pas), NA_integer_),
                                   TRUE ~ Count)  # Keep existing values for other rows
          )
      }
      
      # Close the modal once processing is done
      removeModal()
      
      rv$outfreqkba(x)
      output$outkbafreq <- renderTable({
        rv$outfreqkba()
      })
      
    }else{
      leafletProxy("map") %>%
        clearGroup('Potential KBAs') %>%
        clearGroup('Protected areas') %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                         overlayGroups = c(rv$overlayGroups(), rv$legendcrit()),
                         options = layersControlOptions(collapsed = TRUE))
      
      showModal(modalDialog(
        title = "No KBA and/or protected area reache those threshold",
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
    }
  })
  
  
  ################################################################################################
  # Download KBAs
  ################################################################################################  
  observeEvent(input$downloadKBA, {
    
    if(input$assessKBAs == "Only KBAs"){
      prefix <- paste0("repKBAs_", sub(".*(reduced.*)", "\\1", input$KBAlayer))
    }else if(input$assessKBAs == "Only PAs"){
      prefix <- "repPAs_"
    }else{
      prefix <- paste0("repKBAPAs_", sub(".*(reduced.*)", "\\1", input$KBAlayer))
    }
    
    filtered_sf_rep <- rv$filtered_rep()
    
    if(is.null(filtered_sf_rep)){
      showModal(modalDialog(
        title = "No filtering has been applied",
        "Please apply filtering prior to save filtered KBAs.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    req(filtered_sf_rep)
    outName <- paste0(prefix, "_up", as.character(input$slideUP), "_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),"_led", as.character(input$slideLED),
                      "_lcc", as.character(input$slideLCC))
    
    if(!is.null(rv$layers_rv$criteria5)){
      outName <- paste0(outName, "_", rv$criteria5name(), as.character(input$slideNETcrit5))
      subfolders <- c("cmi", "lcc", "gpp", "led", rv$criteria5name())
    }else{
      subfolders <- c("cmi", "lcc", "gpp", "led")
    }
    
    st_write(filtered_sf_rep, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = outName, driver = "GPKG", append = FALSE)
    
    d <- sub("rep","plot", outName)
    source_parent_dir <- file.path(rv$outdir(), "output/plot")
    destination_parent_dir <- file.path(rv$outdir(), "output", d)
    
    # Ensure the destination subdirectories exist
    for (subfolder in subfolders) {
      dir.create(file.path(destination_parent_dir, subfolder), recursive = TRUE, showWarnings = FALSE)
    }
    
    # Get the list of networks from the sf object
    network_names <- filtered_sf_rep$network
    
    # Iterate over each subfolder
    for (subfolder in subfolders) {
      for (network in network_names) {
        # Define source and destination file paths
        source_file <- file.path(source_parent_dir, subfolder, paste0(network, ".PNG"))
        destination_file <- file.path(destination_parent_dir, subfolder, paste0(network, ".PNG"))
        
        # Check if the source file exists before copying
        if (file.exists(source_file)) {
          file.copy(source_file, destination_file, overwrite = TRUE)
        }
      }
    }
    
    showModal(modalDialog(
      title = "Filtered KBAs/protected_areas downloaded",
      paste0("Filtered KBAs/protected_areas were downloaded in the KBA_analysis.gpkg  under the name ", outName, " found in ", rv$outdir(), "/output"),
      easyClose = TRUE,
      footer = modalButton("OK"))
    )
  })
}