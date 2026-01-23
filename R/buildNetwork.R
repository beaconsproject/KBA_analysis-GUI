buildNetServer <- function(input, output, session, project, map, rv){
  
  #output$netPAs <- renderUI({
  #  req(rv$layers_rv$pas_sf)
    
  #  tagList(
  #    br(),
  #    div(style = "margin-top: -30px;",checkboxInput("forcePAs", label = "Include all PAs in the network", value = F)),
  #    #br(),
  #    #actionButton("confirm_project", "Confirm", class = "btn-warning", style="width:200px")
  #  )
    
  #})
  
  observeEvent(input$tabs, {
    req(input$tabs == "tabNET", rv$outdir())
    
    layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    #rep_kba <- layers[grepl("^rep", layers)]
    rep_kba <- layers[grepl("^KBAs_reduced", layers)]
    pas_ls <- layers[grepl("^protected_areas", layers) & !grepl("protected_areas_upstream", layers)]
    #net_list <- c(rep_kba, reduced_kba, pas_ls)
    net_list <- c(rep_kba, pas_ls)
    if (length(rep_kba) > 0) { 
      updatePickerInput(session = getDefaultReactiveDomain(), "KBArep", choices = net_list, selected = net_list[1])
    } else{
      showModal(modalDialog(
        title = "Layer missing", "You need to assess representation on either KBAs or PAs prior to build a network.",
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
      return()
    }
    
    if ("protected_areas" %in% layers) {
      pas_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas")
      x <- rv$outfreqnet()
      x <- x %>% 
        mutate(Count = case_when(Variables == "PAs" ~  nrow(pas_sf),
                                 Variables == "Filtered PAs" ~ ifelse(!is.null(rv$filtered_pas()), nrow(rv$filtered_pas()), NA_integer_),
                                 TRUE ~ Count))
      rv$outfreqnet(x)
      
      output$outnetfreq <- renderTable({
        rv$outfreqnet()
      })
    } 
  })
  
  observeEvent(input$KBArep, {
    if (input$tabs == "tabNET") {
      #req(!(is.null(input$KBArep)))
      layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
      layers <- layers_info$name
      # Initialize kba_sf and pas_sf as NULL
      kba_sf <- NULL
      if (!(is.null(input$KBArep))){
        kba_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = input$KBArep)
      } 
      
      x <- rv$outfreqnet()
      x <- x %>% 
        mutate(Count = case_when(Variables == "KBAs" ~  nrow(kba_sf),
                                 Variables == "Filtered KBAs" ~ ifelse(!is.null(rv$filtered_kba()), nrow(rv$filtered_kba()), NA_integer_),
                                 TRUE ~ Count))
      rv$outfreqnet(x)
      
      output$outnetfreq <- renderTable({
        rv$outfreqnet()
      })
      
      output$slideNETcrit5 <- renderUI({
        # Check if criteria5() is NULL
        if (!is.null(rv$layers_rv$criteria5)) {
          # If criteria5 is NULL, render the sliderInput with disabled = TRUE
          div(style = "margin-top: -30px;", sliderInput("slideNETcrit5", label = rv$criteria5name(), min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE))
        }
      })
    }
  })
  
  observeEvent(input$buildNet, {
    
    if(input$intactColname == "Please select"){
      showModal(modalDialog(
        title = "Missing intactness column",
        "You must select the column representing the level of intactness in your catchment layer (rangion from 0-1).",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    req(input$set_net)
    req(rv$layers_rv$catchments)
    req(!(input$KBArep==""))
    kba_sf <- NULL
    pas_sf <- NULL
    
    layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    potential_kbas <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = input$KBArep)
    potential_kbas <- potential_kbas %>%
      dplyr::select(network, AWI, area_km2, up_km2, up_AWI, dci)
    
    if ("protected_areas" %in% layers) {
      pas_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas")
    }
    
    if(input$forcePAs){
      if (is.null(pas_sf)){
        showModal(modalDialog(
          title = "Hydrology metrics were not calclulated on protected areas layers", "Make sure protected areas are uploaded in the Set input parameters step",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
        return()
      }else{
        pas_sf <- pas_sf %>% 
          dplyr::select(-NAME, -intact_km2) 
        
        agg_pa_name <- pas_sf %>%
          dplyr::pull(network) %>%     # Extract the `network` column
          unique() %>%                 # Get unique values
          sort() %>%                   # Sort values (optional)
          paste(collapse = "__")
        
        pas_sf <- pas_sf %>% 
          filter(!network %in% potential_kbas$network)
        
        potential_kbas <- rbind(potential_kbas, pas_sf)
        rv$poly_reactive(potential_kbas)
      }
    } else {
      agg_pa_name <- NULL
      rv$poly_reactive(potential_kbas)
    }
    
    showModal(modalDialog(
      title = "Processing",
      "Building network. Please wait...",
      footer = NULL
    ))
    
    #Define outdir name
    if(input$forcePAs){
      netName <- gsub("rep", "", input$KBArep)
      outName <- paste0("net",  netName, "_n", input$set_net, "_includePAs")
    }else{
      netName <- gsub("rep", "", input$KBArep)
      outName <- paste0("net", netName, "_n", input$set_net)
    }
    network_dir <- paste0("output/plot", outName)
    rv$netDir(network_dir)
  
    # Raise warning on `input$set_net`
    if (is.null(input$set_net) || as.integer(input$set_net) < 2) {
      if(isFALSE(input$forcePAs)){
        showModal(modalDialog(
          title = "A minimum of 2 potential KBAs per network is required",
          "Please adjust the network settings to include at least 2 KBAs.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return() 
      }
    }else if(as.integer(input$set_net) > nrow(rv$poly_reactive())){
      showModal(modalDialog(
        title = "The number of KBAs set per network is above the number of KBAs available.",
        "Please revise the number of KBA per network.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # Check if there is a 5 criteria and store the name
    if (!is.null(input$upload_custom)) {
      rastName <- sub("\\..*$", "", input$upload_custom$name)
      rv$criteria5name(rastName)
      updated_grp <- c(rv$legendcrit(), rastName)
      rv$legendcrit(updated_grp) # Update the reactive value
    }
    if (!is.null(input$csv_file)) {
      csv_data <- read.csv(input$csv_file$datapath)
      req_layers <- c("CMI", "LED", "GPP", "LCC", "catchments", "stream", "planning region", "protected areas", "reference area")
      unexpected_layers <- csv_data$Layer[!csv_data$Layer %in% req_layers]
      rv$criteria5name(unexpected_layers)
      updated_grp <- c(rv$legendcrit(), unexpected_layers)
      rv$legendcrit(updated_grp) # Update the reactive value
    }
    
    #Prep criteria
    if (!file.exists(file.path(rv$outdir(), "output/kba_cmi.tif"))) {
      cmi <- process_raster(rv$layers_rv$cmi, rv$refarea_reactive(), rv$outdir(), "kba_cmi", fact = 2, aggregation_fun = "mean")
      kba_cmi <- cmi$original
      cmi_4326 <- cmi$projected
    }else{
      kba_cmi <- rv$layers_rv$cmi
      cmi_4326 <- raster(file.path(rv$outdir(), "output/kba_cmi_4326.tif"))
    }
    if (!file.exists(file.path(rv$outdir(), "output/kba_led.tif"))) {
      led <- process_raster(rv$layers_rv$led, rv$refarea_reactive(), rv$outdir(), "kba_led", fact = 4, aggregation_fun = "mean")
      kba_led <- led$original
      led_4326 <- led$projected
    } else{
      kba_led <- rv$layers_rv$led
      led_4326 <- raster(file.path(rv$outdir(), "output/kba_led_4326.tif"))
    }
    if (!file.exists(file.path(rv$outdir(), "output/kba_gpp.tif"))) {
      gpp <- process_raster(rv$layers_rv$gpp, rv$refarea_reactive(), rv$outdir(), "kba_gpp", fact = 4, aggregation_fun = "mean")
      kba_gpp <- gpp$original
      gpp_4326 <- gpp$projected
    } else{
      kba_gpp <- rv$layers_rv$gpp
      gpp_4326 <- raster(file.path(rv$outdir(), "output/kba_gpp_4326.tif"))
    }
    if (!file.exists(file.path(rv$outdir(), "output/kba_lcc.tif"))) {
      lcc <- process_raster(rv$layers_rv$lcc, rv$refarea_reactive(), rv$outdir(), "kba_lcc", fact = 40, aggregation_fun = "modal", ignored = c(15, 17))
      kba_lcc <- lcc$original
      lcc_4326 <- lcc$projected
    }else{
      kba_lcc <- rv$layers_rv$lcc
      lcc_4326 <- raster(file.path(rv$outdir(), "output/kba_lcc_4326.tif"))
    }
    
    if(!is.null(rv$layers_rv$criteria5)){
      if (!file.exists(file.path(rv$outdir(), "output", paste0(rv$criteria5name(), ".tif")))) {
        crit5 <- process_raster(rv$layers_rv$criteria5, rv$refarea_reactive(), rv$outdir(), rv$criteria5name(), fact = 4)
        kba_criteria5 <- crit5$original
        crit5_4326 <- crit5$projected
      }else{
        kba_criteria5 <- raster(file.path(rv$outdir(), "output", paste0(rv$criteria5name(), ".tif")))
        crit5_4326 <- raster(file.path(rv$outdir(), "output", paste0(rv$criteria5name(), "_4326.tif")))
      }
    }else{
      kba_criteria5 <- NULL
    } 
    
    # Access legend elements
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
    
    layer_to_check <- outName
    if (!layer_to_check %in% layers) {
      potential_kbas <- rv$poly_reactive()
      if(attr(potential_kbas, "sf_column") != "geometry"){
        potential_kbas$geometry <- potential_kbas$geom
      }
      
      if(nrow(potential_kbas)>0){
        
        if(input$forcePAs){
          rep_pas <- layers[grepl("^repPAs", layers)]
          pas_ls <- layers[grepl("^protected_areas", layers) & !grepl("protected_areas_upstream", layers)]
          pas_list <- c(rep_pas, pas_ls)
          if(input$KBArep %in% pas_list){
            k <- nrow(potential_kbas)
            network_names <- gen_network_names(in_names = potential_kbas$network, k = k, force_in = agg_pa_name)
          }else{
            k <- nrow(pas_sf) + as.numeric(input$set_net)
            network_names <- gen_network_names(in_names = potential_kbas$network, k = k, force_in = agg_pa_name)
          }
        }else {
          k <- as.numeric(input$set_net)
          network_names <- gen_network_names(in_names = potential_kbas$network, k = k)
        }
        
        #Check and remove overlapping KBAs. 
        overlaps <- list_overlapping_polygons(conservation_areas_sf = potential_kbas, conservation_areas_id = "network")
        network_names <- network_names[!network_names %in% overlaps]
        
        if(length(network_names)==0){
          showModal(modalDialog(
            title = "Error: Potential KBAs provided can't be used to build networks.",
            " Potential KBAs overlap within the network, which prevent the creation of network.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()  # Stop further execution
        }
        
        # Build the list of networks using the conservation area polygons. Each network will become a single feature in the polygon object.
        networks_sf <- build_network_polygons(conservation_areas_sf = potential_kbas, conservation_areas_id = "network", network_list = network_names)
        
        result_awi <- lapply(1:nrow(networks_sf), function(i) {
          # Union NET and intersect  with catchments
          net_diss <- st_union(networks_sf[i,]) 
          area_km2 <- net_diss %>% st_area(.)/1000000
          net_catch <- st_intersection(rv$layers_rv$catchments, net_diss)
          
          #Calculate total area and intactness for NET
          AWI <- net_catch %>%
            mutate(catch_awi = as.numeric(st_area(.)) * .[[input$intactColname]]) %>%
            st_drop_geometry() %>%
            summarize(AWI = sum(catch_awi, na.rm = TRUE) / 1000000) %>%
            pull(AWI)
          
          #Return a tibble
          tibble(network = networks_sf$network[i],
                 area_km2 = round(as.numeric(area_km2,2)),
                 AWI = round((AWI/as.numeric(area_km2)),4))
        })
        
        
        # Combine all results into a single dataframe
        net_awi <- bind_rows(result_awi)
        
        networks_sf <- networks_sf %>%
          left_join(net_awi[, c("network", "area_km2", "AWI")], by = "network")
        
        #Upstream
        results_list <- list()
        
        # Compute upstream catchments for all polygons (if possible)
        upstream_list <- lapply(1:nrow(networks_sf), function(i) {
          get_upstream_catchments(networks_sf[i, ], "network", rv$layers_rv$catchments)
        })
        
        # Use mapply to iterate and return the results efficiently
        results_list <- mapply(function(pa_id, upstream_list) {
          if (nrow(upstream_list) == 0) return(NULL)
          
          # Dissolve and merge upstream areas
          net_name <- colnames(upstream_list)
          colnames(upstream_list) <- "network"
          upstream_area <- dissolve_catchments_from_table(rv$layers_rv$catchments, upstream_list, "network", calc_area = TRUE, intactness_id  = input$intactColname)
          upstream_area$network <- net_name
          
          st_agr(upstream_area) <- "constant"
          upstream_area <- upstream_area %>%
            st_buffer(dist = 20) %>% 
            st_buffer(dist = -20) %>%
            dplyr::rename(up_km2 = area_km2,
                          up_AWI = AWI) %>%
            mutate(up_km2 = round(up_km2,2),
                   up_AWI = round(up_AWI,4))
          
          return(upstream_area)
        }, pa_id = networks_sf$network, upstream_list = upstream_list, SIMPLIFY = FALSE)
        
        upstream_network_sf <- do.call(rbind, results_list)
        
        if(!is.null(agg_pa_name)){
          # Remove elements of agg_pa_name from the network string
          upstream_network_sf <- upstream_network_sf  %>% 
            mutate(network = str_remove_all(network, paste(agg_pa_name, collapse = "|")),
                   network = str_replace_all(network, "(__)+", "__"),
                   network = str_replace(network, "^__|__$", ""),
                   network = if_else(network =="", "PAs", paste0(network, "__PAs")))
          
          #upstream_network_sf <-upstream_network_sf %>% 
          #  mutate(network = str_replace_all(network, agg_pa_name, "PAs"))                 
        }
        
        #rv$upstream_network_reactive(upstream_network_sf)
        if(!is.null(upstream_network_sf)){
          rv$upstream_network_reactive(upstream_network_sf)
          st_write(upstream_network_sf, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = paste0("upstream_", outName), driver = "GPKG", append = TRUE)
        }else{
          showModal(modalDialog(
            title = "No upstream area found for those network. Layer KBA_upstream won't be created.",
            easyClose = TRUE,
            footer = modalButton("OK"))
          )
        }
        
        #Fix network name
        if(!is.null(agg_pa_name)){
          networks_sf <- networks_sf  %>% 
            mutate(network = str_remove_all(network, paste(agg_pa_name, collapse = "|")),
                   network = str_replace_all(network, "(__)+", "__"),
                   network = str_replace(network, "^__|__$", ""),
                   network = if_else(network =="", "PAs", paste0(network, "__PAs")))
          #networks_sf <-networks_sf %>% 
          #  mutate(network = str_replace_all(network, agg_pa_name, "PAs"))                 
        }
        
        upstream_att <- upstream_network_sf %>%
          st_drop_geometry()
        
        networks_sf <- networks_sf %>%
          left_join(upstream_att[, c("network", "up_km2", "up_AWI")], by = "network")
        # DCI (ON HOLD)
        #networks_sf$dci <- calc_dci(conservation_area_sf = networks_sf, stream_sf = streams())
        
        # calculate dissimilarity metric 
        error_occurred <- FALSE
        tryCatch({
          networks_sf$lcc <- round(calc_dissimilarity(networks_sf, reserves_id="network", rv$refarea_reactive(), kba_lcc, 'categorical', plot_out_dir=file.path(rv$outdir(), network_dir,"lcc"), categorical_class_labels = df_label),3)
          networks_sf$led <- round(calc_dissimilarity(networks_sf, reserves_id="network", rv$refarea_reactive(), kba_led, 'continuous', plot_out_dir=file.path(rv$outdir(), network_dir,"led")),3)
          networks_sf$cmi <- round(calc_dissimilarity(networks_sf, reserves_id="network", rv$refarea_reactive(), kba_cmi, 'continuous', plot_out_dir=file.path(rv$outdir(), network_dir,"cmi")),3)
          networks_sf$gpp <- round(calc_dissimilarity(networks_sf, reserves_id="network", rv$refarea_reactive(), kba_gpp, 'continuous', plot_out_dir=file.path(rv$outdir(), network_dir,"gpp")),3)
          if(!is.null(rv$criteria5name())){
            networks_sf[[rv$criteria5name()]] <- round(calc_dissimilarity(networks_sf, reserves_id="network", rv$refarea_reactive(), kba_criteria5, 'continuous', plot_out_dir=file.path(rv$outdir(), network_dir, criteria5name())),3) 
          }
        }, error = function(err) {
          error_occurred <- TRUE
          showModal(modalDialog(
            title = "Error calculating disimilarity",
            paste("Possible issues may involve partially overlapping objects or the reserve's size being too small relative to the raster's resolution. Code error returns:", err$message),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        })
        if (error_occurred) {
          return(NULL)  # Stop execution of the rest of the observer
        }
        st_write(networks_sf, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = outName, driver = "GPKG", append = TRUE)
        rv$network_reactive(networks_sf)
      }else{
        showModal(modalDialog(
          title = "No network fulffill criterai threshold.",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      }
    }else{
      networks_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = layer_to_check)
      rv$network_reactive(networks_sf)
      upstream_networks_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = paste0("upstream_", outName))
      rv$upstream_network_reactive(upstream_networks_sf)
    }
    
    # Extract unique KBA values for selectInput
    unique_network <- unique(networks_sf$network)
    
    # Update selectInput choices based on filtered KBA values
    updateSelectInput(getDefaultReactiveDomain(), "network", choices = unique_network)
    
    networks_4326 <- st_transform(networks_sf, 4326)
    labelKBA <- rv$reactive_labelKBA()
    #pas_4326 <- pas_sf %>% st_transform(4326)
    
    leafletProxy("map") %>%
      clearControls() %>%
      clearGroup(labelKBA) %>%
      clearGroup("Potential KBAs") %>%
      clearGroup("Protected areas") %>%
      clearGroup("Upstream") %>%  # Clear previous highlight
      clearGroup("CMI") %>%
      clearGroup("LED") %>%
      clearGroup("LCC") %>%
      clearGroup("GPP") %>%
      removeControl("legend_LCC") %>%
      removeControl("legend_LED") %>%
      removeControl("legend_GPP") %>%
      removeControl("legend_CMI") %>%
      #addPolygons(data=pas_4326, color='#6b4b38', fillOpacity = 0.6, weight=2, layerId = pas_4326$network, popup = ~network, group="Protected areas", options = leafletOptions(pane = "layer2")) %>%
      addRasterImage(lcc_4326, colors=lcc_cols, opacity = 1, group="LCC",  maxBytes = 5 * 1024 * 1024) %>%
      addRasterImage(led_4326, colors=led_val.color, opacity = 1, group="LED",  maxBytes = 5 * 1024 * 1024) %>%
      addRasterImage(gpp_4326, colors=val.color, opacity = 1, group="GPP",  maxBytes = 5 * 1024 * 1024) %>%
      addRasterImage(cmi_4326, colors=val.color, opacity = 1, group="CMI",  maxBytes = 5 * 1024 * 1024) %>%
      
      addLegend(pal = led_xpal, values = values(led_4326), opacity = 1, title = "LED",
                position = "bottomright", group="LED", layerId = "legend_LED", labFormat = labeller_function)  %>%
      addLegend(pal = gpp_xpal, values = values(gpp_4326), opacity = 1, title = "GPP",
                position = "bottomright", group="GPP", layerId = "legend_GPP", labFormat = labeller_function)  %>%
      addLegend(pal = cmi_xpal, values = values(cmi_4326), opacity = 1, title = "CMI",
                position = "bottomright", group="CMI", layerId = "legend_CMI", labFormat = labeller_function)  %>%
      addLegend(colors = lcc_cols, label = lcc_labels,  position=c("bottomleft"), opacity = 1, title = "LCC",
                group="LCC", layerId = "legend_LCC") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                       overlayGroups = c(rv$overlayGroups(), rv$legendcrit()),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Streams"))
    
    if(!is.null(rv$layers_rv$criteria5)){
      crit5_4326 <- raster(file.path(rv$outdir(), "output", paste0(rv$criteria5name(), "_4326.tif")))
      leafletProxy("map") %>%
        clearGroup(rv$criteria5name()) %>%
        removeControl("legend_custom") %>%
        addRasterImage(crit5_4326, colors=val.color, opacity = 1, group=rv$criteria5name()) %>%
        addLegend(pal = crit_xpal, values = values(crit5_4326), opacity = 1, title = rv$criteria5name(),
                  position = "bottomright", group=criteria5name(), layerId = "legend_custom", labFormat = labeller_function)  %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                         overlayGroups = c(rv$overlatGroups(), rv$legendcrit()),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("Streams"))
    }
    
    # Close the modal once processing is done
    removeModal()
    
    # Update specific rows based on a condition or manually
    x <- rv$outfreqnet()
    x <- x %>% 
      mutate(Count = case_when(Variables == "Networks" ~ ifelse(!is.null(networks_4326), nrow(networks_4326), NA_integer_),
                               TRUE ~ Count)  # Keep existing values for other rows
      )
    rv$outfreqnet(x)
    
    output$outkbafreq <- renderTable({
      rv$outfreqkba()
    })
    
    # Update max upstream slider
    max_value <- as.integer(max(networks_sf$up_km2, na.rm = TRUE))
    updateSliderInput(
      session = getDefaultReactiveDomain(), 
      inputId = "slideNETUP", 
      max = max_value
    )
  })
  
  ################################################################################################
  # Filter Network
  ################################################################################################  
  observeEvent(input$filterNet, {
    req(rv$layers_rv$catchments)
    req(rv$network_reactive())
    
    network_sf <- rv$network_reactive()
    # criteria5
    if(!is.null(rv$layers_rv$criteria5)){
      network_sf_rep <- filter(network_sf, lcc <= input$slideNETLCC & gpp <= input$slideNETGPP & cmi <= input$slideNETCMI & led <= input$slideNETLED & !!sym(rv$criteria5name()) <=input$slideNETcrit5 & up_km2 <= input$slideNETUP)
    }else{
      network_sf_rep <- filter(network_sf, lcc <= input$slideNETLCC & gpp <= input$slideNETGPP & cmi <= input$slideNETCMI & led <= input$slideNETLED &  up_km2 <= input$slideNETUP)
    }
    
    x <- outfreqnet()
    x$Count[x$Variables=="Filtered networks"] <- nrow(network_sf_rep)
    outfreqnet(x) 
    
    if(nrow(network_sf_rep)>0){
      showModal(modalDialog(
        title = "Processing",
        "Filter KBAs based on dissimilarity metrics threshold. Please wait...",
        footer = NULL
      ))
      # Extract unique KBA values for selectInput
      unique_net <- unique(network_sf_rep$network)
      
      # Update selectInput choices based on filtered KBA values
      updateSelectInput(getDefaultReactiveDomain(), "network", choices = unique_net)
      
      # Close the modal once processing is done
      removeModal()
    }else{
      showModal(modalDialog(
        title = "No network reaches those threshold",
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
    }
  })
  
  observeEvent(input$network, {
    req(input$network)
    req(rv$network_reactive())
    
    # Filter the `sf` object to get the selected KBA based on the input value
    selected_net <- rv$network_reactive() %>%
      filter(network == input$network) %>%
      st_transform(4326)  # Make sure it's in the correct coordinate system for Leaflet
    
    selected_up <- rv$upstream_network_reactive() %>%
      filter(network == input$network) %>%
      st_transform(4326)  # Make sure it's in the correct coordinate system for Leaflet
    
    #Dynamic label
    if(is.null(rv$reactive_labelNET())){
      rv$reactive_labelNET(input$network)
    }
    labelNET <- rv$reactive_labelNET()
    
    # Highlight the selected KBA on the map
    leafletProxy("map") %>%
      clearGroup('Potential KBAs') %>%
      clearGroup(labelNET) %>%
      clearGroup("Upstream") %>%  # Clear previous highlight
      addPolygons(data = selected_net, color = "black",  fillColor = "#989898", fillOpacity = 0.9, weight = 3, layerId = ~network,  # Ensure each polygon has a unique ID
                  group = input$network) %>%
      addPolygons(data = selected_up, color = "blue",  fillColor = "blue", fillOpacity = 0.2, weight = 2, group = "Upstream") %>%
      addLayersControl(
        position = "topright",
        baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
        overlayGroups = c(rv$overlayGroups(), input$network, "Upstream", rv$legendcrit()),
        options = layersControlOptions(collapsed = TRUE)
      )
    rv$reactive_labelNET(input$network)
  })
  
  ####################################################################################################
  # Render Rep Analysis Stats and plot per network
  ####################################################################################################
  observeEvent(input$network, {
    req(input$network)  # Ensure there is a selected KBA
    
    if(is.null(rv$layers_rv$criteria5)){
      # Prepare the table for display
      x <- tibble(
        Variables = c("Area km2", "AWI (%)", "Upstream area km2", "Upstream AWI (%)", 
                      "DCI", "CMI", "GPP", "LED", "LCC"),
        Values = NA
      )
    }else{
      # Prepare the table for display
      x <- tibble(
        Variables = c("Area km2", "AWI (%)", "Upstream area km2", "Upstream AWI (%)", 
                      "DCI", "CMI", "GPP", "LED", "LCC", criteria5name()),
        Values = NA
      )
    }
    
    # Get the filtered polygons and select the one matching the KBA choice
    potential_net <- rv$network_reactive()
    selected_network <- potential_net[potential_net$network == input$network, ]
    
    x$Values[x$Variables == "Area km2"] <- as.numeric(st_area(selected_network))/1000000
    x$Values[x$Variables == "AWI (%)"] <- as.numeric(selected_network$AWI) * 100
    x$Values[x$Variables == "Upstream area km2"] <- selected_network$up_km2
    x$Values[x$Variables == "Upstream AWI (%)"] <- as.numeric(selected_network$up_AWI) * 100
    #(ON HOLD)x$Values[x$Variables == "DCI"] <- round(selected_network$dci, 3)
    x$Values[x$Variables == "DCI"] <- NA
    x$Values[x$Variables == "CMI"] <- selected_network$cmi
    x$Values[x$Variables == "GPP"] <- selected_network$gpp
    x$Values[x$Variables == "LED"] <- selected_network$led
    x$Values[x$Variables == "LCC"] <- selected_network$lcc
    
    if(!is.null(rv$layers_rv$criteria5)){
      x$Values[x$Variables == rv$criteria5name()] <- round(selected_network[[rv$criteria5name()]], 3)
    }
    
    formatted_x <- x %>%
      mutate(
        Values = case_when(
          Variables %in% c("Area km2", "Upstream area km2") ~ as.character(as.integer(Values)),  # No decimals
          Variables %in% c("AWI (%)", "Upstream AWI (%)") ~ formatC(as.numeric(Values), format = "f", digits = 2),  # 2 decimals
          TRUE ~ formatC(as.numeric(Values), format = "f", digits = 3)  # 3 decimals for others
        )
      )
    
    output$outnet <- renderTable({
      formatted_x
    }, digits = 0)  # digits is ignored since we manually formatted the values
    
    #############################
    # Render Rep Analysis PLOT per NET
    # Define a route to serve images from the external directory
    shiny::addResourcePath("imageNET", file.path(rv$outdir(), rv$netDir()))
    
    output$images <- renderUI({
      tagList(
        tags$div(style = "display: flex; flex-wrap: wrap; justify-content: space-around;", 
                 tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                          tags$h3("CMI"),  # Title
                          tags$img(src = paste0("imageNET/cmi/", input$network, ".png"), height = "400px", width = "300px")
                 ),
                 tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                          tags$h3("GPP"),  # Title
                          tags$img(src = paste0("imageNET/gpp/", input$network, ".png"), height = "400px", width = "300px")
                 ),
                 tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                          tags$h3("LED"),  # Title 
                          tags$img(src = paste0("imageNET/led/", input$network, ".png"), height = "400px", width = "300px")
                 ),
                 tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                          tags$h3("LCC"),  # Title 
                          tags$img(src = paste0("imageNET/lcc/", input$network, ".png"), height = "400px", width = "300px")
                 ),
                 if(!is.null(rv$layers_rv$criteria5)){
                   tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                            tags$h3(criteria5name()),  # Title 
                            tags$img(src = paste0("imageNET/", criteria5name(), "/", input$network, ".png"), height = "400px", width = "300px")
                   )
                 }
        )
      )
    })
  })
}
