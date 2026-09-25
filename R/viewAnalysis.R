viewServer <- function(input, output, session, project, map, rv){
  
  observeEvent(input$tabs, {
    req(input$tabs == "tabVIEW", rv$outdir())
    if(file.exists(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))){
      layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
      layers <- layers_info$name
      rep_kba <- layers[grepl("^(rep|net)", layers)]
      if (length(rep_kba) > 0) { 
        updateSelectInput(session = getDefaultReactiveDomain(), "repLayer", choices = rep_kba, selected = rep_kba[1])
      }else{
        showModal(modalDialog(
          title = "Layers missing", "You need to run analysis prior to review the results.",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
        return()
      }
      if("protected_areas" %in% layers){
        pas <- st_read(file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas")
        rv$layers_rv$pas_sf <- pas
      }
    }else{
      showModal(modalDialog(
        title = "Layers missing", "You need to run analysis prior to review the results.",
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
      return()
    }
  })
  
  observeEvent(input$repLayer, {
    req(input$tabs == "tabVIEW", input$repLayer)
    if(input$repLayer != "No layer found"){
      kba_sf <- st_read(file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = input$repLayer)
      x <- rv$outfreqview()
      x <- x %>% 
        mutate(Count = case_when(Variables == "KBAs/PAs/Networks" ~  nrow(kba_sf),
                                 TRUE ~ Count))
      rv$outfreqview(x)
      
      output$outviewfreq <- renderTable({
        rv$outfreqview()
      })
    }else{
      showModal(modalDialog(
        title = "Layers missing", "You need to run analysis prior to review the results.",
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
      return()
    }
  })
  #########################################################
  #-RUN REPRESENTATION
  #########################################################
  observeEvent(input$review, {
    req(input$repLayer != "No layer found")
    
    showModal(modalDialog(
      title = "Mapping the layers", "Please wait...",
      easyClose = TRUE,
      footer = modalButton("OK"))
    )
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
    
    kba_sf <- st_read(file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = input$repLayer)
    rv$kba_sf_reactive(kba_sf)
    rv$plotDir(file.path(rv$outdir(), "output/plot",  input$repLayer))
    
    x <- rv$outfreqview()
    x <- x %>% 
      mutate(Count = case_when(Variables == "KBAs/PAs/Networks" ~  nrow(kba_sf),
                               TRUE ~ Count))
    rv$outfreqview(x)
    
    kba_id <- unique(kba_sf$network)
    updateSelectInput(session = getDefaultReactiveDomain(), "KBA_net", choices = kba_id, selected = kba_id[1])
    
    if (grepl("PAs", input$repLayer, fixed = TRUE)) {
      up_sf <- dplyr::bind_rows(st_read(file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "upstream_KBAs"),
                                st_read(file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas_upstream")
      )
      rv$upstream_reactive(up_sf)
    } else {
      up_sf <- st_read(file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "upstream_KBAs")
      rv$upstream_reactive(up_sf)
    }
    
    #Prep criteria
    cmi_4326<- rast(file.path(rv$outdir(), "output/kba_cmi_4326.tif"))
    led_4326 <- rast(file.path(rv$outdir(), "output/kba_led_4326.tif"))
    gpp_4326 <- rast(file.path(rv$outdir(), "output/kba_gpp_4326.tif"))
    lcc_4326 <- rast(file.path(rv$outdir(), "output/kba_lcc_4326.tif"))
    
    if(!is.null(rv$layers_rv$criteria5)){
      crit5_4326 <- rast(file.path(rv$outdir(), "output", paste0(rv$criteria5name(), "_4326.tif")))
    }else{
      crit5_4326 <- NULL
    }
    
    # Access elements
    legend_data <- prep_legend(cmi_4326, led_4326, gpp_4326, lcc_4326, crit5_4326)
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
    
    #Delete previous dynamic label if
    labelKBA <- rv$reactive_labelVIEW()
    refarea <- rv$refarea_reactive() %>% st_transform(4326)
    rv$overlayGroups(setdiff(rv$overlayGroups(), "Potential KBAs"))
    kba_4326 <- st_transform(kba_sf, 4326)
    
    leafletProxy("map") %>%
      clearControls() %>%
      clearGroup(rv$reactive_labelVIEW()) %>%
      clearGroup('Potential KBAs') %>%
      clearGroup('Protected areas') %>%
      addTiles() %>%
      addPolygons(data=refarea, color='#6b4b38', fill = F, weight=3, group="Reference area", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=kba_4326, color = 'black', fillColor = "transparent", fillOpacity = 0, weight = 2,  group="Potential KBAs/PAs/network", options = leafletOptions(pane = "over")) %>%
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
                       overlayGroups = c(rv$overlayGroups(), "Potential KBAs/PAs/network", rv$legendcrit()),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Streams"))
    
    if(!is.null(rv$layers_rv$pas_sf)){
      pas_4326 <- rv$layers_rv$pas_sf %>% st_transform(4326)
      leafletProxy("map") %>%
        addPolygons(data = pas_4326, color = '#993300', fillColor = "transparent", fillOpacity = 0, weight = 3,  group="Protected areas", options = leafletOptions(pane = "over")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                         overlayGroups = c(rv$overlayGroups(), "Potential KBAs/PAs/network", rv$legendcrit()),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("Streams"))
    }
    
    if(!is.null(rv$layers_rv$criteria5)){
      crit5_4326 <- rv$layers_rv_4326$criteria5
      leafletProxy("map") %>%
        addRasterImage(crit5_4326, colors=val.color, opacity = 1, group=rv$criteria5name()) %>%
        addLegend(pal = crit_xpal, values = values(crit5_4326), opacity = 1, title = rv$criteria5name(),
                  position = "bottomright", group=rv$criteria5name(), labFormat = labeller_function)  %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                         overlayGroups = c(rv$overlayGroups(), "Potential KBAs/PAs/network", rv$legendcrit(), rv$criteria5name()),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("Streams"))
    }
    
    # Close the modal once processing is done
    removeModal()
  })
  
  
  # RENDER KBA FILTERING UI
  output$slideVIEW <- renderUI({
    req(input$review)
    req(rv$kba_sf_reactive())
    
    tagList(
      div("Filter KBAs and/or PAs based on dissimilarity metrics (DMs), upstream area and PAs area",
          style = "font-size: 14px; font-weight: bold; margin-top: 20px; margin-left: 20px;"),
      div("DMs range from 0 to 1. 0 = low dissimilarity or high representation, 1 = high dissimilarity or low representation",
          style = "font-size: 12px; margin-left: 20px; margin-top: 20px;"),
      
      sliderInput("slideCMI_view", "CMI:", min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE),
      sliderInput("slideLED_view", "LED:", min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE),
      sliderInput("slideGPP_view", "GPP:", min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE),
      sliderInput("slideLCC_view", "LCC:", min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE),
      
      uiOutput("slidercrit5_view"),
      
      sliderInput("slideUP_view", "Maximum upstream area (sq.km):", min = 0, max = 100000, value = 25000, step = 1000, ticks = FALSE),
      sliderInput("slidePAs_view", "Minimum PAs area (sq.km):", min = 0, max = 10000, value = 5, step = 100, ticks = FALSE),
      
      actionButton("filterVIEW", "Apply filtering", icon = icon("filter"), class = "btn-primary", style = "width:250px"),
      
      div(style = "margin-top: 20px;", actionButton("downloadVIEW", "Download Filtered KBAs", icon = icon("download"), class = "btn-warning", style = "width:250px")))
  })  
  
  #######################################
  ### Render map, tables and plot based on select KBA/PA
  observeEvent(input$KBA_net, {
    req(input$KBA_net)
    poly_sf_4326 <- rv$kba_sf_reactive() %>% st_transform(4326)
    # Filter the `sf` object to get the selected KBA based on the input value
    selected_polygon <- poly_sf_4326 %>%
      filter(network == input$KBA_net) 
    
    selected_up <- rv$upstream_reactive() %>%
      filter(network == input$KBA_net) %>%
      st_transform(4326)  
    
    # Remove these groups from overlayGroups()
    rv$overlayGroups(setdiff(rv$overlayGroups(), rv$reactive_labelVIEW()))
    legend <- c(rv$overlayGroups(), input$KBA_net, "Upstream")
    rv$overlayGroups(legend)
    
    #Delete previous dynamic label
    labelKBA <- rv$reactive_labelKBA()
    labelNET <- rv$reactive_labelNET()
    labelVIEW <-rv$reactive_labelVIEW()
    
    # Highlight the selected KBA on the map
    leafletProxy("map") %>%
      clearGroup(input$KBA) %>%
      clearGroup(labelKBA) %>%
      clearGroup(labelNET) %>%
      clearGroup(labelVIEW) %>%
      clearGroup("Upstream") %>%  # Clear previous highlight
      addPolygons(data = selected_polygon, fillColor='purple', color= "#000000", weight = 1, group = input$KBA_net) %>%
      addPolygons(data = selected_up, color = "blue",  fillColor = "blue", fillOpacity = 0.2, weight = 2, group = "Upstream") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                       overlayGroups = c(rv$overlayGroups(), "Potential KBAs/PAs/network", rv$legendcrit()), 
                       options = layersControlOptions(collapsed = TRUE)
      )
    
    rv$reactive_labelVIEW(input$KBA_net)
    
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
    if ("dci" %in% names(selected_polygon) || grepl("^net", input$repLayer)) {
      x$Values[x$Variables == "DCI"] <- NA
    } else {
      x$Values[x$Variables == "DCI"] <- round(selected_polygon$dci, 3)
    }
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
    
    output$outview <- renderTable({
      formatted_x
    }, digits = 0)  # digits is ignored since we manually formatted the values
    
    ####################################################################################################
    # Render Rep Analysis PLOT per KBA
    ####################################################################################################
    # Serve images from the external directory
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
      
      boxes <- list(image_box("CMI", paste0("image/cmi/", input$KBA_net, ".png"), "cmi_box"),
                    image_box("GPP", paste0("image/gpp/", input$KBA_net, ".png"), "gpp_box"),
                    image_box("LED", paste0("image/led/", input$KBA_net, ".png"), "led_box"),
                    image_box("LCC", paste0("image/lcc/", input$KBA_net, ".png"), "lcc_box")
      )
      
      if (!is.null(rv$layers_rv$criteria5)) {
        boxes[[5]] <- image_box(rv$criteria5name(), paste0("image/", rv$criteria5name(), "/", input$KBA_net, ".png"), "criteria5_box")
      }
      
      tags$div(class = "image-box-container", boxes)
    })
  })  
  
  ################################################################################################
  # Apply filtering on KBAs
  ################################################################################################  
  observeEvent(input$filterVIEW, {
    req(input$KBA_net)
    
    filetred_sf <- rv$kba_sf_reactive()
    if(!is.null(rv$layers_rv$criteria5)){
      filtered_sf_rep <- filter(filetred_sf, lcc <= input$slideLCC_view & gpp <= input$slideGPP_view & cmi <= input$slideCMI_view & led <= input$slideLED_view & !!sym(rv$criteria5name()) <= input$slidecrit5_view & up_km2 <= input$slideUP_view)
    }else{
      filtered_sf_rep <- filter(filetred_sf, lcc <= input$slideLCC_view & gpp <= input$slideGPP_view & cmi <= input$slideCMI_view & led <= input$slideLED_view & up_km2 <= input$slideUP_view)
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
      updateSelectInput(getDefaultReactiveDomain(), "KBA_net", choices = unique_kbas)
      
      filtered_sf_4326 <- filtered_sf_rep %>% st_transform(4326)
      rv$filtered_kba(filtered_sf_4326)
      leafletProxy("map") %>%
        clearGroup('Potential KBAs/PAs/network') %>% 
        addPolygons(data = filtered_sf_4326, color = 'black', fillColor = "transparent", fillOpacity = 0, weight = 2,
                    layerId = filtered_sf_4326$network, popup = ~network, group = "Potential KBAs/PAs/network", 
                    options = leafletOptions(pane = "over")) %>%
        addLayersControl(position = "topright",
                         overlayGroups = c(rv$overlayGroups(), "Potential KBAs/PAs/network", rv$legendcrit()),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("Streams"))
      
      # Update specific rows based on a condition or manually
      x <- rv$outfreqview()
      x <- x %>% 
        mutate(Count = case_when(Variables == "Filtered KBAs/PAs/Networks" ~ ifelse(!is.null(filtered_sf_rep), nrow(filtered_sf_rep), NA_integer_),
                                 TRUE ~ Count)  # Keep existing values for other rows
        )
      # Close the modal once processing is done
      removeModal()
      
      rv$outfreqview(x)
      output$outviewfreq <- renderTable({
        rv$outfreqview()
      })
    }else{
      leafletProxy("map") %>%
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
  observeEvent(input$downloadVIEW, {
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
    prefix <- sub("_up.*$", "", input$repLayer)
    
    outName <- paste0(prefix, "_up", as.character(input$slideUP_view), "_cmi", as.character(input$slideCMI_view),"_gpp", as.character(input$slideGPP_view),"_led", as.character(input$slideLED_view),
                      "_lcc", as.character(input$slideLCC_view))
    
    if(!is.null(rv$layers_rv$criteria5)){
      outName <- paste0(outName, "_", rv$criteria5name(), as.character(input$slideNETcrit5_view))
      subfolders <- c("cmi", "lcc", "gpp", "led", rv$criteria5name())
    }else{
      subfolders <- c("cmi", "lcc", "gpp", "led")
    }
    browser()
    st_write(filtered_sf_rep, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = outName, driver = "GPKG", append = FALSE)
    
    source_parent_dir <- file.path(rv$outdir(), "output/plot", input$repLayer)
    destination_parent_dir <- file.path(rv$outdir(), "output/plot", outName)
    
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