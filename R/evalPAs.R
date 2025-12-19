evalPAsServer <- function(input, output, session, project, map, rv){

  # Observe map click events to update the selected polygon
  observeEvent(input$map_shape_click, {
    selected_polygon(input$map_shape_click$id)  # Store the layerId of the clicked polygon
  })
  
  # RENDER PAs UI
  output$dciPAs <- renderUI({
    tagList(
      if (is.null(rv$layers_rv$pas_sf)) {
        fileInput(inputId = "upload_pas", label = "Upload protected areas shapefile", multiple = TRUE)
      } else {
        div(HTML('<i class="fa fa-thumb-tack" style="color:#d9534f; "></i>'),
            "Protected areas already uploaded.",
            style = "font-size:15px; margin-left:20px; margin-top:20px;")
      },
      div("Calculate hydrology metrics (PAs)", style = "font-size: 15px; font-weight: bold; margin-left: 15px;margin-top: 40px;"),
      div(style = "margin: 13px; font-size:13px; font-weight: bold", "Calculate DCI and add upstream attributes to KBAs"), 
      actionButton(inputId = "calc_pasdci", label = div(style = "font-size:13px;background-color:gey;color: black",HTML("Run")), class = "btn-warning", style="width:250px")                     
    )
  })
  
  ####################################################################################################
  # -Calculate hydro metrics
  ####################################################################################################
  observeEvent(input$calc_pasdci, {
    #Test on required layers
    if(is.null(rv$layers_rv$pas_sf)){
      showModal(modalDialog(
        title = "No protected areas layer has been uploaded",  
        "Please upload a shapefile" ,
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
      return()
    }
    #Test if streams are uploaded
    if (is.null(rv$layers_rv$streams)) {
      # Create the modal dialog
      showModal(modalDialog(
        title = "Missing Data",
        "Stream layer is missing. Please go back to Set input parameters to upload the stream layer.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    req(rv$layers_rv$pas_sf)
    req(rv$layers_rv$catchments)
    req(rv$layers_rv$streams)
    
    showModal(modalDialog(
      title = "Processing",
      "Calculating hydrology metrics on protected areas. Please wait...",
      footer = NULL
    ))
    
    ####################################################################################################
    # Intactness
    ####################################################################################################
    catchments <- rv$layers_rv$catchments
    pas_sf <- rv$layers_rv$pas_sf %>%
      mutate(network = sprintf("PA_%02d", row_number()),
              area_km2 = st_area(.)/1000000,
      )
    
    pas_catch <- st_intersection(pas_sf, catchments)
    area_catch <- pas_catch %>%
      mutate(catch_awi = as.numeric(st_area(.)) * .[[input$intactColname]]) %>%
      st_drop_geometry() %>%
      group_by(network) %>%
      summarize(intact_km2 = sum(catch_awi, na.rm = TRUE)/1000000)
    pas <- merge(pas_sf[,c("network", "NAME", "area_km2")], area_catch[,c("network", "intact_km2")], by = "network", all.x = TRUE)
    pas$AWI <- round(pas$intact_km2/pas$area_km2, 3)
      
    #Upstream
    results_list <- list()
      
    # Compute upstream catchments for all polygons (if possible)
    upstream_catchments_list <- lapply(1:nrow(pas), function(i) {
      get_upstream_catchments(pas[i, ], "network", catchments)
    })
      
    # Use mapply to iterate and return the results efficiently
    results_list <- mapply(function(pa_id, upstream_list) {
      if (nrow(upstream_list) == 0) return(NULL)
        
      # Filter catchments for upstream list
      area_intact <- catchments[catchments$CATCHNUM %in% upstream_list[[pa_id]], ] %>%
        st_drop_geometry() %>%
        mutate(up_cAWI = as.numeric(Area_total * .[[input$intactColname]]), 
               network = pa_id) %>%
        group_by(network) %>%
        summarize(up_intactkm2 = sum(up_cAWI, na.rm = TRUE)/1000000, .groups = "drop")
        
      # Dissolve and merge upstream areas
      upstream_area <- dissolve_catchments_from_table(catchments, upstream_list, "network")
        
       upstream_area <- upstream_area %>%
        st_buffer(dist = 20) %>% 
        st_buffer(dist = -20)
        
      upstream_area <- upstream_area %>%
          left_join(area_intact[, c("network", "up_intactkm2")], by = "network") %>%
          mutate(up_km2 = st_area(.)/1000000,
                 up_AWI = round(up_intactkm2 / as.numeric(up_km2), 3))
        
      return(upstream_area)
    }, pa_id = pas$network, upstream_list = upstream_catchments_list, SIMPLIFY = FALSE)
      
    pas_up <- do.call(rbind, results_list)
      
    # Export  and update reactive value 
    st_write(pas_up, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas_upstream", driver = "GPKG", append = FALSE)
    rv$pas_upstream_reactive(pas_up)
      
    pas_up <- pas_up %>% st_drop_geometry()
    pas <- merge(pas, pas_up[,c("network","up_km2", "up_AWI")], by = "network", all.x= TRUE)
      
    ####################################################################################################
    # Calculate DCI
    ####################################################################################################
    pas$dci <- calc_dci(conservation_area_sf = pas, 
                        stream_sf = rv$layers_rv$streams)
    # Export  and update reactive value 
    st_write(pas, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas", driver = "GPKG", append = FALSE)
    rv$layers_rv$pas_sf <- pas
      
    # Close the modal once processing is done
    removeModal()
    
    showModal(modalDialog(
      title = "Hydrology metrics added",
      easyClose = TRUE,
      footer = modalButton("OK"))
    ) 
    
    ####################################################################################################
    # -Render PAs map
    ####################################################################################################
    pas_4326 <- st_transform(pas, 4326)
    leafletProxy("map") %>%
      clearGroup("Protected areas") %>%
      addPolygons(data=pas_4326, color='#6b4b38', fillOpacity = 0.6, weight=2, layerId = pas_4326$network, popup = ~network, group="Protected areas", options = leafletOptions(pane = "over")) 
    
    
    ####################################################################################################
    # -Render bottom PAs statistics table
    ####################################################################################################
    outtabPA <- reactive({
      req(input$tabs == 'tabPAs')
      pas <- rv$layers_rv$pas_sf %>%
        st_drop_geometry()
      
      final <- pas %>%
        mutate(PA_ID = network,
               Name = NAME,
               'Area (km2)' = round(area_km2,3),
               'AWI (%)' = AWI,
               DCI = dci, 
               'Upstream area (km2)' = round(up_km2,3),
               'Upstream AWI (%)' = up_AWI) %>%
        dplyr::select(PA_ID, Name, 'Area (km2)', 'AWI (%)', DCI, 'Upstream area (km2)', 'Upstream AWI (%)')
      
      return(final)
    })
    
    output$pastbl <- renderDataTable({
      req(input$tabs == 'tabPAs')
      # Get the reactive data and the selected polygon ID
      table_data <- outtabPA()
      selected_id <- rv$selected_polygon()
      if (is.null(selected_id)) {selected_id <- ""}  # Default to no selection if nothing is clicked
      
      # Create a vector of background colors
      highlight_colors <- ifelse(
        table_data$PA_ID == selected_id,  # Match the selected polygon ID
        "yellow",  # Highlight the matching row
        "white"    # Default background for other rows
      )
      
      # Create the datatable and apply conditional highlight on selected
      datatable(table_data, caption = 'Protected areas statistics', rownames = FALSE, options = list(dom = 'tip', scrollX = TRUE, pageLength = 5),
                class = "compact") %>%
        formatStyle('PA_ID', target = 'row', backgroundColor = styleEqual(
          table_data$PA_ID,  
          highlight_colors))  # Apply corresponding background colors
    })
  })
}