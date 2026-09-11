buildKBAServer <- function(input, output, session, project, map, rv){
  
  
  # RENDER ASSESS REPRESENTATION UI
  output$forceseed <- renderUI({
    req(input$tabs == "tabinput")
    req(input$seedRefARea)
    tagList(
      div("Select reference area.", style = "font-size: 15px; font-weight: bold; margin-left: 15px; margin-top: 20px;"),
      if (is.null(rv$refarea_reactive())) {
        fileInput("upload_refarea", "Upload reference area shapefile", multiple = TRUE)
      } else {
        div(HTML('<i class="fa fa-thumb-tack" style="color:#d9534f; "></i>'), "Reference area already uploaded.", style = "font-size: 12px; margin-top: 20px; margin-left: 30px;")
      }
    )
  })
  
  
  # Observe when the dataset is loaded and update the selectInput choices
  observe({
    req(rv$layers_rv$catchments)  # Ensure the catchments data is available
    catchment_data <- rv$layers_rv$catchments
    # Assuming catchment_data is a dataframe or sf object, extract column names
    colnames <- names(catchment_data)
    
    colzone <- ifelse("ZONE" %in% colnames,  "ZONE", "Please select")
    # Update the choices of the selectInput elements with column names
    updateSelectInput(session = getDefaultReactiveDomain(), "zoneColname", choices = c("Please select", colnames), selected = colzone)
    updateSelectInput(session = getDefaultReactiveDomain(), "arealandColname", choices = colnames, selected="Area_land")
  })
  
  ####################################################################################################
  # -Create BUILDER input
  ####################################################################################################
  observeEvent(input$runBuilderInput, {
    
    #Test if catchments are uploaded
    if (is.null(rv$layers_rv$catchments)) {
      # Create the modal dialog
      showModal(modalDialog(
        title = "Missing Data",
        "Catchments layer is missing. Please go back to Set input parameters to upload catchments layer.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    req(rv$layers_rv$catchments)
    req(rv$dirpath())
    req(rv$project_name())
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Creating BUILDER input. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    out_dir <- rv$outdir()
    
    # Generate neighbours table for catchments - Builder_input file for Builder. Skip this step is nghbrs.csv already exists.
    if (!is.null(input$upload_nghbr)) {
      nghbrs_path <- input$upload_nghbr$datapath
      nghbrs <- read.csv(nghbrs_path)
      rv$nghbrs_reactive(nghbrs)
      write.csv(nghbrs, file=file.path(out_dir,"Builder_input/nghbrs.csv"), row.names=FALSE) # Convert neighbours table to csv file.
    }else{
      nghbrs <- neighbours(rv$layers_rv$catchments)
      rv$nghbrs_reactive(nghbrs)
      write.csv(nghbrs, file=file.path(out_dir,"Builder_input/nghbrs.csv"), row.names=FALSE) # Convert neighbours table to csv file.
    }
    # Create seed list - input file for Builder that identifies where construction of conservation area is to start
    # intact ranges from 0 to 1 and is the minimum required proporational intactness required for a catchment to be a seed (0.8 = 80%)
    # areatarget_value is in m2 and specifies the desired conservation area size (10,000 km2 = 10000000000 m2)
    if (!is.null(input$upload_seed)) {
      seed_path <- input$upload_seed$datapath
      seed <- read.csv(seed_path)
      rv$seed_reactive(seed)
      write.csv(seed, file=file.path(out_dir,"Builder_input/seeds.csv"), row.names=FALSE) # Convert neighbours table to csv file.
    }else{
      if(input$seedRefARea){
        req(rv$refarea_reactive())
        catchments <- rv$layers_rv$catchments[st_within(rv$layers_rv$catchments, rv$refarea_reactive(), sparse = FALSE),]
        seed <- catchments %>%
          filter(input$intactColname >= input$seedintact, STRAHLER == as.numeric(input$set_strahler)) %>%
          seeds(catchments_sf = ., areatarget_value = as.numeric(input$set_areatarget))
        rv$seed_reactive(seed)
        write.csv(seed, file=file.path(out_dir,"Builder_input/seeds.csv"), row.names=FALSE) # Convert neighbours table to csv file.
      }else{
        seed <- rv$layers_rv$catchments %>%
          filter(input$intactColname >= input$seedintact, STRAHLER == as.numeric(input$set_strahler)) %>%
          seeds(catchments_sf = ., areatarget_value = as.numeric(input$set_areatarget))
        rv$seed_reactive(seed)
        write.csv(seed, file=file.path(out_dir,"Builder_input/seeds.csv"), row.names=FALSE) # Convert neighbours table to csv file.
      }
    }
    removeModal()
  })
  
  observeEvent(rv$nghbrs_reactive(), {
    req(rv$nghbrs_reactive())
    removeModal()
    updateActionButton(session, "runBuilderInput", label = "Builder input now set!", icon = icon("check", lib = "font-awesome"))
  })
  
  
  ####################################################################################################
  # -Run BUILDER
  ####################################################################################################
  observeEvent(input$runBuilder>0, { 
    req(rv$layers_rv$catchments)
    req(rv$outdir())
    
    out_dir <- rv$outdir()
    
    seed <- rv$seed_reactive()
    if(is.null(seed)){
      showModal(modalDialog(
        title = "Missing seed",
        "Builder input is missing. Please go back to Create Builder input to create seed table.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    nghbrs <- rv$nghbrs_reactive()
    if(is.null(nghbrs)){
      showModal(modalDialog(
        title = "Missing neighbour table",
        "Builder input is missing. Please go back to Create Builder input to create neighbour table.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    if(input$zoneColname == "Please select"){
      showModal(modalDialog(
        title = "Missing Zone",
        "Please select the column in the catchments table that represent the zone.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    showModal(modalDialog(
      title = "Running BUILDER",
      "Please wait...",
      footer = NULL
    ))
    builder_tab <- NULL
    tryCatch({
      builder_tab <- builder(catchments_sf = rv$layers_rv$catchments,
                             data_source = "catchment",
                             seeds = seed, 
                             reserve_name= NULL,
                             neighbours = nghbrs,
                             out_dir = file.path(out_dir, "Builder_output"),
                             builder_local_path = rv$dirpath(),
                             catchment_level_intactness = as.numeric(input$catchintact), #value from 0 to 1
                             conservation_area_intactness = as.numeric(input$CAintact), 
                             area_target_proportion = 1,
                             area_type = input$areatypeColname, #options are land, water, or landwater
                             construct_conservation_areas = TRUE,
                             area_target_multiplier = 1, #value from 0 to 1
                             handle_isolated_catchments = TRUE,
                             output_upstream = TRUE,
                             output_downstream = TRUE,
                             output_hydrology_metrics = TRUE,
                             area_land = input$arealandColname, 
                             area_water = "Area_water",
                             skeluid = "SKELUID",
                             catchnum = "CATCHNUM",
                             subzone = "FDA_M",
                             zone = input$zoneColname,
                             basin = "BASIN",
                             order1 = "ORDER1",
                             order2 = "ORDER2",
                             order3 = "ORDER3",
                             stream_length = "length_m",
                             intactness = input$intactColname,
                             isolated = "Isolated",
                             unique_identifier = "KBA",
                             handler_summary = FALSE,
                             summary_intactness_props = "\"\"",
                             summary_area_target_props = "\"\"")
    }, error = function(err) {
      # Close the "Please wait" modal if it is open
      removeModal()
      
      # Show an error modal with the error message
      showModal(modalDialog(
        title = "Error Running BUILDER: ", 
        paste("The following error occurred:", conditionMessage(err)),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    })
    
    # Fix PB to KBA
    req(builder_tab)
    builder_tab <- builder_tab %>%
      rename_with(~ str_replace(.x, "PB", "KBA"))
    
    # Convert conservation areas created by builder to polygons.(NOTE: poly_sf is the R object with conservation areas.)
    poly_sf <- dissolve_catchments_from_table(catchments_sf = rv$layers_rv$catchments, 
                                              input_table = builder_tab, 
                                              out_feature_id = "network")
    poly_sf <- poly_sf %>%
      st_buffer(dist = 20) %>% 
      st_buffer(dist = -20)
    
    rv$kba_sf_reactive(poly_sf)  # Store the poly_sf in reactiveVal
    
    # Append the first layer to the GeoPackage
    st_write(poly_sf, dsn = file.path(out_dir, "output/KBA_analysis.gpkg"), layer = "KBAs_builder", driver = "GPKG", append = FALSE)
    
    # Close the modal once processing is done
    removeModal()
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Builder output created.",
      paste0("Number of KBAs created: ", as.character(nrow(poly_sf)), ". Please wait..."),
      easyClose = TRUE,
      footer = NULL)
    )
    
    #groups_to_remove <- c(
    #  "Potential KBAs", "Upstream", rv$reactive_labelKBA(), rv$reactive_labelNET(),
    #  "CMI", "LED", "GPP", "LCC", rv$criteria5name()
    #)
    groups_to_remove <- c("Potential KBAs", "Upstream")
    
    # Remove these groups from overlayGroups()
    rv$overlayKBA(setdiff(rv$overlayKBA(), groups_to_remove))
    legend <- c(rv$overlayKBA(), "Potential KBAs (all)")
    rv$overlayKBA(legend)
    
    kba_sf_4326 <- st_transform(rv$kba_sf_reactive(), 4326) %>% st_simplify(dTolerance = 0.001)
    leafletProxy("map") %>%
      clearControls() %>%
      clearGroup('Potential KBAs') %>%
      clearGroup('Upstream') %>%
      clearGroup(rv$reactive_labelKBA()) %>%
      clearGroup(rv$reactive_labelNET()) %>%
      clearGroup("CMI") %>%
      clearGroup("LED") %>%
      clearGroup("GPP") %>%
      clearGroup("LCC") %>%
      clearGroup(rv$criteria5name()) %>%
      addPolygons(data=kba_sf_4326, color='black', fillColor = "transparent", fillOpacity = 0, weight=1, group="Potential KBAs (all)", options = leafletOptions(pane = "over")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                       overlayGroups = c(rv$overlayGroups(), rv$overlayKBA()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams"))
    
    x <- tibble(
      Variables = c("KBAs", "Reduced KBAs"),
      Count = c(NA, NA))  
    
    if (!is.null(rv$kba_sf_reactive())) {
      x <- x %>% 
        mutate(Count = case_when(Variables == "KBAs" ~  nrow(rv$kba_sf_reactive()),
                                 TRUE ~ Count))
    }
    
    rv$outfreqhydro(x) 
    
    output$outkbahydro <- renderTable({
      rv$outfreqhydro()
    })
    #######
    
    #Test if streams  and catchments are uploaded
    if (is.null(rv$layers_rv$streams) || is.null(rv$layers_rv$catchments)) {
      if(!is.null(rv$layers_rv$streams)){
        showModal(modalDialog(
          title = "Missing Data",
          "Catchments dataset is missing. Please go back to Set input parameters to upload the catchments dataset",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      } else if(!is.null(rv$layers_rv$catchments)){
        showModal(modalDialog(
          title = "Missing Data",
          "Streams dataset is missing. Please go back to Set input parameters to upload the streams dataset",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      } else{
        showModal(modalDialog(
          title = "Missing Data",
          "Catchments and streams dataset are missing. Please go back to Set input parameters to upload both dataset.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
      return()
    }
    
    req(rv$layers_rv$streams)
    req(rv$layers_rv$catchments)
    #layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
    #layers <- layers_info$name
    #layer_to_check <- "KBAs_reducedFALSE"
    
    #if (!layer_to_check %in% layers) {
    #showModal(modalDialog(
    #  title = "Processing",
    #  paste0("Calculating hydrology metrics on ", as.character(nrow(rv$kba_sf_reactive())), " features. Please wait..."),
    #  footer = NULL
    #))
    
    
    # Identify the attributes file and read it
    attributefile <- list.files(file.path(rv$outdir(),"Builder_output"), pattern = "Unique_BAs_attributes")
    attributeStats <- read.csv(file.path(rv$outdir(), "Builder_output", tail(attributefile, 1)))
    
    # Rename column in attributeStats in order to join it with poly_sf
    attributeStats <- attributeStats %>%
      dplyr::rename(network = PBx)
    
    # Join metrics
    poly_sf <- poly_sf %>%
      left_join(attributeStats %>%
                  dplyr::select(network , Area_PB, AWI_PB)) %>%
      mutate(Area_KBA = as.integer(Area_PB/1000000))
    
    poly_sf <- poly_sf %>%
      as.data.frame() %>%                # Convert to data frame, drops `agr`
      dplyr::rename(area_km2 = Area_KBA) %>% # Rename column
      dplyr::rename(AWI = AWI_PB)%>%  
      st_as_sf()
    
    # UPSTREAM AREA (up_km2) AND UPSTREAM INTACTNESS (up_AWI) can be found in the Builder output - see file "*_HYDROLOGY_METRICS.csv"
    # Identify the Hydro metrics file and read it
    hydrofile <- list.files(file.path(rv$outdir(), "Builder_output"), pattern = "HYDROLOGY_METRICS")
    hydroStats <- read.csv(file.path(rv$outdir(), "Builder_output", tail(hydrofile, 1)))
    
    # Fix PB to KBA
    hydroStats <- hydroStats %>%
      mutate(PBx = stringr::str_replace(PBx, "PB", "KBA"))
    
    # Rename column in hydroStats in order to join it with poly_sf
    hydroStats <- hydroStats %>%
      dplyr::rename(network = PBx)
    
    # Join metrics
    poly_sf <- poly_sf %>%
      left_join(hydroStats %>%
                  dplyr::select(network , UpstreamArea, UpstreamAWI)) %>%
      mutate(UpstreamArea = as.integer(UpstreamArea/1000000))
    
    # Rename attributes in poly_sf  
    poly_sf <- poly_sf %>%
      dplyr::rename(up_km2 = UpstreamArea)
    poly_sf <- poly_sf %>% 
      dplyr::rename(up_AWI = UpstreamAWI) 
    
    #Generate upstream area polygons
    upfile <- list.files(file.path(rv$outdir(), "Builder_output"), pattern = "UPSTREAM_CATCHMENTS_COLUMN")
    upstream <- read.csv(file.path(rv$outdir(), "Builder_output", tail(upfile,1)))
    upstream_list <-as_tibble(upstream[,-1])
    
    #Fix PB to KBA and generate upstream area
    upstream_list <- upstream_list %>%
      rename_with(~ str_replace(.x, "PB", "KBA"))
    upstream_area <- dissolve_catchments_from_table(rv$layers_rv$catchments, upstream_list, "network")  
    
    upstream_area <- upstream_area %>%
      st_buffer(dist = 20) %>% 
      st_buffer(dist = -20)
    
    #Update reactiveVal
    rv$upstream_reactive(upstream_area)
    
    # Export. Append the first layer to the GeoPackage
    st_write(upstream_area, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "upstream_KBAs", driver = "GPKG", append = FALSE)
    
    # DENDRITIC CONNECTIVITY (DCI) - CALCULATE AND ADD TO TABLE
    # A measure of longitudinal hydrological connectivity within each conservation area with values ranging from 
    # 0 (low connectivity) to 1 (fully connected).
    
    # Calculate DCI and add the values as a new column (attribute = dci).
    if(attr(poly_sf, "sf_column") != "geometry"){
      poly_sf$geometry <- poly_sf$geom
    }
    
    #removeModal()
    
    poly_sf$dci <- shiny::withProgress(
      message = "Calculating hydrology metrics ",
      detail = "Starting...",
      value = 0,
      {
        
        calc_dci(conservation_area_sf = poly_sf, stream_sf = rv$layers_rv$streams, progress = function(value, detail) {
            shiny::setProgress(value = value, detail = detail)
          })
      }
    )
    
    #Update reactiveVal
    rv$kba_sf_reactive(poly_sf)
    
    # Export. Append the first layer to the GeoPackage
    st_write(poly_sf, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "KBAs_reducedFALSE", driver = "GPKG", append = FALSE)
    
    # Initialize KBA/PAs freq table
    x <- rv$outfreqhydro()
    x <- x %>% 
      mutate(Count = case_when(Variables == "KBAs" ~  nrow(poly_sf),
                               TRUE ~ Count))
    
    # Generate Stat Tables    
    rv$outfreqhydro(x) 
    
    output$outkbahydro <- renderTable({
      rv$outfreqhydro()
    })
    
    showModal(modalDialog(
      title = "Hydrology metrics added",
      easyClose = FALSE,
      footer = modalButton("OK"))
    )  
    
  })
  
  observeEvent(rv$kba_sf_reactive(), {
    req(rv$kba_sf_reactive())
    
    updateActionButton(session, "runBuilder", label = "Builder output created!", icon = icon("check", lib = "font-awesome"))
  })
  ####################################################################################################
  # -Render PAs statistics table
  ####################################################################################################
  #  observeEvent(input$tabs, {
  #    req(input$tabs == 'tabBuilder')
  
  # Initialize KBA/PAs freq table
  #    x <- tibble(
  #      Variables = c("KBAs", "Reduced KBAs"),
  #      Count = c(NA, NA))  
  
  #    layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
  #    layers <- layers_info$name
  
  #    if ("KBAs_builder" %in% layers) {
  #      kba_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "KBAs_builder")
  #      x <- x %>% 
  #        mutate(Count = case_when(Variables == "KBAs" ~  nrow(kba_sf),
  #                                 TRUE ~ Count))
  #    }
  #if ("KBAs_reduced10000" %in% layers) {
  #  kba_reduced_sf <- st_read(dsn = file.path(rv$outdir(),  "output/KBA_analysis.gpkg"), layer = "KBAs_reduced10000")
  #  x <- x %>% 
  #    mutate(Count = case_when(Variables == "Reduced KBAs" ~  nrow(kba_reduced_sf),
  #                             TRUE ~ Count))
  #}  
  
  # Generate Stat Tables    
  #    rv$outfreqhydro(x) 
  
  #    output$outkbahydro <- renderTable({
  #      rv$outfreqhydro()
  #    })
  
  #  })
  
  ####################################################################################################
  # REDUCE KBAs
  ####################################################################################################
  observeEvent(input$tabs, {
    req(input$tabs == "tabKBAs")
    req(rv$layers_rv$catchments)
    
    if(file.exists(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))){
      layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
      layers <- layers_info$name
      if ("KBAs_reducedFALSE" %in% layers) {
        kba_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "KBAs_reducedFALSE")
        rv$kba_sf_reactive(kba_sf)
      } else {
        #Test on required layers
        showModal(modalDialog(
          title = "Missing Data",
          "Calculate DCI and add upstream attributes to KBAs prior to reduce the number.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
    } else {
      #Test on required layers
      showModal(modalDialog(
        title = "Missing Data",
        "KBAs have not been created. Please run Builder and calculate DCI",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    x <- tibble(
      Variables = c("KBAs", "Reduced KBAs"),
      Count = c(NA, NA))  
    
    if (!is.null(rv$kba_sf_reactive())) {
      x <- x %>% 
        mutate(Count = case_when(Variables == "KBAs" ~  nrow(rv$kba_sf_reactive()),
                                 TRUE ~ Count))
    }
    
    rv$outfreqhydro(x) 
    
    output$outkbahydro <- renderTable({
      rv$outfreqhydro()
    })
  })
  
  observeEvent(input$reduce_KBAs, {
    #Test on required layers
    
    if (is.null(rv$layers_rv$streams) || is.null(rv$layers_rv$planreg)) {
      # Create the modal dialog
      showModal(modalDialog(
        title = "Missing Data",
        "One or more required layers are missing. Make sure stream and planning region dataset are uploaded and Builder output  
         on which hydrology metrics have been calculated exist.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    req(rv$layers_rv$streams)
    req(rv$layers_rv$planreg)
    req(!is.null(rv$kba_sf_reactive()))
    
    layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    layer_to_check <- paste0("KBAs_reduced", input$set_grid)
    if (!layer_to_check %in% layers) {
      showModal(modalDialog(
        title = "Processing",
        "Reduce number of potential KBAs. Please wait...",
        footer = NULL
      ))
      # REDUCE NUMBER OF CONSERVATION AREAS
      # Select the top conservation area from each group based on smallest upstream area, largest DCI, and largest upstream intactness
      kba_sf <- rv$kba_sf_reactive()
      
      kba_sf$group_id <- group_conservation_areas(kba_sf, as.numeric(input$set_grid))  
      
      kba_sf <- kba_sf %>%
        group_by(group_id) %>%
        arrange(-dci, up_km2, -up_AWI) %>% # Attribute order indicates their importance when selecting the 'best'from each group. '-' indicates largest to smallest.
        filter(row_number()==1)
      
      # Close the modal once processing is done
      removeModal()
      
      rv$kba_reduce_reactive(kba_sf)
    } else {
      kba_sf <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = paste0("KBAs_reduced", input$set_grid))
      rv$kba_reduce_reactive(kba_sf)
    }
    
    showModal(modalDialog(
      title = paste0("KBAs number reduced to ", as.character(nrow(kba_sf)), "."),
      "To save the reduced set, click **Save reduced KBAs in GPKG**",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
    #groups_to_remove <- c(
    #  "Potential KBAs", "Upstream", rv$reactive_labelKBA(), rv$reactive_labelNET(),
    #  "CMI", "LED", "GPP", "LCC", rv$criteria5name()
    #)
    groups_to_remove <- c("Potential KBAs", "Upstream")
    
    # Remove these groups from overlayGroups()
    rv$overlayKBA(setdiff(rv$overlayKBA(), groups_to_remove))
    legend <- c(rv$overlayKBA(), "Potential KBAs (reduced)")
    rv$overlayKBA(legend)
    
    kba_sf_4326 <- st_transform(kba_sf, 4326) %>% st_simplify(dTolerance = 0.001)
    leafletProxy("map") %>%
      clearControls() %>%
      clearGroup('Potential KBAs') %>%
      clearGroup('Potential KBAs (reduced)') %>%
      clearGroup('Upstream') %>%
      clearGroup(rv$reactive_labelKBA()) %>%
      clearGroup(rv$reactive_labelNET()) %>%
      clearGroup("CMI") %>%
      clearGroup("LED") %>%
      clearGroup("GPP") %>%
      clearGroup("LCC") %>%
      addPolygons(data=kba_sf_4326, fillColor='#666666', color= "#000000", weight = 1,  group="Potential KBAs (reduced)", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery" , "Blank Background"),
                       overlayGroups = c(rv$overlayGroups(), rv$overlayKBA()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Potential KBAs (all)"))
    
    # Initialize KBA/PAs freq table
    x <- rv$outfreqhydro()
    
    x <- x %>% 
      mutate(Count = case_when(Variables == "Reduced KBAs" ~  nrow(rv$kba_reduce_reactive()),
                               TRUE ~ Count))
    rv$outfreqhydro(x)
    
    # Generate Stat Tables  
    output$outkbahydro <- renderTable({
      rv$outfreqhydro()
    })
    
  }) 
  
  observeEvent(input$save_reduce, {
    req(!is.null(rv$kba_reduce_reactive()))
    
    showModal(modalDialog(
      title = "Reduces KBAs layer saved. ",
      easyClose = TRUE,
      footer = modalButton("OK"))
    )
    st_write(rv$kba_reduce_reactive(), dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = paste0("KBAs_reduced", input$set_grid), driver = "GPKG", append = FALSE)
  }, ignoreInit = TRUE)
  
}