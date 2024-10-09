server = function(input, output) {
  
  output$help <- renderText({
    includeMarkdown("upstream.md")
  })
  
  poly_sf_reactive <- reactiveVal()
  upstream_reactive <- reactiveVal()
  ################################################################################################
  # Set catchments
  ################################################################################################
  catchments <- reactive({
    req(input$upload_catch)
    infile <- input$upload_catch
    if (length(infile$datapath) > 1) { # Check if multiple files are uploaded
      dir <- unique(dirname(infile$datapath))  # Get the temp directory
      outfiles <- file.path(dir, infile$name)  # Create new file path with original names
      
      # Strip the base name (without extension) of the first file
      name <- tools::file_path_sans_ext(infile$name[1])  
      purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y)) 
      # Attempt to read the shapefile after renaming
      shp_path <- file.path(dir, paste0(name, ".shp"))
      if (file.exists(shp_path)) {
        i <- sf::st_read(shp_path)  # Use sf::st_read() to read the Shapefile
      } else {
        stop("Shapefile (.shp) is missing.")
      }
    } else {
      stop("Upload all necessary files for the shapefile (.shp, .shx, .dbf, etc.).")
    }
    return(i)
  })
  ################################################################################################
  # Set streams
  ################################################################################################
  streams <- reactive({
    req(input$upload_stream)
    infile <- input$upload_stream
    if (length(infile$datapath) > 1) { # Check if multiple files are uploaded
      dir <- unique(dirname(infile$datapath))  # Get the temp directory
      outfiles <- file.path(dir, infile$name)  # Create new file path with original names
      
      # Strip the base name (without extension) of the first file
      name <- tools::file_path_sans_ext(infile$name[1])  
      purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y)) 
      # Attempt to read the shapefile after renaming
      shp_path <- file.path(dir, paste0(name, ".shp"))
      if (file.exists(shp_path)) {
        i <- sf::st_read(shp_path)  # Use sf::st_read() to read the Shapefile
      } else {
        stop("Shapefile (.shp) is missing.")
      }
    } else {
      stop("Upload all necessary files for the shapefile (.shp, .shx, .dbf, etc.).")
    }
    return(i)
  })
  # Observe when the dataset is loaded and update the selectInput choices
  observe({
    req(catchments())  # Ensure the catchments data is available
    catchment_data <- catchments()
    # Assuming catchment_data is a dataframe or sf object, extract column names
    colnames <- names(catchment_data)
    
    # Update the choices of the selectInput elements with column names
    updateSelectInput(session = getDefaultReactiveDomain(), "zoneColname", choices = colnames)
    updateSelectInput(session = getDefaultReactiveDomain(), "arealandColname", choices = colnames, selected ="Area_land")
    updateSelectInput(session = getDefaultReactiveDomain(), "intactColname", choices = colnames, selected = "intact")
  })
  
  ####################################################################################################
  ####################################################################################################
  # Map viewer
  ####################################################################################################
  ####################################################################################################
  # Render the initial map
  output$map <- renderLeaflet({
    # Re-project
    bnd <- bnd
    map_bounds <- bnd %>% st_bbox() %>% as.character()

    # Render initial map
    map <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      addMapPane(name = "layer1", zIndex=380) %>%
      addMapPane(name = "layer2", zIndex=420) %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>% 
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addPolygons(data=bnd, color='grey', fill=F, weight=1, group="region", options = leafletOptions(pane = "layer1")) %>%
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c(""))
  
    if(!is.null(input$upload_catch)){
      req(catchments())

      # show pop-up ...
      showModal(modalDialog(
       title = "Uploading catchments layers.",
       easyClose = TRUE,
       footer = modalButton("OK")))
     catch_4326 <- st_transform(catchments(), 4326)
     map_bounds1 <- catch_4326 %>% st_bbox() %>% as.character()

     map <- map %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = catch_4326$CATCHNUM, group="Catchments", options = leafletOptions(pane = "layer2")) %>%
      addLayersControl(position = "topright",
                       overlayGroups = c("Catchments"),
                       options = layersControlOptions(collapsed = FALSE))  %>%
      hideGroup(c(""))
    }
    map
  })
  
  ####################################################################################################
  ####################################################################################################
  # Analysis
  ####################################################################################################
  ####################################################################################################
  # Run BUILDER
  observeEvent(input$runBuilder>0, {
    req(catchments())
    req(input$set_wd)
    if(dir.exists(input$set_wd)) {
      out_dir <-input$set_wd
    } else {
      # Create the directory if it doesn't exist
      dir.create(input$set_wd, recursive = TRUE)
      out_dir <-input$set_wd
    }
    f_req <- c("Builder_input", "Builder_output", "output", "rep_layers")
    flist <- list.dirs(out_dir, full.names = FALSE)
    for(f in f_req) {
      # Check if the directory exists in flist
      if(!(f %in% flist)) {
        # Create the directory if it doesn't exist
        dir.create(file.path(out_dir, f), recursive = TRUE)
      }
    }
    # Generate neighbours table for catchments - Builder_input file for Builder. Skip this step is nghbrs.csv already exists.
    if (!file.exists(file.path(out_dir, "Builder_input/nghbrs.csv"))) {
      nghbrs <- neighbours(catchments())
      write.csv(nghbrs, file=file.path(out_dir,"Builder_input/nghbrs.csv"), row.names=FALSE) # Convert neighbours table to csv file.
    }else{
      nghbrs <- read.csv(file.path(out_dir,'Builder_input','nghbrs.csv'))
    }
    # Create seed list - input file for Builder that identifies where construction of conservation area is to start
    # intact ranges from 0 to 1 and is the minimum required proporational intactness required for a catchment to be a seed (0.8 = 80%)
    # areatarget_value is in m2 and specifies the desired conservation area size (10,000 km2 = 10000000000 m2)
    if (!file.exists(file.path(out_dir, "Builder_input/seeds.csv"))) {
      seed <- catchments() %>%
        filter(kba_m2 >= 0, STRAHLER == as.numeric(input$set_strahler), HBTeco ==1) %>%
        seeds(catchments_sf = ., areatarget_value = as.numeric(input$set_areatarget))
      write.csv(seed, file=file.path(out_dir,"Builder_input/seeds.csv"), row.names=FALSE) # Convert neighbours table to csv file.
    }else{
      seed <- read.csv(file.path(out_dir,'Builder_input','seeds.csv'))
    }
  
    # show pop-up ...
    showModal(modalDialog(
      title = "Run BUILDER",
      "Conservation areas/ network will be displayed shortly in red. Please wait...",
      footer = NULL
    ))
    builder_tab <- builder(catchments_sf = catchments(),
                           seeds = seed, 
                           neighbours = nghbrs,
                           out_dir = file.path(out_dir, "Builder_output"),
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
                           stream_length = "STRMLEN",
                           intactness = input$intactColname,
                           isolated = "Isolated",
                           unique_identifier = "PB",
                           handler_summary = FALSE,
                           summary_intactness_props = "\"\"",
                           summary_area_target_props = "\"\"")
    
    
    # Convert conservation areas created by builder to polygons.(NOTE: poly_sf is the R object with conservation areas.)
    poly_sf <- dissolve_catchments_from_table(catchments_sf = catchments(), 
                                              input_table = builder_tab, 
                                              out_feature_id = "network")
    poly_sf_reactive(poly_sf)  # Store the poly_sf in reactiveVal
    
    CA_sf <-st_transform(poly_sf, 4326)
    leafletProxy("map") %>%
      addPolygons(data=CA_sf, fillColor = "red", fillOpacity=0.4, weight=0.1, group="Conservation areas", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(position = "topright",
                       overlayGroups = c("Catchments", "Conservation areas"),
                       options = layersControlOptions(collapsed = FALSE)) 
    
    # Close the modal once processing is done
    removeModal()
  })
  
  observeEvent(input$calc_dci, {
    req(input$set_grid)
    req(!is.null(poly_sf_reactive()))
    showModal(modalDialog(
      title = "Processing",
      "Calculating DCI. UPstream areas will be dispayed in blue on the map. Please wait...",
      footer = NULL
    ))
    
    poly_sf <- poly_sf_reactive()
    tab_reactive <-reactiveVal(2)
    
    poly_sf$group_id <- group_conservation_areas(poly_sf, as.numeric(input$set_grid))  
    #*****st_write(poly_sf,"./output/potential_KBAs_grouped.shp") 
    
    # Identify the attributes file and read it
    attributefile <- list.files(file.path(input$set_wd,"Builder_output"), pattern = "Unique_BAs_attributes")
    attributeStats <- read.csv(file.path(input$set_wd, "Builder_output", attributefile))
    
    # Rename column in attributeStats in order to join it with poly_sf
    attributeStats <- attributeStats %>%
      rename(network = PBx)
    
    # Join metrics
    poly_sf <- poly_sf %>%
      left_join(attributeStats %>%
                  dplyr::select(network , Area_PB, AWI_PB)) %>%
      mutate(Area_PB = as.integer(Area_PB/1000000))
    
    # Rename attributes in poly_sf  
    poly_sf <- poly_sf %>%
      rename(area_km2 = Area_PB)
    poly_sf <- poly_sf %>% 
      rename(AWI = AWI_PB) 
    
    # UPSTREAM AREA (up_km2) AND UPSTREAM INTACTNESS (up_AWI) can be found in the Builder output - see file "*_HYDROLOGY_METRICS.csv"
    # Identify the Hydro metrics file and read it
    hydrofile <- list.files(file.path(input$set_wd, "Builder_output"), pattern = "HYDROLOGY_METRICS")
    hydroStats <- read.csv(file.path(input$set_wd, "Builder_output", hydrofile))
    
    upfile <- list.files(file.path(input$set_wd, "Builder_output"), pattern = "UPSTREAM_CATCHMENTS_COLUMN")
    upstream <- read.csv(file.path(input$set_wd, "Builder_output", upfile))
    
    # Rename column in hydroStats in order to join it with poly_sf
    hydroStats <- hydroStats %>%
      rename(network = PBx)
    
    # Join metrics
    poly_sf <- poly_sf %>%
      left_join(hydroStats %>%
                  dplyr::select(network , UpstreamArea, UpstreamAWI)) %>%
      mutate(UpstreamArea = as.integer(UpstreamArea/1000000))
    
    # Rename attributes in poly_sf  
    poly_sf <- poly_sf %>%
      rename(up_km2 = UpstreamArea)
    poly_sf <- poly_sf %>% 
      rename(up_AWI = UpstreamAWI) 
    
    #Generate upstream area polygons
    upfile <- list.files(file.path(input$set_wd, "Builder_output"), pattern = "UPSTREAM_CATCHMENTS_COLUMN")
    upstream <- read.csv(file.path(input$set_wd, "Builder_output", upfile))
    upstream_list <-as_tibble(upstream[,-1])
    upstream_area <- dissolve_catchments_from_table(catchments(), upstream_list, "network")  
    
    upstream_reactive(upstream_area)
    # Export upstream area shapefile
    #*st_write(upstream_area,file.path(wd,'output','upstream_area.shp'))
    
    # DENDRITIC CONNECTIVITY (DCI) - CALCULATE AND ADD TO TABLE
    # A measure of longitudinal hydrological connectivity within each conservation area with values ranging from 
    # 0 (low connectivity) to 1 (fully connected).
    
    ### MAP
    upstream_area_4326 <- st_transform(upstream_area, 4326)
    leafletProxy("map") %>%
      addPolygons(data=upstream_area_4326, fillColor = "blue", fillOpacity=0.4, weight=0.1, group="Upstream areas", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(position = "topright",
                       overlayGroups = c("Catchments", "Conservation areas", "Upstream areas"),
                       options = layersControlOptions(collapsed = FALSE)) 
    
    # Calculate DCI and add the values as a new column (attribute = dci).
    poly_sf$dci <- calc_dci(conservation_area_sf = poly_sf, 
                            stream_sf = streams())
    poly_sf_reactive(poly_sf)
    
    # Close the modal once processing is done
    removeModal()
  })  

  observeEvent(input$reduce_cas, {
    req(!is.null(poly_sf_reactive()))
    req(!is.null(upstream_reactive()))
    # REDUCE NUMBER OF CONSERVATION AREAS
    # Select the top conservation area from each group based on smallest upstream area, largest DCI, and largest upstream intactness
    #st_write(poly_sf,file.path(wd,'output','benchmark_attributes.shp')) #specify output folder and shapefile name
    poly_sf_filtered <- poly_sf %>%
      group_by(group_id) %>%
      arrange(-dci, up_km2, -up_AWI) %>% # Attribute order indicates their importance when selecting the 'best'from each group. '-' indicates largest to smallest.
      filter(row_number()==1)

    showModal(modalDialog(
      title = pastes0("Number of filtered conservation areas:", as.character(nrow(poly_sf_filtered))),
      easyClose = TRUE,
      footer = modalButton("OK"))
    )
    
    ### MAP
    poly_sf_filtered_4326 <- st_transform(poly_sf_filtered, 4326)
    leafletProxy("map") %>%
      addPolygons(data=poly_sf_filtered_4326, fillColor = "red", fillOpacity=0.4, weight=0.1, group="Conservation areas filtered", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(position = "topright",
                       overlayGroups = c("Catchments", "Conservation areas", "Conservation areas filtered", "Upstream areas"),
                       options = layersControlOptions(collapsed = FALSE))  %>%
      hideGroup(c("Conservation areas"))
    
  }) 
  ###################################################################
  ###################################################################
  ## Download
  ###################################################################
  ###################################################################
  # Function to dynamically create the shapefile
  write_shapefile <- function(data, file, filename) {
    shapefile_path <- file.path(tempdir(), filename)
    st_write(data, shapefile_path, append = FALSE)
    # Optionally zip the shapefile if required
    zip::zipr(zipfile = file, files = list.files(tempdir(), pattern = filename, full.names = TRUE))
  }
  
  # Download handler for first shapefile (Builder)
  output$download_shapefile <- downloadHandler(
    filename = function() {
      paste("KBA_CAs_Builder-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      poly_sf <- poly_sf_reactive() # Reactive data for Builder
      write_shapefile(poly_sf, file, "KBA_CAs_builder.shp")
    }
  )
  
  # Download handler for second shapefile (Attributes)
  output$download_shapefile2 <- downloadHandler(
    filename = function() {
      paste("KBA_CAs_Attributes-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      poly_sf_with_attributes <- poly_sf_reactive() # Reactive data with attributes
      write_shapefile(poly_sf_with_attributes, file, "KBA_CAs_att.shp")
    }
  )
  
  # Download handler for third shapefile (Filtered)
  output$download_shapefile3 <- downloadHandler(
    filename = function() {
      paste("KBA_CAs_Filtered-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      filtered_sf <- filtered_sf_reactive() # Reactive data for filtered conservation areas
      write_shapefile(filtered_sf, file, "KBA_CAs_filtered.shp")
    }
  )
}