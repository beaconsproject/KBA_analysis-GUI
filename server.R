server = function(input, output) {
  
  output$help <- renderText({
    includeMarkdown("upstream.md")
  })
  
  poly_sf_reactive <- reactiveVal()
  
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
      title = "Running Builder. Conservation areas/ network will be displayed shortly in red. Please be patient.",
      easyClose = TRUE,
      footer = modalButton("OK")))
    
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
  })
  
  # Generate shapefile download when button is clicked
  output$download_shapefile <- downloadHandler(
    filename = function() {
      paste("sf_shapefile", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Create a temporary directory to store shapefile components
      tempdir <- tempdir()
      shapefile_path <- file.path(tempdir, "sf_shapefile.shp")
      
      # Write the sf object to a shapefile
      st_write(poly_sf_reactive(), shapefile_path)
      
      # Zip the shapefile components (e.g., .shp, .shx, .dbf)
      zip::zipr(zipfile = file, files = list.files(tempdir, pattern = "sf_shapefile", full.names = TRUE))
    }
  )
}