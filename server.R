server = function(input, output, session) {
  
  # Reactive values 
  input_paths <- reactiveVal(data.frame(Layer = character(), Path = character()))
  kba_sf_reactive <- reactiveVal(NULL)
  kba_reduce_reactive <- reactiveVal(NULL)
  kba_upstream_reactive <- reactiveVal(NULL)
  upstream_reactive <- reactiveVal(NULL)
  pas_sf_reactive <- reactiveVal(NULL)
  pas_upstream_reactive <- reactiveVal(NULL)
  poly_reactive <- reactiveVal()
  upstream_network_reactive <- reactiveVal()
  nghbrs_reactive <- reactiveVal()
  seed_reactive <- reactiveVal()
  reactive_labelKBA <- reactiveVal(NULL)
  reactive_labelNET <- reactiveVal(NULL)
  network_reactive <- reactiveVal()
  dir_exists <- reactiveVal(FALSE)
  builder_exists <- reactiveVal(FALSE)
  criteria5name <- reactiveVal(NULL)
  netDir <- reactiveVal()
  legendcrit <-  reactiveVal(c("CMI", "LED", "GPP", "LCC"))
  selected_polygon <- reactiveVal(NULL)  # Track the selected polygon on map
  refarea_reactive <- reactiveVal(NULL)
  tab_upload_visited <- reactiveVal(FALSE)
  filtered_kba <- reactiveVal(NULL)
  filtered_pas <- reactiveVal(NULL)
  
  outfreqkba <- reactiveVal(
    tibble(Variables = c("KBAs", "Filtered KBAs", "PAs", "Filtered PAs"), Count = NA_integer_)
  )
  outfreqnet <- reactiveVal(
    tibble(Variables = c("KBAs", "Filtered KBAs", "PAs", "Filtered PAs", "Networks", "Filtered networks"), Count = NA_integer_)
  )
  #outfreqnet <- reactiveVal(NULL)
  # Define root access points (change as needed for Windows/Linux/Mac)
  roots <- get_available_drives()
  # Set up directory chooser with expanded access
  shinyDirChoose(input, "directory", roots = roots, session = getDefaultReactiveDomain())
  
  ################################################################################################
  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })
  
  ################################################################################################
  # Set dir
  ################################################################################################
  # Reactive to store the selected directory path
  dirpath <- reactive({
    req(input$directory)
    parseDirPath(roots, input$directory)
  })
  
  # Show selected directory path
  output$dirpath <- renderText({
    dirpath()
  })
  
  observeEvent(input$set_wd, {
    req(input$set_wd)
    
    treedir <- c("output","Builder_input","Builder_output")
    for(d in treedir){
      if(!dir.exists(file.path(dirpath(), d))){
        dir.create(file.path(dirpath(), d))
        showModal(modalDialog(
          title = "Output subdirectories created.",
          "Please select input parameters by either uploading a csv containing input path or by pointing on the source files.",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      }else{
        showModal(modalDialog(
          title = "Output directory selected",
          "Please select input parameters by either uploading a csv containing input path or by pointing on the source files.",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      }
    }
    
    if (file.exists(file.path(dirpath(), "Builder_input/seeds.csv"))) {
        seed <- read.csv(file.path(dirpath(),"Builder_input/seeds.csv"))
        seed_reactive(seed)
    }
    if (file.exists(file.path(dirpath(), "Builder_input/nghbrs.csv"))) {
        nghbrs <- read.csv(file.path(dirpath(),"Builder_input/nghbrs.csv"))
        nghbrs_reactive(nghbrs)
    }
    
    gpk_path <- file.path(dirpath(), "output/KBA_analysis.gpkg")
    if (file.exists(gpk_path)) {
      layers_info <- st_layers(gpk_path)
      layers <- layers_info$name

      if ("KBAs_upstream" %in% layers) {
        upstream_area <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_upstream")
        upstream_reactive(upstream_area)
      }
      if ("KBAs_reduced" %in% layers) {
        kba_reduced <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_reduced")
        kba_sf_reactive(kba_reduced)
      }
      showModal(modalDialog(
        title = "The output directory already exists and the necessary files are present.",
        "The analysis will use those data. If you changed the input files or plan to change the parameters, please set another output directory.",
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
    }
    if(!is.null(planreg())){
      req(planreg())
      st_write(planreg(), dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), 
               layer = "planning region", driver = "GPKG", append = FALSE)
    }
  })
  
  observeEvent(planreg(),{
    if(input$set_wd==0){
      showModal(modalDialog(
        title = "Output directory is missing",
        "Please provide an output directory prior to upload the input files",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(FALSE)  # Stop further execution
    }
  })
  ################################################################################################
  # Validate csv
  ################################################################################################
  # Required layers
  required_layers <- c("catchments", "stream", "planning region", "CMI", "GPP", "LCC", "LED")
 
  # Reactive function to validate the input file
  validate_csv <- reactive({
    req(input$csv_file)  # Ensure the file input is not NULL
    # Read the uploaded CSV file
    csv_data <- read.csv(input$csv_file$datapath)
    
    # Find missing layers
    missing_layers <- setdiff(required_layers, csv_data$Layer)
    if (length(missing_layers) > 0) {
      showModal(modalDialog(
        title = "Missing Layers",
        paste("The uploaded CSV is missing the following layers:",
              paste(missing_layers, collapse = ", "),
              ". Please fix and re-upload."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(FALSE)  # Stop further execution
    } else {
      # Return validated data if all checks pass
      return(TRUE)
    }
  })
  
  ################################################################################################
  # Set catchments
  ################################################################################################
  catchments <- reactive({
    if (!is.null(input$csv_file)) {
      req(validate_csv())
      return(read_shp_from_csv(input$csv_file, "catchments"))
    } else if (!is.null(input$upload_catch)) {
      return(read_shp_from_upload(input$upload_catch))
    }else {
      return(NULL)
    }
  })
  ################################################################################################
  # Set streams
  ################################################################################################
  streams <- reactive({
    if (!is.null(input$csv_file)) {
      req(validate_csv())
      stream_sf <- read_shp_from_csv(input$csv_file, "stream")
      stream_4326 <- stream_sf %>% st_transform(4326) %>% st_simplify(dTolerance = 0.001)
      st_write(stream_4326, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"),
               layer = "stream_4326", driver = "GPKG", append = FALSE)
      return(stream_sf)
    } else if (!is.null(input$upload_stream)) {
      stream_sf <- read_shp_from_upload(input$upload_stream)
      stream_4326 <- stream_sf %>% st_transform(4326) %>% st_simplify(dTolerance = 0.001)
      st_write(stream_4326, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"),
               layer = "stream_4326", driver = "GPKG", append = FALSE)
      return(stream_sf)
    } else{
      return(NULL)
    }
  })
  ################################################################################################
  # Set Planning region
  ################################################################################################
  planreg <- reactive({
    if (!is.null(input$csv_file)) {
      req(validate_csv())
      planreg <- read_shp_from_csv(input$csv_file, "planning region")
      return(planreg)
    } else if (!is.null(input$upload_planreg)) {
      planreg <- read_shp_from_upload(input$upload_planreg)
      return(planreg)
    }else{
      return(NULL)
    }
  })
  ################################################################################################
  # Set LCC
  ################################################################################################  
  lcc <- reactiveVal(NULL)
  observe({
    req(dirpath())
    if (file.exists(file.path(dirpath(), "output/kba_lcc.tif"))) {
      lcc(raster(file.path(dirpath(), "output/kba_lcc.tif")))
    } else if (!is.null(input$upload_lcc)) {
      # Read raster from file upload
      lcc(read_tif_from_upload(input$upload_lcc))
    } else if (!is.null(input$csv_file)) {
      req(validate_csv())
      # Read raster from CSV
      lcc(read_tif_from_csv(input$csv_file, "LCC"))
    } else{
      lcc(NULL)
    }
  })
  
  ################################################################################################
  # Set LED
  ################################################################################################
  # Reactive to handle LED (TIFF files)
  led <- reactiveVal(NULL)
  observe({
    req(dirpath())
    if (file.exists(file.path(dirpath(), "output/kba_led.tif"))) {
      led(raster(file.path(dirpath(), "output/kba_led.tif")))
    } else if (!is.null(input$upload_led)) {
      # Read raster from file upload
      led(read_tif_from_upload(input$upload_led))
    } else if (!is.null(input$csv_file)) {
      req(validate_csv())
      # Read raster from CSV
      led(read_tif_from_csv(input$csv_file, "LED"))
    } else{
      led(NULL)
    }
  })
  ################################################################################################
  # Set GPP
  ################################################################################################
  gpp <- reactiveVal(NULL)
  observe({
    req(dirpath())
    if (file.exists(file.path(dirpath(), "output/kba_gpp.tif"))) {
      gpp(raster(file.path(dirpath(), "output/kba_gpp.tif")))
    } else if (!is.null(input$upload_gpp)) {
      # Read raster from file upload
      gpp(read_tif_from_upload(input$upload_gpp))
    } else if (!is.null(input$csv_file)) {
      req(validate_csv())
      # Read raster from CSV
      gpp(read_tif_from_csv(input$csv_file, "GPP"))
    }else{
      gpp(NULL)
    }
  })
  ################################################################################################
  # Set CMI
  ################################################################################################
  cmi <- reactiveVal(NULL)
  observe({
    req(dirpath())
    if (file.exists(file.path(dirpath(), "output/kba_cmi.tif"))) {
      cmi(raster(file.path(dirpath(), "output/kba_cmi.tif")))
    } else if (!is.null(input$upload_cmi)) {
      # Read raster from file upload
      cmi(read_tif_from_upload(input$upload_cmi))
    } else if (!is.null(input$csv_file)) {
      req(validate_csv())
      # Read raster from CSV
      cmi(read_tif_from_csv(input$csv_file, "CMI"))
    } else{
      cmi(NULL)
    }
  })
  
  ################################################################################################
  # Set criteria5
  ################################################################################################
  criteria5 <- reactive({
    if (!is.null(input$upload_custom)) {
      # Read raster from file upload
      rastName <- sub("\\..*$", "", input$upload_custom$name)
      criteria5name(rastName)
      updateSliderInput(session = getDefaultReactiveDomain(), "slidecrit5", label = rastName)
      updateSliderInput(session = getDefaultReactiveDomain(), "slideNETcrit5", label = rastName)
      return(read_tif_from_upload(input$upload_custom))
    } else if (!is.null(input$csv_file)) {
      csv_data <- read.csv(input$csv_file$datapath)
      req_layers <- c("CMI", "LED", "GPP", "LCC", "catchments", "stream", "planning region", "protected areas", "reference area")
      unexpected_layers <- csv_data$Layer[!csv_data$Layer %in% req_layers]
      # Read raster from CSV
      if (length(unexpected_layers)>0) {
        if(length(unexpected_layers)==1){
          #print(paste("Unexpected layers found:", paste(unexpected_layers, collapse = ", ")))
          path <- csv_data$Path[csv_data$Layer == unexpected_layers]
          if (file.exists(path)) {
            criteria5name(unexpected_layers)
            updateSliderInput(session = getDefaultReactiveDomain(), "slidecrit5", label = unexpected_layers)
            updateSliderInput(session = getDefaultReactiveDomain(), "slideNETcrit5", label = unexpected_layers)
            return(read_tif_from_csv(input$csv_file, unexpected_layers))
          } else {
            stop("The custom vatiable path in the CSV does not exist.")
          }
        }else{
          # show pop-up ...
          showModal(modalDialog(
            title = "Provided layer csv pathways include more than one custom layer.", "The app allow only the addtion of one custom layer at the moment. Please fix the csv.",
            easyClose = TRUE,
            footer = NULL)
          )
        }
      }else{
        return(NULL) 
      }
    }
    # Return NULL if neither source is available
    return(NULL)
  })
  ################################################################################################
  # Set protected areas
  ################################################################################################
  # PAs upload shapefile
  observeEvent(input$upload_pas, {
    req(input$upload_pas)
    pas_sf <- read_shp_from_upload(input$upload_pas)
    st_write(pas_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"),
             layer = "protected areas", driver = "GPKG", append = FALSE)
    pas_sf_reactive(pas_sf)
  })
  
  # PAs upload using csv
  observeEvent(input$csv_file, {
    req(input$csv_file)
    csv_data <- read.csv(input$csv_file$datapath)
    layers_to_check <- "protected areas"
    
    # Check if the required layer exists in the CSV
    if (layers_to_check %in% csv_data$Layer) {
      pas_sf <- read_shp_from_csv(input$csv_file, "protected areas")
      st_write(pas_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"),
               layer = "protected areas", driver = "GPKG", append = FALSE)
      pas_sf_reactive(pas_sf)
    } 
  })
  
  ################################################################################################
  # Set reference area
  ################################################################################################
  observeEvent(input$upload_refarea, {
    req(input$upload_refarea)
    refarea_sf <- read_shp_from_upload(input$upload_refarea)
    st_write(refarea_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"),
             layer = "reference area", driver = "GPKG", append = FALSE)
    refarea_reactive(refarea_sf)
  })
  
  observeEvent(input$csv_file, {
    req(input$csv_file)
    csv_data <- read.csv(input$csv_file$datapath)
    layers_to_check <- "reference area"
    
    # Check if the required layer exists in the CSV
    if (layers_to_check %in% csv_data$Layer) {
      refarea_sf <- read_shp_from_csv(input$csv_file, "reference area")
      st_write(refarea_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"),
               layer = "reference area", driver = "GPKG", append = FALSE)
      refarea_reactive(refarea_sf)
    } 
  })
  ################################################################################################
  # Set intact areas
  ################################################################################################
  intact_4326 <- reactive({
    intact_4326 <- intact %>% st_transform(4326)
    return(intact_4326)
  })
  ################################################################################################
  # Observe when the dataset is loaded and update the selectInput choices
  observe({
    req(catchments())  # Ensure the catchments data is available
    catchment_data <- catchments()
    # Assuming catchment_data is a dataframe or sf object, extract column names
    colnames <- names(catchment_data)
    
    # Update the choices of the selectInput elements with column names
    updateSelectInput(session = getDefaultReactiveDomain(), "zoneColname", choices = colnames, selected = "ecoMDAzone")
    updateSelectInput(session = getDefaultReactiveDomain(), "intactColname", choices = colnames, selected = "intactKBA")
    updateSelectInput(session = getDefaultReactiveDomain(), "arealandColname", choices = colnames, selected="Area_land")
    updateSelectInput(session = getDefaultReactiveDomain(), "intactseedColname", choices = colnames, selected="intactKBA")
    updateSelectInput(session = getDefaultReactiveDomain(), "intactColpas", choices = colnames, selected="intactKBA")
    updateSelectInput(session = getDefaultReactiveDomain(), "intactColNET", choices = colnames, selected = "intactKBA")
    
  })
  

  ####################################################################################################
  ####################################################################################################
  # Map viewer
  ####################################################################################################
  ####################################################################################################
  # Observe tab changes
  observeEvent(input$tabs, {
    if (input$tabs == "tabUpload") {
      tab_upload_visited(TRUE)  # Mark tabUpload as visited
    }
    if (input$tabs != "tabUpload" && !tab_upload_visited() && input$tabs != "overview") {
      # Show modal message if tabUpload has not been visited
      showModal(modalDialog(
        title = "Action Required",
        "Please visit the 'Set input parameters' tab to initialize the map and upload the required dataset before proceeding to the next steps.",
        easyClose = TRUE,
        footer = modalButton("Go to tabUpload")
      ))
      
      # Redirect user back to tabUpload
      updateTabItems(session = getDefaultReactiveDomain(), "tabs", "tabUpload")
    }
  })
  
  # Render the initial map
  output$map <- renderLeaflet({
    # Render initial map
    map <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      fitBounds(lng1 = -121, lat1 = 44, lng2 = -65, lat2 = 78)%>%
      addMapPane(name = "layer1", zIndex=380) %>%
      addMapPane(name = "layer2", zIndex=420) %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>% 
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addPolygons(data=intact_4326(), fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intact areas", options = leafletOptions(pane = "layer1")) %>%
      addPolygons(data=bnd, color='grey', fill=F, weight=1, group="Canada extent", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Intact areas"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c(""))
  })
  
  observeEvent(catchments(), {
    #req(!is.null(catchments()))
    #if(!is.null(catchments())){
      req(catchments())
      # show pop-up ...
      showModal(modalDialog(
       title = "Uploading layers. Please wait...",
       easyClose = TRUE,
       footer = NULL)
      )
      catch_bnd <- st_union(catchments())
      catch_bnd <-catch_bnd %>%
        st_buffer(20) %>%
        st_buffer(-20)
      catch_extent <- st_transform(catch_bnd, 4326)
      map_bounds1 <- catch_extent %>% st_bbox() %>% as.character()

      leafletProxy("map") %>%
       fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
       addPolygons(data=catch_extent, color='black', fill = F, weight=3, group="Catchments extent", options = leafletOptions(pane = "layer2")) %>%
       addLayersControl(position = "topright",
                        baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                        overlayGroups = c("Catchments extent", "Intact areas"),
                        options = layersControlOptions(collapsed = FALSE))  %>%
       hideGroup(c(""))
      
      # Close the modal once processing is done
      removeModal()
    })

  observeEvent(planreg(), {
    #if(!is.null(planreg())){
      #Test if catchments are uploaded
      if (is.null(catchments())) {
        # Create the modal dialog
        showModal(modalDialog(
          title = "Missing Data",
          "Catchments layers is missing. Please go back to Set input parameters to upload catchments layer.",
         easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
      
      req(catchments())
      req(planreg())
      planreg_4326 <- st_transform(planreg(), 4326)
      #map_bounds1 <- catch_extent %>% st_bbox() %>% as.character()
      
      leafletProxy("map") %>%
        #fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
        addPolygons(data=planreg_4326, color='red', fill = F, weight=3, group="Planning region", options = leafletOptions(pane = "layer1")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Intact areas"),
                         options = layersControlOptions(collapsed = FALSE))  %>%
        hideGroup(c(""))
    })

  observeEvent(pas_sf_reactive(), {
    #if(!is.null(pas_sf_reactive())){
      #Test if catchments are uploaded
      if (is.null(catchments())) {
         # Create the modal dialog
        showModal(modalDialog(
           title = "Missing Data",
          "Catchments layers is missing. Please go back to Set input parameters to upload catchments layer.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
          return()
      }
        
      req(pas_sf_reactive())
      req(catchments())
      pas_4326 <- st_transform(pas_sf_reactive(), 4326)
      #map_bounds1 <- catch_extent %>% st_bbox() %>% as.character()
      leafletProxy("map") %>%
        #fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
        addPolygons(data=pas_4326, color='#6b4b38', fillOpacity = 0.6, weight=2, group="Protected areas", options = leafletOptions(pane = "layer1")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Intact areas", "Protected areas"),
                         options = layersControlOptions(collapsed = FALSE))  %>%
        hideGroup(c(""))
      
      # Close the modal once processing is done
     # removeModal()
  }, once = TRUE)

  ####################################################################################################
  ####################################################################################################
  # BUILD KBAs
  ####################################################################################################
  ####################################################################################################
  ####################################################################################################
  # -Create BUILDER input
  ####################################################################################################
  observeEvent(input$runBuilderInput, {
    #Test if catchments are uploaded
    if (is.null(catchments())) {
      # Create the modal dialog
      showModal(modalDialog(
        title = "Missing Data",
        "Catchments layer is missing. Please go back to Set input parameters to upload catchments layer.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    req(catchments())
    req(dirpath())
    # show pop-up ...
    showModal(modalDialog(
      title = "Creating BUILDER input. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    #Clear previous map
    leafletProxy("map") %>%
      clearGroup('Potential KBAs') %>%
      clearGroup('Upstream') %>%
      clearGroup(reactive_labelKBA()) %>%
      clearGroup(reactive_labelNET()) %>%
      clearGroup("CMI") %>%
      clearGroup("LED") %>%
      clearGroup("GPP") %>%
      clearGroup("LCC") %>%
      clearGroup(criteria5name()) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Catchments extent", "Planning region", "Intact areas"),
                       options = layersControlOptions(collapsed = FALSE))
    
    out_dir <- dirpath()
    # Generate neighbours table for catchments - Builder_input file for Builder. Skip this step is nghbrs.csv already exists.
    if (!is.null(input$upload_nghbr)) {
      nghbrs_path <- input$upload_nghbr$datapath
      nghbrs <- read.csv(nghbrs_path)
      nghbrs_reactive(nghbrs)
      write.csv(nghbrs, file=file.path(out_dir,"Builder_input/nghbrs.csv"), row.names=FALSE) # Convert neighbours table to csv file.
    }else{
      if (!file.exists(file.path(out_dir, "Builder_input/nghbrs.csv"))) {
        nghbrs <- neighbours(catchments())
        nghbrs_reactive(nghbrs)
        write.csv(nghbrs, file=file.path(out_dir,"Builder_input/nghbrs.csv"), row.names=FALSE) # Convert neighbours table to csv file.
      }else{
        nghbrs <- read.csv(file.path(out_dir,'Builder_input','nghbrs.csv'))
        nghbrs_reactive(nghbrs)
      }
    }
    # Create seed list - input file for Builder that identifies where construction of conservation area is to start
    # intact ranges from 0 to 1 and is the minimum required proporational intactness required for a catchment to be a seed (0.8 = 80%)
    # areatarget_value is in m2 and specifies the desired conservation area size (10,000 km2 = 10000000000 m2)
    if (!is.null(input$upload_seed)) {
      seed_path <- input$upload_seed$datapath
      seed <- read.csv(seed_path)
      seed_reactive(seed)
      write.csv(seed, file=file.path(out_dir,"Builder_input/seeds.csv"), row.names=FALSE) # Convert neighbours table to csv file.
    }else{
      if (!file.exists(file.path(out_dir, "Builder_input/seeds.csv"))) {
        seed <- catchments() %>%
          filter(input$intactseedColname >= input$seedintact, STRAHLER == as.numeric(input$set_strahler), eco ==1) %>%
          seeds(catchments_sf = ., areatarget_value = as.numeric(input$set_areatarget))
        seed_reactive(seed)
        write.csv(seed, file=file.path(out_dir,"Builder_input/seeds.csv"), row.names=FALSE) # Convert neighbours table to csv file.
      }else{
        seed <- read.csv(file.path(out_dir,"Builder_input/seeds.csv"))
        seed_reactive(seed)
      }
    }

    # show pop-up ...
    showModal(modalDialog(
      title = "Builder input created.",
      
      # Conditional content based on input values
      if (dir_exists()) {
        "The output directory already exists. The app used the data previously generated. 
        If you plan on changing inputs or parameters for this analysis, please point to another directory."
      },
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
    
  ####################################################################################################
  # -Run BUILDER
  ####################################################################################################
  observeEvent(input$runBuilder>0, { 
    req(catchments())
    req(dirpath())
    
    out_dir <- dirpath()
    # Define your patterns
    f_req <- c("Unique_BAs_attributes", "HYDROLOGY_METRICS", "UPSTREAM_CATCHMENTS_COLUMN")
    
    # Get the list of files in the directory
    flist <- list.files(path = file.path(dirpath(), "Builder_output"), pattern = NULL)
    
    match_found <- any(sapply(f_req, function(f) any(grepl(f, flist))))
      # Check if any file matches the pattern
    if(!match_found) {
        # show pop-up ...
        showModal(modalDialog(
          title = "Running BUILDER",
          "Please wait...",
          footer = NULL
        ))
      seed <- seed_reactive()
      nghbrs <- nghbrs_reactive()

      tryCatch({
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
                             stream_length = "length_m",
                             intactness = input$intactColname,
                             isolated = "Isolated",
                             unique_identifier = "KBA",
                             handler_summary = FALSE,
                             summary_intactness_props = "\"\"",
                             summary_area_target_props = "\"\"")
        
        # Fix PB to KBA
        builder_tab <- builder_tab %>%
          rename_with(~ str_replace(.x, "PB", "KBA"))
      
        # Convert conservation areas created by builder to polygons.(NOTE: poly_sf is the R object with conservation areas.)
        poly_sf <- dissolve_catchments_from_table(catchments_sf = catchments(), 
                                                  input_table = builder_tab, 
                                                  out_feature_id = "network")
        poly_sf <- poly_sf %>%
            st_buffer(dist = 20) %>% 
            st_buffer(dist = -20)
        
        kba_sf_reactive(poly_sf)  # Store the poly_sf in reactiveVal

        # Append the first layer to the GeoPackage
        st_write(poly_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_builder", driver = "GPKG", append = TRUE)
        
        # Close the modal once processing is done
        removeModal()
        
        # show pop-up ...
        showModal(modalDialog(
          title = "Builder output created.",
          paste0("Number of KBAs created: ", as.character(nrow(poly_sf))),
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      }, error = function(err) {
        # Close the "Please wait" modal if it is open
        removeModal()
        
        # Show an error modal with the error message
        showModal(modalDialog(
          title = "Error Running BUILDER",
          paste("Please check Builder software is found in your beaconsbuilder library and that you have .NET framework 3.5 installed:", conditionMessage(err)),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      })
      } else {
        poly_sf <- st_read(dsn = file.path(out_dir, "output/KBA_analysis.gpkg"), layer = "KBAs_builder")
        kba_sf_reactive(poly_sf)
        showModal(modalDialog(
          title = "Builder output already exist.",  paste0("The app used the data previously generated.
          If you changed inputs or parameters for this analysis, please point to another directory.",
          "Number of KBAs created:  ", as.character(nrow(poly_sf))),
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      }
  })
  
  ####################################################################################################
  # -Calculate hydro metrics on KBAs
  ####################################################################################################
  observeEvent(input$calc_dci, {
    #Test if streams  and catchments are uploaded
    if (is.null(streams()) || is.null(catchments())) {
      if(!is.null(streams())){
        showModal(modalDialog(
          title = "Missing Data",
          "Catchments dataset is missing. Please go back to Set input parameters to upload the catchments dataset",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      } else if(!is.null(catchments())){
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
    
    req(streams())
    req(catchments())
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    layer_to_check <- "KBAs_reduced_FALSE"
    
    if (!layer_to_check %in% layers) {
      showModal(modalDialog(
        title = "Processing",
        paste0("Calculating hydrology metrics on ", as.character(nrow(kba_sf_reactive())), " features. Please wait..."),
        footer = NULL
      ))
      if (!"KBAs_builder" %in% layers) {
        showModal(modalDialog(
          title = "Layer missing",
          paste0("You need to run Builder prior to run the analysis."),
          footer = NULL
        ))
      }else{
        poly_sf <- kba_sf_reactive()

        # Identify the attributes file and read it
        attributefile <- list.files(file.path(dirpath(),"Builder_output"), pattern = "Unique_BAs_attributes")
        attributeStats <- read.csv(file.path(dirpath(), "Builder_output", attributefile))
    
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
        hydrofile <- list.files(file.path(dirpath(), "Builder_output"), pattern = "HYDROLOGY_METRICS")
        hydroStats <- read.csv(file.path(dirpath(), "Builder_output", hydrofile))
    
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
        upfile <- list.files(file.path(dirpath(), "Builder_output"), pattern = "UPSTREAM_CATCHMENTS_COLUMN")
        upstream <- read.csv(file.path(dirpath(), "Builder_output", upfile))
        upstream_list <-as_tibble(upstream[,-1])
      
        #Fix PB to KBA and generate upstream area
        upstream_list <- upstream_list %>%
          rename_with(~ str_replace(.x, "PB", "KBA"))
        upstream_area <- dissolve_catchments_from_table(catchments(), upstream_list, "network")  
      
        #Update reactiveVal
        upstream_reactive(upstream_area)
      
        # Export. Append the first layer to the GeoPackage
        st_write(upstream_area, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_upstream", driver = "GPKG", append = TRUE)
  
        # DENDRITIC CONNECTIVITY (DCI) - CALCULATE AND ADD TO TABLE
        # A measure of longitudinal hydrological connectivity within each conservation area with values ranging from 
        # 0 (low connectivity) to 1 (fully connected).
    
        # Calculate DCI and add the values as a new column (attribute = dci).
        poly_sf$dci <- calc_dci(conservation_area_sf = poly_sf, 
                            stream_sf = streams())
      
        #Update reactiveVal
        kba_sf_reactive(poly_sf)
    
        # Export. Append the first layer to the GeoPackage
        st_write(poly_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_reduced_FALSE", driver = "GPKG", append = TRUE)
      
        # Close the modal once processing is done
        removeModal()
      }
    } else{
      upstream_area <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_upstream")
      upstream_reactive(upstream_area)
      poly_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_reduced_FALSE")
      kba_sf_reactive(poly_sf)
    } 
    showModal(modalDialog(
      title = "Hydrology metrics added",
      easyClose = FALSE,
      footer = modalButton("OK"))
    )  
    
    kba_sf_4326 <- st_transform(kba_sf_reactive(), 4326) %>% st_simplify(dTolerance = 0.001)
    stream_4326 <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "stream_4326")
    leafletProxy("map") %>%
      addPolygons(data=kba_sf_4326, color='black', fillColor = "transparent", fillOpacity = 0, weight=1, group="Potential KBAs (all)", options = leafletOptions(pane = "layer2")) %>%
      addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs (all)", "Intact areas","Streams"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams"))
    #removeModal()
  })  

  ####################################################################################################
  # REDUCE KBAs
  ####################################################################################################
  observeEvent(input$reduce_KBAs, {
    
    #Test on required layers
    if (is.null(kba_sf_reactive()) || is.null(upstream_reactive()) || is.null(streams()) || is.null(planreg())) {
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
    
    req(!is.null(kba_sf_reactive()))
    req(!is.null(upstream_reactive()))
    req(input$set_grid)
    req(streams())
    req(planreg())
    
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    layer_to_check <- paste0("KBAs_reduced_", input$set_grid)
    if (!layer_to_check %in% layers) {
      showModal(modalDialog(
        title = "Processing",
        "Reduce number of potential KBAs. Please wait...",
        footer = NULL
      ))
      # REDUCE NUMBER OF CONSERVATION AREAS
      # Select the top conservation area from each group based on smallest upstream area, largest DCI, and largest upstream intactness
      kba_sf <- kba_sf_reactive()
      
      kba_sf$group_id <- group_conservation_areas(kba_sf, as.numeric(input$set_grid))  
      
      kba_sf <- kba_sf %>%
        group_by(group_id) %>%
        arrange(-dci, up_km2, -up_AWI) %>% # Attribute order indicates their importance when selecting the 'best'from each group. '-' indicates largest to smallest.
        filter(row_number()==1)
      
      # Close the modal once processing is done
      removeModal()
      
      kba_sf_reactive(kba_sf)
      kba_reduce_reactive(kba_sf)
    } else {
      kba_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = paste0("KBAs_reduced_", input$set_grid))
      kba_reduce_reactive(kba_sf)
    }
    
    showModal(modalDialog(
      title = paste0("KBAs number reduced to ", as.character(nrow(kba_sf)), "."),
      "  Display KBAs. Please wait...",
      footer = NULL
    ))
    
    kba_sf_4326 <- st_transform(kba_sf, 4326) %>% st_simplify(dTolerance = 0.001)
    leafletProxy("map") %>%
      addPolygons(data=kba_sf_4326, fillColor='#666666', color= "#000000", weight = 1,  group="Potential KBAs (reduced)", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs (all)", "Potential KBAs (reduced)", "Intact areas","Streams"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Potential KBAs (all)"))
    # Close the modal once processing is done
    removeModal()
    
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    kba_count <- NA
    pas_count <- NA
    if ("PAs" %in% layers) {
      pas <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs")
      pas_count <- nrow(pas)
    }
    # Generate Stat Tables    
    x <- tibble(
      Variables = c("KBAs", "PAs", "Filtered KBAs", "Filtered PAs"),
      Count = c(nrow(kba_sf), pas_count, NA, NA)  # Adjust other counts if needed
    )
    outfreqkba(x) 
  }) 
  
  observeEvent(input$save_reduce, {
    req(!is.null(kba_reduce_reactive()))
    
    showModal(modalDialog(
      title = "Reduces KBAs layer saved. ",
      easyClose = TRUE,
      footer = modalButton("OK"))
    )
    st_write(kba_reduce_reactive(), dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = paste0("KBAs_reduced_", input$set_grid), driver = "GPKG", append = FALSE)
  }, ignoreInit = TRUE)
  ####################################################################################################
  ####################################################################################################
  # EVALUATE PAs
  ####################################################################################################
  ####################################################################################################
  # Observe map click events to update the selected polygon
  observeEvent(input$map_shape_click, {
    selected_polygon(input$map_shape_click$id)  # Store the layerId of the clicked polygon
  })
  
  ####################################################################################################
  # -Calculate hydro metrics
  ####################################################################################################
  observeEvent(input$calc_pasdci, {
    #Test on required layers
    if(is.null(pas_sf_reactive())){
      showModal(modalDialog(
        title = "No protected areas layer has been uploaded",  
        "Please upload a shapefile" ,
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
      return()
    }
    #Test if streams are uploaded
    if (is.null(streams())) {
      # Create the modal dialog
      showModal(modalDialog(
        title = "Missing Data",
        "Stream layer is missing. Please go back to Set input parameters to upload the stream layer.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    req(pas_sf_reactive())
    req(catchments())
    req(streams())
    
    catchments <- catchments()
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    layer_to_check <- "PAs"
    
    if (!layer_to_check %in% layers) {
      showModal(modalDialog(
        title = "Processing",
        "Calculating hydrology metrics on protected areas. Please wait...",
        footer = NULL
      ))
      # Intactness
      pas_sf <- pas_sf_reactive() %>%
        mutate(network = sprintf("PA_%02d", row_number()),
               area_km2 = st_area(.)/1000000,
        )
      
      pas_catch <- st_intersection(pas_sf, catchments)
      area_catch <- pas_catch %>%
        mutate(catch_awi = as.numeric(st_area(.)) * .[[input$intactColpas]]) %>%
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
          mutate(up_cAWI = as.numeric(Area_total * .[[input$intactColpas]]), 
                 network = pa_id) %>%
          group_by(network) %>%
          summarize(up_intactkm2 = sum(up_cAWI, na.rm = TRUE)/1000000, .groups = "drop")
        
        # Dissolve and merge upstream areas
        upstream_area <- dissolve_catchments_from_table(catchments, upstream_list, "network") %>%
          left_join(area_intact[, c("network", "up_intactkm2")], by = "network") %>%
          mutate(up_km2 = st_area(.)/1000000,
                 up_AWI = round(up_intactkm2 / as.numeric(up_km2), 3))
        
        return(upstream_area)
      }, pa_id = pas$network, upstream_list = upstream_catchments_list, SIMPLIFY = FALSE)
      
      pas_up <- do.call(rbind, results_list)
      
      # Export  and update reactive value 
      st_write(pas_up, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs_upstream", driver = "GPKG", append = TRUE)
      pas_upstream_reactive(pas_up)
      
      pas_up <- pas_up %>% st_drop_geometry()
      pas <- merge(pas, pas_up[,c("network","up_km2", "up_AWI")], by = "network", all.x= TRUE)
      
      # Calculate DCI
      pas$dci <- calc_dci(conservation_area_sf = pas, 
                              stream_sf = streams())
      # Export  and update reactive value 
      st_write(pas, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs", driver = "GPKG", append = TRUE)
      pas_sf_reactive(pas)
      
      # Close the modal once processing is done
      removeModal()
    }else{
      pas_up <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs_upstream")
      pas_upstream_reactive(pas_up)
      pas <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs")
      pas_sf_reactive(pas)
    } 
    
    showModal(modalDialog(
      title = "Hydrology metrics added",
      easyClose = TRUE,
      footer = modalButton("OK"))
    ) 

    ####################################################################################################
    # -Render PAs map
    ####################################################################################################
    pas_4326 <- st_transform(pas, 4326)
    #stream_4326 <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "stream_4326")
    leafletProxy("map") %>%
      clearGroup("Protected areas") %>%
      #clearGroup("Streams") %>%
      addPolygons(data=pas_4326, color='#6b4b38', fillOpacity = 0.6, weight=2, layerId = pas_4326$network, popup = ~network, group="Protected areas", options = leafletOptions(pane = "layer2")) #%>%
      #addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "layer1")) %>%
      
    if(!is.null(kba_reduce_reactive())){
      leafletProxy("map") %>%
        addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs (all)", "Potential KBAs (reduced)", "Intact areas", "Protected areas","Streams"),
                       options = layersControlOptions(collapsed = FALSE))  %>%
      hideGroup(c("Streams"))
    }else if(!is.null(kba_sf_reactive())) {
      leafletProxy("map") %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs (all)", "Intact areas", "Protected areas","Streams"),
                         options = layersControlOptions(collapsed = FALSE))  %>%
        hideGroup(c("Streams"))
    }else{
      leafletProxy("map") %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Intact areas", "Protected areas","Streams"),
                         options = layersControlOptions(collapsed = FALSE))  %>%
        hideGroup(c("Streams"))
    }
    
    ####################################################################################################
    # -Render PAs statistics table
    ####################################################################################################
    outtabPA <- reactive({
      req(input$tabs == 'tabPAs')
      pas <- pas_sf_reactive() %>%
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
      
      #############################
      # -Render PAs frequency table
      #x <- tibble(
      #  Variables = c("PAs", "Filtered PAs"),
      #  Count = c(nrow(pas),NA)
      #)
      #outfreqkba(x)
    
      return(final)
    })
    
    output$pastbl <- renderDataTable({
      req(input$tabs == 'tabPAs')
      # Get the reactive data and the selected polygon ID
      table_data <- outtabPA()
      selected_id <- selected_polygon()
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
  
  ####################################################################################################
  ####################################################################################################
  # ASSESS REPRESENTATION 
  ####################################################################################################
  ####################################################################################################
  #########################################################
  #-UPDATE FREQUENCY TABLE AND MAX UPSTREAM SLIDER
  #########################################################
  observeEvent(input$tabs, {
    req(input$tabs == "tabKBA", dirpath())
    
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    reduced_kba <- layers[grepl("^KBAs_reduced", layers)]
    if (length(reduced_kba) > 0) { 
      if(paste0("KBAs_reduced_", input$set_grid) %in% reduced_kba){
        updateSelectInput(session = getDefaultReactiveDomain(), "KBAlayer", choices = reduced_kba, selected = paste0("KBAs_reduced_", input$set_grid))
      }else{
        updateSelectInput(session = getDefaultReactiveDomain(), "KBAlayer", choices = reduced_kba, selected = reduced_kba[1])
      }
    }
    if ("PAs" %in% layers) {
      pas_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs")
      x <- outfreqkba()
      x <- x %>% 
        mutate(Count = case_when(Variables == "PAs" ~  nrow(pas_sf),
                                 TRUE ~ Count))
      outfreqkba(x)
      
      output$outkbafreq <- renderTable({
        outfreqkba()
      })
    } 
  })
  
  observeEvent(input$KBAlayer, {
    if (input$tabs == "tabKBA") {
      layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
      layers <- layers_info$name
      # Initialize kba_sf and pas_sf as NULL
      kba_sf <- NULL
      if (!(input$KBAlayer=="No KBA generated")){
        kba_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = input$KBAlayer)
        kba_sf_reactive(kba_sf)
      } 
      
      x <- outfreqkba()
      x <- x %>% 
        mutate(Count = case_when(Variables == "KBAs" ~  ifelse(!is.null(kba_sf), nrow(kba_sf), NA_integer_),
                                 TRUE ~ Count))
      outfreqkba(x)
      
      output$outkbafreq <- renderTable({
        outfreqkba()
      })
      
      output$slidercrit5 <- renderUI({
        # Check if criteria5() is NULL
        if (!is.null(criteria5())) {
          # If criteria5 is NULL, render the sliderInput with disabled = TRUE
          div(style = "margin-top: -30px;", sliderInput("slidecrit5", label = criteria5name(), min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE))
        }
      })
      
    }
  })

  #########################################################
  #-RUN REPRESENTATION
  #########################################################
  observeEvent(input$runRep, {
    #Test on required objects
    if (is.null(refarea_reactive()) || is.null(streams()) || is.null(planreg()) || is.null(cmi()) || is.null(gpp()) || is.null(led()) || is.null(lcc())) {
      missing_layers <- c(
        if (is.null(refarea_reactive())) "reference area",
        if (is.null(streams())) "stream",
        if (is.null(planreg())) "planning region",
        if (is.null(cmi())) "CMI",
        if (is.null(gpp())) "GPP",
        if (is.null(led())) "LED",
        if (is.null(lcc())) "LCC"
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
    
    req(refarea_reactive())
    req(catchments())
    req(input$assessKBAs)
    
    showModal(modalDialog(
      title = "Processing representation analysis",
      "Please wait...",
      footer = NULL
    ))

    # Check if there is a 5 criteria and store the name
    if (!is.null(input$upload_custom)) {
      rastName <- sub("\\..*$", "", input$upload_custom$name)
      criteria5name(rastName)
      updated_grp <- c(legendcrit(), rastName)
      legendcrit(updated_grp) # Update the reactive value
    }
    if (!is.null(input$csv_file)) {
      csv_data <- read.csv(input$csv_file$datapath)
      req_layers <- c("CMI", "LED", "GPP", "LCC", "catchments", "stream", "planning region", "protected areas", "reference area")
      unexpected_layers <- csv_data$Layer[!csv_data$Layer %in% req_layers]
      criteria5name(unexpected_layers)
      updated_grp <- c(legendcrit(), unexpected_layers)
      legendcrit(updated_grp) # Update the reactive value
    }
    
    #Prep criteria
    if (!file.exists(file.path(dirpath(), "output/kba_cmi.tif"))) {
      cmi <- process_raster(cmi(), refarea_reactive(), dirpath(), "kba_cmi", fact = 2)
      kba_cmi <- cmi$original
      cmi_4326 <- cmi$projected
    }else{
      kba_cmi <- cmi()
      cmi_4326 <- raster(file.path(dirpath(), "output/kba_cmi_4326.tif"))
    }
    if (!file.exists(file.path(dirpath(), "output/kba_led.tif"))) {
      led <- process_raster(led(), refarea_reactive(), dirpath(), "kba_led", fact = 4)
      kba_led <- led$original
      led_4326 <- led$projected
    } else{
      kba_led <- led()
      led_4326 <- raster(file.path(dirpath(), "output/kba_led_4326.tif"))
    }
    if (!file.exists(file.path(dirpath(), "output/kba_gpp.tif"))) {
      gpp <- process_raster(gpp(), refarea_reactive(), dirpath(), "kba_gpp", fact = 4)
      kba_gpp <- gpp$original
      gpp_4326 <- gpp$projected
    } else{
      kba_gpp <- gpp()
      gpp_4326 <- raster(file.path(dirpath(), "output/kba_gpp_4326.tif"))
    }
    if (!file.exists(file.path(dirpath(), "output/kba_lcc.tif"))) {
      lcc <- process_raster(lcc(), refarea_reactive(), dirpath(), "kba_lcc", fact = 40, aggregation_fun = modal, ignored = c(15, 17))
      kba_lcc <- lcc$original
      lcc_4326 <- lcc$projected
    }else{
      kba_lcc <- lcc()
      lcc_4326 <- raster(file.path(dirpath(), "output/kba_lcc_4326.tif"))
    }
    if(!is.null(criteria5())){
      if (!file.exists(file.path(dirpath(), "output", paste0(criteria5name(), ".tif")))) {
        crit5 <- process_raster(criteria5(), refarea_reactive(), dirpath(), criteria5name(), fact = 4)
        kba_criteria5 <- crit5$original
        crit5_4326 <- crit5$projected
      }else{
        kba_criteria5 <- raster(file.path(dirpath(), "output", paste0(criteria5name(), ".tif")))
        crit5_4326 <- raster(file.path(dirpath(), "output", paste0(criteria5name(), "_4326.tif")))
      }
    }
              
    #Set legend
    cmi_minVar <- min(floor(values(kba_cmi)), na.rm = TRUE)
    cmi_maxVar <- max(ceiling(values(kba_cmi)), na.rm = TRUE)
    cmi_bins.seq <- seq(cmi_minVar, cmi_maxVar, (cmi_maxVar-cmi_minVar)/4)
    xpal <- colorBin("RdYlBu", cmi_bins.seq, bins = cmi_bins.seq, na.color = "transparent")
    val.color <- "RdYlBu"
    
    led_minVar <- min(floor(values(kba_led)), na.rm = TRUE)
    led_maxVar <- max(ceiling(values(kba_led)), na.rm = TRUE)
    led_bins.seq <- seq(led_minVar, led_maxVar, (led_maxVar-led_minVar)/4)
    led_xpal <- colorBin("Blues", led_bins.seq, bins = led_bins.seq, na.color = NA)
    led_val.color <- "Blues"
    
    gpp_minVar <- min(floor(values(kba_gpp)), na.rm = TRUE)
    gpp_maxVar <- max(ceiling(values(kba_gpp)), na.rm = TRUE)
    gpp_bins.seq <- seq(gpp_minVar, gpp_maxVar, (gpp_maxVar-gpp_minVar)/4)
    gppxpal <- colorBin("RdYlBu", gpp_bins.seq, bins = gpp_bins.seq, na.color = "transparent")
    
    unique_sorted_values <- sort(na.omit(unique(values(lcc_4326))))
    df_label = data.frame(values=c(1,2,5,6,8,10,11,12,13,14,15,16,17,18,19), labels=c("Temperate conifer forest", "Taiga conifer forest",
                                                                                      "Broadleaf forest", "Mixed Forest", "Shrubland", "Grassland", 
                                                                                      "Shrubland-lichen-moss", "Grassland-lichen-moss","Barren-lichen-moss",
                                                                                      "Wetland",  "Cropland", "Barren Lands", "Urban", "Water", "Snow"))
    df_label <- df_label[df_label$values %in% unique_sorted_values, ]
    cls <- df_label$labels
    lcc_cols <- read.csv('www/lc_cols.csv') %>%
      filter(value %in% unique_sorted_values) %>%
      mutate(color=rgb(red,green,blue,maxColorValue=255)) %>%
      pull(color)
    selected_cols <- lcc_cols    
    labeller_function <- function(type, breaks) {
      return(c('Low', '', '', 'High'))
    }
    if(!is.null(criteria5())){
      c5_minVar <- min(floor(values(kba_criteria5)), na.rm = TRUE)
      c5_maxVar <- max(ceiling(values(kba_criteria5)), na.rm = TRUE)
      c5_bins.seq <- seq(c5_minVar, c5_maxVar, (c5_maxVar-c5_minVar)/4)
      crit_xpal <- colorBin("RdYlBu", c5_bins.seq, bins = c5_bins.seq, na.color = "transparent")
      val.color <- "RdYlBu"
    }
    
    # Select type of analysis
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    
    
    if(input$assessKBAs == "Only KBAs" || input$assessKBAs == "Both KBAs and PAs"){
      if(is.null(kba_sf_reactive())){
        showModal(modalDialog(
          title = "KBAs are missing from your gpkg.",
          "Please run Builder and calculate hydrology metrics prior to assess representation.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
      set_grid <- sub("^KBAs_reduced_([^_]+)$", "\\1", input$KBAlayer) 
      kba_up <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_upstream") %>%
        dplyr::select(network)
      if(!(paste0("KBAs_rep_", set_grid) %in% layers)) {
        kba_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = input$KBAlayer)

        if(attr(kba_sf, "sf_column") != "geometry"){
          kba_sf$geometry <- kba_sf$geom
        }
        error_occurred <- FALSE
        tryCatch({
          kba_sf$cmi <- calc_dissimilarity(kba_sf, refarea_reactive(), kba_cmi, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot/cmi"))
          kba_sf$led <- calc_dissimilarity(kba_sf, refarea_reactive(), kba_led, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot/led"))
          kba_sf$gpp <- calc_dissimilarity(kba_sf, refarea_reactive(), kba_gpp, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot/gpp"))
          kba_sf$lcc <- calc_dissimilarity(kba_sf, refarea_reactive(), kba_lcc, 'categorical', plot_out_dir=file.path(dirpath(), "/output/plot/lcc"), categorical_class_labels = df_label)
          # criteria5
          if(!is.null(criteria5())){
            kba_sf[[criteria5name()]] <- calc_dissimilarity(kba_sf, refarea_reactive(), kba_criteria5, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot", criteria5name()))
          }
        }, error = function(e) {
          error_occurred <<- TRUE
          # Show an error modal with the error message
          showModal(modalDialog(
            title = "Error calculating disimilarity",
            paste("Possible issues may involve partially overlapping objects or the reserve's size being too small relative to the raster's resolution. Code error returns:", e$message),
            easyClose = FALSE,
            footer = modalButton("OK")
          ))
        })
        if (error_occurred) {
          return(NULL)  # Stop execution of the rest of the observer
        }
        kba_sf <- kba_sf %>%
          dplyr::select(-Area_PB)
        st_write(kba_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = paste0("KBAs_rep_", set_grid), driver = "GPKG", append = FALSE)
        kba_sf_reactive(kba_sf)
        #if ("PAs_rep" %in% layers) {
        #  pas_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs_rep")
        #}else{
        #  pas_sf <- NULL
        #}
      }else{
        kba_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = paste0("KBAs_rep_", set_grid))
        if(attr(kba_sf, "sf_column") != "geometry"){
          kba_sf$geometry <- kba_sf$geom
        }
        if(!is.null(criteria5())){
          if(!has_name(kba_sf, criteria5name())){
            error_occurred <- FALSE
            tryCatch({
              kba_sf[[criteria5name()]] <- calc_dissimilarity(kba_sf, refarea_reactive(), kba_criteria5, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot", criteria5name()))
            }, error = function(err) {
              error_occurred <- TRUE
              # Show an error modal with the error message
              showModal(modalDialog(
                title = "Error calculating disimilarity",
                paste("Possible issues may involve partially overlapping objects or the reserve's size being too small relative to the raster's resolution. Code error returns:", e$message),
                easyClose = TRUE,
                footer = modalButton("OK")
              ))
            })
            if (error_occurred) {
              return(NULL)  # Stop execution of the rest of the observer
            }
          }
        }
        kba_sf_reactive(kba_sf)
        kba_up <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_upstream")
        kba_upstream_reactive(kba_up)
      } 
    } 
    
    if(input$assessKBAs == "Only PAs" || input$assessKBAs == "Both KBAs and PAs"){
      pas_up <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs_upstream") %>%
        dplyr::select(network)
      if (!("PAs_rep" %in% layers)) {
        pas_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs")
        if(attr(pas_sf, "sf_column") != "geometry"){
          pas_sf$geometry <- pas_sf$geom
        }
        error_occurred <- FALSE
        tryCatch({
          pas_sf$cmi <- calc_dissimilarity(pas_sf, refarea_reactive(), kba_cmi, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot/cmi"))
          pas_sf$led <- calc_dissimilarity(pas_sf, refarea_reactive(), kba_led, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot/led"))
          pas_sf$gpp <- calc_dissimilarity(pas_sf, refarea_reactive(), kba_gpp, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot/gpp"))
          pas_sf$lcc <- calc_dissimilarity(pas_sf, refarea_reactive(), kba_lcc, 'categorical', plot_out_dir=file.path(dirpath(), "/output/plot/lcc"), categorical_class_labels = df_label)
          # criteria5
          if(!is.null(criteria5())){
            pas_sf[[criteria5name()]] <- calc_dissimilarity(pas_sf, refarea_reactive(), kba_criteria5, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot", criteria5name()))
          }
          }, error = function(e) {
            error_occurred <<- TRUE
            # Show an error modal with the error message
            showModal(modalDialog(
              title = "Error calculating disimilarity",
              paste("Possible issues may involve partially overlapping objects or the reserve's size being too small relative to the raster's resolution. Code error returns:", e$message),
              easyClose = FALSE,
              footer = modalButton("OK")
            ))
        })
        if (error_occurred) {
          return(NULL)  # Stop execution of the rest of the observer
        }
        st_write(pas_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs_rep", driver = "GPKG", append = FALSE)
        pas_sf_reactive(pas_sf)
        #if (paste0("KBAs_rep_", set_grid) %in% layers) {
        #  set_grid <- sub("^KBAs_reduced_([^_]+)$", "\\1", input$KBAlayer) 
        #  kba_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = paste0("KBAs_rep_", set_grid))
        #  if(!is.null(criteria5())){
        #    if(!has_name(kba_sf, criteria5name())){
        #      if(attr(kba_sf, "sf_column") != "geometry"){
        #        kba_sf$geometry <- kba_sf$geom
         #     }
        #      kba_sf[[criteria5name()]] <- calc_dissimilarity(kba_sf, refarea_reactive(), kba_criteria5, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot", criteria5name()))
         #   }
         # }
         # kba_sf_reactive(kba_sf)
        #}else{
        #  kba_sf <- NULL
        #} 
      }else{
        pas_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs_rep")
        pas_sf_reactive(pas_sf)
        if(attr(pas_sf, "sf_column") != "geometry"){
          pas_sf$geometry <- pas_sf$geom
        }
        pas_up <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs_upstream")
        pas_upstream_reactive(pas_up)
      }
    }

    # Close the modal once processing is done
    removeModal()
    
    showModal(modalDialog(
      title = "Mapping results from the representation analysis...",
      "Please wait...",
      footer = NULL
    ))
    
    stream_4326 <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "stream_4326")
    
    #Delete previous dynamic label if
    labelKBA <- reactive_labelKBA()
    
    leafletProxy("map") %>%
      clearControls() %>%
      clearGroup("Potential KBAs (all)") %>%
      clearGroup("Potential KBAs") %>%
      clearGroup("Potential KBAs (reduced)") %>%
      clearGroup("Protected areas") %>%
      clearGroup(labelKBA) %>%
      clearGroup("Streams") %>%
      addTiles() %>%
      addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "layer1")) %>%
      addRasterImage(lcc_4326, colors=selected_cols, opacity = 1, group="LCC",  maxBytes = 5 * 1024 * 1024) %>%
      addRasterImage(led_4326, colors=led_val.color, opacity = 1, group="LED",  maxBytes = 5 * 1024 * 1024) %>%
      addRasterImage(gpp_4326, colors=val.color, opacity = 1, group="GPP",  maxBytes = 5 * 1024 * 1024) %>%
      addRasterImage(cmi_4326, colors=val.color, opacity = 1, group="CMI",  maxBytes = 5 * 1024 * 1024) %>%
      
      addLegend(pal = led_xpal, values = values(led_4326), opacity = 1, title = "LED",
                position = "bottomright", group="LED", labFormat = labeller_function)  %>%
      addLegend(pal = gppxpal, values = values(gpp_4326), opacity = 1, title = "GPP",
                position = "bottomright", group="GPP", labFormat = labeller_function)  %>%
      addLegend(pal = xpal, values = values(cmi_4326), opacity = 1, title = "CMI",
                position = "bottomright", group="CMI", labFormat = labeller_function)  %>%
      addLegend(colors = selected_cols, label = cls,  position=c("bottomleft"), opacity = 1, title = "LCC",
                group="LCC") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Catchments extent", "Planning region", "Intact areas", "Streams", legendcrit()),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Streams"))

    if(!is.null(criteria5())){
      leafletProxy("map") %>%
        addRasterImage(crit5_4326, colors=val.color, opacity = 1, group=criteria5name()) %>%
        addLegend(pal = crit_xpal, values = values(crit5_4326), opacity = 1, title = criteria5name(),
                  position = "bottomright", group=criteria5name(), labFormat = labeller_function)  %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Intact areas", "Streams", legendcrit()),
                         options = layersControlOptions(collapsed = TRUE)) %>%
          hideGroup(c("Streams"))
    }
    
    if(input$assessKBAs == "Only KBAs"){
      upstream_reactive(kba_up)
      poly_reactive(kba_sf)
      
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
      upstream_reactive(pas_up)
      poly_reactive(pas_sf)
      
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
      upstream_reactive(kbapas_up)
      pas_sf <- pas_sf %>%
        dplyr::select(-NAME, -intact_km2)
      kbapas_sf <- rbind(kba_sf, pas_sf)
      poly_reactive(kbapas_sf)
      
      # Update max upstream slider
      max_value <- as.integer(max(kbapas_sf$up_km2, na.rm = TRUE))
      # Update the slider input with the max value
      updateSliderInput(
        session = getDefaultReactiveDomain(),
        inputId = "slideUP",
        max = max_value
      )
    }
    
    unique_kbas <- unique(poly_reactive()$network)
    updateSelectInput(getDefaultReactiveDomain(), "KBA", choices = unique_kbas)
    
    # Close the modal once processing is done
    removeModal()
  })
  

  #######################################
  ### Render map, tables and plot based on select KBA/PA
  observeEvent(input$KBA, {
    req(input$KBA)
    req(poly_reactive())
    
    poly_sf_4326 <- poly_reactive() %>% st_transform(4326)
    # Filter the `sf` object to get the selected KBA based on the input value
    selected_polygon <- poly_sf_4326 %>%
      filter(network == input$KBA) #%>%

    selected_up <- upstream_reactive() %>%
      filter(network == input$KBA) %>%
      st_transform(4326)  # Make sure it's in the correct coordinate system for Leaflet
    
    #Delete previous dynamic label
    labelKBA <- reactive_labelKBA()
    labelNET <- reactive_labelNET
    # Highlight the selected KBA on the map
    leafletProxy("map") %>%
      clearGroup("Potential KBAs") %>%
      clearGroup("Protected areas") %>%
      clearGroup(labelKBA) %>%
      clearGroup(labelNET) %>%
      clearGroup("Upstream") %>%  # Clear previous highlight
      addPolygons(data = selected_polygon, color = "black",  fillColor = "#989898", fillOpacity = 0.8, weight = 2, group = input$KBA) %>%
      addPolygons(data = selected_up, color = "blue",  fillColor = "blue", fillOpacity = 0.2, weight = 2, group = "Upstream") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Catchments extent", "Planning region", "Intact areas", input$KBA, "Upstream", "Streams", legendcrit()),
                       options = layersControlOptions(collapsed = TRUE)
      )
    
    if(input$assessKBAs == "Only KBAs"){
      kba_sf_4326 <- kba_sf_reactive() %>% st_transform(4326)
      leafletProxy("map") %>%
        addPolygons(data=kba_sf_4326, color='black', fillColor = "transparent", fillOpacity = 1 , weight=1, layerId = kba_sf_4326$network, popup = ~network, group="Potential KBAs", options = leafletOptions(pane = "layer2")) %>%
        addLayersControl(position = "topright",
              baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
              overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs", "Intact areas", input$KBA, "Upstream", "Streams", legendcrit()),
              options = layersControlOptions(collapsed = TRUE)
        )
    }
    if(input$assessKBAs == "Only PAs"){
      pas_sf_4326 <- pas_sf_reactive() %>% st_transform(4326)
      leafletProxy("map") %>%
        addPolygons(data=pas_sf_4326, color='#6b4b38', fillOpacity = 0.4, weight=2, layerId = pas_sf_4326$network, popup = ~network, group="Protected areas", options = leafletOptions(pane = "layer2")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Protected areas", "Intact areas", input$KBA, "Upstream", "Streams", legendcrit()),
                         options = layersControlOptions(collapsed = TRUE)
        )
    }
    if(input$assessKBAs == "Both KBAs and PAs"){
      kba_sf_4326 <- kba_sf_reactive() %>% st_transform(4326)
      pas_sf_4326 <- pas_sf_reactive() %>% st_transform(4326)
      leafletProxy("map") %>%
        addPolygons(data=pas_sf_4326, color='#6b4b38', fillOpacity = 0.4, weight=2, layerId = pas_sf_4326$network, popup = ~network, group="Protected areas", options = leafletOptions(pane = "layer2")) %>%
        addPolygons(data=kba_sf_4326, color='black', fillColor = "transparent", fillOpacity = 0, weight=1, layerId = kba_sf_4326$network, popup = ~network, group="Potential KBAs", options = leafletOptions(pane = "layer2")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs", "Protected areas", "Intact areas", input$KBA, "Upstream", "Streams", legendcrit()),
                         options = layersControlOptions(collapsed = TRUE)
        )
    }
    reactive_labelKBA(input$KBA)

    ####################################################################################################
    # Render summary table
    ####################################################################################################
    # Prepare the table for display
    if(is.null(criteria5())){
      x <- tibble(
        Variables = c("Area km2", "AWI", "Upstream area km2", "Upstream AWI", 
                    "DCI", "CMI", "GPP", "LED", "LCC"),
      Values = NA)
    }else{
      x <- tibble(
        Variables = c("Area km2", "AWI", "Upstream area km2", "Upstream AWI", 
                      "DCI", "CMI", "GPP", "LED", "LCC", criteria5name()),
        Values = NA)
    }
    
    x$Values[x$Variables == "Area km2"] <- as.integer(selected_polygon$area_km2)
    x$Values[x$Variables == "AWI"] <- round(as.numeric(selected_polygon$AWI) * 100, 3)
    x$Values[x$Variables == "Upstream area km2"] <- as.integer(selected_polygon$up_km2)
    x$Values[x$Variables == "Upstream AWI"] <- round(as.numeric(selected_polygon$up_AWI) * 100, 2)
    x$Values[x$Variables == "DCI"] <- round(selected_polygon$dci, 3)
    x$Values[x$Variables == "CMI"] <- round(selected_polygon$cmi, 3)
    x$Values[x$Variables == "GPP"] <- round(selected_polygon$gpp, 3)
    x$Values[x$Variables == "LED"] <- round(selected_polygon$led, 3)
    x$Values[x$Variables == "LCC"] <- round(selected_polygon$lcc, 3)

    if(!is.null(criteria5())){
      x$Values[x$Variables == criteria5name()] <- round(selected_polygon[[criteria5name()]], 3)
    }
    
    formatted_x <- x %>%
      mutate(
        Values = case_when(
          Variables %in% c("Area km2", "Upstream area km2") ~ as.character(as.integer(Values)),  # No decimals
          Variables %in% c("AWI", "Upstream AWI") ~ formatC(as.numeric(Values), format = "f", digits = 2),  # 2 decimals
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
    shiny::addResourcePath("image", file.path(dirpath(), "output/plot"))
    
    output$images <- renderUI({
        tagList(
          tags$div(style = "display: flex; flex-wrap: wrap; justify-content: space-around;", 
                   tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                            tags$h3("CMI"),  # Title
                            tags$img(src = paste0("image/cmi/", input$KBA, ".png"), height = "400px", width = "300px")
                   ),
                   tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                            tags$h3("GPP"),  # Title
                            tags$img(src = paste0("image/gpp/", input$KBA, ".png"), height = "400px", width = "300px")
                   ),
                   tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                            tags$h3("LED"),  # Title 
                            tags$img(src = paste0("image/led/", input$KBA, ".png"), height = "400px", width = "300px")
                   ),
                   tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                            tags$h3("LCC"),  # Title 
                            tags$img(src = paste0("image/lcc/", input$KBA, ".png"), height = "400px", width = "300px")
                   ),
                   if(!is.null(criteria5())){
                     tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                              tags$h3(criteria5name()),  # Title 
                              tags$img(src = paste0("image/", criteria5name(), "/", input$KBA, ".png"), height = "400px", width = "300px")
                     )
                   }
          )
        )
      })
  })
  
  ################################################################################################
  # Apply filtering on KBAs
  ################################################################################################  
  observeEvent(input$filterRep, {
    req(catchments())
    req(poly_reactive())
    
    filetred_sf <- poly_reactive()
    if(!is.null(criteria5())){
      filtered_sf_rep <- filter(filetred_sf, lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & !!sym(criteria5name()) <= input$slidecrit5 & up_km2 <= input$slideUP)
    }else{
      filtered_sf_rep <- filter(filetred_sf, lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & up_km2 <= input$slideUP)
    }
    
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
        filtered_kba(filtered_sf_4326)
        leafletProxy("map") %>%
          clearGroup('Potential KBAs') %>% 
          clearGroup('Protected areas') %>% 
          addPolygons(data = filtered_sf_4326, color = 'black', fillColor = "transparent", fillOpacity = 0, weight = 3,
                      layerId = filtered_sf_4326$network, popup = ~network, group = "Potential KBAs", 
                      options = leafletOptions(pane = "layer2")) %>%
          addLayersControl(position = "topright",
                           overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs", "Intact areas", "Streams", legendcrit()),
                           options = layersControlOptions(collapsed = TRUE)) %>%
          hideGroup(c("Streams"))
        
        # Update specific rows based on a condition or manually
        x <- outfreqkba()
        x <- x %>% 
          mutate(Count = case_when(Variables == "Filtered KBAs" ~ ifelse(!is.null(filtered_sf_rep), nrow(filtered_sf_rep), NA_integer_),
                                   TRUE ~ Count)  # Keep existing values for other rows
          )
      }
      if(input$assessKBAs == "Only PAs"){
        filtered_sf_4326 <- filtered_sf_rep %>% st_transform(4326)
        filtered_pas(filtered_sf_4326)
        leafletProxy("map") %>%
          clearGroup('Protected areas') %>% 
          clearGroup('Potential KBAs') %>% 
          addPolygons(data = filtered_sf_4326, color='#6b4b38', fillOpacity = 0.4, weight=2, layerId = filtered_sf_4326$network, popup = ~network, group = "Protected areas", 
                      options = leafletOptions(pane = "layer2")) %>%
          addLayersControl(position = "topright",
                           overlayGroups = c("Catchments extent", "Planning region", "Protected areas", "Intact areas", "Streams", legendcrit()),
                           options = layersControlOptions(collapsed = TRUE)) %>%
          hideGroup(c("Streams"))
        
        # Update specific rows based on a condition or manually
        x <- outfreqkba()
        x <- x %>% 
          mutate(Count = case_when(Variables == "Filtered PAs" ~ ifelse(!is.null(filtered_sf_rep), nrow(filtered_sf_rep), NA_integer_),
                                   TRUE ~ Count)  # Keep existing values for other rows
          )
      }
      if(input$assessKBAs == "Both KBAs and PAs"){
        kba <- kba_sf_reactive()[kba_sf_reactive()$network %in% filtered_sf_rep$network,]
        filtered_kba(kba)
        kba_4326 <- kba %>% st_transform(4326)
        pas <- pas_sf_reactive()[pas_sf_reactive()$network %in% filtered_sf_rep$network,]
        filtered_pas(pas)
        pas_4326 <- pas %>% st_transform(4326)
        
        leafletProxy("map") %>%
          clearGroup('Protected areas') %>% 
          clearGroup('Potential KBAs') %>%
          addPolygons(data = kba_4326, color = 'black', fillColor = "transparent", fillOpacity = 0, weight = 2,
                      layerId = kba_4326$network, popup = ~network, group = "Potential KBAs", 
                      options = leafletOptions(pane = "layer2")) %>%
          addPolygons(data = pas_4326, color='#6b4b38', fillOpacity = 0.4, weight=2, layerId = pas_4326$network, popup = ~network, group = "Protected areas", 
                      options = leafletOptions(pane = "layer2")) %>%
          addLayersControl(position = "topright",
                           overlayGroups = c("Catchments extent", "Planning region", "Protected areas", "Potential KBAs", "Intact areas", "Streams", legendcrit()),
                           options = layersControlOptions(collapsed = TRUE)) %>%
          hideGroup(c("Streams"))
        
        # Update specific rows based on a condition or manually
        x <- outfreqkba()
        x <- x %>% 
          mutate(Count = case_when(Variables == "Filtered KBAs" ~ ifelse(!is.null(kba), nrow(kba), NA_integer_),
                                   Variables == "Filtered PAs" ~ ifelse(!is.null(pas), nrow(pas), NA_integer_),
                                   TRUE ~ Count)  # Keep existing values for other rows
          )
      }
      
      # Close the modal once processing is done
      removeModal()
      
      outfreqkba(x)
      output$outkbafreq <- renderTable({
        outfreqkba()
      })
      
    }else{
      leafletProxy("map") %>%
        clearGroup('Potential KBAs') %>%
        clearGroup('Protected areas') %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Intact areas", legendcrit()),
                         options = layersControlOptions(collapsed = TRUE))
      
      showModal(modalDialog(
        title = "No KBA and/or protected area reache those threshold",
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
    }
  })
  
  ################################################################################################
  ################################################################################################
  # Build Network
  ################################################################################################  
  ################################################################################################
  observeEvent(input$tabs, {
    req(input$tabs == "tabNET", dirpath())
    
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    rep_kba <- layers[grepl("^KBAs_rep_", layers)]
    if (length(rep_kba) > 0) { 
      if(paste0("KBAs_rep_", input$set_grid) %in% rep_kba){
        updateSelectInput(session = getDefaultReactiveDomain(), "KBArep", choices = rep_kba, selected = paste0("KBAs_rep_", input$set_grid))
      }else{
        updateSelectInput(session = getDefaultReactiveDomain(), "KBArep", choices = rep_kba, selected = rep_kba[1])
      }
    }
    if ("PAs" %in% layers) {
      pas_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs_rep")
      x <- outfreqnet()
      x <- x %>% 
        mutate(Count = case_when(Variables == "PAs" ~  nrow(pas_sf),
                                 Variables == "Filtered PAs" ~ ifelse(!is.null(filtered_pas()), nrow(filtered_pas()), NA_integer_),
                                 TRUE ~ Count))
      outfreqnet(x)
      
      output$outnetfreq <- renderTable({
        outfreqnet()
      })
    } 
  })
  
  observeEvent(input$KBArep, {
    if (input$tabs == "tabNET") {
      #req(!(is.null(input$KBArep)))
      layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
      layers <- layers_info$name
      # Initialize kba_sf and pas_sf as NULL
      kba_sf <- NULL
      if (!(input$KBArep=="No KBA generated")){
        kba_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = input$KBArep)
      } 
      
      x <- outfreqnet()
      x <- x %>% 
        mutate(Count = case_when(Variables == "KBAs" ~  nrow(kba_sf),
                                 Variables == "Filtered KBAs" ~ ifelse(!is.null(filtered_kba()), nrow(filtered_kba()), NA_integer_),
                                 TRUE ~ Count))
      outfreqnet(x)
      
      output$outnetfreq <- renderTable({
        outfreqnet()
      })
      
      output$slideNETcrit5 <- renderUI({
        # Check if criteria5() is NULL
        if (!is.null(criteria5())) {
          # If criteria5 is NULL, render the sliderInput with disabled = TRUE
          div(style = "margin-top: -30px;", sliderInput("slideNETcrit5", label = criteria5name(), min = 0, max = 1, value = 0.2, step = 0.001, ticks = FALSE))
        }
      })
    }
  })
  
  observeEvent(input$buildNet, {
    req(catchments())
    kba_sf <- NULL
    pas_sf <- NULL
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    
    if (!(input$KBArep=="")){
      kba_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = input$KBArep)
      kba_sf <- kba_sf %>%
        dplyr::select(-any_of("group_id"))
      kba_sf_reactive(kba_sf)
      set_grid <- paste0("reduced", sub("^KBAs_rep_([^_]+)$", "\\1", input$KBArep))
      kba_up <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_upstream")
      kba_upstream_reactive(kba_up)
    }else{
      set_grid <- "reducedFALSE"
    } 
    if ("PAs_rep" %in% layers) {
      pas_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs_rep")
      pas_up <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "PAs_upstream") %>%
        dplyr::select(network)
      pas_upstream_reactive(pas_up)
    }
    
    if(input$forceKBA){
      if(!is.null(criteria5())){
        potential_kbas <- filter(kba_sf_reactive(), lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & 
                                   !!sym(criteria5name()) <= input$slidecrit5 & up_km2 <= input$slideUP)
        kba_sf_reactive(potential_kbas)
      }else{
        potential_kbas <- filter(kba_sf_reactive(), lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & up_km2 <= input$slideUP)
        kba_sf_reactive(potential_kbas)
      }
    }
    
    if(input$forcePAs){
      if (is.null(pas_sf)){
        showModal(modalDialog(
          title = "Hydrology metrics were not calclulated on protected areas layers", "Make sure protected areas are uploaded and run the Evaluate PAs step",
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
      }
      if(as.integer(input$set_net)==0){
        poly_reactive(pas_sf)
        showModal(modalDialog(
          title = "No KBA will be added. The representation analysis will run using only the protected areas",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
        upstream_reactive(pas_upstream_reactive())
      } else if (as.integer(input$set_net)>0){
        if(input$assessKBAs=="Only PAs"){
          showModal(modalDialog(
            title = "You need to assess representation on KBAs prior to add KBAs into the network",
            "Please select Assess representation using 'Only on KBAs' or 'Both KBAs and PAs' prior to add them to the network",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return() 
        }else if(as.integer(input$set_net) > nrow(kba_sf_reactive())){
          showModal(modalDialog(
            title = "The number of KBAs set per network is above the number of KBAs available.",
            "Please revise the number of KBA per network.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        } else {
          kba_up <- kba_upstream_reactive()
          
          pas_up <- pas_upstream_reactive() %>%
            dplyr::select(network)
          kbapas_up <- rbind(kba_up, pas_up)
          upstream_reactive(kbapas_up)
          kba_sf <- kba_sf_reactive()
          kbapas_sf <- rbind(kba_sf, pas_sf)
          poly_reactive(kbapas_sf)
        }
   
      }
    } else {
      agg_pa_name <- NULL
      poly_reactive(kba_sf_reactive())
      upstream_reactive(kba_upstream_reactive)
    }
    
    showModal(modalDialog(
      title = "Processing",
      "Building network. Please wait...",
      footer = NULL
    ))
    
    if(input$forcePAs){
      if(input$forceKBA){
        filtering <- paste0("_", set_grid, "_up", as.character(input$slideUP), "_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),"_led", as.character(input$slideLED),
                          "_lcc", as.character(input$slideLCC))
        if(!is.null(criteria5())){
          #potential_kbas <- filter(poly_reactive(), lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & 
          #                         !!sym(criteria5name()) <= input$slidecrit5 & up_km2 <= input$slideUP)
          outName <- paste0("net",  filtering, "_", criteria5name(), as.character(input$slidecrit5), "_kba", input$set_net, "_filter", as.character(input$forceKBA), "_includePAs")
          network_dir <- paste0("output/plot", outName)
          #poly_reactive(potential_kbas)
        }else{
          #potential_kbas <- filter(poly_reactive(), lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & up_km2 <= input$slideUP)
          outName <- paste0("net", filtering, "_kba", input$set_net, "_filter", as.character(input$forceKBA), "_includePAs")
          network_dir <- paste0("output/plot", outName)
          #poly_reactive(potential_kbas)
        }
      }else{
        outName <- paste0("net_", set_grid, "_kba", input$set_net, "_filter", as.character(input$forceKBA), "_includePAs")
        network_dir <- paste0("output/plot", outName)
      }
    } else {
      if(input$forceKBA){
        filtering <- paste0("_", set_grid, "_up", as.character(input$slideUP), "_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),"_led", as.character(input$slideLED),
                            "_lcc", as.character(input$slideLCC))
        if(!is.null(criteria5())){
          #potential_kbas <- filter(poly_reactive(), lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & 
          #                           !!sym(criteria5name()) <= input$slidecrit5 & up_km2 <= input$slideUP)
          outName <- paste0("net",  filtering, "_", criteria5name(), as.character(input$slidecrit5), "_kba", input$set_net, "_filter", as.character(input$forceKBA))
          network_dir <- paste0("output/plot", outName)
          #poly_reactive(potential_kbas)
        }else{
          #potential_kbas <- filter(poly_reactive(), lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & up_km2 <= input$slideUP)
          outName <- paste0("net",  filtering, "_kba", input$set_net, "_filter", as.character(input$forceKBA))
          network_dir <- paste0("output/plot", outName)
          #poly_reactive(potential_kbas)
        }
      }else{
        outName <- paste0("net_", set_grid, "_kba", input$set_net, "_filter", as.character(input$forceKBA))
        network_dir <- paste0("output/plot", outName)
      }
    }
    
    # Wait for user to set `input$set_net` to at least 2
    if (is.null(input$set_net) || as.integer(input$set_net) < 2) {
      showModal(modalDialog(
        title = "A minimum of 2 KBAs per network is required",
        "Please adjust the network settings to include at least 2 KBAs.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return() 
    }else if(as.integer(input$set_net) > nrow(poly_reactive())){
      showModal(modalDialog(
        title = "The number of KBAs set per network is above the number of KBAs available.",
        "Please revise the number of KBA per network.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    netDir(network_dir)
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    layer_to_check <- outName
    
    if (!layer_to_check %in% layers) {
      potential_kbas <- poly_reactive()
      if(attr(potential_kbas, "sf_column") != "geometry"){
        potential_kbas$geometry <- potential_kbas$geom
      }
  
      if(nrow(potential_kbas)>0){
        if(input$forcePAs){
          k <- nrow(pas_sf_reactive()) + as.numeric(input$set_net)
          network_names <- gen_network_names(in_names = potential_kbas$network, k = k, force_in = agg_pa_name)
        }else {
          k <- as.numeric(input$set_net)
          network_names <- gen_network_names(in_names = potential_kbas$network, k = k)
        }

        #Check and remove overlapping KBAs. 
        overlaps <- list_overlapping_polygons(conservation_areas_sf = potential_kbas)
        network_names <- network_names[!network_names %in% overlaps]
    
        # Build the list of networks using the conservation area polygons. Each network will become a single feature in the polygon object.
        networks_sf <- build_network_polygons(conservation_areas_sf = potential_kbas, network_list = network_names)

        # Split networks_sf into individual polygons by "network"
        network_list <- split(networks_sf, networks_sf$network)
        
        #Upstream
        results_list <- list()
        upstream <- upstream_reactive()
        # Use lapply to process each polygon
        results_list <- lapply(network_list, function(network_group) {
          get_stat_on_net(
            net_sf = network_group,
            catchments = catchments(),
            intact_col = input$intactColNET,
            upstream = upstream
          )
        })
        
        # Combine the results into a single data frame
        networks_sf <- do.call(rbind, results_list)
        
        #upstream_network_sf <- get_upstream(networks_sf, upstream_reactive())
        up_list <- lapply(network_list, function(network_group) {
          get_upstream(
            net_sf = network_group,
            upstream = upstream_reactive()
          )
        })
        # Combine the results into a single data frame
        upstream_network_sf <- do.call(rbind, up_list)
        
        if(!is.null(agg_pa_name)){
          upstream_network_sf <-upstream_network_sf %>% 
            mutate(network = str_replace_all(network, agg_pa_name, "PAs"))                 
        }
        
        upstream_network_reactive(upstream_network_sf)
        if(!is.null(upstream_network_sf)){
          st_write(upstream_network_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = paste0("upstream_", outName), driver = "GPKG", append = TRUE)
        }else{
          showModal(modalDialog(
            title = "No upstream area found for those network. Layer KBA_upstream won't be created.",
            easyClose = TRUE,
            footer = modalButton("OK"))
          )
        }
        
        #Fix network name
        if(!is.null(agg_pa_name)){
          networks_sf <-networks_sf %>% 
            mutate(network = str_replace_all(network, agg_pa_name, "PAs"))                 
        }
                       
        # DCI (ON HOLD)
        #networks_sf$dci <- calc_dci(conservation_area_sf = networks_sf, stream_sf = streams())
        
        #Criteria
        kba_cmi <- raster(file.path(dirpath(), "output/kba_cmi.tif"))
        kba_led <- raster(file.path(dirpath(), "output/kba_led.tif"))
        kba_gpp <- raster(file.path(dirpath(), "output/kba_gpp.tif"))
        kba_lcc <- raster(file.path(dirpath(), "output/kba_lcc.tif"))

        if(!is.null(criteria5())){
          kba_crit5 <- raster(file.path(dirpath(), "output",paste0(criteria5name(),".tif")))
        } 
        #Prep criteria legend LCC
        unique_sorted_values <- sort(na.omit(unique(values(kba_lcc))))
        df_label = data.frame(values=c(1,2,5,6,8,10,11,12,13,14,15,16,17,18,19), labels=c("Temperate conifer forest", "Taiga conifer forest",
                                                                                        "Broadleaf forest", "Mixed Forest", "Shrubland", "Grassland", 
                                                                                        "Shrubland-lichen-moss", "Grassland-lichen-moss","Barren-lichen-moss",
                                                                                        "Wetland",  "Cropland", "Barren Lands", "Urban", "Water", "Snow"))
        # calculate dissimilarity metric 
        error_occurred <- FALSE
        tryCatch({
          networks_sf$lcc <- calc_dissimilarity(networks_sf, refarea_reactive(), kba_lcc, 'categorical', plot_out_dir=file.path(dirpath(), network_dir,"lcc"), categorical_class_labels = df_label)
          networks_sf$led <- calc_dissimilarity(networks_sf, refarea_reactive(), kba_led, 'continuous', plot_out_dir=file.path(dirpath(), network_dir,"led"))
          networks_sf$cmi <- calc_dissimilarity(networks_sf, refarea_reactive(), kba_cmi, 'continuous', plot_out_dir=file.path(dirpath(), network_dir,"cmi"))
          networks_sf$gpp <- calc_dissimilarity(networks_sf, refarea_reactive(), kba_gpp, 'continuous', plot_out_dir=file.path(dirpath(), network_dir,"gpp")) 
          if(!is.null(criteria5())){
            networks_sf[[criteria5name()]] <- calc_dissimilarity(networks_sf, refarea_reactive(), kba_crit5, 'continuous', plot_out_dir=file.path(dirpath(), network_dir, criteria5name())) 
          }
        }, error = function(err) {
          error_occurred <- TRUE
          showModal(modalDialog(
            title = "Error calculating disimilarity",
            paste("Possible issues may involve partially overlapping objects or the reserve's size being too small relative to the raster's resolution. Code error returns:", e$message),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        })
        if (error_occurred) {
          return(NULL)  # Stop execution of the rest of the observer
        }
        st_write(networks_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = outName, driver = "GPKG", append = TRUE)
        network_reactive(networks_sf)
      }else{
        showModal(modalDialog(
          title = "No network fulffill criterai threshold.",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      }
    }else{
        networks_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = layer_to_check)
        network_reactive(networks_sf)
        upstream_networks_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = paste0("upstream_", outName))
        upstream_network_reactive(upstream_networks_sf)
    }
    
    # Extract unique KBA values for selectInput
    unique_network <- unique(networks_sf$network)
    
    # Update selectInput choices based on filtered KBA values
    updateSelectInput(getDefaultReactiveDomain(), "network", choices = unique_network)
    
    networks_4326 <- st_transform(networks_sf, 4326)
    labelKBA <- reactive_labelKBA()
    
    leafletProxy("map") %>%
        clearGroup(labelKBA) %>%
        clearGroup("Potential KBAs") %>%
        clearGroup("Protected areas") %>%
        clearGroup("Upstream") %>%  # Clear previous highlight
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Intact areas", "Streams", legendcrit()),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("Streams"))
    
      # Close the modal once processing is done
    removeModal()
    
    # Update specific rows based on a condition or manually
    x <- outfreqnet()
    x <- x %>% 
      mutate(Count = case_when(Variables == "Networks" ~ ifelse(!is.null(networks_4326), nrow(networks_4326), NA_integer_),
                               TRUE ~ Count)  # Keep existing values for other rows
      )
    outfreqnet(x)
    
    output$outkbafreq <- renderTable({
      outfreqkba()
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
    req(catchments())
    req(network_reactive())
    
    network_sf <- network_reactive()
    # criteria5
    if(!is.null(criteria5())){
      network_sf_rep <- filter(network_sf, lcc <= input$slideNETLCC & gpp <= input$slideNETGPP & cmi <= input$slideNETCMI & led <= input$slideNETLED & !!sym(criteria5name()) <=input$slideNETcrit5 & up_km2 <= input$slideNETUP)
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
    req(network_reactive())
    
    # Filter the `sf` object to get the selected KBA based on the input value
    selected_net <- network_reactive() %>%
      filter(network == input$network) %>%
      st_transform(4326)  # Make sure it's in the correct coordinate system for Leaflet
    
    selected_up <- upstream_network_reactive() %>%
      filter(network == input$network) %>%
      st_transform(4326)  # Make sure it's in the correct coordinate system for Leaflet
    
    #Dynamic label
    if(is.null(reactive_labelNET())){
      reactive_labelNET(input$network)
    }
    labelNET <- reactive_labelNET()
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
        overlayGroups = c("Catchments extent", "Planning region", "Intact areas", input$network, "Upstream","Streams", legendcrit()),
        options = layersControlOptions(collapsed = TRUE)
      )
    reactive_labelNET(input$network)
  })
  ####################################################################################################
  # Render Rep Analysis per network
  ####################################################################################################
  observeEvent(input$network, {
    req(input$network)  # Ensure there is a selected KBA
    
   if(is.null(criteria5())){
      # Prepare the table for display
      x <- tibble(
          Variables = c("Area km2", "AWI", "Upstream area km2", "Upstream AWI", 
                        "DCI", "CMI", "GPP", "LED", "LCC"),
          Values = NA
      )
    }else{
      # Prepare the table for display
      x <- tibble(
          Variables = c("Area km2", "AWI", "Upstream area km2", "Upstream AWI", 
                        "DCI", "CMI", "GPP", "LED", "LCC", criteria5name()),
          Values = NA
      )
    }
    

    # Get the filtered polygons and select the one matching the KBA choice
    potential_net <- network_reactive()
    selected_network <- potential_net[potential_net$network == input$network, ]
    
    x$Values[x$Variables == "Area km2"] <- round(as.numeric(st_area(selected_network))/1000000, 2)
    x$Values[x$Variables == "AWI"] <- round(as.numeric(selected_network$AWI) * 100, 2)
    x$Values[x$Variables == "Upstream area km2"] <- round(selected_network$up_km2, 2)
    x$Values[x$Variables == "Upstream AWI"] <- round(as.numeric(selected_network$up_AWI) * 100, 2)
    #(ON HOLD)x$Values[x$Variables == "DCI"] <- round(selected_network$dci, 3)
    x$Values[x$Variables == "DCI"] <- NA
    x$Values[x$Variables == "CMI"] <- round(selected_network$cmi, 3)
    x$Values[x$Variables == "GPP"] <- round(selected_network$gpp, 3)
    x$Values[x$Variables == "LED"] <- round(selected_network$led, 3)
    x$Values[x$Variables == "LCC"] <- round(selected_network$lcc, 3)

    if(!is.null(criteria5())){
      x$Values[x$Variables == criteria5name()] <- round(selected_network[[criteria5name()]], 3)
    }
    
    formatted_x <- x %>%
      mutate(
        Values = case_when(
          Variables %in% c("Area km2", "Upstream area km2") ~ as.character(as.integer(Values)),  # No decimals
          Variables %in% c("AWI", "Upstream AWI") ~ formatC(as.numeric(Values), format = "f", digits = 2),  # 2 decimals
          TRUE ~ formatC(as.numeric(Values), format = "f", digits = 3)  # 3 decimals for others
        )
      )
    
    output$outnet <- renderTable({
      formatted_x
    }, digits = 0)  # digits is ignored since we manually formatted the values
    
    ####################################################################################################
    # Render Rep Analysis PLOT per KBA
    ####################################################################################################
    # Define a route to serve images from the external directory
    shiny::addResourcePath("imageNET", file.path(dirpath(), netDir()))

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
                 if(!is.null(criteria5())){
                   tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                          tags$h3(criteria5name()),  # Title 
                          tags$img(src = paste0("imageNET/", criteria5name(), "/", input$network, ".png"), height = "400px", width = "300px")
                   )
                 }
        )
      )
    })
  })

  ################################################################################################
  # Save features to a geopackage
  output$downloadSample <- downloadHandler(
    filename = function() { "accessPath.csv" },
    content = function(file) {
      # Copy the file from the www folder to the temporary file
      file.copy("www/accessPath.csv", file)
    }
  )
  
  # Save features to a geopackage
  observeEvent(input$downloadData, {
    if(input$KBArep =="No KBA generated"){ 
      set_grid <- "reducedFALSE"
    }else{
      set_grid <- paste0("reduced", sub("^KBAs_rep_([^_]+)$", "\\1", input$KBArep))
    }

    if(input$forcePAs){
      if(input$filterNet>0){
        filtering <- paste0("_", set_grid, "_up", as.character(input$slideNETUP), "_cmi", as.character(input$slideNETCMI),"_gpp", as.character(input$slideNETGPP),"_led", as.character(input$slideNETLED),
                            "_lcc", as.character(input$slideNETLCC))
        if(!is.null(criteria5())){
          potential_net <- filter(network_reactive(), lcc <= input$slideNETLCC & gpp <= input$slideNETGPP & cmi <= input$slideNETCMI & led <= input$slideNETLED & 
                                     !!sym(criteria5name()) <= input$slideNETcrit5 & up_km2 <= input$slideNETUP)
          outName <- paste0("filterednet",  filtering, "_", criteria5name(), as.character(input$slideNETcrit5), "_kba", input$set_net, "_filter", as.character(input$forceKBA), "_includePAs")
        }else{
          potential_net <- filter(network_reactive(), lcc <= input$slideNETLCC & gpp <= input$slideNETGPP & cmi <= input$slideNETCMI & led <= input$slideNETLED & up_km2 <= input$slideNETUP)
          outName <- paste0("filterednet", filtering, "_kba", input$set_net, "_filter", as.character(input$forceKBA), "_includePAs")
        }
      }else{
        potential_net <- network_reactive()
        outName <- paste0("filterednet_",  set_grid, "_kba", input$set_net, "_filter", as.character(input$forceKBA), "_includePAs")
      }
    } else {
      if(input$filterNet>0){
        filtering <- paste0("_", set_grid, "_up", as.character(input$slideNETUP), "_cmi", as.character(input$slideNETCMI),"_gpp", as.character(input$slideNETGPP),"_led", as.character(input$slideNETLED),
                            "_lcc", as.character(input$slideNETLCC))
        if(!is.null(criteria5())){
          potential_net <- filter(network_reactive(), lcc <= input$slideNETLCC & gpp <= input$slideNETGPP & cmi <= input$slideNETCMI & led <= input$slideNETLED & 
                                     !!sym(criteria5name()) <= input$slideNETcrit5 & up_km2 <= input$slideNETUP)
          outName <- paste0("filterednet",  filtering, "_", criteria5name(), as.character(input$slideNETcrit5), "_kba", input$set_net, "_filter", as.character(input$forceKBA))
        }else{
          potential_net <- filter(network_reactive(), lcc <= input$slideNETLCC & gpp <= input$slideNETGPP & cmi <= input$slideNETCMI & led <= input$slideNETLED & up_km2 <= input$slideNETUP)
          outName <- paste0("filterednet",  filtering, "_kba", input$set_net, "_filter", as.character(input$forceKBA))
        }
      }else{
        potential_net <- network_reactive()
        outName <- paste0("filterednet_", set_grid, "_kba", input$set_net, "_filter", as.character(input$forceKBA))
      }
    }
    st_write(potential_net, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = outName, driver = "GPKG", append = FALSE)
    showModal(modalDialog(
      title = "Filtered networks downloaded",
      paste0("Filtered networks were downloaded in the KBA_analysis.gpkg  under the name ", outName, " found in ", dirpath(), "/output"),
      easyClose = TRUE,
      footer = modalButton("OK"))
    )
  })
  
}