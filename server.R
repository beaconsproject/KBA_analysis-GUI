server = function(input, output, session) {
  
  poly_sf_reactive <- reactiveVal()
  poly_filtered_reactive <- reactiveVal()
  upstream_reactive <- reactiveVal()
  upstream_network_reactive <- reactiveVal()
  nghbrs_reactive <- reactiveVal()
  seed_reactive <- reactiveVal()
  reactive_labelKBA <- reactiveVal()
  reactive_labelNET <- reactiveVal()
  network_reactive <- reactiveVal()
  dir_exists <- reactiveVal(FALSE)
  builder_exists <- reactiveVal(FALSE)
  criteria5name <- reactiveVal(NULL)
  legendcrit <-  reactiveVal(c("CMI", "GPP", "LED", "LCC"))
  
  outfreqkba <- reactiveVal(
    tibble(Variables = c("KBAs", "Filtered KBAs"), Count = NA)
  )
  outfreqnet <- reactiveVal(
    tibble(Variables = c("KBAs", "Filtered KBAs", "Networks", "Filtered networks"), Count = NA)
  )
  
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
    
    dirpath()
    treedir <- c("output","Builder_input","Builder_output")
    for(d in treedir){
      if(!dir.exists(file.path(dirpath(), d))){
        dir.create(file.path(dirpath(), d))
        print("Folder created")
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
      if ("KBAs_builder" %in% layers) {
        poly_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_builder")
        poly_sf_reactive(poly_sf)
      }
      if ("KBAs_upstream" %in% layers) {
        upstream_area <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_upstream")
        upstream_reactive(upstream_area)
      }
      if ("KBAs_reduced" %in% layers) {
        poly_filtered <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_reduced")
        poly_filtered_reactive(upstream_area)
      }
      showModal(modalDialog(
        title = "The output directory already exists and the necessary files are present.",
        "The analysis will use those data. If you changed the input files or plan to change the parameters, please set another output directory.",
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
    }
  })
  ################################################################################################
  # Set catchments
  ################################################################################################
  catchments <- reactive({
    req(!is.null(input$csv_file) || !is.null(input$upload_catch)) 
    # Handle CSV File
    if (!is.null(input$csv_file)) {
      req(input$csv_file)
      # Read the CSV file
      csv_data <- read.csv(input$csv_file$datapath)
      # Check if "catchment" layer exists
      if ("catchments" %in% csv_data$Layer) {
        path <- csv_data$Path[csv_data$Layer == "catchments"]
        if (file.exists(path)) {
          return(st_read(path))
        } else {
          stop("The catchment path in the CSV does not exist.")
        }
      } else {
        stop("Catchment layer not found in CSV.")
      }
    }
    # Handle Shapefile Upload
    if (!is.null(input$upload_catch)) {
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
          return(sf::st_read(shp_path))  # Use sf::st_read() to read the Shapefile
        } else {
          stop("Shapefile (.shp) is missing.")
        }
      } else {
        stop("Upload all necessary files for the shapefile (.shp, .shx, .dbf, etc.).")
      }
    } 
  })
  ################################################################################################
  # Set streams
  ################################################################################################
  streams <- reactive({
    req(!is.null(input$csv_file) || !is.null(input$upload_stream)) 
    # Handle CSV File
    if (!is.null(input$csv_file)) {
      req(input$csv_file)
      # Read the CSV file
      csv_data <- read.csv(input$csv_file$datapath)
      # Check if "catchment" layer exists
      if ("catchments" %in% csv_data$Layer) {
        path <- csv_data$Path[csv_data$Layer == "stream"]
        if (file.exists(path)) {
          return(st_read(path))
        } else {
          stop("The catchment path in the CSV does not exist.")
        }
      } else {
        stop("Catchment layer not found in CSV.")
      }
    }
    # Handle Shapefile Upload
    if (!is.null(input$upload_stream)) {
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
          return(sf::st_read(shp_path))  # Use sf::st_read() to read the Shapefile
        } else {
          stop("Shapefile (.shp) is missing.")
        }
      } else {
        stop("Upload all necessary files for the shapefile (.shp, .shx, .dbf, etc.).")
      }
    } 
  })
  
  ################################################################################################
  # Set Planning region
  ################################################################################################
  planreg <- reactive({
    req(!is.null(input$csv_file) || !is.null(input$upload_planreg)) 
    # Handle CSV File
    if (!is.null(input$csv_file)) {
      req(input$csv_file)
      # Read the CSV file
      csv_data <- read.csv(input$csv_file$datapath)
      # Check if "catchment" layer exists
      if ("catchments" %in% csv_data$Layer) {
        path <- csv_data$Path[csv_data$Layer == "planning region"]
        if (file.exists(path)) {
          return(st_read(path))
        } else {
          stop("The catchment path in the CSV does not exist.")
        }
      } else {
        stop("Catchment layer not found in CSV.")
      }
    }
    # Handle Shapefile Upload
    if (!is.null(input$upload_planreg)) {
      req(input$upload_planreg)
      infile <- input$upload_planreg
      if (length(infile$datapath) > 1) { # Check if multiple files are uploaded
        dir <- unique(dirname(infile$datapath))  # Get the temp directory
        outfiles <- file.path(dir, infile$name)  # Create new file path with original names
        
        # Strip the base name (without extension) of the first file
        name <- tools::file_path_sans_ext(infile$name[1])  
        purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y)) 
        # Attempt to read the shapefile after renaming
        shp_path <- file.path(dir, paste0(name, ".shp"))
        if (file.exists(shp_path)) {
          return(sf::st_read(shp_path))  # Use sf::st_read() to read the Shapefile
        } else {
          stop("Shapefile (.shp) is missing.")
        }
      } else {
        stop("Upload all necessary files for the shapefile (.shp, .shx, .dbf, etc.).")
      }
    } 
  })
  
  ################################################################################################
  # Set LCC
  ################################################################################################  
#  lcc <- reactive({
#    req(input$upload_lcc)  # Ensure the file is uploaded

#    # Get the file path from the file input
#    lcc_path <- input$upload_lcc$datapath
#    lcc_tiff <- raster(lcc_path)
#    return(lcc_tiff)
#  })
  lcc <- reactive({
    # Check if lcc file is uploaded first
    if (!is.null(input$upload_lcc)) {
      req(input$upload_lcc)  # Ensure the file is uploaded
      # Get the file path from the file input
      lcc_path <- input$upload_lcc$datapath
      lcc_tiff <- raster(lcc_path)  # Load raster from the uploaded file
      return(lcc_tiff)
    }
    
    # If file not uploaded, check for CSV and parse the path for "LCC"
    if (!is.null(input$csv_file)) {
      req(input$csv_file)  # Ensure the CSV file is provided
      # Read the CSV file
      csv_data <- read.csv(input$csv_file$datapath)
      
      # Check if "LCC" layer exists in the CSV
      if ("LCC" %in% csv_data$Layer) {
        lcc_path <- csv_data$Path[csv_data$Layer == "LCC"]
        if (file.exists(lcc_path)) {
          lcc_tiff <- raster(lcc_path)  # Load raster from the path in CSV
          return(lcc_tiff)
        } else {
          stop("The LCC path in the CSV does not exist.")
        }
      } else {
        stop("LCC layer not found in CSV.")
      }
    }
    
    # Return NULL if neither file is provided
    return(NULL)
  })
  
  ################################################################################################
  # Set LED
  ################################################################################################
#  led <- reactive({
#    req(input$upload_led)  # Ensure the file is uploaded
    
    # Get the file path from the file input
#    led_path <- input$upload_led$datapath
#    led_tiff <- raster(led_path)
#    return(led_tiff)
#  }) 
  led <- reactive({
    # Check if lcc file is uploaded first
    if (!is.null(input$upload_led)) {
      req(input$upload_led)  # Ensure the file is uploaded
      # Get the file path from the file input
      path <- input$upload_led$datapath
      tiff <- raster(path)  # Load raster from the uploaded file
      return(tiff)
    }
    
    # If file not uploaded, check for CSV and parse the path for "LCC"
    if (!is.null(input$csv_file)) {
      req(input$csv_file)  # Ensure the CSV file is provided
      # Read the CSV file
      csv_data <- read.csv(input$csv_file$datapath)
      
      # Check if "LCC" layer exists in the CSV
      if ("LCC" %in% csv_data$Layer) {
        path <- csv_data$Path[csv_data$Layer == "LED"]
        if (file.exists(path)) {
          tiff <- raster(path)  # Load raster from the path in CSV
          return(tiff)
        } else {
          stop("The LCC path in the CSV does not exist.")
        }
      } else {
        stop("LCC layer not found in CSV.")
      }
    }
    
    # Return NULL if neither file is provided
    return(NULL)
  })
  ################################################################################################
  # Set GPP
  ################################################################################################
#  gpp <- reactive({
#    req(input$upload_gpp)  # Ensure the file is uploaded
    
    # Get the file path from the file input
#    gpp_path <- input$upload_gpp$datapath
#    gpp_tiff <- raster(gpp_path)
#    return(gpp_tiff)
#  })
  gpp <- reactive({
    # Check if lcc file is uploaded first
    if (!is.null(input$upload_gpp)) {
      req(input$upload_gpp)  # Ensure the file is uploaded
      # Get the file path from the file input
      path <- input$upload_gpp$datapath
      tiff <- raster(path)  # Load raster from the uploaded file
      return(tiff)
    }
    
    # If file not uploaded, check for CSV and parse the path for "LCC"
    if (!is.null(input$csv_file)) {
      req(input$csv_file)  # Ensure the CSV file is provided
      # Read the CSV file
      csv_data <- read.csv(input$csv_file$datapath)
      
      # Check if "LCC" layer exists in the CSV
      if ("LCC" %in% csv_data$Layer) {
        path <- csv_data$Path[csv_data$Layer == "GPP"]
        if (file.exists(path)) {
          tiff <- raster(path)  # Load raster from the path in CSV
          return(tiff)
        } else {
          stop("The LCC path in the CSV does not exist.")
        }
      } else {
        stop("LCC layer not found in CSV.")
      }
    }
    
    # Return NULL if neither file is provided
    return(NULL)
  })
  ################################################################################################
  # Set CMI
  ################################################################################################
#  cmi <- reactive({
#    req(input$upload_cmi)  # Ensure the file is uploaded
    
#    # Get the file path from the file input
#    cmi_path <- input$upload_cmi$datapath
#    cmi_tiff <- raster(cmi_path)
#    return(cmi_tiff)
#  })
  cmi <- reactive({
    # Check if lcc file is uploaded first
    if (!is.null(input$upload_cmi)) {
      req(input$upload_cmi)  # Ensure the file is uploaded
      # Get the file path from the file input
      path <- input$upload_cmi$datapath
      tiff <- raster(path)  # Load raster from the uploaded file
      return(tiff)
    }
    
    # If file not uploaded, check for CSV and parse the path for "LCC"
    if (!is.null(input$csv_file)) {
      req(input$csv_file)  # Ensure the CSV file is provided
      # Read the CSV file
      #browser()
      csv_data <- read.csv(input$csv_file$datapath)
      
      # Check if "LCC" layer exists in the CSV
      if ("LCC" %in% csv_data$Layer) {
        path <- csv_data$Path[csv_data$Layer == "CMI"]
        if (file.exists(path)) {
          tiff <- raster(path)  # Load raster from the path in CSV
          return(tiff)
        } else {
          stop("The LCC path in the CSV does not exist.")
        }
      } else {
        stop("LCC layer not found in CSV.")
      }
    }
    
    # Return NULL if neither file is provided
    return(NULL)
  })
  ################################################################################################
  # Set criteria5
  ################################################################################################
  criteria5 <- reactive({
    req(input$upload_custom)  # Ensure the file is uploaded
    
    # Get the file path from the file input
    crit5_path <- input$upload_custom$datapath
    crit5_tiff <- raster(crit5_path)
    return(crit5_tiff)
  })
  
  observeEvent(input$criteria5, {
    req(input$criteria5)
    criteria5name(input$criteria5)
    updateSliderInput(session = getDefaultReactiveDomain(), "slidecrit5", label = input$criteria5)
    updateSliderInput(session = getDefaultReactiveDomain(), "slideNETcrit5", label = input$criteria5)
    grp <- c("CMI", "GPP", "LED", "LCC", criteria5name())
    legendcrit(grp)
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
    updateSelectInput(session = getDefaultReactiveDomain(), "intactColname", choices = colnames, selected = "IntactPB")
    updateSelectInput(session = getDefaultReactiveDomain(), "arealandColname", choices = colnames, selected="Area_land")
    updateSelectInput(session = getDefaultReactiveDomain(), "intactseedColname", choices = colnames, selected="kba_m2")
    
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
      fitBounds(lng1 = -121, lat1 = 44, lng2 = -65, lat2 = 78)%>%
      addMapPane(name = "layer1", zIndex=380) %>%
      addMapPane(name = "layer2", zIndex=420) %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>% 
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addPolygons(data=intact_4326(), fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intact areas", options = leafletOptions(pane = "layer2")) %>%
      addPolygons(data=bnd, color='grey', fill=F, weight=1, group="region", options = leafletOptions(pane = "layer1")) %>%
      #fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Intact areas"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c(""))
  
    if(!is.null(input$csv_file) || !is.null(input$upload_catch)){
      req(catchments())

      # show pop-up ...
      showModal(modalDialog(
       title = "Uploading catchments extent. Please wait...",
       easyClose = TRUE,
       footer = NULL)
      )
      catch_bnd <- st_union(catchments())
      catch_bnd <-catch_bnd %>%
        st_buffer(20) %>%
        st_buffer(-20)
      catch_extent <- st_transform(catch_bnd, 4326)
      map_bounds1 <- catch_extent %>% st_bbox() %>% as.character()

      map <- map %>%
       fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
       addPolygons(data=catch_extent, color='black', fill = F, weight=3, group="Catchments extent", options = leafletOptions(pane = "layer2")) %>%
       addLayersControl(position = "topright",
                        baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                        overlayGroups = c("Catchments extent", "Intact areas"),
                        options = layersControlOptions(collapsed = FALSE))  %>%
       hideGroup(c(""))
      
      # Close the modal once processing is done
      removeModal()
    }
    if(!is.null(input$csv_file) || !is.null(input$upload_planreg)){
      req(planreg())
      
      showModal(modalDialog(
        title = "Uploading study region extent. Please wait...",
        easyClose = TRUE,
        footer = NULL
      ))
      
      planreg_4326 <- st_transform(planreg(), 4326)
      map_bounds1 <- catch_extent %>% st_bbox() %>% as.character()
      
      map <- map %>%
        fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
        addPolygons(data=planreg_4326, color='red', fill = F, weight=3, group="Planning region", options = leafletOptions(pane = "layer2")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Intact areas"),
                         options = layersControlOptions(collapsed = FALSE))  %>%
        hideGroup(c(""))
      
      # Close the modal once processing is done
      removeModal()
    }
    map
  })
  
#  # Observe planning region uploads
#  observeEvent(input$upload_planreg, {
#    req(catchments()) # You can adjust this condition if it's not dependent on catchments
    
#    showModal(modalDialog(
#      title = "Uploading study region extent. Please wait...",
#      easyClose = TRUE,
#      footer = NULL
#    ))
    
#    planreg_4326 <- st_transform(planreg(), 4326)
#    map_bounds1 <- planreg_4326 %>% st_bbox() %>% as.character()
    
#    leafletProxy("map") %>%
#      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
#      addPolygons(data=planreg_4326, color='red', fill = F, weight=3, group="Planning region", options = leafletOptions(pane = "layer2")) %>%
#      addLayersControl(position = "topright",
#                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
#                       overlayGroups = c("Catchments extent", "Planning region", "Intact areas"),
#                       options = layersControlOptions(collapsed = FALSE)) %>%
#      hideGroup(c(""))
    
#    removeModal()
#  })
  ####################################################################################################
  ####################################################################################################
  # Analysis
  ####################################################################################################
  ####################################################################################################
  # Create BUilder attributes
  ####################################################################################################
  observeEvent(input$runBuilderInput>0, {
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
    if (!file.exists(file.path(out_dir, "Builder_input/nghbrs.csv"))) {
      nghbrs <- neighbours(catchments())
      nghbrs_reactive(nghbrs)
      write.csv(nghbrs, file=file.path(out_dir,"Builder_input/nghbrs.csv"), row.names=FALSE) # Convert neighbours table to csv file.
    }else{
      nghbrs <- read.csv(file.path(out_dir,'Builder_input','nghbrs.csv'))
      nghbrs_reactive(nghbrs)
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
  # Run BUILDER
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
        
        poly_sf_reactive(poly_sf)  # Store the poly_sf in reactiveVal

        # Append the first layer to the GeoPackage
        st_write(poly_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_builder", driver = "GPKG", append = TRUE)
        
        # Close the modal once processing is done
        removeModal()
        
        # show pop-up ...
        showModal(modalDialog(
          title = "Builder output created.",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      } else {
        poly_sf <- st_read(dsn = file.path(out_dir, "output/KBA_analysis.gpkg"), layer = "KBAs_builder")
        poly_sf_reactive(poly_sf)
        showModal(modalDialog(
          title = "Builder output already exist.", "The app used the data previously generated.
          If you changed inputs or parameters for this analysis, please point to another directory.",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      }
  })
  
  ####################################################################################################
  # CALC DCI
  ####################################################################################################
  observeEvent(input$calc_dci, {
    req(input$set_grid)
    req(!is.null(poly_sf_reactive()))

    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    layer_to_check <- "KBAs_dci"
    if (!layer_to_check %in% layers) {
      
      showModal(modalDialog(
        title = "Processing",
        "Calculating hydrology metrics. Please wait...",
        footer = NULL
      ))
      
      poly_sf <- poly_sf_reactive()
      poly_sf$group_id <- group_conservation_areas(poly_sf, as.numeric(input$set_grid))  

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
      poly_sf_reactive(poly_sf)
    
      # Export. Append the first layer to the GeoPackage
      st_write(poly_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_dci", driver = "GPKG", append = TRUE)
      
      # Close the modal once processing is done
      removeModal()
    } else{
      #poly_sf <- poly_sf_reactive()
      upstream_area <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_upstream")
      upstream_reactive(upstream_area)
    } 
    showModal(modalDialog(
      title = "Hydrology metrics added",
      easyClose = TRUE,
      footer = modalButton("OK"))
    )  
  })  

  ####################################################################################################
  # REDUCE KBAs
  ####################################################################################################
  observeEvent(input$reduce_KBAs, {
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    layer_to_check <- "KBAs_reduced"
    if (!layer_to_check %in% layers) {
      showModal(modalDialog(
        title = "Processing",
        "Reduce number of potential KBAs. Please wait...",
        footer = NULL
      ))
      req(!is.null(poly_sf_reactive()))
      req(!is.null(upstream_reactive()))
      # REDUCE NUMBER OF CONSERVATION AREAS
      # Select the top conservation area from each group based on smallest upstream area, largest DCI, and largest upstream intactness
      poly_sf <- poly_sf_reactive()
      
      poly_sf_filtered <- poly_sf %>%
        group_by(group_id) %>%
        arrange(-dci, up_km2, -up_AWI) %>% # Attribute order indicates their importance when selecting the 'best'from each group. '-' indicates largest to smallest.
        filter(row_number()==1)
      
      # Close the modal once processing is done
      removeModal()
      
      st_write(poly_sf_filtered, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_reduced", driver = "GPKG", append = TRUE)
      poly_filtered_reactive(poly_sf_filtered)
    } else {
      poly_sf_filtered <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_reduced")
      poly_filtered_reactive(poly_sf_filtered)
    }
    showModal(modalDialog(
      title = paste0("Number of filtered KBAs:", as.character(nrow(poly_sf_filtered))),
      easyClose = TRUE,
      footer = modalButton("OK"))
    )
    
    showModal(modalDialog(
      title = "Display KBAs",
       "Please wait...",
      footer = NULL
    ))
    
    poly_sf_filtered_4326 <- st_transform(poly_sf_filtered, 4326)
    stream_4326 <- streams() %>% st_intersection(planreg(), sparse = FALSE) %>% st_transform(4326)
    leafletProxy("map") %>%
      addPolygons(data=poly_sf_filtered_4326, color='#666666', fillColor = "grey", fillOpacity = 0, weight=3, group="Potential KBAs", options = leafletOptions(pane = "layer2")) %>%
      addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs", "Intact areas","Streams"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams"))
    # Close the modal once processing is done
    removeModal()
    
    # Generate Stat Tables    
    x <- tibble(
      Variables = c("KBAs", "Filtered KBAs"),
      Count = c(nrow(poly_sf_filtered),NA)
    )
    outfreqkba(x) 
  }) 
  ####################################################################################################
  # RUN REPRESENTATION ANALYSIS
  ####################################################################################################
  observeEvent(input$runRep, {
    req(catchments())
    poly_sf_filtered <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_reduced")
    poly_filtered_reactive(poly_sf_filtered)
    #req(poly_filtered_reactive())
    showModal(modalDialog(
      title = "Processing",
      "Assessing representation. Please wait...",
      footer = NULL
    ))

    #poly_sf_filtered <- poly_filtered_reactive()
    if(attr(poly_sf_filtered, "sf_column") != "geometry"){
      poly_sf_filtered$geometry <- poly_sf_filtered$geom
    }
    
    # CMI
    if (!file.exists(file.path(dirpath(), "output/kba_cmi.tif"))) {
        cmi_crop <- crop(cmi(), planreg())
        kba_cmi <- mask(cmi_crop, planreg())
        
        # calculate dissimilarity metric
        poly_sf_filtered$cmi <- calc_dissimilarity(poly_sf_filtered, planreg(), kba_cmi, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot/cmi"))
        writeRaster(kba_cmi, file.path(dirpath(), "output/kba_cmi.tif"), format = "GTiff")
        cmi_4326 <- kba_cmi %>% aggregate(fact = 2) %>% projectRaster(crs = "EPSG:4326")
        writeRaster(cmi_4326, file.path(dirpath(), "output/kba_cmi_4326.tif"), format = "GTiff")
    }else{
       kba_cmi <- raster(file.path(dirpath(), "output/kba_cmi.tif"))
       cmi_4326 <- raster(file.path(dirpath(), "output/kba_cmi_4326.tif"))
    }
    
    cmi_minVar <- min(floor(values(kba_cmi)), na.rm = TRUE)
    cmi_maxVar <- max(ceiling(values(kba_cmi)), na.rm = TRUE)
    cmi_bins.seq <- seq(cmi_minVar, cmi_maxVar, (cmi_maxVar-cmi_minVar)/4)
    xpal <- colorBin("RdYlBu", cmi_bins.seq, bins = cmi_bins.seq, na.color = "transparent")
    val.color <- "RdYlBu"
    
    # LED
    if (!file.exists(file.path(dirpath(), "output/kba_led.tif"))) {
        led_crop <- crop(led(), planreg())
        kba_led <- mask(led_crop, planreg())
        
        # calculate dissimilarity metric
        poly_sf_filtered$led <- calc_dissimilarity(poly_sf_filtered, planreg(), kba_led, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot/led"))
        writeRaster(kba_led, file.path(dirpath(), "output/kba_led.tif"), format = "GTiff")
        led_4326 <- kba_led %>% aggregate( fact = 4) %>% projectRaster(crs = "EPSG:4326")
        writeRaster(led_4326, file.path(dirpath(), "output/kba_led_4326.tif"), format = "GTiff")
    } else{
      kba_led <- raster(file.path(dirpath(), "output/kba_led.tif"))
      led_4326 <- raster(file.path(dirpath(), "output/kba_led_4326.tif"))
    }
      
    led_minVar <- min(floor(values(kba_led)), na.rm = TRUE)
    led_maxVar <- max(ceiling(values(kba_led)), na.rm = TRUE)
    led_bins.seq <- seq(led_minVar, led_maxVar, (led_maxVar-led_minVar)/4)
    led_xpal <- colorBin("Blues", led_bins.seq, bins = led_bins.seq, na.color = NA)
    led_val.color <- "Blues"
    
    # GPP
    if (!file.exists(file.path(dirpath(), "output/kba_gpp.tif"))) {
        gpp_crop <- crop(gpp(), planreg())
        kba_gpp <- mask(gpp_crop, planreg())
        
        # calculate dissimilarity metric
        poly_sf_filtered$gpp <- calc_dissimilarity(poly_sf_filtered, planreg(), kba_gpp, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot/gpp"))
        writeRaster(kba_gpp, file.path(dirpath(), "output/kba_gpp.tif"), format = "GTiff")
        gpp_4326 <- kba_gpp %>% aggregate( fact = 4) %>% projectRaster(crs = "EPSG:4326")
        writeRaster(gpp_4326, file.path(dirpath(), "output/kba_gpp_4326.tif"), format = "GTiff")
    } else{
      kba_gpp <- raster(file.path(dirpath(), "output/kba_gpp.tif"))
      gpp_4326 <- raster(file.path(dirpath(), "output/kba_gpp_4326.tif"))
    }
    
    gpp_minVar <- min(floor(values(kba_gpp)), na.rm = TRUE)
    gpp_maxVar <- max(ceiling(values(kba_gpp)), na.rm = TRUE)
    gpp_bins.seq <- seq(gpp_minVar, gpp_maxVar, (gpp_maxVar-gpp_minVar)/4)
    gppxpal <- colorBin("RdYlBu", gpp_bins.seq, bins = gpp_bins.seq, na.color = "transparent")
    
    # LCC
    if (!file.exists(file.path(dirpath(), "output/kba_lcc.tif"))) {
        lcc_crop <- crop(lcc(), planreg())
        kba_lcc <- mask(lcc_crop, planreg())
        kba_lcc[kba_lcc > 19] <- NA # all land cover classes > 19 are NA 
        # dataframe labelling
        df_label = data.frame(values=c(1,2,5,6,8,10,11,12,13,14,15,16,17,18,19), labels=c("Temperate conifer forest", "Taiga conifer forest",
                                                            "Broadleaf forest", "Mixed Forest", "Shrubland", "Grassland", 
                                                            "Shrubland-lichen-moss", "Grassland-lichen-moss","Barren-lichen-moss",
                                                            "Wetland",  "Cropland", "Barren Lands ", "Urban", "Water", "Snow"))
                                                            
        # calculate dissimilarity metric
        poly_sf_filtered$lcc <- calc_dissimilarity(poly_sf_filtered, planreg(), kba_lcc, 'categorical', plot_out_dir=file.path(dirpath(), "/output/plot/lcc"), categorical_class_labels = df_label)
        writeRaster(kba_lcc, file.path(dirpath(), "output/kba_lcc.tif"), format = "GTiff")
        lcc_4326 <- terra::aggregate(rast(kba_lcc), fact = 40, fun = modal) %>% project("EPSG:4326")
        writeRaster(lcc_4326, file.path(dirpath(), "output/kba_lcc_4326.tif"), filetype = "GTiff")
    }else{
      kba_lcc <- raster(file.path(dirpath(), "output/kba_lcc.tif"))
      lcc_4326 <- raster(file.path(dirpath(), "output/kba_lcc_4326.tif"))
    }
    
    
    labeller_function <- function(type, breaks) {
      return(c('Low', '', '', 'High'))
    }
    #Prep criteria legend LCC
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
    
    # criteria5
    if(input$criteria5 != ""){
      if (!file.exists(file.path(dirpath(), "output", paste0(criteria5name(), ".tif")))) {
        criteria5_crop <- crop(criteria5(), planreg())
        kba_criteria5 <- mask(criteria5_crop, planreg())
        
        # calculate dissimilarity metric
        poly_sf_filtered[[criteria5name()]] <- calc_dissimilarity(poly_sf_filtered, planreg(), kba_criteria5, 'continuous', plot_out_dir=file.path(dirpath(), "/output/plot", criteria5name()))
        writeRaster(kba_criteria5, file.path(dirpath(), "output", criteria5name()), format = "GTiff")
        crit5_4326 <- kba_criteria5 %>% aggregate( fact = 4) %>% projectRaster(crs = "EPSG:4326")
        writeRaster(kba_criteria5, file.path(dirpath(), "output", paste0(criteria5name(), "_4326.tif")), format = "GTiff")
      }else{
        kba_criteria5 <- raster(file.path(dirpath(), "output", paste0(criteria5name(), ".tif")))
        crit5_4326 <- raster(file.path(dirpath(), "output", paste0(criteria5name(), "_4326.tif")))
      }
      c5_minVar <- min(floor(values(kba_criteria5)), na.rm = TRUE)
      c5_maxVar <- max(ceiling(values(kba_criteria5)), na.rm = TRUE)
      c5_bins.seq <- seq(c5_minVar, c5_maxVar, (c5_maxVar-c5_minVar)/4)
      crit_xpal <- colorBin("RdYlBu", c5_bins.seq, bins = c5_bins.seq, na.color = "transparent")
      val.color <- "RdYlBu"
    }
    
    # Close the modal once processing is done
    removeModal()
    
    showModal(modalDialog(
      title = "Mapping potential KBAs",
      "Please wait...",
      footer = NULL
    ))
    
    # Save
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    layer_to_check <- "KBAs_att"
    if (!layer_to_check %in% layers) {
      st_write(poly_sf_filtered, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_att", driver = "GPKG", append = TRUE)
      poly_filtered_reactive(poly_sf_filtered)
    } else{
      poly_sf_filtered <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "KBAs_att")
      poly_filtered_reactive(poly_sf_filtered)
    }
    
    poly_sf_filtered_4326 <- poly_sf_filtered %>% st_transform(4326)
    unique_kbas <- unique(poly_sf_filtered$network)
    updateSelectInput(getDefaultReactiveDomain(), "KBA", choices = unique_kbas)
    pop = ~paste("KBA:", network)
    
    leafletProxy("map") %>%
      clearGroup("Potential KBAs") %>%
      addPolygons(data=poly_sf_filtered_4326, color='#666666', fillColor = "grey", fillOpacity = 0, weight=3, layerId = poly_sf_filtered_4326$network, popup = pop, group="Potential KBAs", options = leafletOptions(pane = "layer2")) %>%
      addRasterImage(cmi_4326, colors=val.color, opacity = 1, group="CMI") %>%
      addLegend(pal = xpal, values = values(cmi_4326), opacity = 1, title = "CMI",
                position = "bottomright", group="CMI", labFormat = labeller_function)  %>%
      addRasterImage(gpp_4326, colors=val.color, opacity = 1, group="GPP") %>%
      addLegend(pal = gppxpal, values = values(gpp_4326), opacity = 1, title = "GPP",
                position = "bottomright", group="GPP", labFormat = labeller_function)  %>%
      addRasterImage(led_4326, colors=led_val.color, opacity = 1, group="LED") %>%
      addLegend(pal = led_xpal, values = values(led_4326), opacity = 1, title = "LED",
                position = "bottomright", group="LED", labFormat = labeller_function)  %>%
      addRasterImage(lcc_4326, colors=selected_cols, opacity = 1, group="LCC") %>%
      addLegend(colors = selected_cols, label = cls,  position=c("bottomleft"), opacity = 1, title = "LCC",
                group="LCC") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs", "Intact areas", "Streams", legendcrit()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams"))
    if(input$criteria5 != ""){
      leafletProxy("map") %>%
        addRasterImage(crit5_4326, colors=val.color, opacity = 1, group=criteria5name()) %>%
        addLegend(pal = crit_xpal, values = values(crit5_4326), opacity = 1, title = criteria5name(),
                  position = "bottomright", group=criteria5name(), labFormat = labeller_function)  %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs", "Intact areas", "Streams", legendcrit()),
                         options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(c("Streams"))
    } 
    # Close the modal once processing is done
    removeModal()

    # Update outfreqkba
    outfreqkba(
      outfreqkba() %>% 
        mutate(
          Count = if_else(Variables == "KBAs", nrow(poly_sf_filtered), Count)
        )
    )
  })
  
  #########################################################
  #########################################################
  
  observeEvent(input$filterRep, {
    req(catchments())
    req(poly_filtered_reactive())
    browser()
    poly_sf_filtered <- poly_filtered_reactive()
    if(input$criteria5 != ""){
      poly_sf_rep <- filter(poly_sf_filtered, lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & !!sym(criteria5name()) <= input$slidecrit5 & up_km2 >= input$slideUP)
    }else{
      poly_sf_rep <- filter(poly_sf_filtered, lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & up_km2 >= input$slideUP)
    }
    
    if(nrow(poly_sf_rep)>0){
      showModal(modalDialog(
        title = "Processing",
        "Filter KBAs based on dissimilarity metrics threshold. Please wait...",
        footer = NULL
      ))
      # Extract unique KBA values for selectInput
      unique_kbas <- unique(poly_sf_rep$network)
    
      # Update selectInput choices based on filtered KBA values
      updateSelectInput(getDefaultReactiveDomain(), "KBA", choices = unique_kbas)
    
      poly_sf_rep_4326 <- poly_sf_rep %>% st_transform(4326)
      pop = ~paste("KBA:", network)
    
      leafletProxy("map") %>%
        clearGroup('Potential KBAs') %>% 
        addPolygons(data = poly_sf_rep_4326, color = '#666666', fillColor = "grey", fillOpacity = 0, weight = 3,
                  layerId = poly_sf_rep_4326$network, popup = pop, group = "Potential KBAs", 
                  options = leafletOptions(pane = "layer2")) %>%
        addLayersControl(position = "topright",
                         overlayGroups = c("Catchments extent", "Planning region", "Potential KBAs", "Intact areas", "Streams", legendcrit()),
                         options = layersControlOptions(collapsed = FALSE)) %>%
       hideGroup(c("Streams"))

      # Close the modal once processing is done
      removeModal()
      
      x <- tibble(
        Variables = c("KBAs", "Filtered KBAs"),
        Count = c(nrow(poly_sf_filtered),nrow(poly_sf_rep))
      )
      
      outfreqkba(x)
      
      
    }else{
      leafletProxy("map") %>%
        clearGroup('Potential KBAs') %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Intact areas", legendcrit()),
                         options = layersControlOptions(collapsed = FALSE))
      
      showModal(modalDialog(
        title = "No KBA reaches those threshold",
        easyClose = TRUE,
        footer = modalButton("OK"))
      )
    }
  })
  
  output$outkbafreq <- renderTable({
    outfreqkba()
  })
  
  
  observeEvent(input$KBA, {
    req(input$KBA)
    req(poly_filtered_reactive())
    # Filter the `sf` object to get the selected KBA based on the input value
    selected_kba <- poly_filtered_reactive() %>%
      filter(network == input$KBA) %>%
      st_transform(4326)  # Make sure it's in the correct coordinate system for Leaflet
    
    selected_up <- upstream_reactive() %>%
      filter(network == input$KBA) %>%
      st_transform(4326)  # Make sure it's in the correct coordinate system for Leaflet
    
    #Dynamic label
    if(is.null(reactive_labelKBA())){
      reactive_labelKBA(input$KBA)
    }
    labelKBA <- reactive_labelKBA()
    # Highlight the selected KBA on the map
    leafletProxy("map") %>%
      clearGroup(labelKBA) %>%
      clearGroup("Upstream") %>%  # Clear previous highlight
      addPolygons(data = selected_kba, color = "black",  fillColor = "grey", fillOpacity = 0.5, weight = 3, layerId = ~network,  # Ensure each polygon has a unique ID
        popup = ~paste("KBA:", network), group = input$KBA) %>%
      addPolygons(data = selected_up, color = "blue",  fillColor = "blue", fillOpacity = 0.2, weight = 2, group = "Upstream") %>%
      addLayersControl(
        position = "topright",
        baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
        overlayGroups = c("Catchments extent", "Planning region", "Potential KBA", "Intact areas", input$KBA, "Upstream", "Streams", legendcrit()),
        options = layersControlOptions(collapsed = FALSE)
      )
    reactive_labelKBA(input$KBA)
  })
  ####################################################################################################
  # Render Rep Analysis per KBA
  ####################################################################################################
  observeEvent(input$KBA, {
    req(input$KBA)  # Ensure there is a selected KBA
    
    # Define a route to serve images from the external directory
    shiny::addResourcePath("image", file.path(dirpath(), "output/plot"))
    
    # Get the filtered polygons and select the one matching the KBA choice
    potential_kbas <- poly_filtered_reactive()
    selected_polygon <- potential_kbas[potential_kbas$network == input$KBA, ]
    
    # Prepare the table for display
    if(input$criteria5 != ""){
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
    
    
    x$Values[x$Variables == "Area km2"] <- round(selected_polygon$area_km2, 2)
    x$Values[x$Variables == "AWI"] <- round(as.numeric(selected_polygon$AWI) * 100, 2)
    x$Values[x$Variables == "Upstream area km2"] <- round(selected_polygon$up_km2, 2)
    x$Values[x$Variables == "Upstream AWI"] <- round(as.numeric(selected_polygon$up_AWI) * 100, 2)
    x$Values[x$Variables == "DCI"] <- round(selected_polygon$dci, 2)
    x$Values[x$Variables == "CMI"] <- round(selected_polygon$cmi, 2)
    x$Values[x$Variables == "GPP"] <- round(selected_polygon$gpp, 2)
    x$Values[x$Variables == "LED"] <- round(selected_polygon$led, 2)
    x$Values[x$Variables == "LCC"] <- round(selected_polygon$lcc, 1)
    if(input$criteria5 != ""){
      x$Values[x$Variables == criteria5name()] <- round(selected_polygon[[criteria5name()]], 1)
    }
    output$outkba <- renderTable({
      x
    }, digits = 1)
    
    ####################################################################################################
    # Render Rep Analysis PLOT per KBA
    ####################################################################################################
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
                   tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                            tags$h3(input$criteria5),  # Title 
                            tags$img(src = paste0("image/", input$criteria5, "/", input$KBA, ".png"), height = "400px", width = "300px")
                   )
          )
        )
      })
  })
  ################################################################################################
  ################################################################################################
  # Build Network
  ################################################################################################  
  ################################################################################################
  observeEvent(input$buildNet, {
    req(catchments())
    req(poly_filtered_reactive())
    req(input$set_net)
    showModal(modalDialog(
      title = "Processing",
      "Building network. Please wait...",
      footer = NULL
    ))
    
    if(input$forceKBA){
      if(input$criteria5==""){
        potential_kbas <- filter(poly_filtered_reactive(), lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & up_km2 >= input$slideUP)
        outName <- paste0("Network_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),"_led", as.character(input$slideLED),
                          "_lcc", as.character(input$slideLCC), "_up", as.character(input$slideUP), "_n", input$set_net, "_force", as.character(input$forceKBA))
        network_dir <- paste0("output/plotnet_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),
                              "_led", as.character(input$slideLED),"_lcc" , as.character(input$slideLCC), "_up", as.character(input$slideUP),
                              "_n",input$set_net, "_force", as.character(input$forceKBA))
      }else{
        potential_kbas <- filter(poly_filtered_reactive(), lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & 
                                   !!sym(criteria5name()) <= input$slidecrit5 & up_km2 >= input$slideUP)
        outName <- paste0("Network_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),"_led", as.character(input$slideLED),
                          "_lcc", as.character(input$slideLCC), "_", criteria5name(), as.character(input$slidecrit5), "_up", as.character(input$slideUP), 
                          "_n", input$set_net, "_force", as.character(input$forceKBA))
        network_dir <- paste0("output/plotnet_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),
                              "_led", as.character(input$slideLED),"_lcc" , as.character(input$slideLCC), "_", criteria5name(), as.character(input$slidecrit5), "_up", as.character(input$slideUP),
                              "_n",input$set_net, "_force", as.character(input$forceKBA))
      }
    }else{
      potential_kbas <- poly_filtered_reactive()
      outName <- paste0("Network_up", as.character(input$slideUP), "_n", input$set_net, "_force", as.character(input$forceKBA))
      network_dir <- paste0("output/plotnet_up", as.character(input$slideUP), "_n",input$set_net, "_force", as.character(input$forceKBA))
    }
    
    layers_info <- st_layers(file.path(dirpath(), "output/KBA_analysis.gpkg"))
    layers <- layers_info$name
    layer_to_check <- outName
    
    if (!layer_to_check %in% layers) {
      if(input$set_net<2){
        showModal(modalDialog(
          title = "To build a network, number of KBAs must be higher than 2",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      }
    
      if(attr(potential_kbas, "sf_column") != "geometry"){
        potential_kbas$geometry <- potential_kbas$geom
      }
      
      if(nrow(potential_kbas)>0){
        # Generate all possible network names using 2 benchmarks per network.
        network_names <- gen_network_names(in_names = potential_kbas$network, k = as.numeric(input$set_net))
    
        #Check and remove overlapping KBAs. 
        overlaps <- list_overlapping_polygons(conservation_areas_sf = potential_kbas)
        network_names <- network_names[!network_names %in% overlaps]
    
        # Build the list of networks using the conservation area polygons. Each network will become a single feature in the polygon object.
        networks_sf <- build_network_polygons(conservation_areas_sf = potential_kbas, network_list = network_names)
    
        #Recover area
        kba_in_net <- sep_network_names(networks_sf$network)
        kba_areas <- setNames(potential_kbas$area_km2 , potential_kbas$network)
        network_areas <- sapply(kba_in_net, function(kba_names) {
          sum(kba_areas[kba_names], na.rm = TRUE)  # Sum areas for KBAs in each network
        })
        networks_sf$area_km2 <- network_areas[networks_sf$network]
      
        #Recover intactness
        kba_intactarea <- setNames(potential_kbas$area_km2*potential_kbas$up_AWI , potential_kbas$network)
        network_intact <- sapply(kba_in_net, function(kba_names) {
          sum(kba_intactarea[kba_names], na.rm = TRUE)  # Sum areas for KBAs in each network
        })
        networks_sf$intact_km2 <- network_intact[networks_sf$network]
        networks_sf$AWI <- round(networks_sf$intact_km2/networks_sf$area_km2,2)
      
        #recover upstream area
        kba_uparea <- setNames(potential_kbas$up_km2  , potential_kbas$network)
        network_up <- sapply(kba_in_net, function(kba_names) {
          sum(kba_uparea[kba_names], na.rm = TRUE)  # Sum areas for KBAs in each network
        })
        networks_sf$up_km2 <- network_up[networks_sf$network]
      
        #Recover upstream AWI
        kba_upAWI <- setNames(potential_kbas$up_km2*potential_kbas$up_AWI , potential_kbas$network)
        network_areaintact <- sapply(kba_in_net, function(kba_names) {
          sum(kba_upAWI[kba_names], na.rm = TRUE)  # Sum areas for KBAs in each network
        })
        networks_sf$upintact_km2 <- network_areaintact[networks_sf$network]
        networks_sf$up_AWI <- round(networks_sf$upintact_km2/networks_sf$up_km2,2)
      
        # DCI
        networks_sf$dci <- calc_dci(conservation_area_sf = networks_sf, stream_sf = streams())
      
        # Upstream
        upstream_sf <- upstream_reactive()
        upstream_network_list <- lapply(names(kba_in_net), function(network_name) {
          # Extract the list of KBAs for this network
          kba_names <- kba_in_net[[network_name]]
          # Filter upstream_kba_sf to find units matching KBAs in this network
          upstream_units <- upstream_sf[upstream_sf$network %in% kba_names, ]
          # Add a network column 
          upstream_units$network <- network_name
          return(upstream_units)
        })
        upstream_network_sf <- do.call(rbind, upstream_network_list)
        upstream_network_reactive(upstream_network_sf)
        if(!is.null(upstream_network_sf)){
          st_write(upstream_network_sf, dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "Network_upstream", driver = "GPKG", append = TRUE)
        }else{
          showModal(modalDialog(
            title = "No upstream area found for those network. Layer KBA_upstream won't be created.",
            easyClose = TRUE,
            footer = modalButton("OK"))
          )
        }
      
        #Criteria
        kba_cmi <- raster(file.path(dirpath(), "output/kba_cmi.tif"))
        kba_led <- raster(file.path(dirpath(), "output/kba_led.tif"))
        kba_gpp <- raster(file.path(dirpath(), "output/kba_gpp.tif"))
        kba_lcc <- raster(file.path(dirpath(), "output/kba_lcc.tif"))
        if(!input$criteria5==""){
          kba_crit5 <- raster(file.path(dirpath(), "output",paste0(input$criteria5,".tif")))
        } 
        #Prep criteria legend LCC
        unique_sorted_values <- sort(na.omit(unique(values(kba_lcc))))
        df_label = data.frame(values=c(1,2,5,6,8,10,11,12,13,14,15,16,17,18,19), labels=c("Temperate conifer forest", "Taiga conifer forest",
                                                                                        "Broadleaf forest", "Mixed Forest", "Shrubland", "Grassland", 
                                                                                        "Shrubland-lichen-moss", "Grassland-lichen-moss","Barren-lichen-moss",
                                                                                        "Wetland",  "Cropland", "Barren Lands", "Urban", "Water", "Snow"))
        # calculate dissimilarity metric 
        networks_sf$lcc <- calc_dissimilarity(networks_sf, planreg(), kba_lcc, 'categorical', plot_out_dir=file.path(dirpath(), network_dir,"lcc"), categorical_class_labels = df_label)
      
        networks_sf$led <- calc_dissimilarity(networks_sf, planreg(), kba_led, 'continuous', plot_out_dir=file.path(dirpath(), network_dir,"led"))
      
        networks_sf$cmi <- calc_dissimilarity(networks_sf, planreg(), kba_cmi, 'continuous', plot_out_dir=file.path(dirpath(), network_dir,"cmi"))
      
        networks_sf$gpp <- calc_dissimilarity(networks_sf, planreg(), kba_gpp, 'continuous', plot_out_dir=file.path(dirpath(), network_dir,"gpp")) 
        
        if(!input$criteria5==""){
          networks_sf[[criteria5name()]] <- calc_dissimilarity(networks_sf, planreg(), kba_crit5, 'continuous', plot_out_dir=file.path(dirpath(), network_dir, criteria5name())) 
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
        upstream_networks_sf <- st_read(dsn = file.path(dirpath(), "output/KBA_analysis.gpkg"), layer = "Network_upstream")
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
        clearGroup("Potential KBA") %>%
        clearGroup("Upstream") %>%  # Clear previous highlight
        addPolygons(data = networks_4326, color = '#666666', fillColor = "grey", fillOpacity = 0, weight = 3,
                  layerId = networks_4326$network, group = "Potential network", 
                  options = leafletOptions(pane = "layer2")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Potential network", "Intact areas", "Streams", legendcrit()),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Streams"))
    
      # Close the modal once processing is done
    removeModal()
    # Update outfreqnet
    outfreqnet(
      outfreqnet() %>% 
        mutate(
          Count = if_else(Variables == "KBAs",as.integer(outfreqkba()[1,2]),
                          if_else(Variables == "Filtered KBAs" & isTRUE(input$forceKBA), as.integer(outfreqkba()[2,2]), 
                                  if_else(Variables == "Filtered KBAs" & isFALSE(input$forceKBA), NA, 
                                          if_else(Variables == "Networks", nrow(networks_sf), 
                                                  if_else(Variables == "Filtered networks", NA, Count)))))
        )
    
    )
   # outfreqnet(
   #   outfreqnet() %>% 
   #     mutate(
   #       Count = if_else(Variables == "Networks", nrow(networks_sf), Count)
   #     )
   # )
   # outfreqnet(
   #   outfreqnet() %>% 
   #     mutate(
   #       Count = if_else(Variables == "Filtered networks", NA, Count)
   #     )
   # )
  })
  
  output$outnetfreq <- renderTable({
    outfreqnet()
  })
  

  observeEvent(input$filterNet, {
    req(catchments())
    req(network_reactive())
    
    network_sf <- network_reactive()
    # criteria5
    if(input$criteria5 == ""){
      network_sf_rep <- filter(network_sf, lcc <= input$slideNETLCC & gpp <= input$slideNETGPP & cmi <= input$slideNETCMI & led <= input$slideNETLED & up_km2 >= input$slideNETUP)
    }else{
      network_sf_rep <- filter(network_sf, lcc <= input$slideNETLCC & gpp <= input$slideNETGPP & cmi <= input$slideNETCMI & led <= input$slideNETLED & !!sym(criteria5name()) <=input$slideNETcrit5 & up_km2 >= input$slideNETUP)
    }

    x <- outfreqnet()
    x$Count[x$Variables=="Filtered networks"] <- nrow(network_sf_rep)
    outfreqnet(x) 
    
    # Extract unique KBA values for selectInput
    unique_NET <- unique(network_sf_rep$network)

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
      
      network_sf_rep_4326 <- network_sf_rep %>% st_transform(4326)
      pop = ~paste("Network:", network)
      
      leafletProxy("map") %>%
        clearGroup('Potential KBAs') %>% 
        addPolygons(data = network_sf_rep_4326, color = '#666666', fillColor = "grey", fillOpacity = 0, weight = 3,
                    layerId = network_sf_rep_4326$network, popup = pop, group = "Potential KBAs", 
                    options = leafletOptions(pane = "layer2")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Potential network", "Intact areas", "Streams", legendcrit()),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Streams"))
      
      # Close the modal once processing is done
      removeModal()
    }else{
      leafletProxy("map") %>%
        clearGroup('Potential KBAs') %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Catchments extent", "Planning region", "Intact areas", legendcrit()),
                         options = layersControlOptions(collapsed = FALSE))
      
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
      clearGroup(labelNET) %>%
      clearGroup("Upstream") %>%  # Clear previous highlight
      addPolygons(data = selected_net, color = "black",  fillColor = "grey", fillOpacity = 0.5, weight = 3, layerId = ~network,  # Ensure each polygon has a unique ID
                   group = input$network) %>%
      addPolygons(data = selected_up, color = "blue",  fillColor = "blue", fillOpacity = 0.2, weight = 2, group = "Upstream") %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("Catchments extent", "Planning region", "Potential KBA", "Intact areas", input$network, "Upstream","Streams", legendcrit()),
        options = layersControlOptions(collapsed = FALSE)
      )
    reactive_labelNET(input$network)
  })
  ####################################################################################################
  # Render Rep Analysis per network
  ####################################################################################################
  observeEvent(input$network, {
    req(input$network)  # Ensure there is a selected KBA
    
    if(input$forceKBA){
      if(input$criteria5==""){
        network_dir <- paste0("output/plotnet_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),
                              "_led", as.character(input$slideLED),"_lcc" , as.character(input$slideLCC), "_up", as.character(input$slideUP),
                              "_n",input$set_net, "_force", as.character(input$forceKBA))
        
        # Prepare the table for display
        x <- tibble(
          Variables = c("Area km2", "AWI", "Upstream area km2", "Upstream AWI", 
                        "DCI", "CMI", "GPP", "LED", "LCC"),
          Values = NA
        )
      }else{
        network_dir <- paste0("output/plotnet_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),
                              "_led", as.character(input$slideLED),"_lcc", as.character(input$slideLCC), "_", criteria5name(), as.character(input$slidecrit5), 
                              "_up", as.character(input$slideUP), "_n",input$set_net, "_force", as.character(input$forceKBA))
        # Prepare the table for display
        x <- tibble(
          Variables = c("Area km2", "AWI", "Upstream area km2", "Upstream AWI", 
                        "DCI", "CMI", "GPP", "LED", "LCC", criteria5name()),
          Values = NA
        )
      }
    }else{
      network_dir <- paste0("output/plotnet_up", as.character(input$slideUP), "_n",input$set_net, "_force", as.character(input$forceKBA))
      # Prepare the table for display
      x <- tibble(
        Variables = c("Area km2", "AWI", "Upstream area km2", "Upstream AWI", 
                      "DCI", "CMI", "GPP", "LED", "LCC"),
        Values = NA
      )
    }
    
    
#    if(input$criteria5 == ""){
#      network_dir <- paste0("output/plotnet_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),
#                            "_led", as.character(input$slideLED),"_lcc" , as.character(input$slideLCC), "_up", as.character(input$slideUP),
#                            "_n",input$set_net, "_force", as.character(input$forceKBA))
#      
#      # Prepare the table for display
#      x <- tibble(
#        Variables = c("Area km2", "AWI", "Upstream area km2", "Upstream AWI", 
#                      "DCI", "CMI", "GPP", "LED", "LCC"),
#        Values = NA
#      )
#    }else{
#      network_dir <- paste0("output/plotnet_cmi", as.character(input$slideCMI),"_gpp", as.character(input$slideGPP),
#                              "_led", as.character(input$slideLED),"_lcc", as.character(input$slideLCC), "_", criteria5name(), as.character(input$slidecrit5), 
#                              "_up", as.character(input$slideUP), "_n",input$set_net, "_force", as.character(input$forceKBA))
#      # Prepare the table for display
#      x <- tibble(
#        Variables = c("Area km2", "AWI", "Upstream area km2", "Upstream AWI", 
#                      "DCI", "CMI", "GPP", "LED", "LCC", criteria5name()),
#        Values = NA
#      )
#    }
    
    # Define a route to serve images from the external directory
    shiny::addResourcePath("imageNET", file.path(dirpath(), network_dir))
    
    # Get the filtered polygons and select the one matching the KBA choice
    potential_net <- network_reactive()
    selected_network <- potential_net[potential_net$network == input$network, ]
    
    x$Values[x$Variables == "Area km2"] <- round(as.numeric(st_area(selected_network))/1000000, 2)
    x$Values[x$Variables == "AWI"] <- round(as.numeric(selected_network$AWI) * 100, 2)
    x$Values[x$Variables == "Upstream area km2"] <- round(selected_network$up_km2, 2)
    x$Values[x$Variables == "Upstream AWI"] <- round(as.numeric(selected_network$up_AWI) * 100, 2)
    x$Values[x$Variables == "DCI"] <- round(selected_network$dci, 2)
    x$Values[x$Variables == "CMI"] <- round(selected_network$cmi, 2)
    x$Values[x$Variables == "GPP"] <- round(selected_network$gpp, 2)
    x$Values[x$Variables == "LED"] <- round(selected_network$led, 2)
    x$Values[x$Variables == "LCC"] <- round(selected_network$lcc, 1)
    if(input$criteria5 != ""){
      x$Values[x$Variables == criteria5name()] <- round(selected_network[[criteria5name()]], 1)
    }
    
    output$outnet <- renderTable({
      x
    }, digits = 1)
    ####################################################################################################
    # Render Rep Analysis PLOT per KBA
    ####################################################################################################
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
                 if(input$criteria5 != ""){
                   tags$div(style = "text-align: center; margin: 10px;",  # Center align titles and images
                          tags$h3(input$criteria5),  # Title 
                          tags$img(src = paste0("imageNET/", input$criteria5, "/", input$network, ".png"), height = "400px", width = "300px")
                   )
                 }
        )
      )
    })
  })

  ################################################################################################
  # Save features to a geopackage
  output$downloadData <- downloadHandler(
    filename = function() { paste("KBA_network:cmi", as.character(input$slideCMI),
                                  "gpp", as.character(input$slideGPP),
                                  "led", as.character(input$slideLED),
                                  "lcc", as.character(input$slideLCC),
                                  "_", as.character(input$slideUP),
                                  "_", input$set_net, ".gpkg", sep="") },
    content = function(file) {
      poly_sf_filtered <- poly_filtered_reactive()
      KBA_network <-network_reactive()
      poly_sf_rep <- filter(poly_sf_filtered, lcc <= input$slideLCC & gpp <= input$slideGPP & cmi <= input$slideCMI & led <= input$slideLED & up_km2 <= input$slideUP)
      st_write(KBA_network, dsn=file, layer='KBA_networks_representative', append=FALSE)
      st_write(poly_sf_rep, dsn=file, layer='KBAs_filtered', append=TRUE)
    }
  )
  
}