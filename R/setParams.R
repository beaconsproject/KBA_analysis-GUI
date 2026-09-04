setParamsServer <- function(input, output, session, project, map, rv){
  
  project_is_new <- reactiveVal(FALSE)
  
  ## Observe on actionButton
  observeEvent(input$set_wd, {
    updateActionButton(session, "set_wd", label = "Confirmed", icon = icon("check", lib = "font-awesome"))
  })
  
  #***  
  # Observe map click events to update the selected polygon
  observeEvent(input$map_shape_click, {
    rv$selected_polygon(input$map_shape_click$id)  # Store the layerId of the clicked polygon
  })
  #***
  ################################################################################################
  # Set dir
  ################################################################################################
  # Define root access points (change as needed for Windows/Linux/Mac)
  roots <- get_available_drives()
  shinyDirChoose(input, "directory", roots = roots, session = getDefaultReactiveDomain())
  
  # Reactive to store the selected directory path
  dirpath <- reactive({
    req(input$directory)
    i <- parseDirPath(roots, input$directory)
    rv$dirpath(i)
    return(i)
  })
  
  # Show selected directory path
  output$dirpath <- renderText({
    dirpath()
  })
  
  subprojects <- reactive({
    req(rv$dirpath())
    
    # List ONLY subfolders
    folders <- list.dirs(rv$dirpath(), full.names = FALSE, recursive = FALSE)
    folders
  })
  
  #--------
  # reactive UI on existing project
  output$project_ui <- renderUI({
    req(input$set_wd)
    folders <- subprojects()
    
    # -------------------------------------------
    # No existing subfolders
    main_ui <- if (length(folders) == 0) {
      tagList(
        textInput("new_project", "Enter a name for your new project:"),
      )
    } else{
      # Subfolders exist = existing projects
      tagList(
        div(style = "margin-top: -10px; margin-left: 15px; font-size:15px; font-weight: bold", "Existing project(s) found in this directory"),
        
        radioButtons(
          "project_choice",
          "Select an option:",
          choices = list(
            "Use an existing project" = "existing",
            "Create a new project" = "new"
          )
        ),
        
        # Shown only if "existing" is selected
        conditionalPanel(
          condition = "input.project_choice == 'existing'",
          div(style = "margin-top: -10px;", selectInput("existing_project", "Choose a project:", choices = folders))
          
        ),
        
        # Shown only if "new" is selected
        conditionalPanel(
          condition = "input.project_choice == 'new'",
          div(style = "margin-top: -10px;", textInput("new_project", "Enter a name for your new project:"))
        )
      )
    }
    # -------------------------------------------
    tagList(
      main_ui,
      tags$br(),
      div(style = "margin-top: -25px;",actionButton("confirm_project", "Confirm", class = "btn-warning", style="width:200px"))
    )
  })
  
  # reactive UI on existing project
  output$intactCol_ui <- renderUI({
    req(rv$layers_rv$catchments)
    
    catch_nm <- colnames(rv$layers_rv$catchments)
    tagList(
      br(),
      div(style = "margin-top: -40px;", selectInput("intactColname", label = div(style = "font-size:13px;margin-top: -10px;", "Specify intactness attribute"), choices = c("Please select", catch_nm))),
      #br(),
      #actionButton("confirm_project", "Confirm", class = "btn-warning", style="width:200px")
    )
    
  })
  
  # reactive UI on protected areas
  output$pas_ui <- renderUI({
    req(input$intactColname != "Please select")
    
    if (is.null(rv$layers_rv$pas_sf)) {
      div(style = "margin-top: -10px;", fileInput(inputId = "upload_pas", label  = "Upload Protected Areas - OPTIONAL", multiple = TRUE, accept = c(".shp", ".dbf", ".shx", ".prj")))
    } 
  })
  
  #---------
  # Confirm project
  observeEvent(input$confirm_project, {
    folders <- subprojects()
    
    # Case 1: no existing subfolders → must create new project
    if (length(folders) == 0) {
      req(input$new_project)
      rv$project_name(input$new_project)
      rv$outdir(file.path(rv$dirpath(), rv$project_name()))
      project_is_new(TRUE)
      dir.create(file.path(rv$dirpath(), rv$project_name()))
      
      showModal(modalDialog(
        title = "Output directory is empty",
        "Please upload spatial layers and run the analysis in the output directory",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
      treedir <- c("output","Builder_input","Builder_output", "data")
      for(d in treedir){
        dir.create(file.path(rv$dirpath(), rv$project_name(), d))
        showModal(modalDialog(
          title = "Output subdirectories created.",
          "Please select input parameters by either uploading a csv containing input path or by pointing on the source files.",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      }
    }
    
    # Case 2: existing subfolders: user chooses existing
    req(input$project_choice)
    if (input$project_choice == "existing") {
      req(input$existing_project)
      rv$project_name(input$existing_project)
      rv$outdir(file.path(rv$dirpath(), rv$project_name()))
      project_is_new(FALSE)
    
      layers <-NULL
      if(file.exists(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))){
        layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
        layers <- layers_info$name
      }
      
      
      # Load builder input
      if(file.exists(file.path(rv$outdir(), "Builder_input/seeds.csv"))){
        rv$seed_reactive(read.csv(file.path(rv$outdir(), "Builder_input/seeds.csv")))
      }else{
        showModal(modalDialog(
          title = "Builder input don't exist.",
          "Please create Builder input and run Builder under the tab Build KBAs.",
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      }
      if(file.exists(file.path(rv$outdir(), "Builder_input/nghbrs.csv"))){
        rv$nghbrs_reactive(read.csv(file.path(rv$outdir(), "Builder_input/nghbrs.csv")))
      }
      
      # Load spatial object
      manifest_file <- file.path(rv$outdir(), "data/layer_paths.csv")
      if(file.exists(manifest_file)){
        rv$layer_paths(read.csv(manifest_file))
        paths <- rv$layer_paths()
        rv$layers_rv$catchments <- st_read(paths$Path[paths$Layer == "catchments"])
        rv$layers_rv_4326$catchments <- rv$layers_rv$catchments %>% st_transform(4326)
        
        rv$layers_rv$streams <- st_read(paths$Path[paths$Layer == "stream"]) 
        rv$layers_rv_4326$streams <- rv$layers_rv$streams %>% st_transform(4326)
        
        rv$layers_rv$planreg <- st_read(paths$Path[paths$Layer == "planning region"])
        rv$layers_rv_4326$planreg <- rv$layers_rv$planreg %>% st_transform(4326)
        
        if ("protected areas" %in% paths$Layer &&
            file.exists(paths$Path[paths$Layer == "protected areas"])) {
          
          rv$layers_rv$pas_sf <-st_read(paths$Path[paths$Layer == "protected areas"])
          rv$layers_rv_4326$pas_sf <- sf::st_transform(rv$layers_rv$pas_sf, 4326)
        }
        
        if ("reference area" %in% paths$Layer &&
            file.exists(paths$Path[paths$Layer == "reference area"])){
          rv$refarea_reactive(st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "reference area"))
        } 
        
        # Load raster
        if(file.exists(file.path(rv$outdir(), "output/kba_lcc.tif"))){
          rv$layers_rv$lcc <- terra::rast(file.path(rv$outdir(), "output/kba_lcc.tif"))
          rv$layers_rv_4326$lcc <- terra::rast(file.path(rv$outdir(), "output/kba_lcc_4326.tif"))
        } else{
          rv$layers_rv$lcc <- terra::rast(paths$Path[paths$Layer == "LCC"])
        }
        if(file.exists(file.path(rv$outdir(), "output/kba_led.tif"))){
          rv$layers_rv$led <- terra::rast(file.path(rv$outdir(), "output/kba_led.tif"))
          rv$layers_rv_4326$led <- terra::rast(file.path(rv$outdir(), "output/kba_led_4326.tif"))
        } else{
          rv$layers_rv$led <- terra::rast(paths$Path[paths$Layer == "LED"])
        } 
        if(file.exists(file.path(rv$outdir(), "output/kba_gpp.tif"))){
          rv$layers_rv$gpp <- terra::rast(file.path(rv$outdir(), "output/kba_gpp.tif"))
          rv$layers_rv_4326$gpp <- terra::rast(file.path(rv$outdir(), "output/kba_gpp_4326.tif"))
        } else{
          rv$layers_rv$gpp <- terra::rast(paths$Path[paths$Layer == "GPP"])
        } 
        if(file.exists(file.path(rv$outdir(), "output/kba_cmi.tif"))){
          rv$layers_rv$cmi <- terra::rast(file.path(rv$outdir(), "output/kba_cmi.tif"))
          rv$layers_rv_4326$cmi <- terra::rast(file.path(rv$outdir(), "output/kba_cmi_4326.tif"))
        } else{
          rv$layers_rv$cmi <- terra::rast(paths$Path[paths$Layer == "CMI"])
        }
        
        #criteria 5
        req_layers <- c("CMI", "LED", "GPP", "LCC", "catchments", "stream", "planning region", "protected areas", "reference area")
        unexpected_layers <- paths$Layer[!paths$Layer %in% req_layers]
        if (length(unexpected_layers)>0) {
          if(length(unexpected_layers)==1){
            rv$criteria5name(unexpected_layers)
            if (file.exists(file.path(rv$outdir(), "output", paste0(unexpected_layers, ".tif")))) {
              rv$layers_rv$criteria5 <- terra::rast(file.path(rv$outdir(), "output", paste0(unexpected_layers, ".tif")))
              rv$layers_rv_4326$criteria5 <- terra::rast(file.path(rv$outdir(), "output", paste0(unexpected_layers, "_4326.tif")))
            } else{
              orig_path <- paths$Path[paths$Layer == unexpected_layers]
              rv$layers_rv$criteria5 <- terra::rast(orig_path)
            }
          }
        }
      }else{
        rv$layers_rv$catchments <- st_read(file.path(rv$outdir(), "data/catchments.shp"))
        rv$layers_rv_4326$catchments <- rv$layers_rv$catchments %>% st_transform(4326)
        
        rv$layers_rv$streams <- st_read(file.path(rv$outdir(), "data/stream.shp")) 
        rv$layers_rv_4326$streams <- rv$layers_rv$streams %>% st_transform(4326)
        
        rv$layers_rv$planreg <- st_read(file.path(rv$outdir(), "data/planning region.shp"))
        rv$layers_rv_4326$planreg <- rv$layers_rv$planreg %>% st_transform(4326)
        
        if (file.exists(file.path(rv$outdir(), "data/protected areas"))) {
          
          rv$layers_rv$pas_sf <-st_read(file.path(rv$outdir(), "data/protected areas.shp"))
          rv$layers_rv_4326$pas_sf <- sf::st_transform(rv$layers_rv$pas_sf, 4326)
        }
        
        if (file.exists(file.path(rv$outdir(), "data/reference area"))){
          rv$refarea_reactive(st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "reference area"))
        } 
        
        # Load raster
        if(file.exists(file.path(rv$outdir(), "output/kba_lcc.tif"))){
          rv$layers_rv$lcc <- terra::rast(file.path(rv$outdir(), "output/kba_lcc.tif"))
          rv$layers_rv_4326$lcc <- terra::rast(file.path(rv$outdir(), "output/kba_lcc_4326.tif"))
        } else{
          rv$layers_rv$lcc <- terra::rast(file.path(rv$outdir(), "data/LCC.tif"))
        }
        if(file.exists(file.path(rv$outdir(), "output/kba_led.tif"))){
          rv$layers_rv$led <- terra::rast(file.path(rv$outdir(), "output/kba_led.tif"))
          rv$layers_rv_4326$led <- terra::rast(file.path(rv$outdir(), "output/kba_led_4326.tif"))
        } else{
          rv$layers_rv$led <- terra::rast(file.path(rv$outdir(), "data/LED.tif"))
        } 
        if(file.exists(file.path(rv$outdir(), "output/kba_gpp.tif"))){
          rv$layers_rv$gpp <- terra::rast(file.path(rv$outdir(), "output/kba_gpp.tif"))
          rv$layers_rv_4326$gpp <- terra::rast(file.path(rv$outdir(), "output/kba_gpp_4326.tif"))
        } else{
          rv$layers_rv$gpp <- terra::rast(file.path(rv$outdir(), "data/GPP.tif"))
        } 
        if(file.exists(file.path(rv$outdir(), "output/kba_cmi.tif"))){
          rv$layers_rv$cmi <- terra::rast(file.path(rv$outdir(), "output/kba_cmi.tif"))
          rv$layers_rv_4326$cmi <- terra::rast(file.path(rv$outdir(), "output/kba_cmi_4326.tif"))
        } else{
          rv$layers_rv$cmi <- terra::rast(file.path(rv$outdir(), "data/CMI.tif"))
        }
        
        #criteria 5
        req_layers <- c("CMI", "LED", "GPP", "LCC", "catchments", "stream", "planning region", "protected areas", "reference area")
        lf <- list.files(file.path(rv$outdir(), "data"), pattern = "\\.(shp|tif)$", ignore.case = TRUE)
        lf_no_ext <- tools::file_path_sans_ext(lf)
        unexpected_layers <- lf_no_ext[!lf_no_ext %in% req_layers]
        if (length(unexpected_layers)>0) {
          if(length(unexpected_layers)==1){
            rv$criteria5name(unexpected_layers)
            if (file.exists(file.path(rv$outdir(), "output", paste0(unexpected_layers, ".tif")))) {
              rv$layers_rv$criteria5 <- terra::rast(file.path(rv$outdir(), "output", paste0(unexpected_layers, ".tif")))
              rv$layers_rv_4326$criteria5 <- terra::rast(file.path(rv$outdir(), "output", paste0(unexpected_layers, "_4326.tif")))
            } else{
              orig_path <- file.path(rv$outdir(), "data", paste0(unexpected_layers, ".tif"))
              rv$layers_rv$criteria5 <- terra::rast(orig_path)
            }
          }
        }
      }
      
    }
    
    # Case 3: existing subfolders: user chooses new
    if (input$project_choice == "new") {
      req(input$new_project)
      
      project_path <- file.path(rv$dirpath(), input$new_project)
      
      if (dir.exists(project_path)) {
        showModal(modalDialog(
          title = "Project name unavailable",
          paste0( "The project name '", input$new_project,"' already exists.\n\n", "Please choose a different project name to avoid overwriting data."),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)      
      }
      
      rv$project_name(input$new_project)
      rv$outdir(project_path)
      project_is_new(TRUE)
      
      dir.create(project_path, recursive = TRUE)
      treedir <- c("output", "Builder_input", "Builder_output", "data")
      
      for (d in treedir) {
        dir.create(file.path(project_path, d))
      }
      
      showModal(
        modalDialog(title = "Project created",  "Output subdirectories were successfully created. Please select input parameters.",
                    easyClose = TRUE,
                    footer = modalButton("OK")
        ))
      return(NULL)
    }
  })
  
  # reactive UI on new project
  output$newproject_ui <- renderUI({
    req(project_is_new())
    
    tagList(
      radioButtons("setUpload", "Set the source for spatial dataset:",
                   choices = list("Use csv with file pathways" = "useCSV", 
                                  "Upload individual layer" = "indUpload"),
                   selected = character(0), 
                   inline = FALSE)
      ,
      conditionalPanel(
        condition="input.setUpload=='useCSV'",
        div(style = "margin-top: -20px;",fileInput("csv_file", "Upload CSV file", accept = ".csv"))
      ),
      conditionalPanel(
        condition=" input.setUpload=='indUpload'",
        div(style = "margin-top: -20px;",fileInput(inputId = "upload_catch", label = "Catchments dataset", multiple = TRUE)),
        div(style = "margin-top: -30px;",fileInput(inputId = "upload_stream", label = "Streams dataset", multiple = TRUE)),
        div(style = "margin-top: -30px;",fileInput(inputId = "upload_planreg", label = "Planning region", multiple = TRUE)),
        div(style = "margin-top: -30px;",fileInput(inputId = "upload_lcc", label = "LCC", multiple = FALSE)),
        div(style = "margin-top: -30px;",fileInput(inputId = "upload_led", label = "LED", multiple = FALSE)),
        div(style = "margin-top: -30px;",fileInput(inputId = "upload_cmi", label = "CMI", multiple = FALSE)),
        div(style = "margin-top: -30px;",fileInput(inputId = "upload_gpp", label = "GPP", multiple = FALSE)),
        div(style = "margin-top: -30px;",fileInput(inputId = "upload_custom", label = "Custom criteria", multiple = FALSE))
      )
    )
  }) 
  
  ################################################################################################
  # Validate csv
  required_layers <- c("catchments", "stream", "planning region", "CMI", "GPP", "LCC", "LED")
  
  # Reactive function to validate the input file
  validate_csv <- reactive({
    req(input$csv_file)  # Ensure the file input is not NULL
    
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
  # Read CSV
  observeEvent(input$csv_file, {
    req(validate_csv())  # ensure CSV is valid
    csv_data <- read.csv(input$csv_file$datapath)
    layer_paths <- setNames(csv_data$Path, csv_data$Layer)
    
    rv$layer_paths(layer_paths)
    write.csv(csv_data, file.path(rv$outdir(), "data/layer_paths.csv"))
    
    rv$layers_rv$catchments  <- read_shp_from_csv(input$csv_file, "catchments")
    rv$layers_rv_4326$catchments <- rv$layers_rv$catchments %>% st_transform(4326)
    
    rv$layers_rv$streams     <- read_shp_from_csv(input$csv_file, "stream")
    rv$layers_rv_4326$streams <- rv$layers_rv$streams %>% st_transform(4326)
    
    rv$layers_rv$planreg     <- read_shp_from_csv(input$csv_file, "planning region")
    rv$layers_rv_4326$planreg <- rv$layers_rv$planreg %>% st_transform(4326)
    
    ## OPTIONAL: protected areas ----
    if ("protected areas" %in% csv_data$Layer) {
      pas_sf <- read_shp_from_csv(input$csv_file, "protected areas")
      
      n <- nrow(pas_sf)
      if (!"PA_ID" %in% colnames(pas_sf)) {
        pas_sf$PA_ID <- seq_len(n)
      }
      if (!"NAME" %in% colnames(pas_sf)) {
        pas_sf$NAME <- NA_character_
      }
      
      rv$layers_rv$pas_sf <- pas_sf
      rv$layers_rv_4326$pas_sf <- sf::st_transform(rv$layers_rv$pas_sf, 4326)
    } else {
      rv$layers_rv$pas_sf <- NULL
      rv$layers_rv_4326$pas_sf <- NULL
    }
    
    ## OPTIONAL: reference area ----
    if ("reference area" %in% csv_data$Layer) {
      rv$refarea_reactive(
        read_shp_from_csv(input$csv_file, "reference area")
      )
      st_write(rv$refarea_reactive(), dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "reference area", driver = "GPKG", append = TRUE)
    } else {
      rv$refarea_reactive(NULL)
    }
    
    rv$layers_rv$lcc         <- read_tif_from_csv(input$csv_file, "LCC")
    rv$layers_rv$led         <- read_tif_from_csv(input$csv_file, "LED")
    rv$layers_rv$gpp         <- read_tif_from_csv(input$csv_file, "GPP")
    rv$layers_rv$cmi         <- read_tif_from_csv(input$csv_file, "CMI")
    
    #criteria 5
    req_layers <- c("CMI", "LED", "GPP", "LCC", "catchments", "stream", "planning region", "protected areas", "reference area")
    unexpected_layers <- csv_data$Layer[!csv_data$Layer %in% req_layers]
    if (length(unexpected_layers)>0) {
      if(length(unexpected_layers)==1){
        path <- csv_data$Path[csv_data$Layer == unexpected_layers]
        if (file.exists(path)) {
          rv$criteria5name(unexpected_layers)
          rv$layers_rv$criteria5 <- read_tif_from_csv(input$csv_file, unexpected_layers)
          updateSliderInput(session = getDefaultReactiveDomain(), "slidecrit5", label = unexpected_layers)
          updateSliderInput(session = getDefaultReactiveDomain(), "slideNETcrit5", label = unexpected_layers)
        } else {
          stop("The custom variable path in the CSV does not exist.")
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
  })
  
  ################################################################################################
  # Observe on independent file upload
  # --- Individual shapefile uploads
  observeEvent(input$upload_catch, {
    rv$layers_rv$catchments <- read_shp_from_upload(input$upload_catch)
    st_write(rv$layers_rv$catchments, file.path(rv$outdir(), "data/catchments.shp"), append = FALSE)
    rv$layers_rv_4326$catchments <- rv$layers_rv$catchments %>% st_transform(4326)
    paths <- rv$layer_paths()
  })
  
  observeEvent(input$upload_stream, {
    rv$layers_rv$streams <- read_shp_from_upload(input$upload_stream)
    st_write(rv$layers_rv$streams, file.path(rv$outdir(), "data/stream.shp"), append = FALSE)
    rv$layers_rv_4326$streams <- rv$layers_rv$streams %>% st_transform(4326)
    paths <- rv$layer_paths()
  })
  
  observeEvent(input$upload_planreg, {
    rv$layers_rv$planreg <- read_shp_from_upload(input$upload_planreg)
    st_write(rv$layers_rv$planreg, file.path(rv$outdir(), "data/planning region.shp"), append = FALSE)
    rv$layers_rv_4326$planreg <- rv$layers_rv$planreg %>% st_transform(4326)
    paths <- rv$layer_paths()
  })
  
  observeEvent(input$upload_pas, {
    pas_sf <- read_shp_from_upload(input$upload_pas)
    n <- nrow(pas_sf)
    if (!"PA_ID" %in% colnames(pas_sf)) {
      pas_sf$PA_ID <- seq_len(n)
    }
    if (!"NAME" %in% colnames(pas_sf)) {
      pas_sf$NAME <- NA_character_
    }
    st_write(pas_sf, file.path(rv$outdir(), "data/protected areas.shp"), append = FALSE)
    rv$layers_rv$pas_sf <- pas_sf
    rv$layers_rv_4326$pas_sf <- sf::st_transform(pas_sf, 4326)
    paths <- rv$layer_paths()
  })
  
  # --- Individual raster uploads
  observeEvent(input$upload_lcc, {
    rv$layers_rv$lcc <- read_tif_from_upload(input$upload_lcc)
    writeRaster(rv$layers_rv$lcc, file.path(rv$outdir(), "data/LCC.tif"), overwrite = TRUE)
  })
  
  observeEvent(input$upload_led, {
    rv$layers_rv$led <- read_tif_from_upload(input$upload_led)
    writeRaster(rv$layers_rv$led, file.path(rv$outdir(), "data/LED.tif"), overwrite = TRUE)
  })
  
  observeEvent(input$upload_cmi, {
    rv$layers_rv$cmi <- read_tif_from_upload(input$upload_cmi)
    writeRaster(rv$layers_rv$cmi, file.path(rv$outdir(), "data/CMI.tif"), overwrite = TRUE)
  })
  
  observeEvent(input$upload_gpp, {
    rv$layers_rv$gpp <- read_tif_from_upload(input$upload_gpp)
    writeRaster(rv$layers_rv$gpp, file.path(rv$outdir(), "data/GPP.tif"), overwrite = TRUE)
  })
  
  observeEvent(input$upload_custom, {
    rv$layers_rv$criteria5 <- read_tif_from_upload(input$upload_custom)
    rastName <- sub("\\..*$", "", input$upload_custom$name)
    rv$criteria5name(rastName)
    writeRaster(rv$layers_rv$criteria5, file.path(rv$outdir(), "data", paste0(rastName,".tif")), overwrite = TRUE)
    
    updateSliderInput(session = getDefaultReactiveDomain(), "slidecrit5", label = rastName)
    updateSliderInput(session = getDefaultReactiveDomain(), "slideNETcrit5", label = rastName)
  })
  
  ####################################################################################################
  #  Test on required attributes
  ####################################################################################################
  observeEvent(rv$layers_rv$catchments, {
    req(rv$layers_rv$catchments)
    missing_cols <- check_colnames(rv$layers_rv$catchments, c("Isolated", "length_m", "FDA_M", "Area_land", "Area_water", "Area_total", "CATCHNUM", "ORDER1", "ORDER2", "ORDER3", "BASIN", "SKELUID"))
    
    if (length(missing_cols) > 0) {
      showModal(modalDialog(
        title = "Missing required column",
        paste0("In the catchments layer, the following column(s) are missing: ",  paste(missing_cols, collapse = ", ")),
        easyClose = TRUE,
        footer = modalButton("OK")
      )
      )
      rv$layers_rv$catchment <- NULL
    }
    
  }, ignoreNULL = TRUE)
  ####################################################################################################
  # Map viewer
  ####################################################################################################
  #Control legend
  init_legend <- c("Intact areas")
  rv$overlayGroups(init_legend)
  
  observeEvent(rv$layers_rv$planreg, {
    #Test if catchments are uploaded
    if (is.null(rv$layers_rv$catchments)) {
      # Create the modal dialog
      showModal(modalDialog(
        title = "Missing Data",
        "Catchments layers is missing. Please go back to Set input parameters to upload catchments layer.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    req(rv$layers_rv$catchments)
    req(rv$layers_rv$planreg)
    planreg_4326 <- rv$layers_rv_4326$planreg
    
    legend <- c(rv$overlayGroups(), "Planning region")
    rv$overlayGroups(legend)
    map_bounds1 <- planreg_4326 %>% st_bbox() %>% as.character()
    
    leafletProxy("map") %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=planreg_4326, color='black', fill = F, weight=3, group="Planning region", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = rv$overlayGroups(),
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  # STREAMS
  observeEvent(rv$layers_rv$streams, {
    req(rv$layers_rv$planreg)
    req(rv$layers_rv$streams)
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Uploading layers. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    stream_4326 <- rv$layers_rv_4326$streams
    
    legend <- c(rv$overlayGroups(), "Streams")
    rv$overlayGroups(legend)
    
    leafletProxy("map") %>%
      addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = rv$overlayGroups(),
                       options = layersControlOptions(collapsed = FALSE))  %>%
      hideGroup(c("Streams")) 
    
    #Remove modal after rendering
    session$sendCustomMessage("remove_modal_js", list())
    
  })
  
  # PROTECTED AREAS 
  # -calculate dci + render
  observeEvent(list(rv$layers_rv$pas_sf, input$intactColname), {
    req(rv$layers_rv$pas_sf,
        input$intactColname,
        input$intactColname != "Please select")
    
    # Faire un test PA_ID et Name 
    legend <- c(rv$overlayGroups(), "Protected areas")
    rv$overlayGroups(legend)
    
    showModal(modalDialog(
      title = "Calculating hydrology metrics on protected areas. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    switch_dci <- TRUE
    
    if(file.exists(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))){
      gpkg_path <- file.path(rv$outdir(), "output/KBA_analysis.gpkg")
      layers <- sf::st_layers(gpkg_path)$name
      if("protected_areas" %in% layers){
        pas <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas")
        rv$layers_rv$pas_sf <- pas
        pas_up <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas_upstream")
        rv$pas_upstream_reactive(pas_up)
        switch_dci <- FALSE
      }
    }
    
    if(switch_dci){
      required_cols <- c("area_km2", "AWI","dci")
      pas <-rv$layers_rv$pas_sf
      pas_colnames <- colnames(pas)
      
      if (any(!required_cols %in% pas_colnames)) {
        catchments <- rv$layers_rv$catchments
        pas_sf <- pas %>%
          mutate(network = sprintf("PA_%02d", row_number()),
                 area_km2 = st_area(.)/1000000)
        
        pas_catch <- st_intersection(pas_sf, catchments)
        area_catch <- pas_catch %>%
          mutate(catch_awi = as.numeric(st_area(.)) * .[[input$intactColname]]) %>%
          st_drop_geometry() %>%
          group_by(network) %>%
          summarize(intact_km2 = sum(catch_awi, na.rm = TRUE)/1000000)
        pas <- merge(pas_sf[,c("network", "NAME", "area_km2")], area_catch[,c("network", "intact_km2")], by = "network", all.x = TRUE)
        pas$AWI <- round(pas$intact_km2/pas$area_km2, 3)
        
        pas$dci <- calc_dci(conservation_area_sf = pas, stream_sf = rv$layers_rv$streams)
      }
      
      if (any(!c("up_km2", "up_AWI") %in% pas_colnames)){
        catchments <- rv$layers_rv$catchments
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
        
        pas_up_att <- pas_up %>% st_drop_geometry()
        pas <- pas %>%
          left_join(pas_up_att[,c("network","up_km2", "up_AWI")], by = "network")
      }
      st_write(pas, dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas", driver = "GPKG", append = FALSE) 
    }

    pas_4326 <- pas %>% st_transform(4326)
    
    leafletProxy("map") %>%
      addPolygons(data=pas_4326, color='#6b4b38', fillOpacity = 0.6, weight=2, layerId = pas_4326$network, popup = ~network, group="Protected areas", options = leafletOptions(pane = "over")) %>% 
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = rv$overlayGroups(),
                       options = layersControlOptions(collapsed = FALSE))  %>%
      hideGroup(c("Streams"))
    
    removeModal()
    rv$pas_ready(TRUE)
  }, ignoreInit = TRUE)
  
  observeEvent(rv$refarea_reactive(), {
    req(rv$refarea_reactive())
    
    legend <- c(rv$overlayGroups(), "Reference area")
    rv$overlayGroups(legend)
    
    refarea_4326 <- st_transform(rv$refarea_reactive(), 4326)
    
    leafletProxy("map") %>%
      addPolygons(data=refarea_4326, color='red', fill = F, weight=3, group="Reference area", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = rv$overlayGroups(),
                       options = layersControlOptions(collapsed = FALSE))  %>%
      hideGroup(c("Streams"))
  }, once = TRUE)
  
  observeEvent(input$remove_modal, {
    removeModal()
    updateActionButton(session, "confirm_project", label = "Confirmed", icon = icon("check", lib = "font-awesome"))
  })
  
  ####################################################################################################
  # -Render bottom PAs statistics table
  ####################################################################################################
  outtabPA <- reactive({
    req(input$tabs == 'tabUpload')
    req(rv$pas_ready())
    
    pas <- st_read(dsn = file.path(rv$outdir(), "output/KBA_analysis.gpkg"), layer = "protected_areas") %>%
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
  
  output$pastbl <- DT::renderDT({
    req(input$tabs == 'tabUpload')
    req(rv$pas_ready())
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
}