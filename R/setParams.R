setParamsServer <- function(input, output, session, project, map, rv){
  
  project_is_new <- reactiveVal(FALSE)

  ## Observe on actionButton
  observeEvent(input$set_wd, {
    updateActionButton(session, "set_wd", label = "Confirmed", icon = icon("check", lib = "font-awesome"))
  })
  
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
        div(style = "margin: 15px; font-size:15px; font-weight: bold", "Existing project(s) found in this directory"),

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
          selectInput("existing_project", "Choose a project:", choices = folders)
          
        ),
        
        # Shown only if "new" is selected
        conditionalPanel(
          condition = "input.project_choice == 'new'",
          textInput("new_project", "Enter a name for your new project:")
        )
      )
    }
    # -------------------------------------------
    tagList(
      main_ui,
      tags$br(),
      actionButton("confirm_project", "Confirm", class = "btn-warning", style="width:200px")
    )
  })
  
  # reactive UI on existing project
  output$intactCol_ui <- renderUI({
    req(rv$layers_rv$catchments)
    
    catch_nm <- colnames(rv$layers_rv$catchments)
    tagList(
      br(),
      div(style = "margin-top: -20px;", selectInput("intactColname", label = div(style = "font-size:13px;margin-top: -10px;", "Specify intactness attribute"), choices = c("Please select", catch_nm))),
      #br(),
      #actionButton("confirm_project", "Confirm", class = "btn-warning", style="width:200px")
    )
  
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
      return() 
      }
    }
    
    # Case 2: existing subfolders: user chooses existing
    if (input$project_choice == "existing") {
      req(input$existing_project)
      rv$project_name(input$existing_project)
      rv$outdir(file.path(rv$dirpath(), rv$project_name()))
      project_is_new(FALSE)
      
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
        rv$seed_reactive(read.csv(file.path(rv$outdir(), "Builder_input/nghbrs.csv")))
      }

      # Load spatial object
      manifest_file <- file.path(rv$outdir(), "data/layer_paths.rds")
      rv$layer_paths(readRDS(manifest_file))
      paths <- rv$layer_paths()
      
      rv$layers_rv$catchments <- st_read(paths[["catchments"]])
      rv$layers_rv_4326$catchments <- rv$layers_rv$catchments %>% st_transform(4326)
      
      rv$layers_rv$streams <- st_read(paths[["stream"]])
      rv$layers_rv_4326$streams <- rv$layers_rv$streams %>% st_transform(4326)
      
      rv$layers_rv$planreg <- st_read(paths[["planning region"]])
      rv$layers_rv_4326$planreg <- rv$layers_rv$planreg %>% st_transform(4326)
      
      if (!is.null(paths[["protected areas"]]) && file.exists(paths[["protected areas"]])) {
        rv$layers_rv$pas_sf <- st_read(paths[["protected areas"]])
        rv$layers_rv_4326$pas_sf <- rv$layers_rv$pas_sf %>% st_transform(4326)
      }

      if ("reference area" %in% names(paths) && !is.null(paths[["reference area"]]) && file.exists(paths[["reference area"]])) {
        rv$refarea_reactive(st_read(paths[["reference area"]]))
      } 
      
      # Load raster
      if(file.exists(file.path(rv$outdir(), "output/kba_lcc.tif"))){
        rv$layers_rv$lcc <- terra::rast(file.path(rv$outdir(), "output/kba_lcc.tif"))
        rv$layers_rv_4326$lcc <- terra::rast(file.path(rv$outdir(), "output/kba_lcc_4326.tif"))
      } else{
        rv$layers_rv$lcc <- terra::rast(paths[["LCC"]])
      }
      if(file.exists(file.path(rv$outdir(), "output/kba_led.tif"))){
        rv$layers_rv$led <- terra::rast(file.path(rv$outdir(), "output/kba_led.tif"))
        rv$layers_rv_4326$led <- terra::rast(file.path(rv$outdir(), "output/kba_led_4326.tif"))
      } else{
        rv$layers_rv$led <- terra::rast(paths[["LED"]])
      } 
      if(file.exists(file.path(rv$outdir(), "output/kba_gpp.tif"))){
        rv$layers_rv$gpp <- terra::rast(file.path(rv$outdir(), "output/kba_gpp.tif"))
        rv$layers_rv_4326$gpp <- terra::rast(file.path(rv$outdir(), "output/kba_gpp_4326.tif"))
      } else{
        rv$layers_rv$gpp <- terra::rast(paths[["GPP"]])
      } 
      if(file.exists(file.path(rv$outdir(), "output/kba_cmi.tif"))){
        rv$layers_rv$cmi <- terra::rast(file.path(rv$outdir(), "output/kba_cmi.tif"))
        rv$layers_rv_4326$cmi <- terra::rast(file.path(rv$outdir(), "output/kba_cmi_4326.tif"))
      } else{
        rv$layers_rv$cmi <- terra::rast(paths[["CMI"]])
      }
      
      #if(!is.null(rv$criteria5name())){
       # if(file.exists(file.path(rv$outdir(), "output/kba_cmi.tif"))){
      #    rv$layers_rv$cmi <- terra::rast(file.path(rv$outdir(), "output/kba_cmi.tif"))
      #    rv$layers_rv_4326$cmi <- terra::rast(file.path(rv$outdir(), "output/kba_cmi_4326.tif"))
      #  } else{
      #    rv$layers_rv$cmi <- terra::rast(paths[["CMI"]])
       # }
      #} rv$layers_rv$criteria5 <- terra::rast(paths[[paste0(rv$criteria5name(), ".tif")]])
      
      
    }
    
    # Case 3: existing subfolders: user chooses new
    if (input$project_choice == "new") {
      req(input$new_project)
      
      rv$project_name(input$new_project)
      rv$outdir(file.path(rv$dirpath(), rv$project_name()))
      project_is_new(TRUE)
      dir.create(file.path(rv$dirpath(), rv$project_name()))
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
      return()
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
      fileInput("csv_file", "Upload CSV file", accept = ".csv")
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
    
    rv$layer_paths <- layer_paths
    saveRDS(layer_paths, file.path(rv$outdir(), "data/layer_paths.rds"))
    
    rv$layers_rv$catchments  <- read_shp_from_csv(input$csv_file, "catchments")
    rv$layers_rv_4326$catchments <- rv$layers_rv$catchments %>% st_transform(4326)
    
    rv$layers_rv$streams     <- read_shp_from_csv(input$csv_file, "stream")
    rv$layers_rv_4326$streams <- rv$layers_rv$streams %>% st_transform(4326)
    
    rv$layers_rv$planreg     <- read_shp_from_csv(input$csv_file, "planning region")
    rv$layers_rv_4326$planreg <- rv$layers_rv$planreg %>% st_transform(4326)
    
    ## OPTIONAL: protected areas ----
    if ("protected areas" %in% csv_data$Layer) {
      rv$layers_rv$pas_sf <- read_shp_from_csv(input$csv_file, "protected areas")
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
    } else {
      rv$refarea_reactive(NULL)
    }

    rv$layers_rv$lcc         <- read_tif_from_csv(input$csv_file, "LCC")
    rv$layers_rv$led         <- read_tif_from_csv(input$csv_file, "LED")
    rv$layers_rv$gpp         <- read_tif_from_csv(input$csv_file, "GPP")
    rv$layers_rv$cmi         <- read_tif_from_csv(input$csv_file, "CMI")

  })
  
  ################################################################################################
  # Observe on independent file upload
  # --- Individual shapefile uploads
  observeEvent(input$upload_catch, {
    rv$layers_rv$catchments <- read_shp_from_upload(input$upload_catch)
    rv$layers_rv_4326$catchments <- rv$layers_rv$catchments %>% st_transform(4326)
    paths <- rv$layer_paths()
    paths[["catchments"]] <- input$upload_catch$datapath
    rv$layer_paths(paths)
  })
  
  observeEvent(input$upload_stream, {
    rv$layers_rv$streams <- read_shp_from_upload(input$upload_stream)
    rv$layers_rv_4326$streams <- rv$layers_rv$streams %>% st_transform(4326)
    paths <- rv$layer_paths()
    paths[["stream"]] <- input$upload_stream$datapath
    rv$layer_paths(paths)
  })
  
  observeEvent(input$upload_planreg, {
    rv$layers_rv$planreg <- read_shp_from_upload(input$upload_planreg)
    rv$layers_rv_4326$planreg <- rv$layers_rv$planreg %>% st_transform(4326)
    paths <- rv$layer_paths()
    paths[["planning region"]] <- input$upload_planreg$datapath
    rv$layer_paths(paths)
  })
  
  observeEvent(input$upload_pas, {
    rv$layers_rv$pas_sf <- read_shp_from_upload(input$upload_pas)
    rv$layers_rv_4326$pas_sf <- rv$layers_rv$pas_sf %>% st_transform(4326)
    paths <- rv$layer_paths()
    paths[["protected areas"]] <- input$upload_pas$datapath
    rv$layer_paths(paths)
  })
  
  # --- Individual raster uploads
  observeEvent(input$upload_lcc, {
    rv$layers_rv$lcc <- read_tif_from_upload(input$upload_lcc)
    paths <- rv$layer_paths()
    paths[["LCC"]] <- input$upload_lcc$datapath
    rv$layer_paths(paths)
  })
  
  observeEvent(input$upload_led, {
    rv$layers_rv$led <- read_tif_from_upload(input$upload_led)
    paths <- rv$layer_paths()
    paths[["LED"]] <- input$upload_led$datapath
    rv$layer_paths(paths)
  })
  
  observeEvent(input$upload_cmi, {
    rv$layers_rv$cmi <- read_tif_from_upload(input$upload_cmi)
    paths <- rv$layer_paths()
    paths[["CMI"]] <- input$upload_cmi$datapath
    rv$layer_paths(paths)
  })
  
  observeEvent(input$upload_gpp, {
    rv$layers_rv$gpp <- read_tif_from_upload(input$upload_gpp)
    paths <- rv$layer_paths()
    paths[["GPP"]] <- input$upload_gpp$datapath
    rv$layer_paths(paths)
  })
  
  observeEvent(input$upload_custom, {
    rv$layers_rv$criteria5 <- read_tif_from_upload(input$upload_custom)
    paths <- rv$layer_paths()
    paths[[rv$criteria5name()]] <- input$upload_custom$datapath
    rv$layer_paths(paths)
  })
  

  # Set criteria5
  criteria5 <- reactive({
    if (!is.null(input$upload_custom)) {
      # Read raster from file upload
      rastName <- sub("\\..*$", "", input$upload_custom$name)
      rv$criteria5name(rastName)
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
            rv$criteria5name(unexpected_layers)
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
  
  observeEvent(rv$layers_rv$streams, {
    req(rv$layers_rv$planreg)
    req(rv$layers_rv$streams)
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Uploading layers. Please wait...",
      easyClose = TRUE,
      footer = modalButton("OK"))
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
    
    #Trigger a JS callback to remove modal after rendering
    session$sendCustomMessage("remove_modal_js", list())
  
  })
  
  observeEvent(rv$layers_rv$pas_sf, {
    req(rv$layers_rv$pas_sf)
    
    legend <- c(rv$overlayGroups(), "Protected areas")
    rv$overlayGroups(legend)
    
    pas_4326 <- rv$layers_rv_4326$pas_sf
    
    leafletProxy("map") %>%
      addPolygons(data=pas_4326, color='#6b4b38', fillOpacity = 0.6, weight=2, group="Protected areas", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = rv$overlayGroups(),
                       options = layersControlOptions(collapsed = FALSE))  %>%
      hideGroup(c("Streams"))
  }, once = TRUE)
  
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
}