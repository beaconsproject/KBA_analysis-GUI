server = function(input, output, session) {
  
  # Reactive values 
  reactiveValsList <-  list(dirpath = reactiveVal(NULL),
                            project_name = reactiveVal(NULL),
                            outdir = reactiveVal(NULL),
                            layer_paths = reactiveVal(NULL),
                            display1_name = reactiveVal(),
                            display2_name = reactiveVal(),
                            display3_name = reactiveVal(),
                            layers_rv = reactiveValues(streams = NULL, 
                                                       planreg = NULL,
                                                       catchments = NULL,
                                                       lcc = NULL,
                                                       cmi = NULL,
                                                       gpp = NULL, 
                                                       led = NULL,
                                                       criteria5 = NULL,
                                                       pas_sf = NULL,
                                                       display1_sf = NULL,
                                                       display2_sf = NULL,
                                                       display3_sf = NULL),
                            layers_rv_4326 = reactiveValues(streams = NULL, 
                                                            planreg = NULL,
                                                            catchments = NULL,
                                                            lcc = NULL,
                                                            cmi = NULL,
                                                            gpp = NULL, 
                                                            led = NULL,
                                                            criteria5 = NULL,
                                                            pas_sf = NULL),
                            nghbrs_reactive = reactiveVal(),
                            seed_reactive = reactiveVal(),
                            kba_sf_reactive = reactiveVal(NULL),
                            kba_reduce_reactive = reactiveVal(NULL),
                            kba_upstream_reactive = reactiveVal(NULL),
                            upstream_reactive = reactiveVal(NULL),
                            pas_sf_reactive = reactiveVal(NULL),
                            pas_upstream_reactive = reactiveVal(NULL),
                            poly_reactive = reactiveVal(),
                            upstream_network_reactive = reactiveVal(),
                            reactive_labelKBA = reactiveVal(NULL),
                            reactive_labelNET = reactiveVal(NULL),
                            kba_init_label = reactiveVal(NULL),
                            network_reactive = reactiveVal(),
                            dir_exists = reactiveVal(FALSE),
                            plotDir = reactiveVal(),
                            netDir = reactiveVal(),
                            selected_polygon = reactiveVal(NULL) , # Track the selected polygon on map
                            refarea_reactive = reactiveVal(NULL),
                            tab_upload_visited = reactiveVal(FALSE),
                            pas_ready = reactiveVal(FALSE),
                            filtered_kba = reactiveVal(NULL),
                            filtered_pas = reactiveVal(NULL),
                            filtered_rep = reactiveVal(NULL),
                            overlayGroups = reactiveVal(character()),
                            overlayKBA = reactiveVal(character()),
                            legendcrit =  reactiveVal(c("CMI", "LED", "GPP", "LCC")),
                            criteria5name = reactiveVal(NULL),
                            
                            outfreqhydro = reactiveVal(
                              tibble(Variables = c("KBAs", "Reduced KBAs"), Count = NA_integer_)
                            ),
                            outfreqkba = reactiveVal(
                              tibble(Variables = c("KBAs", "Filtered KBAs", "PAs", "Filtered PAs"), Count = NA_integer_)
                            ),
                            outfreqnet = reactiveVal(
                              tibble(Variables = c("KBAs", "PAs", "Networks", "Filtered networks"), Count = NA_integer_)
                            )
  )
  
  output$overviewMD <- renderUI({
    HTML(markdown::markdownToHTML(text = overview_md_text, fragment.only = TRUE))
  })
  ################################################################################################
  # test if .Net Framework is installed
  if (!dotnet_installed()) {
    showModal(modalDialog(
      title = "Missing .NET Framework",
      "This app requires Microsoft .NET Framework. Please install it from https://dotnet.microsoft.com/en-us/download/dotnet-framework",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }
  
  ################################################################################################
  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })
  
  # Render the initial map
  output$map <- renderLeaflet({
    
    intact_4326 <- intact %>% st_transform(4326)
    
    # Render initial map
    isolate({
      map <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
        fitBounds(lng1 = -121, lat1 = 44, lng2 = -65, lat2 = 78)%>%
        addMapPane(name = "ground", zIndex=380) %>%
        addMapPane(name = "over", zIndex=420) %>%
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>% 
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
        addProviderTiles("CartoDB.PositronNoLabels", group = "Blank Background") %>%
        addPolygons(data=intact_4326, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intact areas", options = leafletOptions(pane = "ground")) %>%
        addPolygons(data=bnd, color='grey', fill=F, weight=1, group="Canada extent", options = leafletOptions(pane = "ground")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                         overlayGroups = reactiveValsList$overlayGroups(),
                         options = layersControlOptions(collapsed = FALSE))
    })
  })
  
  myMap <- leafletProxy("map", session)
  
  #Control on tab
  modalServer(input, output, session, project, reactiveValsList)
  
  #Set input parameters
  setParamsServer(input, output, session, project, myMap, reactiveValsList)
  
  # Add display layers
  addDisplayServer(input, output, session, project, myMap, reactiveValsList)
  
  #Run BUILDER
  buildKBAServer(input, output, session, project, myMap, reactiveValsList)
  
  #Assess representation
  assessRepServer(input, output, session, project, myMap, reactiveValsList)
  
  #Build network
  buildNetServer(input, output, session, project, myMap, reactiveValsList)
  
  #Convert to shp
  convertServer(input, output, session, project, myMap, reactiveValsList)
  
}