addDisplayServer <- function(input, output, session, project, map, rv){
  
  ################################################################################################
  ################################################################################################
  # Add display elements
  ################################################################################################
  ################################################################################################
  # Display1
  display1_sf <- eventReactive(input$confExtra,{
    req(input$confExtra)  
    i <- NULL
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Uploading layers. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    if(input$extraupload == "extrashp"){
      if(!is.null(input$display1)){
        req(input$display1)
        i <- read_shp_from_upload(input$display1) %>%
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid()
        
        shp_file <- input$display1$name[grepl("\\.shp$", input$display1$name)][1]
        name <- tools::file_path_sans_ext(shp_file)
        rv$display1_name(name)
        
        geom_type <- unique(sf::st_geometry_type(i))
        if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
        } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTILINESTRING"))
        } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
          i <- suppressWarnings(sf::st_cast(i, "POINT"))
        }
      }
    } else if (input$extraupload == "extragpkg"){
      req(input$display4)
      req(input$display4a)
      if(input$display4a != "Select a layer"){
        i <- st_read(input$display4$datapath, layer = input$display4a, quiet = TRUE) 
        name <- substr(input$display4a, 1, 25)
        rv$display1_name(name)
      }
    }
    rv$layers_rv$display1_sf <- i
    return(i)
  })
  # display2
  display2_sf <- eventReactive(input$confExtra,{
    req(input$confExtra)  
    i <- NULL
    
    if(input$extraupload == "extrashp"){
      if(!is.null(input$display2)){
        req(input$display2)
        i <- read_shp_from_upload(input$display2) %>%
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid()
        
        shp_file <- input$display2$name[grepl("\\.shp$", input$display2$name)][1]
        name <- tools::file_path_sans_ext(shp_file)
        rv$display2_name(name)
        
        geom_type <- unique(sf::st_geometry_type(i))
        if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
        } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTILINESTRING"))
        } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
          i <- suppressWarnings(sf::st_cast(i, "POINT"))
        }
      }
    } else if (input$extraupload == "extragpkg"){
      req(input$display4)
      req(input$display4b)
      if(input$display4b != "Select a layer"){
        i <- st_read(input$display4$datapath, layer = input$display4b, quiet = TRUE)
        name <- substr(input$display4b, 1, 25)
        rv$display2_name(name)
      }
    }
    rv$layers_rv$display2_sf <- i
    return(i)
  })  
  # display3
  display3_sf <- eventReactive(input$confExtra,{
    req(input$confExtra)  
    i <- NULL
    
    if(input$extraupload == "extrashp"){
      if(!is.null(input$display3)){
        req(input$display3)
        i <- read_shp_from_upload(input$display3) %>%
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid()
        
        shp_file <- input$display3$name[grepl("\\.shp$", input$display3$name)][1]
        name <- tools::file_path_sans_ext(shp_file)
        rv$display3_name(name)
        
        geom_type <- unique(sf::st_geometry_type(i))
        if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
        } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTILINESTRING"))
        } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
          i <- suppressWarnings(sf::st_cast(i, "POINT"))
        }
      }
    } else if (input$extraupload == "extragpkg"){
      req(input$display4)
      req(input$display4c)
      if(input$display4c != "Select a layer"){
        i <- st_read(input$display4$datapath, layer = input$display4c, quiet = TRUE)
        name <- substr(input$display4c, 1, 25)
        rv$display3_name(name)
      }
    }
    rv$layers_rv$display3_sf <- i
    return(i)
  })  
  
  ####################################################################################################
  # Map viewer - display element
  ####################################################################################################
  observeEvent(input$confExtra,{ 
    map <- leafletProxy("map") %>%
      clearGroup(rv$display1_name()) %>%
      clearGroup(rv$display2_name()) %>%
      clearGroup(rv$display3_name())
    
    grps <- rv$overlayGroups()
    
    if (isMappable(display1_sf())) { 
      display1 <- st_transform(rv$layers_rv$display1_sf, 4326)
      geom_type <- unique(sf::st_geometry_type(display1))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map <- map %>% addPolygons(data=display1,  fillColor='#FF9933', stroke=F, fill = T, fillOpacity = 0.5, group=rv$display1_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map <- map %>% addPolylines(data = display1, color = '#FF9933', weight = 2, group = rv$display1_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map <- map %>% addCircleMarkers(data = display1, color = '#FF9933', radius = 5, fillOpacity = 0.7, group = rv$display1_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }
      grps <- c(grps,rv$display1_name())
    } 
    if (isMappable(display2_sf())) { 
      display2 <- st_transform(rv$layers_rv$display2_sf, 4326)
      geom_type <- unique(sf::st_geometry_type(display2))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map <- map %>% addPolygons(data=display2,  fillColor='#330066', stroke=F, fill = T, fillOpacity = 0.5, group=rv$display2_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map <- map %>% addPolylines(data = display2, color = '#330066', weight = 2, group = rv$display2_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map <- map %>% addCircleMarkers(data = display2, color = '#330066', radius = 5, fillOpacity = 0.7, group = rv$display2_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }
      grps <- c(grps,rv$display2_name())
    } 
    if (isMappable(display3_sf())) { 
      display3 <- st_transform(rv$layers_rv$display3_sf, 4326)
      geom_type <- unique(sf::st_geometry_type(display3))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map <- map %>% addPolygons(data=display3,  fillColor='#003333', stroke=F, fill = T, fillOpacity = 0.5, group=rv$display3_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map <- map %>% addPolylines(data = display3, color = '#003333', weight = 2, group = rv$display3_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map <- map %>% addCircleMarkers(data = display3, color = '#003333', radius = 5, fillOpacity = 0.7, group = rv$display3_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }
      grps <- c(grps,rv$display3_name())
    } 
    rv$overlayGroups(grps)
    
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c(rv$overlayGroups()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments"))
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Additional layers uploaded, please wait to have them displayed...", 
      easyClose = TRUE,
      footer = NULL)
    )
  }) 
}