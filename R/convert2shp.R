convertServer <- function(input, output, session, project, map, rv){
  
  observeEvent(input$dwdSHP,{
    
    if(file.exists(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))){
      showModal(modalDialog(
        title = "Converting output data into shapefile",
        "Please wait",
        easyClose = TRUE,
        footer = NULL
      ))

      out_path <- file.path(rv$outdir(), "output")
      layers_info <- st_layers(file.path(rv$outdir(), "output/KBA_analysis.gpkg"))
      layers <- layers_info$name
      
      for(l in layers){
        i <- st_read(dsn = file.path(out_path, "KBA_analysis.gpkg"), layer = l)
        l_name <- str_replace_all(l, " ", "_")
        st_write(i, dsn = file.path(out_path, paste0(l_name, ".shp")), driver = "ESRI Shapefile", append = FALSE)
      }
      showModal(modalDialog(
        title = "Data converted",
        "Data are now accessible as shapefile in the output folder. ",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else {
      #Test on required layers
      showModal(modalDialog(
        title = "Missing Data",
        "No data were yet created. Please run the analysis prior to download data into shapefile. ",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
  })

}