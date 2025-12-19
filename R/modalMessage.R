modalServer <- function(input, output, session, project, rv){
  
  # Observe tab changes
  observeEvent(input$tabs, {

    if (input$tabs != "tabUpload" && is.null(input$confirm_project) && input$tabs != "overview") {
      # Show modal message if tabUpload has not been visited
      showModal(modalDialog(
        title = "Action Required",
        "Please visit the 'Set input parameters' tab to initialize the map and upload the required dataset before proceeding to the next steps.",
        easyClose = TRUE,
        footer = modalButton("Go to Set input parameters")
      ))
      
      # Redirect user back to tabUpload
      updateTabItems(session = getDefaultReactiveDomain(), "tabs", "tabUpload")
    }
  })
  
  # Observe tab changes
  observeEvent(input$tabs, {
    
    if (input$tabs != "tabUpload" && !is.null(input$confirm_project) && input$tabs != "overview") {
      req(rv$layers_rv$catchments)
      
      if(input$intactColname == "Please select"){
        showModal(modalDialog(
          title = "Missing intactness column",
          "You must specify the column in your catchment layer that represents intactness. Please go back to the Set input parameters section",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        # Redirect user back to tabUpload
        updateTabItems(session = getDefaultReactiveDomain(), "tabs", "tabUpload")
      }
    } 
  })
}  




