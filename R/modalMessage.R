modalServer <- function(input, output, session, project){
  
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
}  
  
