ui = dashboardPage(skin="blue",
                   dashboardHeader(title = "KBA analysis"),
                   dashboardSidebar(
                     width = 275,
                     sidebarMenu(id = "tabs",
                                 menuItem("Overview", tabName = "overview", icon = icon("th")),
                                 menuItem("Explorer", tabName = "explorer", icon = icon("th"), startExpanded = TRUE,
                                          menuSubItem("Upload data", tabName = "tabUpload", icon = icon("th")),
                                          menuSubItem("Create Builder input", tabName = "tabinput", icon = icon("th")),                
                                          menuSubItem("Run Builder", tabName = "tabBuilder", icon = icon("th")),                
                                          menuSubItem("Assess representation", tabName = "tabDCI", icon = icon("th")),
                                          menuSubItem("Filter KBAs", tabName = "tabKBA", icon = icon("th")),
                                          menuSubItem("Create KBAs network", tabName = "tabNET", icon = icon("th"))
                                 ),
                                 menuItem("Download results", tabName = "download", icon = icon("th")),
                                 hr()
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabUpload'",
                       HTML("<h4>&nbsp; &nbsp; Upload spatial dataset</h4>"),
                       div(style = "margin-top: 0px;",fileInput(inputId = "upload_catch", label = "Catchments dataset", multiple = TRUE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_stream", label = "Streams dataset", multiple = TRUE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_planreg", label = "Planning region", multiple = TRUE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_lcc", label = "LCC", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_led", label = "LED", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_cmi", label = "CMI", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_gpp", label = "GPP", multiple = FALSE))
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabinput'",  
                       textInput("set_wd", "Specify output directory", value = "C:/temp/KBA"),
                       textInput("set_strahler", "Specify Strahler index to create seedlist", value = 1),
                       textInput("set_areatarget", "Specify area target for building conservation areas (sq.m)", value = 10000000000),
                       div(style = "margin-top: -20px;",fileInput(inputId = "upload_seed", label = "Use test seedlist", multiple = FALSE)),
                       actionButton("runBuilderInput", "Create Builder input", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:200px"),
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabBuilder'", 
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "Set catchment parameters "),
                       div(style = "margin-top: -20px;", textInput("catchintact", label = div(style = "font-size:13px;", "Specify catchments intactness (0-1)"), value = "1")),
                       div(style = "margin-top: -20px;",selectInput("intactColname", label = div(style = "font-size:13px;margin-top: -10px;", "Select intactness attribute"), choices = "IntactPB")),
                       div(style = "margin-top: -20px;",textInput("CAintact", label = div(style = "font-size:13px;", "Specify CAs intactness (0-1)"), value = "1")),
                       div(style = "margin-top: -20px;",selectInput("zoneColname", label = div(style = "font-size:13px;", "Select zone attribute"), choices = c("MDAzone", "ecoMDAzone", "ecoZone"), selected = "ecoMDAzone")),
                       div(style = "margin-top: -20px;",selectInput("areatypeColname", label = div(style = "font-size:13px;margin: 0px;", "Select area_type attribute"), choices = c("landwater", "land", "water"), selected = "landwater")),
                       div(style = "margin-top: -20px;",selectInput("arealandColname", label = div(style = "font-size:13px;margin: 0px;", "Select area_land attribute"), choices = "Area_land")),
                       actionButton("runBuilder", "Run builder", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:200px")                     
                       ),
                     conditionalPanel(
                       condition="input.tabs=='tabDCI'",
                       textInput("set_grid", "1. Set grid cell size", value = 10000),
                       actionButton(inputId = "calc_dci", label = div(style = "font-size:13px;background-color:gey;color: black",HTML("2. Calculate hydrology metrics")), class = "btn-warning", style="width:250px"),
                       tags$br(),
                       actionButton(inputId = "reduce_KBAs", label = div(style = "font-size:13px;background-color:gey;color: black",HTML("3. Reduce de number of KBAs")), class = "btn-warning", style="width:250px"),
                       tags$br(),
                       actionButton("runRep", "4. Run representation analysis", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                       tags$br()#,
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabKBA'",
                       div("Filter KBAs based on criteria dissimilarity metrics (DMS)", style = "font-size: 14px;font-weight: bold; margin-top : 20px; margin-left : 20px; "),
                       div("DMs range from 0 (low dissimilarity) to 1 (high dissimilarity)", style = "font-size: 12px; margin-top : 20px; margin-left : 20px; "),
                       #HTML("<h4>&nbsp; &nbsp; Filter criteria based on dissimilarity metrics</h4>"),
                       div(style = "margin-top: 0px;",sliderInput("slideCMI", label="CMI:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideLED", label="LED:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideGPP", label="GPP:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideLCC", label="LCC:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideUP", label="Upstream area (sq.km):", min=0, max=150000, value = 5000, step=5000, ticks=FALSE)),
                       actionButton("filterRep", "Apply dissimilarity metrics filtering", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabNET'",
                       div("Build network", style = "font-size: 14px;font-weight: bold; margin-top : 20px; margin-left : 20px; "),
                       textInput("set_net", "Set numbers of KBAs per network", value = 2),
                       div(style = "margin-top: -30px;",checkboxInput("forceKBA", label = "Force KBA filtering in the network", value = F)),
                       actionButton("buildNet", "Build network", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                       
                     ),
                     conditionalPanel(
                       condition="input.tabs=='download'",
                       div("Download resulting KBAs", style = "font-size: 14px;font-weight: bold; margin-top : 20px; margin-left : 20px; "),
                       checkboxInput("filteredKBAs", label = "Download filtered KBAs", value = T),
                       checkboxInput("networkKBAs", label = "Download KBAs networks", value = T),
                       tags$style(type="text/css", "#downloadData {background-color:gey;color: black}"),
                       div(style="position:relative; left:calc(10%);", downloadButton("downloadData", "Download results"))
                     )
                   ),     
                   dashboardBody(
                     useShinyjs(),
                     tags$head(tags$style(".skin-blue .sidebar a { color: #8a8a8a; }")),
                     
                     tabItems(
                       # Overview tab: three tabPanels
                       tabItem(tabName = "overview",
                               fluidRow(
                                 tabBox(id = "one", width = 8,
                                        tabPanel(HTML("Overview"), includeMarkdown("docs/overview.md")),
                                        tabPanel(HTML("Quick start"), includeMarkdown("docs/quick_start.md")),
                                        tabPanel(HTML("Dataset"), includeMarkdown("docs/datasets.md"))
                                 )
                               )
                       ),
                       
                       # Explorer tab: two tabBoxes
                       tabItem(tabName = "tabUpload",
                               fluidRow(
                                 # Mapview for multiple tabs
                                  condition = "input.tabs == 'tabUpload' || input.tabs == 'tabinput' || input.tabs == 'tabBuilder' || input.tabs == 'tabDCI'",
                                  tabBox(id = "mapBox", width = 10,
                                          tabPanel(HTML("<b>Mapview</b>"),
                                                   leafletOutput("map", height = 750) %>% withSpinner(),
                                                   fluidRow(uiOutput("images"))  # Placeholder for images below the map
                                          ),
                                          tabPanel("Guidance",
                                                 # Dynamically update the content of Guidance based on selected tab
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabUpload'",
                                                   includeMarkdown("./Rmd/upload_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabinput'",
                                                   includeMarkdown("./Rmd/input_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabBuilder'",
                                                   includeMarkdown("./Rmd/builder_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabDCI'",
                                                   includeMarkdown("./Rmd/representation_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabKBA'",
                                                   includeMarkdown("./Rmd/filterKBA_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabNET'",
                                                   includeMarkdown("./Rmd/buildNet_doc.md")
                                                 )
                                        )
                                 ),
                                 
                                 #  Tabset panel for DCI
                                # conditionalPanel(
                                #     condition = "input.tabs == 'tabDCI'",
                                #     tabBox(id = "metricsBox", width = 4,
                                #            tabsetPanel(id = "tabset1",
                                #                        tabPanel(HTML("<h4>Potential KBA metrics</h4>"), selectInput("KBA", label="Select KBAs:", choices=c("CMI","CMI2055","GPP","LED","LANDCOVER"),selected="CMI"),
                                #                                 tableOutput("tab1", height=480))
                                #            )
                                #     )
                                #   )
                                 conditionalPanel(
                                   condition = "input.tabs == 'tabKBA'",
                                   tabBox(id = "metricsBox", width = 2,
                                          tabsetPanel(id = "tabset1",
                                                      tabPanel(HTML("<h4>Potential KBA metrics</h4>"), 
                                                               selectInput("KBA", label = "Select KBAs:", choices = NULL),  # Initially empty, updated dynamically
                                                               tableOutput("tab1")
                                                      )
                                          )
                                   )
                                 ),
                                conditionalPanel(
                                  condition = "input.tabs == 'tabNET'",
                                  tabBox(id = "metricsNET", width = 2,
                                         tabsetPanel(id = "tabsetNET",
                                                     tabPanel(HTML("<h4>Potential KBA network metrics</h4>"), 
                                                              selectInput("network", label = "Select network:", choices = NULL),  # Initially empty, updated dynamically
                                                              tableOutput("tabnet")
                                                     )
                                         )
                                  )
                                )
                               )
                       )
                     )
                   )
)