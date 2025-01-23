ui = dashboardPage(skin="black",
                   dashboardHeader(title = tags$div(
                     tags$img(
                       src = "logoblanc.png",  # Replace with your logo file name
                       height = "50px",   # Adjust the height of the logo
                       style = "margin-right: 10px;"  # Add some spacing around the logo
                     ),"BEACONs KBA Analysis"), titleWidth = 400,
                     # Add Reload Button Next to Sidebar Toggle
                     tags$li(
                       class = "dropdown",
                       actionButton(
                         "reload_btn",
                         label = "Reload",
                         icon = icon("refresh"),
                         style = "color: black; background-color: orange; border: none; font-size: 16px;"
                       ),
                       style = "position: absolute; left: 50px; top: 10px;"  # Adjust margin for placement next to the toggle
                     ),
                     tags$li(
                       class = "dropdown",  # Required for dropdown functionality
                       dropdownMenu(
                         type = "tasks", 
                         badgeStatus = NULL,
                         icon = icon("life-ring"),  # Life-ring icon triggering dropdown
                         headerText = "",  # No header text in dropdown
                         menuItem("Website", href = "https://beaconsproject.ualberta.ca/", icon = icon("globe")),
                         menuItem("GitHub", href = "https://github.com/beaconsproject/", icon = icon("github")),
                         menuItem("Contact us", href = "mailto: beacons@ualberta.ca", icon = icon("address-book"))
                       ),
                       # Plain Text "About Us" Positioned Next to Dropdown
                       tags$span(
                         "About Us", 
                         style = "font-size: 16px; position: relative; top: 15px; right: 10px; white-space: nowrap; color: white;"
                       )
                     )
                   ),
                   dashboardSidebar(
                     width = 275,
                     sidebarMenu(id = "tabs",
                                 menuItem("Overview", tabName = "overview", icon = icon("th")),
                                 menuItem("Set input parameters", tabName = "tabUpload", icon = icon("th"), startExpanded = FALSE),
                                 menuItem("Build KBAs (optional)", tabName = "build_kbas", icon = icon(name = "fas fa-tools", lib = "font-awesome"), startExpanded = FALSE,
                                          #menuSubItem("Step 1: Set input parameters", tabName = "tabUpload", icon = icon("th")),
                                          menuSubItem("Create Builder input", tabName = "tabinput", icon = icon("th")),                
                                          menuSubItem("Run Builder", tabName = "tabBuilder", icon = icon(name = "fas fa-play", lib = "font-awesome")),                
                                          menuSubItem("Calculate hydrology metrics (KBAs)", tabName = "tabDCI", icon = icon(name = "fas fa-plus-circle", lib = "font-awesome"))
                                          ),
                                 menuItem("Evaluate PAs (optional)", tabName = "load_pas", icon = icon(name = "fas fa-search", lib = "font-awesome"), startExpanded = FALSE,
                                          menuSubItem("Calculate hydrology metrics (PAs)", tabName = "tabPAs", icon = icon(name = "fas fa-plus-circle", lib = "font-awesome"))
                                          ),
                                 menuItem("Assess representation", tabName = "assess", icon = icon(name = "fas fa-compass", lib = "font-awesome"), startExpanded = FALSE,
                                          menuSubItem(HTML('<span style="display: inline-block; vertical-align: top; margin-left: 5px;">Upload reference area<br>and assess representation</span>'), tabName = "tabKBA", icon = icon(name = "fas fa-map", lib = "font-awesome")),
                                          menuSubItem("Create KBA Networks", tabName = "tabNET", icon = icon(name = "fas fa-project-diagram", lib = "font-awesome"))#,
                                          #menuSubItem("Download Filtered Networks", tabName = "download", icon = icon(name = "fas fa-download", lib = "font-awesome"))
                                          ),
                                 hr()
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabUpload'",
                       shinyDirButton("directory", "Select output Directory", "Please select a folder", icon = icon(name = "fas fa-folder", lib = "font-awesome")),
                       div(style = "width: 250px;margin-left: 20px;", verbatimTextOutput("dirpath")), 
                       actionButton("set_wd", "Confirm", icon = icon(name = "check", lib = "font-awesome"), class = "btn-warning", style="width:200px"),
                       tags$br(),
                       HTML("<h4>&nbsp; &nbsp; Upload spatial dataset</h4>"),
                       # File input to upload CSV
                       fileInput("csv_file", "Use csv with file pathways", accept = ".csv"),
                       div(style = "margin: 15px; margin-top: -10px; font-size:13px;font-weight: bold", "  --  Or upload individual layer --"),
                       div(style = "margin-top: -20px;",fileInput(inputId = "upload_catch", label = "Catchments dataset", multiple = TRUE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_stream", label = "Streams dataset", multiple = TRUE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_planreg", label = "Planning region", multiple = TRUE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_lcc", label = "LCC", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_led", label = "LED", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_cmi", label = "CMI", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_gpp", label = "GPP", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_custom", label = "Custom criteria", multiple = FALSE)),
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabinput'",  
                       div(style = "margin: 15px; font-size:15px; font-weight: bold", "Use existing files"),
                       div(style = "margin-top: -10px;",fileInput(inputId = "upload_seed", label = NULL, placeholder  = "Upload seedlist .csv", multiple = FALSE)),
                       div(style = "margin-top: -10px;",fileInput(inputId = "upload_nghbr", label = NULL,  placeholder = "Upload neighbours .csv", multiple = FALSE)),
                       div(style = "margin: 15px; font-size:15px; font-weight: bold", "-- Or"),
                       div(style = "margin: 15px; font-size:15px; font-weight: bold", "Create seedlist (if required)"),
                       div(style = "margin-top: -20px;", selectInput("intactseedColname", label = div(style = "font-size:13px;margin-top: -10px;", "Specify intactness attribute"), choices = "intactKBA")),
                       div(style = "margin-top: -20px;", textInput("seedintact", label = div(style = "font-size:13px;", "Specify minimum seed intactness (0-1)"), value = "0")),
                       div(style = "margin-top: -20px;", textInput("set_strahler", label = div(style = "font-size:13px;","Specify Strahler Order ≤"), value = 1)),
                       div(style = "margin-top: -20px;", textInput("set_areatarget", label = div(style = "font-size:13px;","Specify area target (m2)"), value = 10000000000)),
                       actionButton("runBuilderInput", "Run Builder input", icon = icon(name = "file-csv", lib = "font-awesome"), class = "btn-warning", style="width:200px"),
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabBuilder'", 
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "Specify minimum intactness (0-1) "),
                       div(style = "margin-top: -20px;", textInput("catchintact", label = div(style = "font-size:13px;", "--catchment-level"), value = "0")),
                       div(style = "margin-top: -20px;",textInput("CAintact", label = div(style = "font-size:13px;", "--KBAs-level"), value = "0")),
                       div(style = "margin-top: -20px;",selectInput("areatypeColname", label = div(style = "font-size:13px;margin: 0px;", "Specify area target type for KBA size"), choices = c("landwater", "land", "water"), selected = "landwater")),
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "Specify catchment attributes "),
                       div(style = "margin-top: -20px;",selectInput("intactColname", label = div(style = "font-size:13px;margin-top: -10px;", "--intactness"), choices = "intactKBA")),
                       div(style = "margin-top: -20px;",selectInput("zoneColname", label = div(style = "font-size:13px;", "--zone"), choices = c("MDAzone", "ecoMDAzone", "ecoZone"), selected = "ecoMDAzone")),
                       div(style = "margin-top: -20px;",selectInput("arealandColname", label = div(style = "font-size:13px;margin: 0px;", "--area land"), choices = "Area_land")),
                       actionButton("runBuilder", "Run Builder", icon = icon(name = "file-csv", lib = "font-awesome"), class = "btn-warning", style="width:200px")                     
                       ),
                     conditionalPanel(
                       condition="input.tabs=='tabDCI'",
                       div(style = "margin: 14px; font-size:15px; font-weight: bold", "1. Hydrology Metrics "),
                       div(style = "margin: 13px; font-size:13px; font-weight: bold", "Calculate DCI and add upstream attributes to KBAs"), 
                       actionButton(inputId = "calc_dci", label = div(style = "font-size:13px;background-color:gey;color: black",HTML("Run")), class = "btn-warning", style="width:250px"),
                       tags$br(),
                       div(style = "margin: 14px; font-size:15px; font-weight: bold", "2. Reduce number of KBAs "),
                       div(style = "margin-top: -20px;",textInput("set_grid", label = div(style = "font-size:13px;margin: 0px;", "Specify grid cell size"), value = 10000)),
                       actionButton(inputId = "reduce_KBAs", label = div(style = "font-size:13px;background-color:gey;color: black",HTML("Run")), class = "btn-warning", style="width:250px")
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabPAs'",
                       fileInput(inputId = "upload_pas", label = "Upload protected areas shapefile", multiple = TRUE),
                       div(style = "margin-top: -20px;",selectInput("intactColpas", label = div(style = "font-size:13px;margin-top: -10px;", "Specify intactness attribute"), choices = "intactKBA")),
                       div(style = "margin: 13px; font-size:13px; font-weight: bold", "Calculate DCI and add upstream attributes to KBAs"), 
                       actionButton(inputId = "calc_pasdci", label = div(style = "font-size:13px;background-color:gey;color: black",HTML("Run")), class = "btn-warning", style="width:250px")                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabKBA'",
                       fileInput(inputId = "upload_refarea", label = "Upload reference area shapefile", multiple = TRUE),
                       div(style = "margin-top: 0px;",radioButtons("assessKBAs", "Assess representation using:", choices = c("Only KBAs", "Only PAs","Both KBAs and PAs"),
                                                                   selected = NULL)),
                       actionButton("runRep", "Run representation analysis", icon = icon(name = "image", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                       div("Filter KBAs and/or PAs based on dissimilarity metrics (DMs) and upstream area", style = "font-size: 14px;font-weight: bold; margin-top : 20px; margin-left : 20px; "),
                       div("DMs range from 0 to 1. 0 = low dissimilarity or high representation, 1 = high dissimilarity or low representation", style = "font-size: 12px; margin-top : 20px; margin-left : 20px; "),
                       div(style = "margin-top: 0px;",sliderInput("slideCMI", label="CMI:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideLED", label="LED:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideGPP", label="GPP:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideLCC", label="LCC:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       uiOutput("slidercrit5"),  # Dynamic UI for slidecrit5
                       div(style = "margin-top: -30px;",sliderInput("slideUP", label="Maximum upstream area (sq.km):", min=0, max=100000, value = 25000, step=1000, ticks=FALSE)),
                       actionButton("filterRep", "Apply filtering", icon = icon(name = "filter", lib = "font-awesome"), class = "btn-primary", style="width:250px"),
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabNET'",
                       div(style = "margin-top: -20px;", textInput("set_net", "Set numbers of KBAs per network", value = 0)),
                       div(style = "margin-top: -30px;", checkboxInput("forceKBA", label = "Apply KBA filtering in the network", value = F)),
                       div(style = "margin-top: -30px;",checkboxInput("forcePAs", label = "Force PAs in the network", value = F)),
                       div(style = "margin-top: -20px;",selectInput("intactColNET", label = div(style = "font-size:13px;margin-top: -10px;", "Specify intactness attribute"), choices = "intactKBA")),
                       actionButton("buildNet", "Build network", icon = icon(name = "link", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                       div(style = "margin: 13px; margin-top: 20px; font-size:14px; font-weight: bold", "Filter Networks"), 
                       div(style = "margin-top: -20px;",sliderInput("slideNETCMI", label="CMI:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideNETLED", label="LED:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideNETGPP", label="GPP:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideNETLCC", label="LCC:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       uiOutput("slideNETcrit5"),  # Dynamic UI for slidecrit5
                       div(style = "margin-top: -30px;",sliderInput("slideNETUP", label="Maximum upstream area (sq.km):", min=0, max=100000, value = 25000, step=1000, ticks=FALSE)),
                       actionButton("filterNet", "Apply filtering", icon = icon(name = "filter", lib = "font-awesome"), class = "btn-primary", style="width:250px"),
                       div(style = "margin-top: 20px;",actionButton("downloadData", "Download Filtered Networks", icon = icon(name = "fas fa-download", lib = "font-awesome"), class = "btn-warning", style="width:250px"))
                       
                     )#,
                     #conditionalPanel(
                     #  condition="input.tabs=='download'",
                     #  actionButton("downloadData", "Download Filtered Networks", icon = icon(name = "filter", lib = "font-awesome"), class = "btn-warning", style="width:250px")
                     #)
                   ),     
                   dashboardBody(
                     useShinyjs(),
                     tags$head(
                       # Link to custom CSS for the orange theme
                       tags$link(rel = "stylesheet", type = "text/css", href = "green-theme.css")
                     ),
                     tabItems(
                       # Overview tab: three tabPanels
                       tabItem(tabName = "overview",
                               fluidRow(
                                 tabBox(id = "one", width = 8,
                                        tabPanel(HTML("Overview"), includeMarkdown("docs/overview.md")),
                                        tabPanel(HTML("User guide"), includeMarkdown("docs/user_guide.md")),
                                        tabPanel(HTML("Dataset"), includeMarkdown("docs/datasets.md"))
                                 )
                               )
                       ),
                       
                       # Explorer tab: two tabBoxes
                       tabItem(tabName = "tabUpload",
                               fluidRow(
                                  # Mapview for multiple tabs
                                  tabBox(id = "mapBox", width = 10,
                                          tabPanel("Mapview",
                                                   leafletOutput("map", height = 750) %>% withSpinner(),
                                                   conditionalPanel(
                                                     condition = "input.tabs == 'tabPAs'",
                                                     dataTableOutput("pastbl")  # Use tableOutput for basic table
                                                   ),
                                                   fluidRow(uiOutput("images"))  # Placeholder for images below the map
                                          ),
                                          tabPanel("Guidance",
                                                 # Dynamically update the content of Guidance based on selected tab
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabUpload'",
                                                   includeMarkdown("./Rmd/setParams_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabinput'",
                                                   includeMarkdown("./Rmd/builderInput_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabBuilder'",
                                                   includeMarkdown("./Rmd/runBuilder_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabDCI'",
                                                   includeMarkdown("./Rmd/KBAmetrics_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabPAs'",
                                                   includeMarkdown("./Rmd/PAmetrics_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabKBA'",
                                                   includeMarkdown("./Rmd/assessRep_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabNET'",
                                                   includeMarkdown("./Rmd/createNet_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'download'",
                                                   includeMarkdown("./Rmd/dwd_doc.md")
                                                 )
                                        )
                                 ),
                                 conditionalPanel(
                                   #condition = "input.tabs == 'tabKBA'",  # Updated condition
                                   condition = "input.tabs == 'tabKBA' && input.mapBox === 'Mapview'", 
                                   tabBox(id = "metricsBox", width = 2,
                                          tabsetPanel(id = "tabset1",
                                                      tabPanel(HTML("<h4>Number of potential KBAs and protected areas</h4>"), 
                                                               tableOutput("outkbafreq"),
                                                               selectInput("KBA", label = "Select KBAs/PAs:", choices = NULL),  # Initially empty, updated dynamically
                                                               tableOutput("outkba")
                                                      )
                                          )
                                   )
                                 ),
                                conditionalPanel(
                                  condition = "input.tabs == 'tabNET' && input.mapBox === 'Mapview'",  # Updated condition
                                  tabBox(id = "metricsNET", width = 2,
                                         tabsetPanel(id = "tabsetNET",
                                                     tabPanel(HTML("<h4>Number of potential KBA network</h4>"), 
                                                              tableOutput("outnetfreq"),
                                                              selectInput("network", label = "Select network:", choices = NULL),  # Initially empty, updated dynamically
                                                              tableOutput("outnet")
                                                     )
                                         )
                                  )
                                )
                               )
                       )
                     )
                   )
)