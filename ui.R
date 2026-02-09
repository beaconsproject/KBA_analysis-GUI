ui = dashboardPage(skin="black",
                   dashboardHeader(title = tags$div(
                     tags$img(
                       src = "logoblanc.png",  # Replace with your logo file name
                       height = "50px",   # Adjust the height of the logo
                       style = "margin-right: 10px;"  # Add some spacing around the logo
                     ),"BEACONs KBA Explorer"), titleWidth = 400,
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
                         shinydashboard::menuItem("Website", href = "https://beaconsproject.ualberta.ca/", icon = icon("globe")),
                         shinydashboard::menuItem("GitHub", href = "https://github.com/beaconsproject/", icon = icon("github")),
                         shinydashboard::menuItem("Contact us", href = "mailto: beacons@ualberta.ca", icon = icon("address-book"))
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
                                 shinydashboard::menuItem("Overview", tabName = "overview", icon = icon("th")),
                                 shinydashboard::menuItem("Set input parameters", tabName = "tabUpload", icon = icon("th"), startExpanded = FALSE),
                                 shinydashboard::menuItem("Add display elements (OPTIONAL)", tabName = "addLayers", icon = icon(name = "fas fa-plus", lib = "font-awesome")),
                                 shinydashboard::menuItem("Build KBAs (optional)", tabName = "build_kbas", icon = icon(name = "fas fa-tools", lib = "font-awesome"), startExpanded = FALSE,
                                                          menuSubItem("Create Builder input", tabName = "tabinput", icon = icon("th")),                
                                                          menuSubItem("Run Builder and calculate DCI", tabName = "tabBuilder", icon = icon(name = "fas fa-play", lib = "font-awesome")),                
                                                          menuSubItem("Reduce KBAs (OPTIONAL)", tabName = "tabKBAs", icon = icon(name = "fas fa-plus-circle", lib = "font-awesome"))
                                 ),
                                 shinydashboard::menuItem("Assess representation", tabName = "assess", icon = icon(name = "fas fa-compass", lib = "font-awesome"), startExpanded = FALSE,
                                                          menuSubItem(HTML('<span style="display: inline-block; vertical-align: top; margin-left: 5px;">Assess single KBAs (optional)</span>'), tabName = "tabKBA", icon = icon(name = "fas fa-map", lib = "font-awesome")),
                                                          menuSubItem("Create and assess KBA networks", tabName = "tabNET", icon = icon(name = "fas fa-project-diagram", lib = "font-awesome"))#,
                                                          #menuSubItem("Download Filtered Networks", tabName = "download", icon = icon(name = "fas fa-download", lib = "font-awesome"))
                                 ),
                                 shinydashboard::menuItem("Convert as Shapefiles (OPTIONAL)", tabName = "convert", icon = icon(name = "fas fa-download", lib = "font-awesome")),
                                 hr()
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabUpload'",
                       shinyDirButton("directory", "Select output directory", "Please select a folder", icon = icon(name = "fas fa-folder", lib = "font-awesome")),
                       div(style = "width: 250px;margin-left: 20px;", verbatimTextOutput("dirpath")), 
                       actionButton("set_wd", "Confirm", class = "btn-warning", style="width:200px"),
                       tags$br(),
                       uiOutput("project_ui"), 
                       uiOutput("newproject_ui"),
                       uiOutput("intactCol_ui"),
                       uiOutput("pas_ui")
                     ),
                     # EXTRA LAYERS
                     conditionalPanel(
                       condition="input.tabs=='addLayers'",
                       radioButtons("extraupload", "Select source for extra layers to be displayed:",
                                    choices = list("Shapefile" = "extrashp", 
                                                   "GeoPackage" = "extragpkg"),
                                    selected = character(0), 
                                    inline = TRUE)
                     ),
                     conditionalPanel(
                       condition = "input.tabs=='addLayers' && input.extraupload == 'extrashp'",
                       div(style = "margin-top: -10px;",fileInput(inputId = "display1",   label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#663300; margin-right:8px; border:1px solid #000;"></span>Select layer 1'),
                                                                  multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'), placeholder = "Select a ShapeFile")),
                       div(style = "margin-top: -30px;",fileInput(inputId = "display2", label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#330066; margin-right:8px; border:1px solid #000;"></span>Select layer 2'),
                                                                  multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'), placeholder = "Select a ShapeFile")),
                       div(style = "margin-top: -30px;",fileInput(inputId = "display3", label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#003333; margin-right:8px; border:1px solid #000;"></span>Select layer 3'),
                                                                  multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'), placeholder = "Select a ShapeFile"))
                     ),
                     conditionalPanel(
                       condition = "input.tabs=='addLayers' && input.extraupload == 'extragpkg'",
                       fileInput(inputId = "display4", label = HTML("<h5><b>OPTIONAL - </b>Upload a GeoPackage that contains layers to be displayed on the map.</h5>"),
                                 multiple = FALSE, accept = ".gpkg", placeholder = "Select a GeoPackage"),
                       div(style = "margin-top: -10px;", selectInput("display4a", label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#663300; margin-right:8px; border:1px solid #000;"></span>Select layer 1'), choices = NULL)),
                       div(style = "margin-top: -20px;", selectInput("display4b", label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#330066; margin-right:8px; border:1px solid #000;"></span>Select layer 2'), choices = NULL)),
                       div(style = "margin-top: -20px;", selectInput("display4c", label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#003333; margin-right:8px; border:1px solid #000;"></span>Select layer 3'), choices = NULL))
                     ),
                     conditionalPanel(
                       condition = "input.tabs == 'addLayers'",
                       br(),
                       hr(),
                       br(),
                       actionButton("confExtra", "Confirm", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:250px")
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabinput'",  
                       div(style = "margin: 15px; font-size:15px; font-weight: bold", "Use existing files"),
                       div(style = "margin-top: -10px;",fileInput(inputId = "upload_seed", label = NULL, placeholder  = "Upload seedlist .csv", multiple = FALSE)),
                       div(style = "margin-top: -10px;",fileInput(inputId = "upload_nghbr", label = NULL,  placeholder = "Upload neighbours .csv", multiple = FALSE)),
                       div(style = "margin: 15px; font-size:15px; font-weight: bold", "-- Or"),
                       div(style = "margin: 15px; font-size:15px; font-weight: bold", "Create seedlist (if required)"),
                       div(style = "margin-top: -20px;", textInput("seedintact", label = div(style = "font-size:13px;", "Specify minimum seed intactness (0-1)"), value = "0")),
                       div(style = "margin-top: -20px;", textInput("set_strahler", label = div(style = "font-size:13px;","Specify Strahler Order ≤"), value = 1)),
                       div(style = "margin-top: -20px;", textInput("set_areatarget", label = div(style = "font-size:13px;","Specify area target (m2)"), value = 10000000000)),
                       tags$br(),
                       div(style = "margin-top: -30px;",checkboxInput("seedRefARea", label = "Constrain seeds to the reference area", value = F)),
                       uiOutput("forceseed"), 
                       tags$br(),
                       tags$hr(),
                       actionButton("runBuilderInput", "Set Builder input", icon = icon(name = "play", lib = "font-awesome"), class = "btn-warning", style="width:200px"),
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabBuilder'", 
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "Specify minimum intactness (0-1) "),
                       div(style = "margin-top: -20px;", textInput("catchintact", label = div(style = "font-size:13px;", "--catchment-level"), value = "0")),
                       div(style = "margin-top: -20px;",textInput("CAintact", label = div(style = "font-size:13px;", "--KBAs-level"), value = "0")),
                       div(style = "margin-top: -20px;",selectInput("areatypeColname", label = div(style = "font-size:13px;margin: 0px;", "Specify area target type for KBA size"), choices = c("landwater", "land", "water"), selected = "landwater")),
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "Specify catchment attributes "),
                       div(style = "margin-top: -20px;",selectInput("zoneColname", label = div(style = "font-size:13px;", "--zone"), choices = c("MDAzone", "ecoMDAzone", "ZONE"), selected = "ZONE")),
                       div(style = "margin-top: -20px;",selectInput("arealandColname", label = div(style = "font-size:13px;margin: 0px;", "--area land"), choices = "Area_land")),
                       tags$br(),
                       tags$br(),
                       tags$hr(),
                       actionButton("runBuilder", "Run Builder", icon = icon(name = "play", lib = "font-awesome"), class = "btn-warning", style="width:200px")                     
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabKBAs'",
                       div(style = "margin: 14px; font-size:15px; font-weight: bold", "Reduce number of KBAs"),
                       div(style = "margin: 12px; font-size:15px;", "KBAs are reduced based on spatial similarity within a user-defined grid, favoring KBAs with the strongest hydrological properties. "),
                       div(style = "margin-top: -20px;",textInput("set_grid", label = div(style = "font-size:13px;margin: 0px;", "Specify grid cell size"), value = 10000)),
                       actionButton(inputId = "reduce_KBAs", label = div(style = "font-size:13px;background-color:gey;color: black",HTML("Run")), class = "btn-warning", style="width:250px"),
                       tags$br(),
                       tags$br(),
                       actionButton(inputId = "save_reduce", label = div(style = "font-size:13px;background-color:gey;color: black",HTML("Save reduced KBAs in GPKG")), class = "btn-warning", style="width:250px")
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabPAs'",
                       uiOutput("dciPAs")
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabKBA'",
                       selectInput("KBAlayer", "Select potential KBAs layer", choices = "No KBA generated", multiple = FALSE),
                       uiOutput("assessRep"),
                       uiOutput("filterRep")
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabNET'",
                       pickerInput("KBArep", "Select KBA Layer", 
                                   choices = NULL, multiple = FALSE, options = list(
                                     `live-search` = TRUE,
                                     `style` = "btn-default",
                                     `size` = 5)),
                       #div(style = "margin-top: -20px;", selectInput("KBArep", "Select KBA layer", choices = NULL, multiple = FALSE)),
                       numericInput(inputId = "set_net", label   = "Set number of potential KBAs per network", value = 2, min = 2, step = 1),
                       #uiOutput("netPAs"),
                       div(style = "margin-top: -30px;",checkboxInput("forcePAs", label = "Include all PAs in the network", value = F)),
                       actionButton("buildNet", "Build network", icon = icon(name = "link", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                       div(style = "margin: 13px; margin-top: 20px; font-size:14px; font-weight: bold", "Filter Networks"), 
                       div(style = "margin-top: -20px;",sliderInput("slideNETCMI", label="CMI:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideNETLED", label="LED:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideNETGPP", label="GPP:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideNETLCC", label="LCC:", min=0, max=1, value = 0.2, step=0.001, ticks=FALSE)),
                       uiOutput("slideNETcrit5"),  # Dynamic UI for slidecrit5
                       div(style = "margin-top: -30px;",sliderInput("slideNETUP", label="Maximum upstream area (sq.km):", min=0, max=100000, value = 25000, step=1000, ticks=FALSE)),
                       actionButton("filterNet", "Apply filtering", icon = icon(name = "filter", lib = "font-awesome"), class = "btn-primary", style="width:250px"),
                       div(style = "margin-top: 20px;",actionButton("downloadNET", "Download Filtered Networks", icon = icon(name = "fas fa-download", lib = "font-awesome"), class = "btn-warning", style="width:250px"))
                     ),
                     conditionalPanel(
                       condition="input.tabs=='convert'",
                       actionButton("dwdSHP", "Convert GPKG layers as Shapefiles", class = "btn-warning", style="width:250px")
                     )
                   ),     
                   dashboardBody(
                     useShinyjs(),
                     tags$head(
                       # Link to custom CSS for the orange theme
                       tags$link(rel = "stylesheet", type = "text/css", href = "green-theme.css"),
                       tags$script(HTML("
  // Disable PA choices
  Shiny.addCustomMessageHandler('disablePAchoices', function(message) {
    $('input[value=\"Only PAs\"]').prop('disabled', true);
    $('input[value=\"Both KBAs and PAs\"]').prop('disabled', true);
  });

  // Enable PA choices
  Shiny.addCustomMessageHandler('enablePAchoices', function(message) {
    $('input[value=\"Only PAs\"]').prop('disabled', false);
    $('input[value=\"Both KBAs and PAs\"]').prop('disabled', false);
  });
")),
                       tags$style(HTML("
    .treeview-menu > li > a {
      margin-left: 20px;
    }
  "))
                     ),
                     # Custom JS to delay removing modal
                     tags$script(HTML("
      Shiny.addCustomMessageHandler('remove_modal_js', function(message) {
        setTimeout(function() {
          Shiny.setInputValue('remove_modal', Math.random());
        }, 1500); // Adjust delay in ms as needed
      });
    ")),
                     
                     tabItems(
                       # Overview tab: three tabPanels
                       tabItem(tabName = "overview",
                               fluidRow(
                                 tabBox(id = "one", width = 8,
                                        tabPanel(HTML("Overview"), htmlOutput("overviewMD")),
                                        #tabPanel(HTML("Overview"), includeMarkdown("docs/overview.md")),
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
                                                 leafletOutput("map", height = 750),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabUpload'",
                                                   DT::DTOutput("pastbl")  # Use tableOutput for basic table
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
                                                   condition = "input.tabs == 'addLayers'",
                                                   includeMarkdown("./Rmd/addLayers_doc.md")
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
                                                   condition = "input.tabs == 'tabKBAs'",
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
                                                   condition = "input.tabs == 'convert'",
                                                   includeMarkdown("./Rmd/dwd_doc.md")
                                                 )
                                        )
                                 ),
                                 conditionalPanel(
                                   condition = "(input.tabs == 'tabBuilder' ||input.tabs == 'tabKBAs') && input.mapBox === 'Mapview'",
                                   tabBox(id = "metricsBox", width = 2,
                                          tabsetPanel(id = "tabsethydro",
                                                      tabPanel(HTML("<h4>Number of KBAs</h4>"), 
                                                               tableOutput("outkbahydro")
                                                      )
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
                                                      tabPanel(HTML("<h4>Number of potential KBA networks</h4>"), 
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