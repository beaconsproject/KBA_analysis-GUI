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
                                 menuItem("Explorer", tabName = "explorer", icon = icon("th"), startExpanded = TRUE,
                                          menuSubItem("Step 1: Set input parameters", tabName = "tabUpload", icon = icon("th")),
                                          menuSubItem("Step 2: Create Builder input", tabName = "tabinput", icon = icon("th")),                
                                          menuSubItem("Step 3: Run Builder", tabName = "tabBuilder", icon = icon("th")),                
                                          menuSubItem("Step 4: Reduce number of KBAs", tabName = "tabDCI", icon = icon("th")),
                                          menuSubItem("Step 5: Assess representation", tabName = "tabKBA", icon = icon("th")),
                                          menuSubItem("Step 6: Create KBAs network", tabName = "tabNET", icon = icon("th"))
                                 ),
                                 menuItem("Download results", tabName = "download", icon = icon("th")),
                                 hr()
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabUpload'",
                       shinyDirButton("directory", "Select output Directory", "Please select a folder", icon = icon(name = "fa-solid fa-folder", lib = "font-awesome")),
                       div(style = "width: 250px;margin-left: 20px;", verbatimTextOutput("dirpath")), 
                       actionButton("set_wd", "Confirm", icon = icon(name = "check", lib = "font-awesome"), class = "btn-warning", style="width:200px"),
                       tags$br(),
                       HTML("<h4>&nbsp; &nbsp; Upload spatial dataset</h4>"),
                       # File input to upload CSV
                       fileInput("csv_file", "Choose CSV containing files path", accept = ".csv"),
                       div(style = "margin: 15px; margin-top: -20px; font-size:13px;font-weight: bold", "  --  Or  --"),
                       div(style = "margin: 15px; font-size:13px;font-weight: bold", "Upload layers"),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_catch", label = "Catchments dataset", multiple = TRUE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_stream", label = "Streams dataset", multiple = TRUE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_planreg", label = "Planning region", multiple = TRUE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_lcc", label = "LCC", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_led", label = "LED", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_cmi", label = "CMI", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_gpp", label = "GPP", multiple = FALSE)),
                       div(style = "margin-top: -30px;",fileInput(inputId = "upload_custom", label = "Custom criteria", multiple = FALSE)),
                       div(style = "margin-top: -20px;", textInput("criteria5", label = div(style = "font-size:13px;", "Set custom criteria accronym"), value = "")),
                       # Add JavaScript to limit the input length to 10 characters (change as needed)
                       tags$script(HTML("$(document).on('shiny:inputinitialized', function(event) {
                             if (event.name === 'criteria5') {$('#criteria5').attr('maxlength', 10);}
                             });
                       ")),
                       actionButton("save_path", "Save path into csv", icon = icon(name = "check", lib = "font-awesome"), class = "btn-warning", style="width:200px")
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabinput'",  
                       #textInput("set_wd", "Specify output directory", value = "C:/temp/KBA"),
                       #tags$br(),
                       div(style = "margin: 15px; font-size:15px; font-weight: bold", "Create seedlist"),
                       div(style = "margin-top: -20px;", selectInput("intactseedColname", label = div(style = "font-size:13px;margin-top: -10px;", "Specify intactness attribute"), choices = "kba_m2")),
                       div(style = "margin-top: -20px;", textInput("seedintact", label = div(style = "font-size:13px;", "Set seed intactness threshold (0-1)"), value = "0")),
                       div(style = "margin-top: -20px;", textInput("set_strahler", label = div(style = "font-size:13px;","Set Strahler index"), value = 1)),
                       div(style = "margin-top: -20px;", textInput("set_areatarget", label = div(style = "font-size:13px;","Set area target for building conservation areas (sq.m)"), value = 10000000000)),
                       div(style = "margin-left: 45px; font-size:15px; font-weight: bold", "Or"),
                       div(style = "margin-top: -10px;",fileInput(inputId = "upload_seed", label = "Use test seedlist", multiple = FALSE)),
                       actionButton("runBuilderInput", "Create Builder input", icon = icon(name = "file-csv", lib = "font-awesome"), class = "btn-warning", style="width:200px"),
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabBuilder'", 
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "Set catchment parameters "),
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "Specify minimum intactness (0-1) "),
                       div(style = "margin-top: -20px;", textInput("catchintact", label = div(style = "font-size:13px;", "--catchment"), value = "0")),
                       div(style = "margin-top: -20px;",textInput("CAintact", label = div(style = "font-size:13px;", "--KBAs"), value = "0")),
                       div(style = "margin-top: -20px;",selectInput("areatypeColname", label = div(style = "font-size:13px;margin: 0px;", "Specify area target type"), choices = c("landwater", "land", "water"), selected = "landwater")),
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "Specify catchment attributes "),
                       div(style = "margin-top: -20px;",selectInput("intactColname", label = div(style = "font-size:13px;margin-top: -10px;", "--intactness"), choices = "IntactPB")),
                       div(style = "margin-top: -20px;",selectInput("zoneColname", label = div(style = "font-size:13px;", "--zone"), choices = c("MDAzone", "ecoMDAzone", "ecoZone"), selected = "ecoMDAzone")),
                       div(style = "margin-top: -20px;",selectInput("arealandColname", label = div(style = "font-size:13px;margin: 0px;", "--area land"), choices = "Area_land")),
                       actionButton("runBuilder", "Run builder", icon = icon(name = "file-csv", lib = "font-awesome"), class = "btn-warning", style="width:200px")                     
                       ),
                     conditionalPanel(
                       condition="input.tabs=='tabDCI'",
                       actionButton(inputId = "calc_dci", label = div(style = "font-size:13px;background-color:gey;color: black",HTML("1. Calculate hydrology metrics")), class = "btn-warning", style="width:250px"),
                       tags$br(),
                       textInput("set_grid", "2. Set grid cell size", value = 10000),
                       actionButton(inputId = "reduce_KBAs", label = div(style = "font-size:13px;background-color:gey;color: black",HTML("3. Reduce number of KBAs")), class = "btn-warning", style="width:250px")
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabKBA'",
                       actionButton("runRep", "Run representation analysis", icon = icon(name = "image", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                       div("Filter KBAs based on criteria dissimilarity metrics (DMS)", style = "font-size: 14px;font-weight: bold; margin-top : 20px; margin-left : 20px; "),
                       div("DMs range from 0 (low dissimilarity) to 1 (high dissimilarity)", style = "font-size: 12px; margin-top : 20px; margin-left : 20px; "),
                       #HTML("<h4>&nbsp; &nbsp; Filter criteria based on dissimilarity metrics</h4>"),
                       div(style = "margin-top: 0px;",sliderInput("slideCMI", label="CMI:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideLED", label="LED:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideGPP", label="GPP:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideLCC", label="LCC:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       # Nested condition for slidecrit5, only shows if both conditions are met
                       conditionalPanel(
                         condition = "input.criteria5 != ''",
                         div(style = "margin-top: -30px;", sliderInput("slidecrit5", label = "Criteria", min = 0, max = 1, value = 0.2, step = 0.1, ticks = FALSE))
                       ),
                       div(style = "margin-top: -30px;",sliderInput("slideUP", label="Upstream area (sq.km):", min=0, max=25000, value = 0, step=500, ticks=FALSE)),
                       actionButton("filterRep", "Apply dissimilarity metrics filtering", icon = icon(name = "filter", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabNET'",
                       div("Build network", style = "font-size: 14px;font-weight: bold; margin-top : 20px; margin-left : 20px; "),
                       textInput("set_net", "Set numbers of KBAs per network", value = 2),
                       div(style = "margin-top: -30px;",checkboxInput("forceKBA", label = "Force KBA filtering in the network", value = F)),
                       actionButton("buildNet", "Build network", icon = icon(name = "link", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                       div(style = "margin-top: 0px;",sliderInput("slideNETCMI", label="CMI:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideNETLED", label="LED:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideNETGPP", label="GPP:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       div(style = "margin-top: -30px;",sliderInput("slideNETLCC", label="LCC:", min=0, max=1, value = 0.2, step=0.1, ticks=FALSE)),
                       # Nested condition for slidecrit5, only shows if both conditions are met
                       conditionalPanel(
                         condition = "input.criteria5 != ''",
                         div(style = "margin-top: -30px;", sliderInput("slideNETcrit5", label = "Criteria", min = 0, max = 1, value = 0.2, step = 0.1, ticks = FALSE))
                       ),
                       div(style = "margin-top: -30px;",sliderInput("slideNETUP", label="Upstream area (sq.km):", min=0, max=25000, value = 0, step=500, ticks=FALSE)),
                       actionButton("filterNet", "Apply dissimilarity metrics filtering", icon = icon(name = "filter", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                       
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
                       tabItem(tabName = "tabdir",
                               fluidRow(
                                 condition = "input.tabs == 'tabdir'",
                                 tabBox(id = "mapDir", width = 10,
                                        tabPanel("Guidance", includeMarkdown("./Rmd/dir_doc.md")))
                               )
                       ),
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
                                                   includeMarkdown("./Rmd/step1_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabinput'",
                                                   includeMarkdown("./Rmd/step2_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabBuilder'",
                                                   includeMarkdown("./Rmd/step3_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabDCI'",
                                                   includeMarkdown("./Rmd/step4_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabKBA'",
                                                   includeMarkdown("./Rmd/step5_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'tabNET'",
                                                   includeMarkdown("./Rmd/step6_doc.md")
                                                 )
                                        )
                                 ),
                                 conditionalPanel(
                                   condition = "input.tabs == 'tabKBA'",
                                   tabBox(id = "metricsBox", width = 2,
                                          tabsetPanel(id = "tabset1",
                                                      tabPanel(HTML("<h4>Potential KBA metrics</h4>"), 
                                                               tableOutput("outkbafreq"),
                                                               selectInput("KBA", label = "Select KBAs:", choices = NULL),  # Initially empty, updated dynamically
                                                               tableOutput("outkba")
                                                      )
                                          )
                                   )
                                 ),
                                conditionalPanel(
                                  condition = "input.tabs == 'tabNET'",
                                  tabBox(id = "metricsNET", width = 2,
                                         tabsetPanel(id = "tabsetNET",
                                                     tabPanel(HTML("<h4>Potential KBA network metrics</h4>"), 
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