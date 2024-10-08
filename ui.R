ui = dashboardPage(skin="blue",
                   dashboardHeader(title = "KBA Analysis"),
                   dashboardSidebar(
                     width = 275,
                     sidebarMenu(id = "tabs",
                                 menuItem("Overview", tabName = "overview", icon = icon("th")),
                                 menuItem("Explorer", tabName = "explorer", icon = icon("th"), startExpanded = TRUE,
                                          menuSubItem("Create conservation area", tabName = "tabBuilder", icon = icon("th")),                
                                          menuSubItem("Assess representation", tabName = "tabRanker", icon = icon("th")),
                                          menuSubItem("Generate upstream and downstream", tabName = "upstream", icon = icon("arrows-rotate"))),
                                 menuItem("Download results", tabName = "download", icon = icon("th")),
                                 hr()
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabBuilder'",
                       HTML("<h4>&nbsp; &nbsp; Create conservation area polygons</h4>"),
                       fileInput(inputId = "upload_catch", label = "Upload catchments dataset", multiple = TRUE),
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "Catchment dataset "),
                       div(style = "margin-top: -20px;", textInput("catchintact", label = div(style = "font-size:13px;", "Specify catchments intactness (0-1)"), value = "1")),
                       #textInput("catchintact", label = div(style = "font-size:13px;", "Specify catchments intactness (0-1)"), value = "1"),
                       div(style = "margin-top: -20px;",selectInput("zoneColname", label = div(style = "font-size:13px;", "Select zone attribute"), choices = "MDAzone")),
                       div(style = "margin-top: -20px;",textInput("CAintact", label = div(style = "font-size:13px;", "Specify CAs intactness (0-1)"), value = "1")),
                       div(style = "margin-top: -20px;",selectInput("areatypeColname", label = div(style = "font-size:13px;margin: 0px;", "Select area_type attribute"), choices = c("landwater", "land", "water"), selected = "landwater")),
                       div(style = "margin-top: -20px;",selectInput("arealandColname", label = div(style = "font-size:13px;margin: 10px;", "Select area_land attribute"), choices = "Area_land")),
                       div(style = "margin-top: -20px;",selectInput("intactColname", label = div(style = "font-size:13px;margin-top: -10px;", "Select zone attribute"), choices = "intact")),
                       textInput("set_wd", "Specify output directory", value = "C:/temp/KBA"),
                       textInput("set_strahler", "Specify Strahler index to create seedlist", value = 1),
                       textInput("set_areatarget", "Specify area target for building conservation areas (sq.m)", value = 10000000),
                       actionButton("runBuilder", "Run builder", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:200px"),
                       downloadButton("download_shapefile", "Download Shapefile")
                     ),
                     conditionalPanel(
                       condition="input.tabs=='tabRanker'",
                       HTML("<h4>&nbsp; &nbsp; Choose Area of Interest (AOI) from &nbsp; &nbsp; one of the two options:</h4>"),
                       fileInput(inputId = "upload_poly", label = div(style = "font-size:13px", "1 - Upload polygon(s) (gpkg) to select underlying catchments"), multiple = FALSE, accept = ".gpkg"),
                       div(style = "margin: 15px; margin-top: -30px; font-size:13px;font-weight: bold", "Or "),
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "2 - Select a set of catchments on the map"),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       actionButton(inputId = "gen_aoi_button", label = div(style = "font-size:13px;background-color:gey;color: black",HTML(" Confirm AOI boundary (Analysis AOI)")), icon = icon(name = "check", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                       tags$br(),
                       tags$br()
                     ), 
                     conditionalPanel(
                       condition="input.tabs=='upstream'",
                       actionButton("goButtonDown", label  = div(style = "font-size:13px;background-color:gey;color: black", HTML("View upstream and
                    <br /> downstream intactness")), icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:200px")
                     ),     
                     conditionalPanel(
                       condition="input.tabs=='download'",
                       HTML("<h5>&nbsp; &nbsp; Specify EPSG <a href='https://spatialreference.org/'>spatial reference system</a> for <br> &nbsp; &nbsp; gpkg.</h5>"),
                       HTML("<h5>&nbsp; &nbsp; (Default: NAD83 Canada Albers)</h5>"),
                       textInput("textproj", "", value = "102001"),
                       tags$style(type="text/css", "#downloadData {background-color:gey;color: black}"),
                       div(style="position:relative; left:calc(10%);", downloadButton("downloadData", "Download results"))
                     )
                   ),     
                   dashboardBody(
                     useShinyjs(),
                     tags$head(tags$style(".skin-blue .sidebar a { color: #8a8a8a; }")),
                     tabItems(
                       tabItem(tabName="overview",
                               fluidRow(
                                 tabBox(id = "one", width="8",
                                        tabPanel(HTML("Overview"), includeMarkdown("docs/overview.md")),
                                        tabPanel(HTML("Quick start"), includeMarkdown("docs/quick_start.md")),
                                        tabPanel(HTML("Dataset"), includeMarkdown("docs/datasets.md"))
                                 ),
                               )
                       ),
                       tabItem(tabName="tabBuilder",
                               fluidRow(
                                 tabBox(id = "three", width="8",
                                        tabPanel(HTML("<b>Mapview</b>"),
                                                 leafletOutput("map", height = 750) %>% withSpinner()
                                        )
                                 ),
                                 #tabBox(id = "two", width="4", title = HTML("<h4>Area Intactness and Hydrology statistics</h4>"),
                                 tabBox(id = "two", width="4",
                                        tabsetPanel(id="tabset1",
                                                    tabPanel(HTML("<h4>Area Intactness and Hydrology statistics</h4>"), tableOutput("tab1"),
                                                             "*Catchment Area Weighted Intactness",)
                                        )
                                 ),
                                 tabBox(id = "three", width="4",  
                                        tabsetPanel(id="tabset2",
                                                    tabPanel(HTML("<h4>Dendritic Connectivity Index (DCI)</h4>"), tableOutput("tabDCI")),
                                                    tabPanel(HTML("<h4>Fire statistics</h4>"), tableOutput("tabFires"))
                                        )
                                 )
                               )
                       )
                     )
                   )
)
