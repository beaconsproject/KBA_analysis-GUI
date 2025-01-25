# Check and install packages if missing
required_packages <- c(
  "leaflet", "shiny", "purrr", "markdown", "shinydashboard", "shinyjs", 
  "shinycssloaders", "devtools", "beaconsbuilder", "dplyr", "tidyr", "sf", 
  "zip", "readr", "beaconstools", "terra", "stringr", "shinyFiles", "DT","rlang", "leafgl"
)

terra::terraOptions(tempdir = tempdir(), memfrac = 0.5)


bpcrs <- terra::crs("PROJCRS[\"NAD_1983_Albers\",BASEGEOGCRS[\"NAD83\",DATUM[\"North American Datum 1983\",ELLIPSOID[\"GRS 1980\",6378137,298.257222101,
              LENGTHUNIT[\"metre\",1]],ID[\"EPSG\",6269]],PRIMEM[\"Greenwich\",0,ANGLEUNIT[\"Degree\",0.0174532925199433]]],CONVERSION[\"unnamed\",METHOD[\"Albers Equal Area\",ID[\"EPSG\",9822]],
              PARAMETER[\"Latitude of false origin\",63.4,ANGLEUNIT[\"Degree\",0.0174532925199433],ID[\"EPSG\",8821]],PARAMETER[\"Longitude of false origin\",-91.867,ANGLEUNIT[\"Degree\",0.0174532925199433],ID[\"EPSG\",8822]],
              PARAMETER[\"Latitude of 1st standard parallel\",49,ANGLEUNIT[\"Degree\",0.0174532925199433],ID[\"EPSG\",8823]],PARAMETER[\"Latitude of 2nd standard parallel\",77,ANGLEUNIT[\"Degree\",0.0174532925199433],
              ID[\"EPSG\",8824]],PARAMETER[\"Easting at false origin\",0,LENGTHUNIT[\"metre\",1],ID[\"EPSG\",8826]],PARAMETER[\"Northing at false origin\",0,LENGTHUNIT[\"metre\",1],
              ID[\"EPSG\",8827]]],CS[Cartesian,2],AXIS[\"(E)\",east,ORDER[1],LENGTHUNIT[\"metre\",1,ID[\"EPSG\",9001]]],AXIS[\"(N)\",north,ORDER[2],LENGTHUNIT[\"metre\",1,ID[\"EPSG\",9001]]]]")

# Install any missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load the packages
invisible(lapply(required_packages, library, character.only = TRUE))

source("./R/utils_KBA.R")
source("./R/builder_KBA.R")


bnd <- st_read("./www/Canada_WGS84.shp")
intact <- st_read("./www/KBAIntactAreasbnd_nad83.shp")

MB <- 1024^2

UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)
#########################################################
#########################################################
#         ADDON FUNCTIONS
#########################################################
#########################################################
# sep_network_names :Fix sep_network_names
sep_network_names <- function (network_names){
    out_val <- lapply(network_names, function(x) {
      strsplit(x, "__")[[1]]
    })
    names(out_val) <- network_names
  return(out_val)
}

# get_available_drives: Helper function to detect available drives (Windows only)
get_available_drives <- function() {
  drives <- c(paste0(LETTERS, ":/")) # Generate list of potential drives
  available_drives <- drives[file.exists(drives)] # Keep only existing drives
  names(available_drives) <- available_drives
  available_drives
}

# process_raster: crop and mask criteria layer
process_raster <- function(input_raster, ref_area, dir_path, file_name, fact = 4, aggregation_fun = NULL, ignored = NULL) {
  output_path <- file.path(dir_path, "output", paste0(file_name, ".tif"))
  projected_path <- file.path(dir_path, "output", paste0(file_name, "_4326.tif"))
  #browser()
  #cropped <- crop(input_raster, ref_area, snap = "near")
  #masked <- mask(cropped, ref_area)
  masked <- crop(input_raster, ref_area, snap = "near")
  if (!is.null(ignored)) {
    masked[masked %in% ignored] <- NA # cropland = 15, urban = 17 are NA 
  }
  terra::writeRaster(masked, output_path, filetype = "GTiff")
  if (!is.null(aggregation_fun)) {
    aggregated <- terra::aggregate(masked, fact = fact, fun = aggregation_fun)
    projected <- project(aggregated, "EPSG:4326")
    terra::writeRaster(projected, projected_path, filetype = "GTiff")
  } else {
    aggregated <- aggregate(masked, fact = fact)
    projected <- project(aggregated, "EPSG:4326")
    terra::writeRaster(projected, projected_path, filetype = "GTiff")
  }
  list(original = masked, projected = projected)
}

# read_shp_from_csv: read layer from path found in csv uploaded with fileInput
read_shp_from_csv <- function(csv_file, layer_name) {
  req(csv_file)
  csv_data <- read.csv(csv_file$datapath)
  if (layer_name %in% csv_data$Layer) {
    path <- csv_data$Path[csv_data$Layer == layer_name]
    if (file.exists(path)) {
      return(sf::st_read(path))
    } else {
      stop(paste("The path for", layer_name, "in the CSV does not exist."))
    }
  } else {
    stop(paste(layer_name, "layer not found in CSV."))
  }
}

# read_shp_from_upload: read a shapefile from fileInput
read_shp_from_upload <- function(upload_input) {
  req(upload_input)
  infile <- upload_input
  if (length(infile$datapath) > 1) {
    dir <- unique(dirname(infile$datapath))
    outfiles <- file.path(dir, infile$name)
    name <- tools::file_path_sans_ext(infile$name[1])
    purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y))
    shp_path <- file.path(dir, paste0(name, ".shp"))
    if (file.exists(shp_path)) {
      #return(sf::st_read(shp_path))
      shp <- sf::st_read(shp_path)
      #browser()
      # Check CRS to ensure it's NAD_83_Albers
      #if (isFALSE(st_crs(shp) == st_crs(4269))) {
      #  #stop("The shapefile does not use the NAD_83_Albers projection. Please reproject prior to upload")
     #   showModal(modalDialog(
      #    title = "Wrong projection",
      #    "The shapefile does not use the NAD_83_Albers projection. Please reproject prior to upload",
     #     easyClose = TRUE,
       #   footer = modalButton("OK")
      #  ))
     #   return()
      #}
      return(shp)
    } else {
      stop("Shapefile (.shp) is missing.")
    }
  } else {
    stop("Upload all necessary files for the shapefile (.shp, .shx, .dbf, etc.).")
  }
}

# read_tif_from_csv: Read raster file from CSV
read_tif_from_csv <- function(csv_file, layer_name) {
  req(csv_file)  # Ensure the CSV file is provided
  csv_data <- read.csv(csv_file$datapath)
  
  # Check if the specified layer exists in the CSV
  if (layer_name %in% csv_data$Layer) {
    path <- csv_data$Path[csv_data$Layer == layer_name]
    if (file.exists(path)) {
      return(terra::rast(path))  # Load raster using the raster package
    } else {
      stop(paste("The path for", layer_name, "in the CSV does not exist."))
    }
  } else {
    stop(paste(layer_name, "layer not found in CSV."))
  }
}

# read_tif_from_upload: Read raster file from fileInput
read_tif_from_upload <- function(upload_input) {
  req(upload_input)  # Ensure the file is uploaded
  path <- upload_input$datapath
  if (file.exists(path)) {
    return(terra::rast(path))  # Load raster using the raster package
  } else {
    stop("The uploaded raster file does not exist.")
  }
}

# get_stat_on_net: compute stats on NET
get_stat_on_net <- function(net_sf, catchments, intact_col, upstream) {
  # Union NET and intersect  with catchments
  #browser()
  net_diss <- st_union(net_sf) 
  area_km2 <- net_diss %>% st_area(.)/1000000
  net_catch <- st_intersection(catchments, net_diss)
  
  #Calculate total area and intactness for NET
  AWI <- net_catch %>%
    mutate(catch_awi = as.numeric(st_area(.)) * .[[intact_col]]) %>%
    st_drop_geometry() %>%
    summarize(AWI = sum(catch_awi, na.rm = TRUE) / 1000000)

  #Calculate area and intactness for upstream NET
  p_name <- sep_network_names(net_sf$network)
  up_net <- upstream[upstream$network %in% p_name[[1]],]
  
  if(nrow(up_net)>0){
    up_bind <- st_union(up_net)
    up_km2 <- up_bind %>% st_area(.)/1000000
    up_catch <- st_intersection(catchments, up_bind)
  
    up_intactkm <- up_catch %>%
      mutate(catch_awi = as.numeric(st_area(.)) * .[[intact_col]]) %>%
      st_drop_geometry() %>%
      summarize(up_AWI = sum(catch_awi, na.rm = TRUE) / 1000000)
    
    up_km2 <- as.numeric(up_km2)
    up_AWI <- as.numeric(up_intactkm)/as.numeric(up_km2)
  }else{
    up_km2 <- 0
    up_AWI <- 0
  } 
  net_sf <- net_sf %>% 
    st_as_sf() %>%
    mutate(area_km2 = as.numeric(area_km2),
           AWI = as.numeric(AWI)/as.numeric(area_km2),
           up_km2 = up_km2,
           up_AWI = up_AWI)
  return(net_sf)
}

# get_stat_on_net: compute stats on NET
get_upstream <- function(net_sf, upstream) {
  # Extract upstream 
  p_name <- sep_network_names(net_sf$network)
  up_net <- upstream[upstream$network %in% p_name[[1]],]
  if(nrow(up_net)>0){
    up_bind <- st_union(up_net) %>% 
      st_as_sf()%>% 
      mutate(network = net_sf$network)
    return(up_bind)
  }else{
    return(NULL)
  } 
}
