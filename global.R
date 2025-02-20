# Check and install packages if missing
required_packages <- c(
  "leaflet", "shiny", "purrr", "markdown", "shinydashboard", "shinyjs", "exactextractr",
  "shinycssloaders", "devtools", "beaconsbuilder", "dplyr", "tidyr", "sf", 
  "zip", "readr", "beaconstools", "terra", "stringr", "shinyFiles", "DT","rlang", "leafgl", "raster", "shinyWidgets"
)

terra::terraOptions(tempdir = tempdir(), memfrac = 0.5)


bpcrs <- "PROJCRS[\"NAD_1983_Albers\",BASEGEOGCRS[\"NAD83\",DATUM[\"North American Datum 1983\",ELLIPSOID[\"GRS 1980\",6378137,298.257222101,
              LENGTHUNIT[\"metre\",1]],ID[\"EPSG\",6269]],PRIMEM[\"Greenwich\",0,ANGLEUNIT[\"Degree\",0.0174532925199433]]],CONVERSION[\"unnamed\",METHOD[\"Albers Equal Area\",ID[\"EPSG\",9822]],
              PARAMETER[\"Latitude of false origin\",63.4,ANGLEUNIT[\"Degree\",0.0174532925199433],ID[\"EPSG\",8821]],PARAMETER[\"Longitude of false origin\",-91.867,ANGLEUNIT[\"Degree\",0.0174532925199433],ID[\"EPSG\",8822]],
              PARAMETER[\"Latitude of 1st standard parallel\",49,ANGLEUNIT[\"Degree\",0.0174532925199433],ID[\"EPSG\",8823]],PARAMETER[\"Latitude of 2nd standard parallel\",77,ANGLEUNIT[\"Degree\",0.0174532925199433],
              ID[\"EPSG\",8824]],PARAMETER[\"Easting at false origin\",0,LENGTHUNIT[\"metre\",1],ID[\"EPSG\",8826]],PARAMETER[\"Northing at false origin\",0,LENGTHUNIT[\"metre\",1],
              ID[\"EPSG\",8827]]],CS[Cartesian,2],AXIS[\"(E)\",east,ORDER[1],LENGTHUNIT[\"metre\",1,ID[\"EPSG\",9001]]],AXIS[\"(N)\",north,ORDER[2],LENGTHUNIT[\"metre\",1,ID[\"EPSG\",9001]]]]"

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

# Function to check if all required shapefile components exist
check_shp <- function(shapefile_path) {
  folder_path <- dirname(shapefile_path)
  base_name <- tools::file_path_sans_ext(basename(shapefile_path))
  
  required_extensions <- c(".shp", ".shx", ".dbf", ".prj")
  required_files <- paste0(file.path(folder_path, base_name), required_extensions)
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0) {
    showModal(modalDialog(
      title = "Extension File Missing",
      paste(paste(tools::file_ext(missing_files), collapse = ", "), " extension is missing from ", base_name, 
            ". Make sure all required extension (.shp, .shx, .dbf, .prj) files exist prior to upload the shapefile."),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    showNotification("Shapefile is incomplete. Please provide all required files.", type = "error")
    req(FALSE)  # Stop further execution
  }
  
  return(TRUE)  # Return TRUE if all files exist
}

# process_raster: crop and mask criteria layer
process_raster <- function(input_raster, ref_area, dir_path, file_name, fact = 4,  aggregation_fun = NULL, ignored = NULL) {
  output_path <- file.path(dir_path, "output", paste0(file_name, ".tif"))
  projected_path <- file.path(dir_path, "output", paste0(file_name, "_4326.tif"))
  cropped <- crop(input_raster, ref_area, snap = "near", extend = TRUE)
  masked <- mask(cropped, ref_area)
  #masked <- crop(input_raster, ref_area, snap = "near")
  if (!is.null(ignored)) {
    masked[masked %in% ignored] <- NA # cropland = 15, urban = 17 are NA 
  }
  raster::writeRaster(masked, output_path, format = "GTiff")
  if (!is.null(aggregation_fun)) {
    aggregated <- terra::aggregate(rast(masked), fact = fact, fun = aggregation_fun)
    projected <- project(aggregated, crs(bpcrs))
    terra::writeRaster(projected, projected_path, filetype = "GTiff")
  } else {
    aggregated <- aggregate(masked, fact = fact)
    projected <- projectRaster(aggregated, crs = bpcrs)
    raster::writeRaster(projected, projected_path, format = "GTiff")
    
  }
  list(original = masked, projected = projected)
}




# read_shp_from_csv: read layer from path found in csv uploaded with fileInput
read_shp_from_csv <- function(csv_file, layer_name) {
  req(csv_file)
  
  csv_data <- read.csv(csv_file$datapath, stringsAsFactors = FALSE)
  if (!(layer_name %in% csv_data$Layer)) {
    showModal(modalDialog(
      title = "Layer Not Found",
      paste("The layer", layer_name, "was not found in the CSV."),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    showNotification("Layer not found in CSV. Check your file.", type = "error")
    req(FALSE)  # Stop further execution
  }
  
  path <- csv_data$Path[csv_data$Layer == layer_name]
  if (!file.exists(path)) {
    showModal(modalDialog(
      title = "Invalid Path",
      paste("The path for", layer_name, "does not exist."),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    showNotification("Invalid file path in CSV. Check your file.", type = "error")
    req(FALSE)  # Stop further execution
  }
  
  # Check if all required shapefile components are present
  check_shp(path)
  # If everything is okay, read the shapefile
  return(sf::st_read(path))
}

# read_shp_from_upload: read a shapefile from fileInput
read_shp_from_upload <- function(upload_input) {
  req(upload_input)
  required_extensions <- c("shp", "shx", "dbf", "prj")
  infile <- upload_input
  file_extensions <- tools::file_ext(infile$name)
  if (all(required_extensions %in% file_extensions)) {
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
      showModal(modalDialog(
        title = "Shapefile (.shp) is missing.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
  } else {
    showModal(modalDialog(
      title = "Extension file is missing",
      "Please upload all necessary files for the shapefile (.shp, .shx, .dbf and .prj).",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return()
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
      return(raster::raster(path))  # Load raster using the raster package
    } else {
      showModal(modalDialog(
        title = paste("The path for", layer_name, "in the CSV does not exist."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
      #stop(paste("The path for", layer_name, "in the CSV does not exist."))
    }
  } else {
    showModal(modalDialog(
      title = paste(layer_name, "layer not found in CSV."),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return()
    #stop(paste(layer_name, "layer not found in CSV."))
  }
}

# read_tif_from_upload: Read raster file from fileInput
read_tif_from_upload <- function(upload_input) {
  req(upload_input)  # Ensure the file is uploaded
  path <- upload_input$datapath
  if (file.exists(path)) {
    return(raster::raster(path))  # Load raster using the raster package
  } else {
    showModal(modalDialog(
      title = "The uploaded raster file does not exist.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return()
    #stop("The uploaded raster file does not exist.")
  }
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
