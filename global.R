# Load the packages
library(leaflet)
library(shiny)
library(purrr)
library(markdown)
library(shinydashboard)
library(shinyjs)
library(exactextractr)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet.extras2)
library(ggplot2)
library(zip)
library(readr)
library(terra)
library(stringr)
library(shinyFiles)
library(DT)
library(rlang)
library(leafgl)
library(raster)
library(shinyWidgets)
library(usethis)
library(qs)

terra::terraOptions(tempdir = tempdir(), memfrac = 0.5)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

bnd <- st_read("./www/Canada_WGS84.shp")
intact <- st_read("./www/KBAIntactAreasbnd_nad83.shp")

MB <- 1024^2

UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)

# Define the last update date (deployment date)
#last_update <- Sys.Date()  # or use Sys.time() for full timestamp
last_update <- "2026-01-23"  # or use Sys.time() for full timestamp

# Read the Markdown file
overview_md <- readLines("docs/overview.md")

# Replace placeholder in the Markdown
overview_md <- c(
  paste0('<div style="text-align: right; font-size:0.9em; color: gray;">Last update: ', last_update, '</div>'),
  overview_md
)

# Convert to a single string for rendering
overview_md_text <- paste(overview_md, collapse = "\n")

# turn off scientifc notation to avoid 1e10
options(scipen = 999)
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
  masked <- terra::trim(masked, value = NA)

  if (!is.null(ignored)) {
    masked[masked %in% ignored] <- NA # cropland = 15, urban = 17 are NA 
  }
  
  terra::writeRaster(masked, output_path, filetype = "GTiff")
  aggregated <- terra::aggregate(masked, fact = fact, fun = aggregation_fun)
  projected <- project(aggregated, "EPSG:4326")
  terra::writeRaster(projected, projected_path, filetype = "GTiff")
  
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
    #showNotification("Layer not found in CSV. Check your file.", type = "error")
    return(NULL)  # Stop further execution
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
  la <- sf::st_read(path) %>%
    dplyr::select(-any_of(c("fid", "FID")))
  return(la)
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
        shp <- sf::st_read(shp_path) %>%
          dplyr::select(-any_of(c("fid", "FID"))) %>%
          sf::st_zm(drop = TRUE, what = "ZM")
        attr(shp, "name") <- name
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
      return(terra::rast(path))  # Load raster using the terra package
    } else {
      showModal(modalDialog(
        title = paste("The path for", layer_name, "in the CSV does not exist."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
  } else {
    showModal(modalDialog(
      title = paste(layer_name, "layer not found in CSV."),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return()
  }
}

# read_tif_from_upload: Read raster file from fileInput
read_tif_from_upload <- function(upload_input) {
  req(upload_input)  # Ensure the file is uploaded
  path <- upload_input$datapath
  if (file.exists(path)) {
    return(terra::rast(path))  # Load terra using the raster package
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


prep_legend <- function(kba_cmi, kba_led, kba_gpp, lcc_4326, criteria5 = NULL) {
  
  # Set legend for CMI
  cmi_minVar <- min(floor(values(kba_cmi)), na.rm = TRUE)
  cmi_maxVar <- max(ceiling(values(kba_cmi)), na.rm = TRUE)
  cmi_bins.seq <- seq(cmi_minVar, cmi_maxVar, length.out = 5)
  xpal <- colorBin("RdYlBu", cmi_bins.seq, bins = cmi_bins.seq, na.color = "transparent")
  val.color <- "RdYlBu"
  
  # Set legend for LED
  led_minVar <- min(floor(values(kba_led)), na.rm = TRUE)
  led_maxVar <- max(ceiling(values(kba_led)), na.rm = TRUE)
  led_bins.seq <- seq(led_minVar, led_maxVar, length.out = 5)
  led_xpal <- colorBin("Blues", led_bins.seq, bins = led_bins.seq, na.color = NA)
  led_val.color <- "Blues"
  
  # Set legend for GPP
  gpp_minVar <- min(floor(values(kba_gpp)), na.rm = TRUE)
  gpp_maxVar <- max(ceiling(values(kba_gpp)), na.rm = TRUE)
  gpp_bins.seq <- seq(gpp_minVar, gpp_maxVar, length.out = 5)
  gppxpal <- colorBin("RdYlBu", gpp_bins.seq, bins = gpp_bins.seq, na.color = "transparent")
  
  # Prepare labels for LCC
  unique_sorted_values <- sort(na.omit(unique(values(lcc_4326))))
  df_label <- data.frame(values = c(1,2,5,6,8,10,11,12,13,14,15,16,17,18,19), 
                         labels = c("Temperate conifer forest", "Taiga conifer forest",
                                    "Broadleaf forest", "Mixed Forest", "Shrubland", "Grassland", 
                                    "Shrubland-lichen-moss", "Grassland-lichen-moss","Barren-lichen-moss",
                                    "Wetland", "Cropland", "Barren Lands", "Urban", "Water", "Snow"))
  df_label <- df_label[df_label$values %in% unique_sorted_values, ]
  cls <- df_label$labels
  
  # Read LCC colors
  lcc_cols <- read.csv('www/lc_cols.csv') %>%
    filter(value %in% unique_sorted_values) %>%
    mutate(color = rgb(red, green, blue, maxColorValue = 255)) %>%
    pull(color)
  selected_cols <- lcc_cols    
  
  # Labeller function
  labeller_function <- function(type, breaks) {
    return(c('Low', '', '', 'High'))
  }
  
  # Set legend for criteria5 if it exists
  if (!is.null(criteria5)) {
    c5_minVar <- min(floor(values(criteria5)), na.rm = TRUE)
    c5_maxVar <- max(ceiling(values(criteria5)), na.rm = TRUE)
    c5_bins.seq <- seq(c5_minVar, c5_maxVar, length.out = 5)
    crit_xpal <- colorBin("RdYlBu", c5_bins.seq, bins = c5_bins.seq, na.color = "transparent")
    val.color <- "RdYlBu"
  } else {
    crit_xpal <- NULL
  }
  
  # Return everything as a list
  return(list(cmi_xpal = xpal, led_xpal = led_xpal, gpp_xpal = gppxpal, 
              cmi_bins = cmi_bins.seq, led_bins = led_bins.seq, gpp_bins = gpp_bins.seq,
              df_label = df_label, lcc_labels = cls, lcc_cols = selected_cols, crit_xpal = crit_xpal, val.color = val.color, 
              led_val.color = led_val.color, labeller_function = labeller_function))
}

