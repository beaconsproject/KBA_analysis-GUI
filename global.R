library(leaflet)
library(shiny)
library(purrr)
library(markdown)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(devtools)
library(beaconsbuilder) # code needs to be repaired.
library(dplyr)
library(tidyr)
library(sf)
library(zip)
library(raster)
library(readr)
library(beaconstools)
library(terra)
library(stringr)
library(shinyFiles)
library(DT)
#source("./R/beaconshydro.R")
#source("./R/utils.R")
source("./R/utils_KBA.R")
source("./R/builder_KBA.R")

bnd <- st_read("./www/Canada_WGS84.shp")
intact <- st_read("./www/KBAIntactAreasbnd_nad83.shp")

# Helper function to detect available drives (Windows only)
get_available_drives <- function() {
  drives <- c(paste0(LETTERS, ":/")) # Generate list of potential drives
  available_drives <- drives[file.exists(drives)] # Keep only existing drives
  names(available_drives) <- available_drives
  available_drives
}

# crop and mask criteria layer
process_raster <- function(input_raster, ref_area, dir_path, file_name, fact = 4, crs = "EPSG:4326", aggregation_fun = NULL) {
  output_path <- file.path(dir_path, "output", file_name)
  projected_path <- file.path(dir_path, "output", paste0(file_name, "_4326.tif"))
  
  cropped <- crop(input_raster, ref_area)
  masked <- mask(cropped, ref_area)
  raster::writeRaster(masked, output_path, format = "GTiff")
  if (!is.null(aggregation_fun)) {
    aggregated <- terra::aggregate(rast(masked), fact = fact, fun = aggregation_fun)
    projected <- project(aggregated, crs)
    terra::writeRaster(projected, projected_path, filetype = "GTiff")
  } else {
    aggregated <- aggregate(masked, fact = fact)
    projected <- projectRaster(aggregated, crs = crs)
    raster::writeRaster(projected, projected_path, format = "GTiff")
  }
  list(original = masked, projected = projected)
}

MB <- 1024^2

UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)
