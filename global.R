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


MB <- 1024^2

UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)
