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
#source("./R/beaconshydro.R")
#source("./R/utils.R")
source("./R/utils_KBA.R")
source("./R/builder_KBA.R")

bnd <- st_read("./www/Canada_WGS84.shp")
MB <- 1024^2

UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)
