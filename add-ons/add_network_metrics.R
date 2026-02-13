# Adding metrics to Networks 
# This version uses inputLayers and derive analysis from it. 

library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(exactextractr)
library(psych)
library(readr)


########################################################################
########################################################################
#  Set params
########################################################################
########################################################################
# Set working directory
# working directory has:
#  - metricsNet.R
#  - representation.R
#  - inputLayers.csv
#  - output directory where output GPKG from the KBA explorer is found
setwd("E:/MelinaStuff/BEACONs/request/Lucy/addMetrics")

# source BEACONs R functions
source("./metricsNet.R")
source("./representation.R")

# Set path to KBA Explorer output (KBA_analysis.gpkg) where network are stored
outGPKG <- "./output/KBA_analysis.gpkg"
# Set network layer
kba_layer <- "wwf9_kba2best_network"
# Set network column
net_name <- "netName"

# Read network and reference* (*required for dissimilarity metrics)
net_sf <- st_read(outGPKG, layer = kba_layer)
ref_area <-  st_read("./bnd/wwf9_kba1.shp"))

# Set inputLayer and outLayer (path, filename.shp)
inputLayer <- read_csv("./inputLayers.csv"))
outLayer <- file.path("./output/netMetrics_final.shp")


########################################################################
########################################################################
###        ANALYSIS
# (Don't change anything below this line)
########################################################################
########################################################################

for (i in seq_len(nrow(inputLayer))) {

  lyr  <- inputLayer$Layer[i]
  path <- inputLayer$Path[i]
  type <- inputLayer$`Type of analysis`[i]
  
  if (type == "amount_area_rast") {
    ras <- rast(path)
    vals <- inputLayer$Value[i]

    if (!is.na(vals) && nchar(vals) > 0) {
      values_rast <- as.numeric(strsplit(vals, "-")[[1]])
    } else {
      values_rast <- NULL
    }
    net_sf <- area_from_raster(net_sf, net_name, ras, lyr, values_rast)
  }
  
  if (type == "amount_area_vect") {
    vect <- st_read(path)
    net_sf <- area_from_vector(net_sf, net_name, vect, lyr)
  }
  
  if (type == "calc_dissimilarity_cont") {
    ras <- rast(path)
    
    net_sf <- check_proj(net_sf, ras) 
    ref_area <- check_proj(ref_area, ras) 
    
    net_sf[[lyr]] <- calc_dissimilarity(net_sf, net_name, ref_area, ras, "continuous")
  }
  if (type == "calc_dissimilarity_cat") {
    ras <- rast(path)

    net_sf <- check_proj(net_sf, ras) 
    ref_area <- check_proj(ref_area, ras) 
    
    net_sf[[lyr]] <- calc_dissimilarity(net_sf, net_name, ref_area, ras, "categorical")
  }
  if (type == "geometric_mean") {
    ras <- rast(path)
    
    net_sf <- check_proj(net_sf, ras) 

    net_sf <- geometric_mean(net_sf, net_name, ras, lyr)
  }
 #                    warning(paste("Unknown analysis type:", type))
#                     net_sf
}

## Save
st_write(net_sf, outLayer)
