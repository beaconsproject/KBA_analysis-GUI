# Load the packages
TOOLS_REF     <- "v0.1.0"   # git tag of R_tools_public
TOOLS_VERSION <- "0.1.0"    # Version: in its DESCRIPTION

tools_dev <- Sys.getenv("BEACONS_TOOLS_DEV")
if (nzchar(tools_dev)) {
  # Developers only: use a local R_tools_public copy (unreleased changes)
  pkgload::load_all(tools_dev, export_all = FALSE)
} else {
  if (!requireNamespace("beaconsRtools", quietly = TRUE) ||
      utils::packageVersion("beaconsRtools") < TOOLS_VERSION) {
    message("Installing beaconsRtools ", TOOLS_REF, " from GitHub...")
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
    remotes::install_github(paste0("beaconsproject/R_tools_public@", TOOLS_REF),
                            upgrade = "never")
  }
  library(beaconsRtools)
}

library(shiny)
library(leaflet)
library(sf)
library(shinydashboard)
library(shinyFiles) #shinyDirButton
library(shinyWidgets) #pickerInput
library(shinyjs) #useShinyjs
library(purrr)
library(markdown)
library(tibble)
library(DT)
library(dplyr)
library(terra)
library(exactextractr)
library(tidyr)
library(stringr)
library(ggplot2)
#library(leaflet.extras2)

terra::terraOptions(tempdir = tempdir(), memfrac = 0.5)

# Documentation images: kept in docs/pics and served at "pics/", so the same
# src="pics/<file>.png" works in the app and when the .md files are read on GitHub.
shiny::addResourcePath("pics", "docs/pics")

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
# Check if Microsoft .Net Framework is installed
dotnet_installed <- function() {
  key <- "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\NET Framework Setup\\NDP\\v4\\Full"
  out <- try(shell(paste0('reg query "', key, '" /v Release'), intern = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) return(FALSE)
  # Check if there is a "Release" line
  any(grepl("Release", out))
}

# get_available_drives: Helper function to detect available drives (Windows only)
get_available_drives <- function() {
  drives <- c(paste0(LETTERS, ":/")) # Generate list of potential drives
  available_drives <- drives[file.exists(drives)] # Keep only existing drives
  names(available_drives) <- available_drives
  available_drives
}

# test prior to map that layer is not null, sf and has rows
isMappable <- function(x) {
  !is.null(x) && inherits(x, "sf") && nrow(x) > 0
}

# Layer names in the project's GeoPackage, or character(0) if it doesn't exist yet
gpkg_path <- function(outdir) file.path(outdir, "output", "KBA_analysis.gpkg")

gpkg_layers <- function(outdir) {
  path <- gpkg_path(outdir)
  if (!file.exists(path)) return(character(0))
  sf::st_layers(path)$name
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

# check_layer_crs: warn the user at upload if a layer has no CRS, a CRS that doesn't match
# its coordinates, or doesn't overlap the reference layer (bnd = Canada by default).
# crs: sf crs object; bb: sf bbox of the layer; label: layer name shown in the pop-up.
# Returns TRUE if all checks pass, FALSE (after showing a pop-up) otherwise.
check_layer_crs <- function(crs, bb, label, ref = bnd) {
  warn <- function(title, msg) {
    showModal(modalDialog(title = title, msg, easyClose = TRUE, footer = modalButton("OK")))
    FALSE
  }
  # 1. No CRS: .prj missing, empty or not recognised
  if (is.na(crs)) {
    return(warn(paste0(label, ": no projection"),
                "This layer has no coordinate reference system (projection). Define its projection in QGIS or ArcGIS and upload it again."))
  }
  # Empty layer: no extent to check
  if (any(is.na(bb))) return(TRUE)
  # 2. CRS says longitude/latitude but the coordinates are out of range (likely metres)
  if (isTRUE(crs$IsGeographic) &&
      (any(abs(bb[c("xmin", "xmax")]) > 180) || any(abs(bb[c("ymin", "ymax")]) > 90))) {
    return(warn(paste0(label, ": wrong projection"),
                "The layer projection is longitude/latitude, but its coordinates are not in degrees. Check the projection of the layer (.prj file) in QGIS or ArcGIS."))
  }
  # 3. Layer extent doesn't overlap the reference layer
  if (!is.null(ref)) {
    overlap <- tryCatch({
      box <- sf::st_transform(sf::st_as_sfc(bb), sf::st_crs(ref))
      sf::st_intersects(box, sf::st_as_sfc(sf::st_bbox(ref)), sparse = FALSE)[1, 1]
    }, error = function(e) FALSE)
    if (!isTRUE(overlap)) {
      return(warn(paste0(label, ": outside the study region"),
                  "This layer does not overlap Canada. Check that it has the right projection (.prj file)."))
    }
  }
  TRUE
}

# read_shp_from_csv: read layer from path found in csv uploaded with fileInput
read_shp_from_csv <- function(csv_file, layer_name, crs = NULL) {
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
  # Stop here (after a pop-up) if the projection is missing or wrong
  req(check_layer_crs(sf::st_crs(la), sf::st_bbox(la), layer_name))
  if (!is.null(crs) && sf::st_crs(la) != crs) {
    la <- sf::st_transform(la, crs)
  }
  return(la)
}

# read_shp_from_upload: read a shapefile from fileInput
read_shp_from_upload <- function(upload_input, crs = NULL) {
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
      # Stop here (after a pop-up) if the projection is missing or wrong
      req(check_layer_crs(sf::st_crs(shp), sf::st_bbox(shp), name))
      if (!is.null(crs) && sf::st_crs(shp) != crs) {
        la <- sf::st_transform(shp, crs)
      }
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

# check_raster_crs: check_layer_crs() for a terra raster, plus same projection as the catchments.
# catch_crs: catchments CRS (sf crs object or WKT string), NULL to skip that test.
# Returns TRUE if all checks pass, FALSE (after showing a pop-up) otherwise.
check_raster_crs <- function(r, label, catch_crs = NULL) {
  r_crs <- if (terra::crs(r) == "") sf::NA_crs_ else sf::st_crs(terra::crs(r))
  e <- as.vector(terra::ext(r))
  bb <- sf::st_bbox(c(xmin = e[["xmin"]], ymin = e[["ymin"]], xmax = e[["xmax"]], ymax = e[["ymax"]]),
                    crs = r_crs)
  if (!check_layer_crs(r_crs, bb, label)) return(FALSE)
  if (inherits(catch_crs, "crs")) catch_crs <- catch_crs$wkt
  if (!is.null(catch_crs) && !terra::same.crs(r, catch_crs)) {
    showModal(modalDialog(
      title = paste0(label, ": projection differs from the other layers"),
      "Please provide a raster using the same projection as the catchments.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(FALSE)
  }
  TRUE
}

# read_tif_from_csv: Read raster file from CSV
read_tif_from_csv <- function(csv_file, layer_name, catch_crs) {
  req(csv_file)  # Ensure the CSV file is provided
  csv_data <- read.csv(csv_file$datapath)
  
  la <- terra::rast(csv_data$Path[csv_data$Layer == layer_name])
  # Stop the observer here (after a pop-up) if the projection is missing, wrong or differs from catchments
  req(check_raster_crs(la, layer_name, catch_crs))
  return(la)
}

# read_tif_from_upload: Read raster file from fileInput
read_tif_from_upload <- function(upload_input, catch_crs = NULL) {
  req(upload_input)  # Ensure the file is uploaded
  path <- upload_input$datapath
  if (file.exists(path)) {
    r <- terra::rast(path)
    # Stop the observer here (after a pop-up) if the projection is missing, wrong or differs from catchments
    req(check_raster_crs(r, upload_input$name, catch_crs))
    return(r)
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

#########################################################
# Criteria rasters on the map
#########################################################
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
  aggregated <- terra::aggregate(input_raster, fact = fact, fun = aggregation_fun)
  projected <- project(aggregated, "EPSG:4326")
  terra::writeRaster(projected, projected_path, filetype = "GTiff")
  
  list(original = masked, projected = projected)
}

# LCC class labels and colours (read once at start-up)
LCC_CLASSES <- data.frame(values = c(1,2,5,6,8,10,11,12,13,14,15,16,17,18,19),
                          labels = c("Temperate conifer forest", "Taiga conifer forest",
                                     "Broadleaf forest", "Mixed Forest", "Shrubland", "Grassland",
                                     "Shrubland-lichen-moss", "Grassland-lichen-moss","Barren-lichen-moss",
                                     "Wetland", "Cropland", "Barren Lands", "Urban", "Water", "Snow"))
lc_cols_tab <- read.csv("www/lc_cols.csv")
LCC_COLS <- setNames(rgb(lc_cols_tab$red, lc_cols_tab$green, lc_cols_tab$blue, maxColorValue = 255), lc_cols_tab$value)

# lcc_class_labels: LCC classes present in a raster (used for dissimilarity and legend)
# If cache_file is given, classes are read from it when it exists, otherwise computed and saved to it
lcc_class_labels <- function(lcc, cache_file = NULL) {
  if (!is.null(cache_file) && file.exists(cache_file)) {
    present <- read.csv(cache_file)$value
  } else {
    present <- as.data.frame(terra::freq(lcc, bylayer = FALSE))$value
    if (!is.null(cache_file)) write.csv(data.frame(value = present), cache_file, row.names = FALSE)
  }
  LCC_CLASSES[LCC_CLASSES$values %in% present, ]
}

# raster_range: min/max of a raster without loading all cells in R
raster_range <- function(r) {
  terra::setMinMax(r)
  rg <- as.numeric(terra::minmax(r))
  if (any(!is.finite(rg))) rg <- range(terra::values(r), na.rm = TRUE)
  rg
}

# prep_legend_fast: legend bins/labels from the display (4326) rasters
prep_legend <- function(cmi, led, gpp, lcc, crit5 = NULL) {
  bins <- function(r) {
    rg <- raster_range(r)
    seq(floor(rg[1]), ceiling(rg[2]), length.out = 5)
  }
  df_label <- lcc_class_labels(lcc)
  list(cmi_bins = bins(cmi), led_bins = bins(led), gpp_bins = bins(gpp),
       crit_bins = if (!is.null(crit5)) bins(crit5),
       lcc_labels = df_label$labels,
       lcc_cols = unname(LCC_COLS[as.character(df_label$values)]))
}

# add criteria rasters and their legends to a leaflet proxy
add_criteria_layers <- function(map, r, leg, crit_name = NULL) {
  pal <- function(b, p) leaflet::colorBin(p, b, bins = b, na.color = "transparent")
  lab <- function(type, breaks) c("Low", "", "", "High")

  map <- map %>%
    clearGroup(c("CMI", "GPP", "LED", "LCC", crit_name)) %>%
    removeControl(c("legendCMI", "legendGPP", "legendLED", "legendLCC", "legendCustom")) %>%
    addRasterImage(r$lcc, colors = leg$lcc_cols, opacity = 1, group = "LCC", maxBytes = 10 * 1024 * 1024) %>%
    addRasterImage(r$led, colors = "Blues", opacity = 1, group = "LED", maxBytes = 10 * 1024 * 1024) %>%
    addRasterImage(r$gpp, colors = "RdYlBu", opacity = 1, group = "GPP", maxBytes = 10 * 1024 * 1024) %>%
    addRasterImage(r$cmi, colors = "RdYlBu", opacity = 1, group = "CMI", maxBytes = 10 * 1024 * 1024) %>%
    addLegend(pal = pal(leg$cmi_bins, "RdYlBu"), values = leg$cmi_bins, opacity = 1, title = "CMI",
              position = "bottomright", group = "CMI", layerId = "legendCMI", labFormat = lab) %>%
    addLegend(pal = pal(leg$gpp_bins, "RdYlBu"), values = leg$gpp_bins, opacity = 1, title = "GPP",
              position = "bottomright", group = "GPP", layerId = "legendGPP", labFormat = lab) %>%
    addLegend(pal = pal(leg$led_bins, "Blues"), values = leg$led_bins, opacity = 1, title = "LED",
              position = "bottomright", group = "LED", layerId = "legendLED", labFormat = lab) %>%
    addLegend(colors = leg$lcc_cols, labels = leg$lcc_labels, opacity = 1, title = "LCC",
              position = "bottomleft", group = "LCC", layerId = "legendLCC")

  if (!is.null(crit_name) && !is.null(r$crit5)) {
    map <- map %>%
      addRasterImage(r$crit5, colors = "RdYlBu", opacity = 1, group = crit_name, maxBytes = 10 * 1024 * 1024) %>%
      addLegend(pal = pal(leg$crit_bins, "RdYlBu"), values = leg$crit_bins, opacity = 1, title = crit_name,
                position = "bottomright", group = crit_name, layerId = "legendCustom", labFormat = lab)
  }
  map
}

# draw criteria rasters once; skipped when already on the map
draw_criteria <- function(rv, cmi, led, gpp, lcc, crit5 = NULL) {
  
  if (isTRUE(rv$criteria_drawn())) return(invisible(FALSE))
  if (is.null(rv$legend_data())) {
    rv$legend_data(prep_legend(cmi, led, gpp, lcc, crit5))
  }
  leafletProxy("map") %>%
    add_criteria_layers(list(cmi = cmi, led = led, gpp = gpp, lcc = lcc, crit5 = crit5),
                        rv$legend_data(), rv$criteria5name())
  rv$criteria_drawn(TRUE)
  invisible(TRUE)
}

# force criteria rasters and legends to be recomputed and redrawn
reset_criteria <- function(rv) {
  rv$criteria_drawn(FALSE)
  rv$legend_data(NULL)
}

.make_metric_progress <- function(n_metrics) {
  metric_number <- 0L
  function(metric_name) {
    metric_number <<- metric_number + 1L
    start <- (metric_number - 1L) / n_metrics
    end <- metric_number / n_metrics
    function(value, detail) {
      shiny::setProgress(
        value = start + (end - start) * value,
        detail = paste0(metric_name, ": ", detail)
      )
    }
  }
}

.copy_contents <- function(source_dir, destination_dir) {
  if (!dir.exists(source_dir)) {
    return(invisible(FALSE))
  }
  
  dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
  items <- list.files(source_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE, recursive = TRUE)
  
  for (item in items) {
    relative_path <- substring(item, nchar(source_dir) + 2)
    destination <- file.path(destination_dir, relative_path)
    
    if (dir.exists(item)) {
      dir.create(destination, recursive = TRUE, showWarnings = FALSE)
    } else {
      dir.create(dirname(destination), recursive = TRUE,showWarnings = FALSE)
      file.copy(item, destination, overwrite = FALSE)
    }
  }
  
  invisible(TRUE)
}
