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

#########################################################
# Projection (CRS) check across all input layers
#########################################################
# The app never reprojects input layers: all of them must already share one projection.

# layer_crs: sf crs of an sf object or a terra SpatRaster (NA if the layer has no projection)
layer_crs <- function(x) {
  if (inherits(x, "SpatRaster")) {
    wkt <- terra::crs(x)
    if (!nzchar(wkt)) return(sf::NA_crs_)
    return(sf::st_crs(wkt))
  }
  sf::st_crs(x)
}

# crs_label: readable projection name, e.g. "NAD83 / Canada Atlas Lambert (EPSG:3978)".
# Without an EPSG code the name alone can hide differences (two custom projections both called
# "NAD_1983_Albers" with different parameters), so the PROJ parameters are added.
crs_label <- function(crs) {
  if (is.na(crs)) return("No projection (.prj missing or not recognised)")
  name <- crs$Name
  if (is.null(name) || is.na(name) || !nzchar(name)) name <- crs$input
  epsg <- crs$epsg
  if (!is.null(epsg) && !is.na(epsg)) return(paste0(name, " (EPSG:", epsg, ")"))
  proj <- crs$proj4string
  if (!is.null(proj) && !is.na(proj) && nzchar(proj)) {
    proj <- trimws(gsub("\\+(no_defs|type=crs)", "", proj))
    name <- paste0(name, " [", proj, "]")
  }
  name
}

# input_layers: named list of the input layers currently loaded (NULL = not loaded)
input_layers <- function(rv) {
  crit5_name <- rv$criteria5name()
  if (is.null(crit5_name) || !nzchar(crit5_name)) crit5_name <- "Custom criterion"
  layers <- list(
    "Catchments"      = rv$layers_rv$catchments,
    "Streams"         = rv$layers_rv$streams,
    "Planning region" = rv$layers_rv$planreg,
    "Protected areas" = rv$layers_rv$pas_sf,
    "Reference area"  = rv$refarea_reactive(),
    "LCC"             = rv$layers_rv$lcc,
    "LED"             = rv$layers_rv$led,
    "GPP"             = rv$layers_rv$gpp,
    "CMI"             = rv$layers_rv$cmi
  )
  layers[[crit5_name]] <- rv$layers_rv$criteria5
  layers
}

# check_crs_consistency: compare the projection of all layers (named list of sf / SpatRaster, NULLs ignored).
# All the same -> TRUE, no pop-up.
# Otherwise -> pop-up listing every layer with its projection, the ones that differ in red, and FALSE.
check_crs_consistency <- function(layers) {
  layers <- Filter(Negate(is.null), layers)
  if (length(layers) == 0) return(TRUE)

  crs_list <- lapply(layers, layer_crs)

  # Group layers sharing the same projection (sf compares CRS by meaning, not by text)
  group <- rep(NA_integer_, length(crs_list))
  reps  <- list()
  for (i in seq_along(crs_list)) {
    if (is.na(crs_list[[i]])) next
    hit <- which(vapply(reps, function(r) isTRUE(r == crs_list[[i]]), logical(1)))
    if (length(hit) > 0) {
      group[i] <- hit[1]
    } else {
      reps[[length(reps) + 1]] <- crs_list[[i]]
      group[i] <- length(reps)
    }
  }
  if (!anyNA(group) && length(reps) <= 1) return(TRUE)

  # The projection shared by most layers is the reference; the others are flagged
  main    <- if (all(is.na(group))) NA_integer_ else as.integer(names(which.max(table(group))))
  differs <- is.na(group) | group != main
  differs[is.na(differs)] <- TRUE

  rows <- lapply(seq_along(layers), function(i) {
    style <- if (differs[i]) "color: #c0392b; font-weight: bold;" else ""
    tags$tr(tags$td(style = style, names(layers)[i]),
            tags$td(style = style, crs_label(crs_list[[i]])))
  })

  showModal(modalDialog(
    title = "The layers do not all have the same projection",
    size = "l",
    tags$p("All input layers (shapefiles and rasters) must use the same projection.",
           "The layers in red differ from the others:"),
    tags$table(class = "table table-condensed",
               tags$thead(tags$tr(tags$th("Layer"), tags$th("Projection"))),
               tags$tbody(rows)),
    tags$p("Please reproject these layers outside the app (e.g., in QGIS or ArcGIS) so that all layers",
           "share the same projection, then upload them again."),
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
  FALSE
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
  terra::rast(csv_data$Path[csv_data$Layer == layer_name])
}

# read_tif_from_upload: Read raster file from fileInput
read_tif_from_upload <- function(upload_input) {
  req(upload_input)  # Ensure the file is uploaded
  path <- upload_input$datapath
  if (file.exists(path)) {
    return(terra::rast(path))
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
