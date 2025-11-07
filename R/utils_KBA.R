make_catchnum_integer <- function(catchments_sf){
  
  # check CATCHNUM exists
  if(!"CATCHNUM" %in% names(catchments_sf)){
    stop("Catchments must contain column 'CATCHNUM'")
  }
  
  catchments_sf$CATCHNUM <- as.integer(catchments_sf$CATCHNUM)
  
  return(catchments_sf)
}

check_catchnum <- function(catchments_sf){
  
  # check CATCHNUM exists
  if(!"CATCHNUM" %in% names(catchments_sf)){
    stop("Catchments must contain column 'CATCHNUM'")
  }
}

# gen_network_names_zone
#' Create a vector of network names sepcifying the zone.
#'
#' Takes a vector of conservation area names and combines them into network names using the separator: \code{"__"}.
#'
#' Conservation area names should never include the separator: \code{"__"}, however \code{"_"} is acceptable.
#' 
#' By default all combinations of conservation area names will be created based on the \code{k} parameter. 
#' The length of the output will therefore equal \code{choose(length(in_names), k)}.
#' 
#' Input names are ordered in the network name based using \code{sort()}.
#' 
#' The output vector of names can be filtered using force_in to include specific names.
#'
#' @param in_names List of vector of input names. Usually the names of conservation areas from the beaconsbuilder package (e.g. PB_0001) and/or user provided reserves (e.g. PA_1).
#' @param k Integer > 0 and <= \code{length(in_names)}. Count of names to appear in each output network name.
#' @param force_in Vector of names to force in to the output vector (usually a subset of in_names but could be a substring of an output 
#' name, see examples). Only network names containing one of these vectors will be included in the output.
#'
#' @return A vector of network names.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' gen_network_names(c("PB_1", "PB_2", "PB_3", "PB_11"), 2)
#' 
#' gen_network_names(c("PB_1", "PB_2", "PB_3", "PB_11"), 2, "PB_1")
#' 
#' gen_network_names(c("PB_1", "PB_2", "PB_3", "PB_11"), 3, c("PB_1", "PB_11"))
#' 
#' gen_network_names(c("PB_1", "PB_2", "PB_3", "PB_11"), 3, "PB_1__PB_11")

gen_network_names <- function(in_names, k, force_in = c()){
  
  # check k
  if(k < 1){
    stop("k should be 1 or more")
  }
  if(k > length(in_names)){
    stop("k cannot be > length(in_names)")
  }
  
  names_combined <- utils::combn(sort(as.character(in_names)), k, simplify=FALSE) # simplify=FALSE returns a list
  out_names <- sapply(names_combined, function(x) paste0(x,collapse="__"))
  
  if(length(force_in) > 0){
    out_names <- unique(unlist(lapply(force_in, function(x){
      c(
        out_names[grepl(paste0(x, "__"), out_names)], # matches the name followed by __
        out_names[grepl(paste0(x, "$"), out_names)] # matches the name when it ends the string
        # this regexp assumes no in_name is a complete duplicate of another in_name.
        # For example this will fail if there are two reserves named "a" and "aa" and you try to
        # force_in "a".
      )
    })))
  }
  return(out_names)
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

### group_conservation_areas ###
#
#' Group conservation areas based on overlap using a grid.
#' 
#' Groups are assigned based on polygon centroid membership within 
#' an intersecting grid. Centroids in the same grid cell are assigned the same group id.
#'
#' @param conservation_areas_sf sf object of conservation areas.
#' @param grid_size Numeric size of grid cells to create in units matching the crs of \code{conservation_areas_sf}.
#'
#' @return Vector of group ids matching the input polygons.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' reserves <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   builder_table_sample,
#'   "network", 
#'   dissolve_list = c("PB_0001", "PB_0002", "PB_0003"))
#' group_conservation_areas(reserves, 10000)
group_conservation_areas <- function(conservation_areas_sf, grid_size){
  
  sf::st_agr(conservation_areas_sf) = "constant"
  
  # Make centroids - or pointOnPoly
  points_sf <- conservation_areas_sf %>%
    sf::st_centroid()
  
  # Make grid covering points
  grid_sf <- sf::st_make_grid(points_sf, cellsize = c(grid_size, grid_size), what = 'polygons')
  grid_sf <- sf::st_sf(geometry = grid_sf, data.frame('grid_id' = 1:length(grid_sf)))
  
  # remove grid_id if it already exists
  if("grid_id" %in% colnames(points_sf)){
    points_sf <- points_sf %>%
      dplyr::select(-.data$grid_id)
  }
  
  # spatial join fishnet grid id to conservation areas
  points_sf <- points_sf %>%
    sf::st_join(grid_sf)
  
  # convert grid ids to ordered vector starting at 1
  mappings <- data.frame(grid_id = unique(points_sf$grid_id), new_val = 1:length(unique(points_sf$grid_id)))
  
  points_sf <- points_sf %>%
    dplyr::left_join(mappings, by = "grid_id")
  
  return(points_sf$new_val)
}

