check_proj <-  function(obj1, obj2){
  target_crs <- NULL
  
  if (inherits(obj2, "sf") || inherits(obj2, "sfc") || inherits(obj2, "SpatVector")) {
    target_crs <- st_crs(obj2)
  } else if (inherits(obj2, "SpatRaster")) {
    target_crs <- st_crs(crs(obj2, proj=TRUE)) # convert terra CRS to st_crs
  } else if (is.character(obj2) || is.numeric(obj2)) {
    target_crs <- st_crs(obj2)
  } else {
    stop("obj2 must be a spatial object (sf, SpatVector, SpatRaster), a character CRS, or a numeric EPSG code")
  }
  
  # Reproject if needed
  if (!st_crs(obj1) == target_crs) {
    obj1 <- st_transform(obj1, crs = target_crs)
  }
  
  return(obj1)
}


geometric_mean <- function(netLayer, netName, ras, ras_name){

  # Make sure geom exists. Rename if it doesn't
  geom_idx <- which(names(netLayer) == attr(netLayer, "sf_column"))
  names(netLayer)[geom_idx] <- "geom"
  st_geometry(netLayer) <- "geom"
  
  # dissolve the networks to get polygons for extract
  net_shp_dslv <- netLayer %>%
    group_by(netName) %>%
    summarise(geom = st_union(geom))
  
  # split benchmarks into blocks of 10 for processing
  net_list <- unique(as.character(netLayer[[netName]]))
  net_list_grouped <- split(net_list, ceiling(seq_along(net_list)/10))
  
  # run in blocks of 10 - seems optimal for maintaining a fast extract
  counter <- 1
  for(net_list_g in net_list_grouped){
    
    print(paste0("Block ", counter, " of ", length(net_list_grouped)))
    counter <- counter + 1
    
    net_shp_g <- net_shp_dslv[net_shp_dslv$netName %in% net_list_g,] # subset dissolved networks by the block of netnames
    x <- exact_extract(ras, net_shp_g) # extract
    
    names(x) <- net_list_g # name the list elements by their associated netname
    
    for(net in net_list_g){
      
      # for each network in the block, extract the values...
      vel_vals <- x[[net]] %>% # get the data frame of values for the network
        filter(coverage_fraction > 0.5) %>% # only keep values from cells with at least half their area in the polygon
        pull(value)
      
      result <- round(psych::geometric.mean(vel_vals, na.rm=T),3)
      netLayer[[paste0("gm_",ras_name)]][netLayer[[netName]] == net] <- result
    }
  }
  return(netLayer)
}


area_from_raster <- function(netLayer, netName, ras, ras_name, values = NULL) {
  cellarea_km2 <- prod(res(ras)) / 1e6     # m²

  # Make sure geom exists. Rename if it doesn't
  geom_idx <- which(names(netLayer) == attr(netLayer, "sf_column"))
  names(netLayer)[geom_idx] <- "geom"
  st_geometry(netLayer) <- "geom"
  
  # dissolve the networks to get polygons for extract
  net_shp_dslv <- netLayer %>%
    group_by(netName) %>%
    summarise(geom = st_union(geom))

  # initialize output column
  netLayer[[ras_name]] <- NA_real_
  
  # split networks into blocks
  net_list <- unique(as.character(net_shp_dslv[[netName]]))
  net_list_grouped <- split(net_list, ceiling(seq_along(net_list) / 10))
  
  counter <- 1
  for (net_list_g in net_list_grouped) {
    
    message("block ", counter, " of ", length(net_list_grouped))
    counter <- counter + 1
    
    net_shp_g <- net_shp_dslv[
      net_shp_dslv[[netName]] %in% net_list_g, ]
    
    x <- exact_extract(ras, net_shp_g)
    names(x) <- net_list_g
    
    for (net in net_list_g) {
      
      df <- x[[net]]
      
      area_km2 <- sum(
        df$coverage_fraction[df$value %in% values],
        na.rm = TRUE
      ) * cellarea_km2
      
      netLayer[[ras_name]][netLayer[[netName]] == net] <- area_km2
    }
  }
  
  netLayer
}
  

area_from_vector <- function(netLayer, netName, vecLayer, vec_name) {

  # ensure same CRS
  if (st_crs(netLayer) != st_crs(vecLayer)) {
    vecLayer <- st_transform(vecLayer, st_crs(netLayer))
  }
  
  # ensure geometry column is named "geom"
  geom_idx <- which(names(netLayer) == attr(netLayer, "sf_column"))
  names(netLayer)[geom_idx] <- "geom"
  st_geometry(netLayer) <- "geom"

  # ensure geometry column is named "geom"
  geom_idx <- which(names(vecLayer) == attr(vecLayer, "sf_column"))
  names(vecLayer)[geom_idx] <- "geom"
  st_geometry(vecLayer) <- "geom"

  # dissolve networks
  net_shp_dslv <- netLayer %>%
    group_by(.data[[netName]]) %>%
    summarise(geom = st_union(geom), .groups = "drop")
  
  # dissolve vectLayer
  lyr_dslv <- vecLayer %>%
    summarise(geom = st_union(geom), .groups = "drop")

    # intersect
  inter <- st_intersection(net_shp_dslv, lyr_dslv)
  
  # area in km²
  inter$area_km2 <- as.numeric(st_area(inter)) / 1e6
  
  # sum per network
  area_tbl <- inter %>%
    group_by(.data[[netName]]) %>%
    summarise(area_km2 = sum(area_km2), .groups = "drop")
  
  # output column name
  netLayer[[vec_name]] <- NA_real_
  
  # join back
  netLayer[[vec_name]][match(netLayer[[netName]], area_tbl[[netName]])] <- area_tbl$area_km2
  
  return(netLayer)
}

### calc_dissimilarity ###
#
#' Calculate dissimilarity values between a set of polygons and a reference area.
#'
#' For a list of features (e.g. conservation areas or networks), calculate the dissimilarity value between each feature and a reference area for the 
#' provided raster layer. Continuous rasters use the KS-statistic to compare distributions, categorical rasters use the Bray-Curtis
#' statistic. Graphs comparing distributions can optionally be created and saved to a user provided file path.
#' 
#' NA values are always removed. For categorical rasters, values can optionally be subset for the calculation and graphs using 
#' \code{categorical_class_values}.
#'
#' @param reserves_sf sf object with unique id column named \code{network}
#' @param reserves_id String matching the unique identifier column in .
#' @param reference_sf sf object of the reference area to compare against.
#' @param raster_layer Raster object that will be clipped to the reference and reserve areas, with crs matching reserves_sf
#' @param raster_type 'categorical' will use Bray-Curtis, 'continuous' will use KS-statistic.
#' @param categorical_class_values Vector of raster values in \code{raster_layer} of type 'categorical' to include in the calculation. 
#' Allows unwanted values to be dropped. Defaults to include all non-NA values.
#' @param plot_out_dir Path to folder in which to save plots. Default is not to create plots. 
#' Only creates plots if valid file path is provided. Dir will be created if it doesn't exist.
#' @param categorical_class_labels Optional data.frame object with columns \code{values} and \code{labels} indicating the label to use in Bray-Curtis graphs
#' for each raster value. Defaults to using the raster values. Labels can be provided for all or a subset of values. See examples.
#'
#' @return A vector of dissimilarity values matching the order of the input \code{reserves_sf}. Optionally a dissimilarity plot saved in the 
#' \code{plot_out_dir} for each computed value.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats ks.test
#' @export
#'
#' @examples
#' reserves <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   builder_table_sample, 
#'   "network",
#'   dissolve_list = c("PB_0001", "PB_0002", "PB_0003"))
#' calc_dissimilarity(reserves, "network", ref_poly, led_sample, 'categorical')
#' calc_dissimilarity(reserves, , ref_poly, led_sample, 'categorical', c(1,2,3,4,5))
#' calc_dissimilarity(reserves, "network", ref_poly, led_sample, 'categorical', c(1,2,3,4,5), 
#'   "C:/temp/plots", data.frame(values=c(1,2,3,4,5), labels=c("one","two","three","four","five")))
#' calc_dissimilarity(reserves, ref_poly, led_sample, 'continuous', plot_out_dir="C:/temp/plots")
calc_dissimilarity <- function(
    reserves_sf, reserves_id=NULL, reference_sf,
    raster_layer, raster_type,
    categorical_class_values=c(),
    plot_out_dir=NULL,
    categorical_class_labels=data.frame()
){
  stopifnot(st_crs(reserves_sf) == st_crs(reference_sf))
  stopifnot(st_crs(reserves_sf) == st_crs(raster_layer))
  
  make_plots <- !is.null(plot_out_dir)
  if (make_plots) {
    dir.create(plot_out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # ---------------------------------------
  # 1. Extract reference values ONCE
  # ---------------------------------------
  ref_ext <- exactextractr::exact_extract(raster_layer, reference_sf, progress = FALSE)[[1]]
  
  # Convert to fast logical vector operations
  keep <- !is.na(ref_ext$value) & ref_ext$coverage_fraction > 0.5
  
  if (raster_type == "categorical" && length(categorical_class_values) > 0) {
    keep <- keep & ref_ext$value %in% categorical_class_values
  }
  
  reference_vals <- ref_ext$value[keep]
  
  # ---------------------------------------
  # 2. Setup IDs and results
  # ---------------------------------------
  net_ids <- as.character(reserves_sf[[reserves_id]])
  n_res <- length(net_ids)
  result_vector <- numeric(n_res)   # preallocate
  
  # group into chunks of 10
  groups <- split(net_ids, ceiling(seq_along(net_ids)/10))
  
  out_i <- 1
  
  # ---------------------------------------
  # 3. Process each block of 10 networks
  # ---------------------------------------
  for (grp in groups) {
    
    message("processing ", grp[1], " ...")
    
    # Subset reserves once per block
    reserves_sf_block <- reserves_sf[reserves_sf[[reserves_id]] %in% grp, ]
    
    # Extract once per block
    ext_list <- exactextractr::exact_extract(raster_layer, reserves_sf_block, progress = FALSE)
    names(ext_list) <- grp
    
    # -------------------------------------
    # 4. For every reserve in the block
    # -------------------------------------
    for (id in grp) {
      
      dat <- ext_list[[id]]
      
      keep <- !is.na(dat$value) & dat$coverage_fraction > 0.5
      if (raster_type == "categorical" && length(categorical_class_values) > 0) {
        keep <- keep & dat$value %in% categorical_class_values
      }
      
      target_vals <- dat$value[keep]
      
      # run dissimilarity (already optimized)
      if (raster_type == "categorical") {
        result <- bc_stat(reference_vals, target_vals)
      } else {
        result <- ks_stat(reference_vals, target_vals)
      }
      
      result_vector[out_i] <- result
      out_i <- out_i + 1
      
      # plot
      if (make_plots) {
        outf <- file.path(plot_out_dir, paste0(id, ".png"))
        if (raster_type == "categorical") {
          plt <- bc_plot(reference_vals, target_vals,
                         plotTitle = paste0(id, " (BC=", result, ")"),
                         labels=categorical_class_labels)
        } else {
          plt <- ks_plot(reference_vals, target_vals,
                         plotTitle = paste0(id, " (KS=", result, ")"))
        }
        ggplot2::ggsave(outf, plt)
      }
    }
  }
  
  result_vector
}