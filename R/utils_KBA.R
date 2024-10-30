make_catchnum_integer <- function(catchments_sf){
  
  # check CATCHNUM exists
  if(!"CATCHNUM" %in% names(catchments_sf)){
    stop("Catchments must contain column 'CATCHNUM'")
  }
  
  catchments_sf$CATCHNUM <- as.integer(catchments_sf$CATCHNUM)
  
  return(catchments_sf)
}

check_for_geometry <- function(in_sf){
  
  if(!"geometry" %in% names(in_sf)){
    stop("Must contain column: geometry")
  }
}

logical_to_integer <- function(x){
  
  if(!x %in% c(TRUE, FALSE)){
    stop("input must be TRUE or FALSE")
  }
  
  return(as.integer(x))
}

check_seeds_areatargets <- function(seeds){
  if(!all(seeds$Areatarget > 0)){
    stop("All Areatarget values must be > 0")
  }
}

check_seeds_in_catchments <- function(seeds, catchments_sf){
  if(!all(seeds$CATCHNUM %in% catchments_sf$CATCHNUM)){
    if(any(seeds$CATCHNUM %in% catchments_sf$CATCHNUM)){
      warning("Not all seeds are in catchments_sf") # warning if some are present and builder can run
    } else{
      stop("None of the seeds are in catchments_sf") # error if no seeds are present
    }
  }
}

check_colnames <- function(x, x_name, cols){
  for(col in cols){
    if(!col %in% colnames(x)){
      stop(paste0("Column '", col, "' not in table '", x_name, "'"))
    }
  }
}

make_all_integer <- function(x, cols = NULL){
  if(is.null(cols)){
    colss <- colnames(x)
  }else{
    colss <- cols
  }
  for(col in colss){
    if(col %in% colnames(x)){
      if(!is.integer(x[[col]])){
        warning(paste0("is.integer(", col, ") == FALSE; converting to integer"))
        x[[col]] <- as.integer(x[[col]])
      }
    }
  }
  return(x)
}

make_all_numeric <- function(x, cols = NULL){
  if(is.null(cols)){
    colss <- colnames(x)
  }else{
    colss <- cols
  }
  for(col in colss){
    if(col %in% colnames(x)){
      if(!is.numeric(x[[col]])){
        warning(paste0("is.numeric(", col, ") == FALSE; converting to numeric"))
        x[[col]] <- as.numeric(x[[col]])
      }
    }
  }
  return(x)
}

make_all_character <- function(x, cols = NULL){
  if(is.null(cols)){
    colss <- colnames(x)
  }else{
    colss <- cols
  }
  for(col in colss){
    if(col %in% colnames(x)){
      if(!is.character(x[[col]])){
        warning(paste0("is.character(", col, ") == FALSE; converting to character"))
        x[[col]] <- as.character(x[[col]])
      }
    }
  }
  return(x)
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

