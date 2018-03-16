#' @export
as_SpatialPolygons <- function(x){
  UseMethod("as_SpatialPolygons")
}
#' @export
as_SpatialPolygons.map <- function(x){
  maptools::map2SpatialPolygons(x, IDs=sapply(strsplit(x$names, ":"), "[", 1L))
}




#' @export
as_map <- function(x, ...){
  UseMethod("as_map")
}
#' @export
as_map.default <- function(x, region = ".", exact = FALSE, plot = FALSE, ...){
  stop("Don't know how to coerce object of class '", class(x), "' into a map")
}
#' @export
as_map.SpatialLines <- function(x, region = ".", exact = FALSE, plot = FALSE, ...){
  spdf <- sp::SpatialLinesDataFrame(x, data=data.frame(1:length(x)),
                                    match.ID = FALSE)
  as_map(spdf, region = region, exact = exact, plot = plot, ...)
}
#' @export
as_map.SpatialLinesDataFrame <- function(x, region = ".", exact = FALSE, plot = FALSE, ...){
  maps::map(x, region = region, exact = exact, plot = plot, fill = TRUE, ...)
}
#' @export
as_map.SpatialPolygons <- function(x, region = ".", exact = FALSE, plot = FALSE, ...){
  as_map(as(x, "SpatialPolygonsDataFrame"),  region = region, exact = exact, plot = plot, ...)
}
#' @export
as_map.SpatialPolygonsDataFrame <- function(x, region = ".", exact = FALSE, plot = FALSE, ...){
   suppressWarnings(maps::map(x, region = region, exact = exact, plot = plot, 
                       fill = TRUE, ...))
}


#' @export
as_tibble.map <- function(x, ...){
  tibble::as_tibble(ggplot2::fortify(x))
}
#' @export
as_map_tibble <- function(x, region = ".", exact = FALSE, ...){
  UseMethod("as_map_tibble")
}
#' @export
as_map_tibble.map <- function(x){
  as_tibble(x)
}
#' @export
as_map_tibble.default <- function(x, region = ".", exact = FALSE, ...){
  as_map_tibble(as_map(x))
}

