#' Graticules and Degree Labels
#'
#' Add graticules, degree labels and a frame to a map plot.
#'
#' @details
#' \describe{
#'   \item{Graticules}{inspired by \url{https://gist.github.com/valentinitnelav/c7598fcfc8e53658f66feea9d3bafb40}}
#' }
#' @export
#' @param data If `NULL` uses [graticules()] and [degrees()] to compute the
#' required data.
geom_graticule <- function(mapping = NULL, data = NULL, stat = "identity",
   position = "identity", proj = NULL, long_0 = 0,
   lat_min = -80, lat_max = 80, lat_by = 20, lat_n = NULL,
   long_min = -180 + long_0, long_max = 180 + long_0, long_by = 40,
   long_n = NULL, ..., na.rm = TRUE, show.legend = NA, inherit.aes = FALSE){

  default_aes <- ggplot2::aes_(x=~long, y=~lat, group=~group)
  mapping = aes_intersect(mapping, default_aes)

  data <- data %||% graticules(proj = proj, long_0 = long_0, lat_min = lat_min,
    lat_max = lat_max, lat_by = lat_by, lat_n = lat_n, long_min = long_min,
    long_max = long_max, long_by = long_by, long_n = long_n)

  ggplot2::geom_path(mapping = mapping, data = data, stat = stat, position = position,
    ..., na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
}

#' @rdname geom_graticule
#' @export
geom_gratframe <- function(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", proj = NULL, long_0 = 0, lat_min = -80,
    lat_max = 80, long_min = -180 + long_0, long_max = 180 + long_0,
    ..., na.rm = TRUE, show.legend = NA, inherit.aes = FALSE){

  default_aes <- ggplot2::aes_(x=~long, y=~lat, group=~group)
  mapping = aes_intersect(mapping, default_aes)

  data <- data %||% graticules(proj = proj, long_0 = long_0, lat_min = lat_min,
    lat_max = lat_max, lat_by = NULL, lat_n = 2, long_min = long_min,
    long_max = long_max, long_by = NULL, long_n = 2)

  ggplot2::geom_path(mapping = mapping, data = data, stat = stat, position = position,
    ..., na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
}

#' @rdname geom_graticule
#' @export
graticules <- function(proj = NULL, long_0 = 0, lat_min = -80, lat_max = 80,
    lat_by = 20, lat_n = NULL, long_min = -180 + long_0, long_max = 180 + long_0,
    long_by = 40, long_n = NULL){

  lat_breaks <- compute_breaks(lat_min, lat_max, lat_by, lat_n)
  long_breaks <- compute_breaks(long_min, long_max, long_by, long_n)

  # create a bounding box and assign CRS to box
  box <- as(raster::extent(long_min, long_max, lat_min, lat_max), "SpatialPolygons")

  # create graticules/grid lines from box
  # Spatial Lines
  gl_args <- list(box)
  if(length(lat_breaks > 0)) gl_args$norths <- lat_breaks
  if(length(long_breaks > 0)) gl_args$easts <- long_breaks

  graticules <- do.call(sp::gridlines, gl_args) %>% sl2df

  project(graticules, proj, long_0)
}

sl2df <- function(x){
  x <- sp::SpatialLinesDataFrame(x, data=data.frame(1:length(x)), match.ID = FALSE)
  suppressWarnings(ggplot2::map_data(x)) %>% as_tibble
}
