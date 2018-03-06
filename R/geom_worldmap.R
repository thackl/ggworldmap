#' resplit the map polygons at the new edges for rotate shift
#' this is magic to fix polygons running outside the shifted world bounds
#' https://seethedatablog.wordpress.com/2016/12/31/r-shift-centralprime-meridian-of-world-map/
#' @export
geom_worldmap <- function(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", map = 'world', proj = NULL, long_0 = 0, long_min = -180 + long_0,
    long_max = 180 + long_0, lat_min = -90, lat_max = 90,
    proj_extra = NULL, ..., na.rm = FALSE, show.legend = NA,
    inherit.aes = FALSE){

  default_aes <- ggplot2::aes_(x=~long, y=~lat, group=~group)
  mapping = aes_intersect(mapping, default_aes)

  data <- data %||% worldmap(map = map, proj = proj, long_0 = long_0, long_min = long_min,
      long_max = long_max, lat_min = lat_min, lat_max = lat_max, proj_extra = proj_extra)

  ggplot2::geom_polygon(mapping = mapping, data = data, stat = stat,
      position = position, ..., na.rm = na.rm, show.legend = show.legend,
      inherit.aes = inherit.aes)
}
#' @export
worldmap <- function(map = "world", proj = NULL, long_0 = 0, long_min = -180 + long_0,
    long_max = 180 + long_0, lat_min = -90, lat_max = 90, proj_extra = NULL, fill=TRUE, plot=FALSE,
    ...){

  # get map as SpatialPolygons
  if(is.character(map)){
    map <- map2sp(maps::map(map, fill=fill, plot=plot))
  } else if(!is(map, "SpatialPolygons")) {
    stop("unknown map format")
  }

  long_0 <- long_0 %||% 0
  long_max <- long_max %||% ( 180 + long_0)
  long_min <- long_min %||% (-180 + long_0)
  lat_min <- lat_min %||% -90
  lat_max <- lat_max %||% 90

  d <- .00001 # nugde crop off by this tiny bit to prevent overlaps between boxes
  # split/crop & bind to match long_0
  if(long_min < -180 && long_max > 180){ # running outside the default polygon map
    stop("Cannot handle long_min and long_max both outside -180:180")
  }else if(long_min < -180 || long_max > 180){ # running outside the default polygon map
    map_a <- map %>% crop(wrap_dd(long_min) + d, 180, lat_min, lat_max)
    map_b <- map %>% crop(-180, wrap_dd(long_max) - d, lat_min, lat_max)
    map <- rbind(map_a, map_b, makeUniqueIDs = TRUE)
  }else{
    map <- map %>% crop(long_min, long_max, lat_min, lat_max)
  }

  sp2df(map) %>%
    smooth_crop_lines(long_min, long_max, d = d*2) %>%
    project(proj, long_0, proj_extra = proj_extra)
}

sp_box <- function(long_min, long_max, lat_min = -90, lat_max = 90,
                     proj = "+proj=longlat +datum=WGS84"){
  box <- as(raster::extent(long_min, long_max, lat_min, lat_max), "SpatialPolygons")
  sp::proj4string(box) <- proj
  box
}

sp_line <- function(long_0 = NULL, proj = "+proj=longlat +datum=WGS84"){
  long_0 %||% stop("long_0 required")
  sp::SpatialLines(list(sp::Lines(list(sp::Line(cbind(180+long_0,c(-90,90)))), ID="line")), proj4string=sp::CRS(proj))
}


map2sp <- function(x, proj = "+proj=longlat +datum=WGS84"){
  maptools::map2SpatialPolygons(x, IDs=sapply(strsplit(x$names, ":"), "[", 1L),
                                proj4string=sp::CRS(proj))
}

sp2df <- function(x){
  x <- as(x, "SpatialPolygonsDataFrame")
  suppressWarnings(ggplot2::map_data(x)) %>% as_tibble
}

buffer <- function(spgeom, byid = TRUE, ...){
  suppressWarnings(rgeos::gBuffer(spgeom = spgeom, byid = byid, ...))
}

crop <- function(spgeom, long_min, long_max, lat_min = -90, lat_max = 90,
    proj = "+proj=longlat +datum=WGS84", buffer_width = 0, byid = TRUE, ...){

  if(!is.null(buffer_width)){
    spgeom <- buffer(spgeom, width = buffer_width)
  }

  rgeos::gIntersection(spgeom, sp_box(long_min = long_min, long_max = long_max,
      lat_min = lat_min, lat_max = lat_max, proj = proj), byid=TRUE, ...)
}

smooth_crop_lines <- function(mr, long_min, long_max, d = 0.00002){
  # this is magic (or a ugly hack) to make round edges in projections, i.e.
  # cuts lines into many segments - round edges after projection
  bs.x <- which(
    mr$long > long_max - d & mr$long < long_max + d |
    mr$long > long_min + d & mr$long < long_min + d )
  bsf.x <- bs.x[diff(bs.x)==1]
  mx <- mr[0,] # init empty df to store extra points

  for(i in bsf.x){
    fr <- mr[i,]
    to <- mr[i+1,]
    dt <- abs(fr$lat-to$lat)
    if(dt<1) next # ignore cuts shorter than 1 deg

    bn.lat <- seq(fr$lat, to$lat)
    bn.lat <- bn.lat[2:length(bn.lat)]; # ignore first element, equals from
    bn.n <- length(bn.lat)

    # generate + store new points
    mx <- rbind(mx, data.frame(
      long=fr$long,
      lat=bn.lat,
      group=fr$group,
      order=seq(to$order,to$order+bn.n-1),
      region=fr$region,
      subregion=fr$subregion
    ))
    q
    ## adjust order in polygon df
    mr$order[(i+1):length(mr$order)] = mr$order[(i+1):length(mr$order)] + bn.n +1

  }

  rbind(mr, mx) %>% # integrate original and new points
    dplyr::arrange(order) # reorder
}
