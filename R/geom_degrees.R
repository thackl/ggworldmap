#' @export
geom_degree <- function(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", proj = NULL, long_0 = 0,
    lat_min = -90, lat_max = 90, lat_by = 20, lat_n = NULL,
    lat_at = c(long_min, long_max), lat_nudge = 5,
    long_min = -180 + long_0, long_max = 180 + long_0, long_by = 30,
    long_n = NULL, long_at = c(lat_min, lat_max), long_nudge = 10,
    proj_extra=NULL, ..., na.rm = FALSE, show.legend = NA, inherit.aes = FALSE){

  long_0 <- long_0 %||% 0

  default_aes <- aes_(x=~long, y=~lat, label=~label)
  mapping = aes_intersect(mapping, default_aes)

  data <- data %||% degrees(proj = proj, long_0 = long_0, lat_min = lat_min,
    lat_max = lat_max, lat_by = lat_by, lat_n = lat_n, lat_at = lat_at,
    lat_nudge = lat_nudge, long_min = long_min, long_max = long_max,
    long_by = long_by, long_n = long_n, long_at = long_at, long_nudge = long_nudge,  
    proj_extra=proj_extra)

  geom_text(mapping = mapping, data = data, stat = stat, position = position, ...,
    na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes) 
}
#' @export
degrees <- function(proj = NULL, long_0 = 0, lat_min = -90, lat_max = 90,
    lat_by = 20, lat_n = NULL, lat_at = c(long_min, long_max),
    lat_nudge = 5, long_min = -180 + long_0, long_max = 180 + long_0, long_by = 30,
    long_n = NULL, long_at = c(lat_min, lat_max), long_nudge = 10,
    proj_extra=NULL){

  long_0 <- long_0 %||% 0

  lat_breaks <- compute_breaks(lat_min, lat_max, lat_by, lat_n)
  long_breaks <- compute_breaks(long_min, long_max, long_by, long_n)
  
  degrees_lat <- tibble(
    lat = rep(lat_breaks, length(lat_at)),
    long = rep(lat_at, each=length(lat_breaks)),
    label = lat %>% wrap_dd %>% dd2dms(TRUE) %>% pretty_dms)

  degrees_long <- tibble(
    lat = rep(long_at, each=length(long_breaks)),
    long = rep(long_breaks, length(long_at)),
    label = long %>% wrap_dd %>% dd2dms() %>% pretty_dms)

  if(!is.null(proj)){
    degrees_lat %<>% project(proj, long_0, proj_extra=proj_extra)
    degrees_long %<>% project(proj, long_0, proj_extra=proj_extra)
  }
  
  degrees_long <- mutate(degrees_long, lat = ifelse(lat >0, lat + lat_nudge, lat - lat_nudge))
  degrees_lat <- mutate(degrees_lat, long = ifelse(long >0, long + long_nudge, long - long_nudge))

  bind_rows(degrees_lat, degrees_long)
}

wrap_dd <- function(r, shift=0){ # between -180 and 180
    sapply(r, function(x){
        if(is.na(x)) return(NA);
        x <- x - (360*round((x-shift)/360))
        if(x == -180) x <- 180 ## R round would alternate -180 and 180
        return(x)})
}

#' use "°" for degree (instead of d), and also if no minutes
pretty_dms <- function (x, ...){
  if (!inherits(x, "DMS")) 
    stop("not a DMS object")
  if (!x@NS) 
    tag <- c("W", "E")
  else tag <- c("S", "N")
  res <- ifelse(x@WS, tag[1], tag[2])
  res <- paste(ifelse(round(x@sec, digits = 3) != "0", paste(round(x@sec, 
                             digits = 3), "\"", sep = ""), ""), res, sep = "")
  res <- paste(ifelse(((x@min != 0) | (round(x@sec, digits = 3) != 
        "0")), paste("°", x@min, "'", sep = ""), ""), res, sep = "")
  res <- paste(x@deg, "°", res, sep = "")
  res
}


