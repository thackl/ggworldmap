#' @export
#project <- function(lat, long, shift=0, projection="robin", projection_extra=NULL){
#    projection_opts=paste(paste0("+proj=", projection), paste0("+lon_0=", shift*-1), projection_extra)
#    rgdal::project(cbind(long, lat), proj=projection_opts)[,1]/10^5 - shift
#}

#' @export
project <- function(...){
  UseMethod("project")
}

#' @export
project.default <- rgdal::project

#' @export
project.data.frame <- function(.data, ...){
  project(as_tibble(.data), ...)
}

#' @export
project.tbl_df <- function(.data, proj = NULL, long_0 = 0, lat=lat,
    long=long, long_proj=long, proj_extra=NULL){

  proj <- proj %||% "longlat"
  long_0 <- long_0 %||% 0

  # short-circuits on long_0=NULL/0 & proj=NULL/longlat
  if(proj == "longlat" && long_0 == 0) return(.data)

  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)
  long_proj <- rlang::quo_text(rlang::enquo(long_proj))
  
  proj_opts <- paste(paste0("+proj=", proj), paste0("+lon_0=", long_0), proj_extra)
  
  mutate(.data,
         !! long_proj := rgdal::project(cbind(!! long, !! lat),
         proj = proj_opts)[,1]/10^5 + long_0)
}
