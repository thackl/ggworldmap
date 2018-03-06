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
  project(tibble::as_tibble(.data), ...)
}

#' @export
#' @param ... other arguments for [rgdal::project]
project.tbl_df <- function(.data, proj = NULL, long_0 = 0, proj_extra = NULL,
    scale_factor = 1e-5, lat = lat, long = long, long_proj = NULL, lat_proj = NULL, ...){

  long_0 <- long_0 %||% 0

  if(is.null(proj)){
    if(long_0 != 0){
      stop("not implemented")
    }
    return(.data) # unprojected
  }

  scale_factor <- scale_factor %||% 1
  lat <- rlang::quo_text(rlang::enquo(lat))
  long <- rlang::quo_text(rlang::enquo(long))
  lat_proj <- lat_proj %||% lat
  long_proj <- long_proj %||% long

  proj_opts <- paste(paste0("+proj=", proj), paste0("+lon_0=", long_0), proj_extra)

  .data[c(long_proj, lat_proj)] <- rgdal::project(as.matrix(.data[c(long, lat)]), proj = proj_opts, ...)
  .data[c(long_proj, lat_proj)] <- .data[c(long_proj, lat_proj)] * scale_factor
  .data
}
