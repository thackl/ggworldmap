#' Project geographic coordinates
#'
#' `project()` is a wrapper around [rgdal::project()] that plays nice with
#' magrittr/tidyverse-style pipes and variable selection syntax.
#'
#' @param .data A tbl with latitude and longitude
#' @param proj A name of a cartographic projection or a complete proj4-string
#' specifying a transformation supported by the \href{http://proj4.org}{PROJ.4}
#' project.
#' @param long_0 Latitude of the central meridian (added to the proj4-string as
#' `+lon_0=<long_0>`
#' @param scale_factor to apply to results of projections. PROJ4 uses meters as
#' resolution for most transformation.
#' @param lat,long Column names (unquoted) or positions of latitude and
#' longitude in `.data`
#' @param lat_new,long_new Column names (quoted) for storing the projection
#' results. If NULL, the original latitude/longitude columns are used.
#' @param ... other arguments passed to [rgdal::project]
#' @export
#' @examples
#' library(tidyverse)
#'
#' tibble(lat = c(-45,0,45), long = 60, stuff = LETTERS[1:3]) %>%
#'   project("robin", 90)
#'
#' tibble(stuff = LETTERS[1:3], LAT = c(-45,0,45), LON = 60) %>%
#'   project("robin", 90, 1, LAT, LON, "lat robin [m]", "long robin [m]")
project <- function(.data, proj = NULL, long_0 = NULL, scale_factor = 1e-5,
    lat = lat, long = long, lat_new = NULL, long_new = NULL, ...){
  UseMethod("project")
}
#' @export
project.default <- rgdal::project
#' @export
project.data.frame <- function(.data, ...){
  project(tibble::as_tibble(.data), ...)
}
#' @export
project.tbl_df <- function(.data, proj = NULL, long_0 = NULL,
    scale_factor = 1e-5, lat = lat, long = long, lat_new = NULL,
    long_new = NULL, ...){

  if(is.null(proj)) return(.data) # unprojected

  proj <- glue_proj4(proj, long_0)
  scale_factor <- scale_factor %||% 1

  ll_vars <- vars_lat_long(names(.data), !! enquo(lat), !! enquo(long))
  ll2_vars <- c(lat_new %||% ll_vars[1], long_new %||% ll_vars[2])

  .data[ll2_vars] <- rgdal::project(as.matrix(.data[ll_vars]), proj = proj, ...) *
    scale_factor
  .data
}
