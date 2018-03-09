#' Worldmap themes
#'
#' Themes based on ggplot2 themes that play well with ggworldmaps.
#'
#' @name theme_worldmap
#' @details
#' \describe{
#'
#' \item{`theme_gray`}{A theme based on ‘ggplot2::theme_light’, with light grey
#' lines and axes, to direct more attention towards the data.}
#'
#' }
NULL

#' @export
#' @rdname theme_worldmap
theme_worldmap_light <- function(...){
  theme_light(base_size = 15) +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
      panel.border = element_rect(color = NA)) +
    theme(...)
}
