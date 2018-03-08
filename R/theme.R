#' @export
theme_worldmap_light <- function(...){
  theme_light(base_size = 15) +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
      panel.border = element_rect(color = NA)) +
    theme(...)
}
