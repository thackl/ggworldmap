#' @export
geom_array <- function(mapping = NULL, data = NULL, stat = "array",
    position = "identity", ..., nrow = 10, spread = c(x = 4,y = 4),
    hjust = 0.5, vjust = 0, show.legend = NA, inherit.aes = TRUE){

  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPoint,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(nrow = nrow, spread = spread, hjust = hjust, vjust = vjust, na.rm = FALSE, ...))
}
## #' @export
## stat_array <- function(mapping = NULL, data = NULL, geom = "point",
##     position = "identity", nrow = 10, spread = c(x = 4,y = 4), ...,
##     show.legend = NA, inherit.aes = TRUE){

##   layer(
##     data = data, mapping = mapping, stat = "array", geom = geom,
##     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
##     check.param = FALSE, params = list(nrow = nrow, spread = spread, na.rm = FALSE, ...))
## }
#' @export
StatArray <- ggproto(
  "StatArray", Stat,
  compute_panel = function(data, scales, nrow = 10, spread = c(4,4),
      hjust = 0.5, vjust = 0.5){
    data <- data %>%
      dplyr::group_by(x, y) %>%
      dplyr::mutate( 
        .x=stack_x(x, nrow, spread[1], hjust),
        .y=stack_y(y, nrow, spread[length(spread)], vjust)
      ) %>% dplyr::ungroup() %>% # cannot modify grouping var
      dplyr::mutate(x = .x, .x = NULL, y = .y, .y = NULL)
    print(data %>% as_tibble)
    data
  },
  required_aes = c("x","y")
)

stack_x <- function(x, nrow = 5, spread = 1, hjust = .5){
  n <- length(x)
  ncol <- ceiling(n / nrow)
  o <- (rep(seq_len(ncol), each=nrow)[seq_len(n)] - 1) * spread
  x + o - max(o) * hjust
}


stack_y <- function(y, nrow = 5, spread = 1, vjust = 0){
  n <- length(y)
  ncol <- ceiling(n / nrow)
  o <- (rep(seq_len(nrow), ncol)[seq_len(n)] -1) * spread
  y + o - max(o) * vjust
}
