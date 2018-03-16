#' @export
geom_array <- function(mapping = NULL, data = NULL, stat = "array",
    position = "identity", ..., nrow = 10, ncol = NULL, spread = c(x = 4,y = 4),
    hjust = 0.5, vjust = 0, show.legend = NA, inherit.aes = TRUE){

  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPoint,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(nrow = nrow, ncol = ncol, spread = spread, hjust = hjust, vjust = vjust, na.rm = FALSE, ...))
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
StatArray <- ggplot2::ggproto(
  "StatArray", ggplot2::Stat,
  compute_panel = function(data, scales, nrow = 10, ncol = NULL, spread = c(4,4),
      hjust = 0.5, vjust = 0.5){

    nmax <- dplyr::count(data, x, y) %>% pull(n) %>% max()

    if(!is.null(ncol)){
      nmax %||% stop('nmax required if ncol specified')
      m <- ceiling(nmax / (ncol * nrow))
      cat("geom_array: showing one array point per", m, "data points\n")
    }

    data <- dplyr::group_by(data, x, y) %>%
      dplyr::mutate( 
        .x=stack_x(x, nrow = nrow, ncol = ncol, nmax = nmax, spread = spread[1], hjust = hjust),
        .y=stack_y(y, nrow = nrow, ncol = ncol, nmax = nmax, spread = spread[length(spread)], vjust = vjust)
      ) %>% dplyr::ungroup() %>% # cannot modify grouping var
      dplyr::mutate(x = .x, .x = NULL, y = .y, .y = NULL)
    data
  },
  required_aes = c("x","y")
)

stack_x <- function(x, nrow = 5, ncol = NULL, nmax = NULL, spread = 1, hjust = .5){
  n <- length(x)

  if(is.null(ncol)){
    ncol <- ceiling(n / nrow)
    m <- 1 # default multiplicator
  }else{
    nmax %||% stop('nmax required if ncol specified')
    # multiplicator: stack points if there are more than slots in array
    m <- ceiling(nmax / (ncol * nrow))
  }

  # offset for each point
  o <- (rep(seq_len(ncol), each=nrow * m)[seq_len(n)] - 1) * spread
  x + o - max(o) * hjust
}

stack_y <- function(y, nrow = 5, ncol = NULL, nmax = NULL, spread = 1, vjust = 0){
  n <- length(y)
  m <- 1 # default multiplicator

  if(is.null(ncol)){
    ncol <- ceiling(n / nrow)
    m <- 1 # default multiplicator
  }else{
    nmax %||% stop('nmax required if ncol specified')
    # multiplicator: stack points if there are more than slots in array
    m <- ceiling(nmax / (ncol * nrow))
  }

  # offset for each point
  o <- (rep(rep(seq_len(nrow), ncol), each = m)[seq_len(n)] -1) * spread
  y + o - max(o) * vjust
}
