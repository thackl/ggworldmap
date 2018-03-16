#' Concentrate observations at geographic coordinates.
#'
#' Concentrate observations that are close to each other at a few representative
#' locations. This is useful primarily for displaying purposes. It allows to
#' declutter observations and reduce locally scattered groups to a few locations
#' of interest.
#' @param lambda A clustering coefficient. Used in combination with the total
#' range spanned by the data to estimate a cluster cutoff.
#' @param lat,long Column names (unquoted) or positions of latitude and
#' longitude in `.data`
#' @param lat_new,long_new Column names (quoted) for storing the projection
#' results. If NULL, the original latitude/longitude columns are used.
#' @param group_var if not-`NULL` keep a grouping variable for the concentrated
#' locations with this column name
#' @param ... Currently not used.
#' @inheritParams project
#' @export
#' @examples
#' df <- tibble::tibble(
#'   foo = rep(LETTERS[1:3], each = 3),
#'   lat = rnorm(9, rep(c(-30, -20, 50), each = 3)),
#'   long = rnorm(9, rep(c(-90, -100, 85), each = 3)))
#'
#' df %>% concentrate()
#'
#' df %>% concentrate(.2, long_new = 'x', lat_new = 'y', group_var = "i")
concentrate <- function(.data, lambda = .05, lat = lat, long = long,
    lat_new = NULL, long_new = NULL, group_var = NULL, ...){
  UseMethod("concentrate")
}
#' @export
concentrate.data.frame <- function(.data, ...){
  concentrate(as_tibble(.data), ...)
}
#' @export
concentrate.tbl_df <- function(.data, lambda = .05, lat = lat, long = long,
    lat_new = NULL, long_new = NULL, group_var = NULL, ...){
  ll_vars <- vars_lat_long(names(.data), !! rlang::enquo(lat), !! rlang::enquo(long))
  ll2_vars <- c(lat_new %||% ll_vars[1], long_new %||% ll_vars[2])
  ll3_vars <- ll2_vars

  if(!is.null(group_var)){
    ll2_vars <- c(ll2_vars, ".i")
    ll3_vars <- c(ll3_vars, group_var)
  }

  # cluster
  h <- lambda * diff(range(.data[ll_vars]))
  i <- .data[ll_vars] %>% dist() %>%
    hclust(method = "ward.D2") %>% cutree(h=h)

  # centroid xy
  .data[ll3_vars] <- .data[ll_vars] %>%
    dplyr::mutate(.i = i) %>%
    dplyr::group_by(.i) %>% dplyr::mutate(
      !! ll2_vars[1] := mean(!! rlang::enquo(lat)),
      !! ll2_vars[2] := mean(!! rlang::enquo(long))) %>%
    .[ll2_vars]
  .data
}
