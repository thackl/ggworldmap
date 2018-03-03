#' @export
concentrate <- function(...){
  UseMethod("concentrate")
}
#' @export
concentrate.data.frame <- function(.data, ...){
  concentrate(as_tibble(.data), ...)
}
#' @export
concentrate.tbl_df <- function(.data, lat=lat, long=long, lambda = .05, ...){
  lat_quo <- rlang::enquo(lat)
  long_quo <- rlang::enquo(long)

  lat_str <- rlang::quo_text(lat_quo)
  long_str <- rlang::quo_text(long_quo)

  h <- lambda * (.data %>%
    select(!! long_quo, !! lat_quo) %>%
    range %>% diff)
  
  # cluster
  i <- .data %>%
    select(!! long_quo, !! lat_quo) %>%
    dist() %>% hclust(method = "ward.D2") %>% cutree(h=h) 

  # centroid xy
  .data %>% mutate(.i = i) %>%
    group_by(.i) %>% mutate(
     !! lat_str  := mean(!! lat_quo),
     !! long_str := mean(!! long_quo))
}
