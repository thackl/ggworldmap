`%||%` <- function(x, y){
  if(is.null(x)) y else x
}

`%&&%` <- function(x, y) {
  if (is.null(x)) NULL else y
}

if_null_else <- function(cond, true, false){
  if(is.null(cond)) true else false
}

compute_breaks <- function(min, max, by, n = NULL){
  if(is.null(n)){
    if(min < 0 && max > 0){ # try to include zero
      c(rev(seq(-by, min, by=-by)),
        seq(0, max, by=by))
    }else{
      seq(min, max, by=by)
    }
  }else{
    seq(min, max, length.out=n)
  }
}

vars_lat_long <- function (vars, lat = lat, long = long){
  lat_var <- tidyselect::vars_pull(vars, !! enquo(lat))
  long_var <- tidyselect::vars_pull(vars, !! enquo(long))
  c(lat_var, long_var)
}

glue_proj4 <- function(proj, long_0){
  if(is.null(proj) || proj == "")
    stop("projection required")
  if(!substr(proj,1,1) == "+")
    proj <- paste0("+proj=", proj)
  if(!(is.null(long_0) || long_0 == ""))
    proj <- paste0(proj, " +lon_0=", long_0)
  proj
}
