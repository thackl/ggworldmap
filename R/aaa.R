`%||%` <- function(x, y){
  if(is.null(x)) y else x
}

`%&&%` <- function(x, y) {
  if (is.null(x)) NULL else y
}

null_then_else <- function(x, y, z){
  if(is.null(x)) y else z
}

compute_breaks <- function(min, max, by, n = NULL){
  if(is.null(n)){
    if(min < 0){ # try to include zero
      c(seq(-by, min, by=-by),
        seq(0, max, by=by))
    }else{
      seq(min, max, by=by)
    }
  }else{
    seq(min, max, length.out=n)
  }
}
