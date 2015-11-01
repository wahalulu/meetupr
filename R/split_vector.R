#' Splits a vector into a groups of a specific number
#' @param v A vector to split
#' @param s The number of elements per group
split_vector <- function(v, s) {
  grps <- ceiling(length(v) / s)
  last_grp <- length(v) %% s
  f <- unlist(lapply(1:grps, function(x) {
    if (x < grps)
      rep(x, s)
    else rep(x, if(last_grp == 0) s else last_grp)
  }))
  split(v, f)
}
