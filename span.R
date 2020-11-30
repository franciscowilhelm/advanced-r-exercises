span <- function(x, f) {
  tmp <- rle(x)
  tmp$endpos <- cumsum(tmp$lengths)
  tmp$beginpos <- tmp$endpos - (tmp$lengths-1)
  
  idx <- tmp$lengths[tmp$values == f] #length of sequences for which predicate is true
  idx <- idx[idx == max(idx)] #return longest
  idx <- tmp$lengths == idx & tmp$values == f
  out <- c(tmp$beginpos[idx], tmp$endpos[idx])
}

arg_max <- function(x, f) {
  # print(f)
  f_out <- f(x)
  maxpos <- f_out == max(f_out)
  x[maxpos]
}

arg_min <- function(x, f) {
  # print(f)
  f_out <- f(x)
  minpos <- f_out == min(f_out)
  x[minpos]
}