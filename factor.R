# new
factor <- function(x = character(), levels = unique(x)) {
  # if (levels < unique(x) {
  #   levels <- "<NA>"
  # }
  
  ind <- match(x, levels)
  if(any(is.na(ind))) {
    levels = c(levels, "NA")
    ind[is.na(ind)] <- as.integer(max(ind, na.rm = TRUE)+1)
    warning("One or more value does not have a matching level.", call. = FALSE)
  }
  
  
  validate_factor(new_factor(ind, levels))
}

new_factor <- function(x = integer(), levels = character(), contrasts = double()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  stopifnot(is.double(contrasts))
  
  structure(
    x,
    levels = levels,
    class = "factor",
    contrasts = contrasts
  )
}
