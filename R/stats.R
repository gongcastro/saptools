#' Rescale standardised variable
#'
#' @param x Numeric vector to be rescaled
#' @param mean Numeric value indicating the mean of the original vector
#' @param sd Numeric value indicating the standard deviation of the original vector
#' @export
rescale_variable <- function(x, mean, sd) {
  (x * sd) + mean
}


#' Generate a regular sequence of \code{n} elements in the range of a numeric vector \code{x}
#'
#' @param x Numeric vector
#' @param n Length of the vector to be generated
#' @export
seq_range <- function(x, n) {
  seq(min(x, na.rm = TRUE),
    max(x, na.rm = TRUE),
    length.out = n
  )
}
