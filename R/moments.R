#' Calculate the kurtosis of a numeric series
#'
#'
#' @export
kurtosis <- function(x, na.rm = FALSE) {

  if(na.rm) {
    x <- na.omit(x)
  }

  n <- length(x)

  numerator   <-  sum( (x - mean(x)) ^ 4 ) / n
  denominator <- (sum( (x - mean(x)) ^ 2 ) / n) ^ 2

  numerator / denominator
}

#' Calculate the skewness of a numeric series
#'
#'
#' @export
skewness <- function(x, na.rm = FALSE) {

  if(na.rm) {
    x <- na.omit(x)
  }

  n <- length(x)

  numerator   <-  sum( (x - mean(x)) ^ 3 ) / n
  denominator <- (sum( (x - mean(x)) ^ 2 ) / n) ^ (3 / 2)

  numerator / denominator
}
