#' Calculates the Trimmed Mean
#'
#' This function calculates the trimmed mean of a numeric vector
#' (e.g., mean that ignores the smallest and largest values).
#'
#' @param x A numeric vector. From this vector, the trimmed mean is calculated.
#' @param s Numeric. Number of smaller values to remove from the calculation.
#' @param l Numeric. Number of larger values to remove from the calculation.
#' @return Numeric. The trimmed mean.
#'
#' @examples
#' Calculate the trimmed mean
#' trimmed_mean(x = c(1, 7, 3, 2, 5, 0.5, 9, 10), s = 1, l = 2)  # Returns mean of c(1, 7, 3, 2, 5) = 3.6
#'
#' @export
trimmed_mean <- function(x = NULL, s = 0, l = 0) {

  # check that the length is longer than s + l
  if (length(x) <= (s + l)) {
    stop("Input vector must be larger than s + l.")
  }

  # arranging from smallest to largest
  arranged_vector <- sort(x)

  # trimming the vector to exclude the first s values and last l values
  trimmed_vector <- arranged_vector[(s+1):(length(arranged_vector)-1)]

  #  returning the mean
  return(mean(trimmed_vector))


}
