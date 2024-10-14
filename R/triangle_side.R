#' Calculate Triangle Side Length with Pythagorean Theorem
#'
#' This function calculates the length of the third side of a right triange,
#'  given the lengths of the other two sides.
#'
#' @param side_a Numeric. The length of the first side of the triangle
#' @param side_b Numeric. The length of the second side of the triangle
#' @param side_c Numeric. The length of the hypotenuse (third) side of the triangle
#' @return Numeric. The result of the pythagorean theorem calculating the missing side

#' @examples
#' Calculate the hypotenuse of a triangle
#' triangle_side(side_a = 3, side_b = 4)  #Returns 5

#' Calculate side b of a triangle
#' triangle_side(side_a = 3, side_c = 5)  #Returns 4

#' @export
triangle_side <- function(side_a = NULL, side_b = NULL, side_c = NULL) {

  # Checks if only two sides were inputted into the function
  sides_input <- sum(!is.null(c(side_a, side_b, side_c)))
  if (sides_input != 2){
    stop("You must input exactly two sides")
  }

  # Checking if slide lengths are positive
  if (any(c(side_a, side_b, side_c) < 0)) {
    stop("Side lengths must be positive.")
  }


  #  Calculates hypotenuse from sides a and b
  if(is.null(side_c)){
    return( sqrt( side_a^2 + side_b^2 ))
  }

  # calculates side a if side b and hypotenuse (side c) are given.
  #  ALso checks that side c is bigger than side b
  if(is.null(side_a)){
    if(side_c - side_b <= 0){
      stop("Side c must be bigger than side b.")
    }
    return( sqrt( side_c^2 - side_b^2))
  }

  # calculates side b if side a and hypotenuse (side c) are given
  # ALso checks that side c is bigger than side a
  if(is.null(side_b)){
    if(side_c - side_a <= 0){
      stop("Side c must be bigger than side a.")
    }
    return( sqrt( side_c^2 - side_a^2))
  }


}
