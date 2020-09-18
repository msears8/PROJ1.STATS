#' @title A function for finding the z values.
#'
#' @param z The z values for a dataset
#'
#' @return The s^2 value
#' @export
#'
#' @examples
#'(enter z array, return standard value squared)
s2value=function(z){
  (sd(z))^2
}
