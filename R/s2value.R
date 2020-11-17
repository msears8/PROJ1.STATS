#' @title S squared function
#' @Description A function for finding the z values.
#'
#' @param z The z values for a dataset
#'
#' @return The s^2 value
#' @export
#'
#' @examples
#'obj=mpg.df$MPG; z=(obj-mean(obj))/sd(obj); s2value(z)
s2value=function(z){
  (sd(z))^2
}
