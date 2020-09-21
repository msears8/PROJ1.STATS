#' Marble probability function
#'
#' @param w probability of drawing white marble
#' @param b probability of drawing black marble
#' @param n sample size
#'
#' @return sf, sample size 'n' marbles drawn without replacement
#' @export
#'
#' @examples
#' potential output: 1, 1, 0, 0, 1 if n=5
marbles=function(w, b, n){
  sf = sample (rep(c(1, 0), c(w,b)), size = n, replace = FALSE)
}
