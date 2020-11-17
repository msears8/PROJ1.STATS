#' @title Marble probability function
#' @description A function that produces a sample of black and white marbles.
#'
#' @param w number of white marbles
#' @param b number of black marbles
#' @param n sample size
#'
#' @return sf, sample size 'n' marbles drawn without replacement
#' @export
#'
#' @examples
#' m<-marbles(12, 8, 5);
#' potential output: 1, 1, 0, 0, 1
marbles=function(w, b, n){
  sf = sample (rep(c(1, 0), c(w,b)), size = n, replace = FALSE)
  sf
  }
