#' Central Limit Theorem function
#'
#' @param n number of observations
#' @param iter number of iterations
#' @param a interval minimum
#' @param b interval maximum
#'
#' @return w, a vector containing the sum of each column
  #' containing random deviates from the from the
  #' uniform distribution on the interval from
  #' min (a) to max (b) and a corresponding histogram.
#' @export
#'
#' @examples myclt(n=2, iter=1000, a=1, b=4)
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
