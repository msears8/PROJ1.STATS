#' @title Central Limit Theorem function
#' @description A function that creates a histogram from a vector and represents the CLT.
#' @param n number of observations
#' @param iter number of iterations
#' @param a interval minimum
#' @param b interval maximum
#'
#' @return a histogram created from a vector (sm), which contains the sum of
  #' each column that is comprised of random deviates from the
  #' from the uniform distribution on the interval from
  #' min (a) to max (b).
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
}
