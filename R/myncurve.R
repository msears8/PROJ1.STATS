#' @title myncurve function
#' @description A function that creates a normal curve with shaded probability.
#'
#' @param a upper bound of x in probability
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return a normal curve with the input parameters, the shaded area representing probability, and the probability value
#' @export
#'
#' @examples myncurve(2, 0, 1) would output a plot with a probability area of .9772
#'
myncurve = function(a, mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(-100,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(-100,xcurve,a),c(0,ycurve,0),col="Red")
  prob=pnorm(a, mean=mu,sd=sigma)
  prob=round(prob,4)
  text((((mu-3*sigma)+(mu+3*sigma))/2), .01,paste("Area=",prob))
}
