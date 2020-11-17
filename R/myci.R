#' @title myci
#' @description A function that returns the confidence interval of a dataset.
#'
#' @param x the dataset with which confidence interval will be found
#'
#' @return the confidence interval of dataset x
#' @export
#'
#' @examples myci(rnorm(30,mean=10,sd=12)) = 8.13018, 15.64350
#'
myci = function(x){
  t=qt(0.975,length(x))
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(length(x))
  ci[2]=mean(x)+t*sd(x)/sqrt(length(x))
  ci

  obj=t.test(x,conf.level=0.95)
  names(obj)
  return(ci)
}
