myci = function(x){
  t=qt(0.975,length(x))
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(length(x))
  ci[2]=mean(x)+t*sd(x)/sqrt(length(x))
  ci

  obj=t.test(d,conf.level=0.95)
  names(obj)
  return(ci)
}
