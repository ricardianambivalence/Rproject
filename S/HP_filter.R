## hodrick prescott filter

# call the function as hpfilter(target_series, lambda=choose)

hpfilter <- function(x,lambda=1600){
  eye <- diag(length(x)) # a diagonal ID matrix of len(x) by len(x)
  result <- solve(eye+lambda*crossprod(diff(eye,lag=1,d=2)),x)
  return(result)
}
