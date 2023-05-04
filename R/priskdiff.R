priskdiff <- function(p0, p1, n0, n1) {
  rds <- c()
  probs <- c()
  for(i in 0:n0) {
    for(j in 0:n1) {
      rds <- c(rds, (i*n1-j*n0)/(n0*n1))
      probs <- c(probs, dbinom(x=i, size=n0, prob=p0)*dbinom(x=j, size=n1, prob=p1))

    }
  }
  agg <- aggregate(probs, by=list(rds=rds), FUN=sum)
  agg$x <- cumsum(agg$x)
  return(agg)
}
