#' Density of exact risk difference
#'
driskdiff <- function(piTreatment, piControl, nTreatment, nControl) {
  riskDifference <- c()
  probability <- c()
  for(i in 0:nControl) {
    for(j in 0:nTreatment) {
      riskDifference <- c(riskDifference, (j*nControl-i*nTreatment)/(nControl*nTreatment))
      probability <- c(probability, dbinom(x=i, size=nControl, prob=piControl)*dbinom(x=j, size=nTreatment, prob=piTreatment))

    }
  }
  densities <- aggregate(probability, by=list(riskDifference=riskDifference), FUN=sum)
  names(densities) <- c("Risk difference", "Probability")
  return(densities)
}
