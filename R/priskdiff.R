#' Distribution function of exact risk difference
#'
#' @param piTreatment Event rate in treatment group.
#' @param piControl Event rate in control group.
#' @param nTreatment Number of subjects in treatment group.
#' @param nControl Number of subjects in control group.
#'
#' @return Aggregated data of risk differences and cumulative probabilities.
#'
#' @export
priskdiff <- function(piTreatment, piControl, nTreatment, nControl) {
  riskDifference <- c()
  probability <- c()
  for(i in 0:nControl) {
    for(j in 0:nTreatment) {
      riskDifference <- c(riskDifference, (j*nControl-i*nTreatment)/(nControl*nTreatment))
      probability <- c(probability, dbinom(x=i, size=nControl, prob=piControl)*dbinom(x=j, size=nTreatment, prob=piTreatment))

    }
  }
  cumProbabilities <- aggregate(probability, by=list(riskDifference=riskDifference), FUN=sum)
  cumProbabilities$x <- cumsum(cumProbabilities$x)
  names(cumProbabilities) <- c("Risk difference", "Cumulative probability")
  return(cumProbabilities)
}
