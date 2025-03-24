#' Calculate power for given success rates
#'
#' @param piTreatment Treatment success rate.
#' @param piControl Control success rate.
#' @param nTreatment Number of subjects in treatment group.
#' @param nControl Number of subjects in control group.
#' @param alpha One-sided significance level.
#' @param piNull Null hypothesis success rates. Should consist of two entries, the first for the treatment success rate under the null hypothesis, the second for the control success rate under the null hypothesis. Defaults to \code{piControl} for both entries.
#'
#' @return The power for the given scenario.
#'
#' @export
#'
#'
getPower <- function(piTreatment, piControl, nTreatment, nControl, alpha, piNull = c(piControl, piControl)) {

  criticalValue <- getCriticalValue(alpha = alpha, piTreatment = piNull[1], piControl = piNull[2], nTreatment = nTreatment, nControl = nControl,
                                    mustExceed = TRUE)

  probs <- driskdiff(piTreatment = piTreatment, piControl = piControl, nTreatment = nTreatment, nControl = nControl)
  cumProbs <- priskdiff(piTreatment = piTreatment, piControl = piControl, nTreatment = nTreatment, nControl = nControl)

  rejectData <- cbind(
    riskDiff = probs[,1],
    probGeq = 1 - cumProbs[,2] + probs[,2])

  indicator <- rejectData[,1] > criticalValue
  rejectData <- rejectData[indicator,]

  return(unname(rejectData[1,2]))

}
