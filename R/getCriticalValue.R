#' Identify the critical value for the exact risk difference test
#'
#' @param alpha One-sided significance level.
#' @param piTreatment Event rate in treatment group.
#' @param piControl Event rate in control group.
#' @param nTreatment Number of subjects in treatment group.
#' @param nControl Number of subjects in control group.
#' @param mustExceed Logical indicating whether the observed risk difference must exceed the critical value for rejection (default \code{TRUE}). If \code{FALSE}, the observed risk difference must be equal to or greater than the critical value.
#'
#' @return Critical value.
#'
#' @export

getCriticalValue <- function(alpha, piTreatment, piControl, nTreatment, nControl, mustExceed = TRUE) {
  probs <- driskdiff(piTreatment = piTreatment, piControl = piControl, nTreatment = nTreatment, nControl = nControl)
  cumProbs <- priskdiff(piTreatment = piTreatment, piControl = piControl, nTreatment = nTreatment, nControl = nControl)

 rejectData <- cbind(
   riskDiff = probs[,1],
   probGeq = 1 - cumProbs[,2] + probs[,2],
   reject = 1 - cumProbs[, 2] + probs[,2] < alpha)

  if(mustExceed) {
    isCriticalValue <- c(diff(rejectData[,3]), 0)
  }
  else {
    isCriticalValue <- c(0, diff(rejectData[,3]))
  }

  criticalValue <- rejectData[,1][which(isCriticalValue == 1)]
  return(criticalValue)
}
