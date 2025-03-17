#' Plot risk difference
#'
#' @param agg Data frame with columns rds and x, created by \code{driskdiff} or \code{priskdiff}.
#' @param title Optional plot title.
#' @export
plotriskdiff <- function(agg, title="") {
  names(agg) <- c("rds", "x")
  ggplot2::ggplot(data=agg, mapping=ggplot2::aes(x=rds, y=x)) + ggplot2::geom_point() + ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::ggtitle(title) +
    ggplot2::labs(x = "Risk difference", y = "Probability")
}
