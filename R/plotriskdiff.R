plotriskdiff <- function(agg, title="") {
  names(agg) <- c("rds", "x")
  ggplot(data=agg, mapping=aes(x=rds, y=x)) + geom_point() + geom_line() + theme_bw() + ggtitle(title) +
    labs(x = "Risk difference", y = "Probability")
}
