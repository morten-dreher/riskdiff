plotriskdiff <- function(agg, title="") {
  ggplot(data=agg, mapping=aes(x=rds, y=x)) + geom_point() + geom_line() + theme_bw() + ggtitle(title)
}
