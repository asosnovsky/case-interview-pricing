library(cluster)

plot_elast <- function(dt) {
  dt %>% ggplot(aes(
    y=dprice,
    x=dqty,
    group=mstrprodid,
    color=mstrprodid
  )) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE)
  
}

plot_kmeans <- function(dt, centers = 5) {
  fit = kmeans(x=dt, centers = centers)
  clusplot(dt, fit$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0)
  print(fit)
  fit
}