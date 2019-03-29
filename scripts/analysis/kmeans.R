
lapply(datasets, function(x) x %>% 
     mutate(
       price = replace_na(price, 0),
       cluster = kmeans(
         tibble(price),
         3
       )$cluster
     ) %>% 
     group_by(cluster) %>% 
     summarise(
       min = min(price),
       mean = avg(price),
       max = max(price),
       N = n()
     ) %>% 
     arrange(min) %>% 
     mutate(
       cluster_name = c("Low Priced", "Medium Priced", "High Priced")
     ) %>% 
     select(
       cluster_name,
       min, mean, max, N
     )
)
