
datasets$all %>% #ungroup() %>% 
  filter(
    is.finite(dprice) &
      is.finite(dqty) &
      is.finite(dstore)
  ) %>% 
#  {lm(dqty_bs~dprice+dstore, data=.)} %>% summary
  group_map(
    ~broom::tidy(lm(dqty_bs~dprice+dstore, data=.))
  )

datasets$all %>% 
  mutate(
    period = year + (week_num-1)/53
  ) %>% 
  arrange(mstrprodid, period) %>% 
  select(period, mstrprodid, qty_bs) %>% 
  group_by(period) %>% 
  spread(mstrprodid, qty_bs) %>% 
  col_cors(remove=F) %>% 
  group_by(var1) %>% 
  summarise(
    v=sum(value>0.85),
    vars = paste(var2[value>0.85], collapse = ',')
  ) %>% 
  arrange(desc(v))

product_summaries$all %>% filter(mstrprodid %in% c(18994,18985,68102,18986,55264,24199,63055)) %>% View  

product_summaries$all %>%  
  filter(
    product_class %in% c("Other", "Had Promotional Period")
  ) %>%
  select(
    #-c(mstrprodid, product_class)
    #revenue_bs,
    revenue_bs.annual_mean,
    qty_bs.annual_mean,
    price.mean,
    price.sd
  ) %>% 
  {
    fit = kmeans(., 5)
    library(cluster)
    clusplot(., fit$cluster, color=TRUE, shade=TRUE,
             labels=2, lines=0)
    fit
  } 
-> fit
  
product_summaries$all %>% 
  filter(
    product_class %in% c("Other", "Had Promotional Period")
  ) %>%
  mutate(
    cluster = fit$cluster
  ) %>% filter(cluster == 4) %>% View 
  group_by(cluster) %>% 
  summarise(n())


