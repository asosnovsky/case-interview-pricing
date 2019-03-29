datasets$all %>% 
  select(-cor_cluster_d) %>% 
  cor_clustering(3) %>% 
  filter(cor_cluster_d>1) %>% 
  group_by(cor_cluster_d, week_num) %>% 
  summarise(
    r=summary(lm(dqty~dprice))$r.squared
  )

tmp %>% 
  group_by(cluster) %>% summarise(n()) %>% View
  mutate(N=n()) %>% 
  ungroup() %>% filter(N<max(N)) %>% 
  select(-N) -> tmp
  
datasets$all %>% inner_join(tmp, by='mstrprodid') %>% 
  group_by(cluster, quarter) %>% 
  summarise(
    p = summary(lm(dqty~dprice))$r.squared
  )
  
ggplot(aes(
    x=dprice, y=dqty,
    color=as.factor(cluster)
  ))+
  geom_point() +
  geom_smooth(method='lm')
  
  