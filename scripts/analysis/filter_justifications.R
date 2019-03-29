data %>% 
  group_by(
    mstrprodid
  ) %>% 
  summarise(
    N=n()
  ) %>% 
  summarise(
    mean(N>1),
    sum(N>1)
  )

data %>% 
  group_by(mstrprodid) %>% 
  filter(any(price_all == 0)) %>% 
  ungroup() %>% 
  summarise(length(unique(mstrprodid)))

data %>% 
  group_by(mstrprodid) %>% 
  filter(any(price_loyalty == 0)) %>% 
  ungroup() %>% 
  summarise(length(unique(mstrprodid)))

data %>% 
  group_by(mstrprodid) %>% 
  filter(any(price_non_loyalty == 0)) %>% 
  ungroup() %>% 
  summarise(length(unique(mstrprodid)))

data %>% summary() 

data$mstrprodid %>% unique() %>% length
4/255

datasets$all %>% 
  filter(
    is.infinite(dqty)
  )
