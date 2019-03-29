read_xlsx("./case_study.xlsx") -> data

data %>% 
  mutate(
    x=(price_loyalty*qty_loyalty+price_non_loyalty*qty_non_loyalty)/qty_all
  ) %>% 
  select(
    mstrprodid, x, price_all, price_loyalty, price_non_loyalty
  ) %>% 
  summarise(
    sum(abs(x-price_all) > 1E-2, na.rm=T),
    mean(abs(x-price_all) > 1E-2, na.rm=T),
    n()
  ) 

data %>% 
  select(
    mstrprodid, qty_all, qty_loyalty, qty_non_loyalty
  ) %>% 
  filter(
    qty_all != qty_loyalty+qty_non_loyalty
  )
