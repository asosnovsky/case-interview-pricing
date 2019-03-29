save_dataset_list(datasets, 'data/datasets')
save_dataset_list(product_summaries, 'data/product_summaries')
save_dataset_list(ela_summary, "data/ela_summary")
write_csv(joined_dataset, "data/joined_dataset.csv")

lapply(datasets, function(x) x %>% 
   group_by(mstrprodid, year, quarter) %>% 
   summarise(
     price.mean = mean(price),
     qty = sum(qty),
     qty_bs = sum(qty_bs),
     revenue = sum(qty*price),
     revenue_bs = sum(qty_bs*price),
     elasticity.mean = avg(elasticity)
   )
) %>% save_dataset_list("data/quaterly")


lapply(datasets, function(x) x %>% 
         mutate(period = year + (week_num-1)/53) %>% 
         arrange(mstrprodid, period) %>% 
         select(mstrprodid, period, qty_bs) %>% 
         group_by(period) %>% 
         spread(mstrprodid, qty_bs)%>% 
         col_cors
) %>% save_dataset_list("data/cors")
