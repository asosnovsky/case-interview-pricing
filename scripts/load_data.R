rm(list=ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)

source("./scripts/funcs.R")
source("./scripts/plots.R")

read_xlsx("./case_study.xlsx") %>% 
  mutate(
    mstrprodid = as.character(mstrprodid),
    date = as.Date(week, format = "%e %B, %Y"),
    year = year(date),
    week_num = week(date),
    quarter = quarter(date)
  ) -> data
data %>% {
  lapply(list(
    all = select(., mstrprodid, year, quarter, week_num, qty=qty_all, price=price_all, store=store_all),
    loyalty = select(., mstrprodid, year, quarter, week_num, qty=qty_loyalty, price=price_loyalty, store=store_loyalty),
    non_loyalty = select(., mstrprodid, year, quarter, week_num, qty=qty_non_loyalty, price=price_non_loyalty, store=store_non_loyalty)
  ), function(x) x %>%
      arrange(mstrprodid, year, week_num) %>% 
      group_by(mstrprodid) %>% 
      mutate(
        qty = as.integer(qty),
        store = as.integer(store),
        qty_bs = qty/store,
        dprice = c(NA, diff(price)/price[1:(n()-1)] ),
        dqty = c(NA, diff(qty)/qty[1:(n()-1)] ),
        dqty_bs = c(NA, diff(qty_bs)/qty_bs[1:(n()-1)]),
        dstore = c(NA, diff(store)/store[1:(n()-1)]),
        elasticity = dqty_bs/dprice
      ) %>% ungroup %>% 
    mutate(
      elasticity = replace(
        elasticity,
        !is.finite(elasticity),
        NA
      )
    ) %>% 
    compute_clusters %>% 
    cor_clustering(7) %>% 
    ungroup
  )
} -> datasets

apply_to_list(datasets, function(x, n) {
  x %>% select(
    mstrprodid, 
    year, quarter, week_num, 
    cluster_name, cor_cluster_d,
    price, qty, qty_bs, store, elasticity,
    dprice, dqty, dqty_bs
  ) %>% 
    setNames(c(
      'pid','year','quarter','week_num', 
      paste0( c(
        "price_tier_", "cor_cluster_",
        "price_", "qty_", "qty_bs_", "store_",
        "elasticity_", "dprice_", "dqty_", "dqty_bs_"
      ), n)
    ))
}, function(l,r) 
  inner_join(l,r,by=c('pid','year','quarter','week_num'))
) -> joined_dataset

lapply(datasets, function(x) x %>% 
   group_by(mstrprodid) %>% 
   arrange(year, week_num) %>% 
   summarise(
     qty = sum(qty),
     qty_bs = sum(qty_bs),
     revenue = sum(qty*price),
     revenue_bs = sum(qty_bs*price),
     store.first = store[1],
     store.mean = avg(store),
     store.last = store[n()],
     price.first = price[1],
     price.last = price[n()],
     price.mean = avg(price),
     price.max = max(price),
     price.min = min(price),
     price.sd = sd(price),
     price.m3 = avg(price)/sd(price),
     elasticity.mean = avg(elasticity),
     dprice.mean = avg(dprice),
     dqty_bs.mean = avg(dqty_bs),
     period.min = min(year + (week_num-1)/53),
     period.max = max(year + (week_num-1)/53),
     N = n()
   ) %>% ungroup %>% 
   mutate(
     revenue.annual_mean = revenue/(period.max-period.min+1),
     revenue_bs.annual_mean = revenue_bs/(period.max-period.min+1),
     qty.annual_mean = qty/(period.max-period.min+1),
     qty_bs.annual_mean = qty_bs/(period.max-period.min+1),
     elasticity.overall = dqty_bs.mean/dprice.mean, 
     product_class = case_when(
       price.sd == 0 ~ "Unchanging",
       price.max == 0 ~ "Promotional",
       price.min == 0 ~ "Had Promotional Period",
       N <= 2 ~ "Insufficient Data",
       TRUE ~ "Other"
       
     )
   ) 
) -> product_summaries

lapply(datasets, function(x) x%>% 
   group_by(mstrprodid, year, quarter) %>% 
   summarise(
     N=sum(!is.na(price)),
     qty=sum(qty_bs),
     elasticity=avg(elasticity),
     price_sd=sd(price, na.rm=T),
     price=avg(price)
   ) %>% 
   group_by(mstrprodid, quarter) %>% 
   summarise(
     qty=qty[n()],
     elasticity=avg(elasticity),
     price=avg(price),
     N=sum(N),
     price_sd.mean = mean(price_sd)
   ) %>% ungroup() %>% {
     select(., 
            mstrprodid,
            quarter,
            elasticity
     ) %>% group_by(mstrprodid) %>% 
       spread(quarter, elasticity) %>% 
       gather(quarter, elasticity, -c(mstrprodid)) %>% 
       mutate(quarter = as.integer(quarter)) -> ela 
     right_join(select(.,  mstrprodid, quarter, price, qty, price_sd.mean, N),ela,
                by=c("mstrprodid","quarter")
     )
   } %>% 
   group_by(mstrprodid, quarter) %>% 
   summarise(
     price = mean(price, na.rm=T),
     qty = mean(qty, na.rm=T),
     N = sum(N, na.rm=T),
     elasticity = mean(elasticity, na.rm=T),
     price_sd.mean = mean(price_sd.mean, na.rm=T)
   ) %>% 
   group_by(mstrprodid) %>% 
   filter(
     sum(N) > 2*3
   ) %>% 
   filter(
     mean(price_sd.mean, na.rm=T) > 0
   ) %>% 
   mutate(
     elasticity = replace_na(elasticity, mean(elasticity, na.rm=T)),
     price = replace_na(price, mean(price, na.rm=T)),
     qty = replace_na(qty, mean(qty, na.rm=T))
   ) %>% ungroup() %>% 
   select(
     mstrprodid,
     quarter,
     price, qty,
     elasticity
   ) 
) -> ela_summary

source("scripts/save_datas.R")
