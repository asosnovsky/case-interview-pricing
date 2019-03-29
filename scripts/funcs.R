
col_cors <- function(dt, remove_dups = F) {
  dt %>% cor %>% 
    as.data.frame %>% 
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1) %>%
    filter(var1 != var2) %>% 
    filter(!is.na(value)) -> 
    tmp
  
  if (remove_dups) {
    tmp %>% 
      unite(vars, var1, var2, sep=',') %>% 
      distinct(vars, value) %>% 
      separate(vars, c("var1", "var2"), ',') ->
      tmp
  }
  
  tmp %>% 
    arrange(desc(abs(value))) %>% 
    as_tibble
}

apply_to_list <- function(lst, mapper, reducer=bind_rows) {
  Map(mapper, lst, names(lst)) %>% 
  reduce(reducer)
}

avg <- function(x, na.rm=T, inf.rm=T) {
  if(inf.rm) {
    mean(x[!is.infinite(x)], na.rm = na.rm)
  }else{
    mean(x, na.rm=na.rm)
  }
}


save_dataset_list <- function(dt, save_loc) {
  dir.create(save_loc, showWarnings = F, recursive = T)
  for (n in names(dt)) {
    dt[[n]] %>% 
      write_csv(file.path(
        save_loc,
        paste0(n, ".csv")
      ))
  }
}

load_dataset_folder <- function(save_loc, ...) {
  files = dir(save_loc)
  csv_f = files[grep("\\.csv$", files)]
  fnames = gsub('\\.csv$', '', csv_f)
  lapply(fnames, function(name) 
    read_csv(file.path(save_loc, paste0(name, '.csv')),...)
  ) -> dt
  names(dt) = fnames
  dt
}

compute_clusters <- function(dt) {
  dt %>% 
    filter(!is.na(price)) %>% 
    group_by(year) %>% 
    mutate(
      cluster = kmeans(
        tibble(price),
        3
      )$cluster
    ) %>% {
      group_by(., cluster, year) %>% 
        summarise(
          min = min(price)
        ) %>% 
        arrange(year, min) %>% 
        group_by(year) %>% 
        mutate(
          cluster_name = c("Low Priced", "Medium Priced", "High Priced")
        ) %>% select(year, cluster, cluster_name) -> 
        clusters
      
      left_join(., clusters, by=c('cluster', 'year'))
    } %>% ungroup() %>% 
    mutate(
      price = replace(
        price,
        price == -10,
        NA
      )
    ) %>% select(-cluster)
}

cor_clustering <- function(dt, nclusters=5) {
  dt %>% mutate(period = year + (week_num-1)/53) %>% 
    arrange(mstrprodid, period) %>% 
    select(mstrprodid, period, qty_bs) %>% 
    group_by(period) %>% 
    spread(mstrprodid, qty_bs) %>% ungroup %>% select(-period) %>% 
    cor %>% 
    as.data.frame %>% 
    rownames_to_column('var1') %>%
    gather(var2, value, -c(var1)) %>% 
    mutate(
      value = replace_na(value, 0)
    ) %>% 
    group_by(var1) %>% spread(var2, value) %>% 
    column_to_rownames('var1') %>% as.matrix %>% as.dist %>% 
    hclust(method = 'complete') %>% cutree(nclusters) %>% {
      tibble(
        mstrprodid=names(.),
        cor_cluster_d=as.numeric(.)
      )
    } -> tmp
  inner_join(
    dt %>% mutate(mstrprodid=as.integer(mstrprodid)), 
    tmp %>% mutate(mstrprodid=as.integer(mstrprodid)), 
    by="mstrprodid"
  )
}
