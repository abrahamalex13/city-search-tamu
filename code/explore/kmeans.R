#kmeans.R


#pre-screening for final clusters data ---------

cities_keep <- inmoves_scholars_age_sub_crs %>% 
  dplyr::filter(pop_total.1 > 1e6)

df_cluster <- cities_keep %>%  
  dplyr::select(-c(MET2013_chr, pop_total.1, `N/A`))

#end pre-screen ---------




#how might majors cluster together?
km <- kmeans(t(df_cluster), centers = 10)
# View(data.frame("cluster" = km$cluster, "degree" = colnames(df_cluster)))


#cluster cities directly; use empirical guide for K choice
calc_ch_index <- function(df_cluster, K) {
  
  n <- nrow(df_cluster)
  
  km <- kmeans(df_cluster, K) 
  w <- km$tot.withinss
  b <- km$betweenss
  ch <- ( b/(K-1) ) / (w/(n-K))
  
  return(ch)
  
}
ch_index <- 
  unlist( lapply(2:20, df_cluster = df_cluster, FUN = calc_ch_index) )
which.max(ch_index)

km <- kmeans(df_cluster, centers = 2)
View(data.frame("cluster" = km$cluster, 
                "degree" = cities_keep[["MET2013_chr"]]))
