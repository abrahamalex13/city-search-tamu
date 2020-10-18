#kmeans.R



#how might majors cluster together?
df_cluster <- inmoves_scholars_age_sub_crs %>% 
  dplyr::select(-c(MET2013_chr, pop_total.1, `N/A`))

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
                "degree" = inmoves_scholars_age_sub_crs[["MET2013_chr"]]))
