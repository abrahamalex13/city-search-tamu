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

km <- kmeans(df_cluster, centers = 3)




#interpret km groupings ---------

df_explore1 <- cbind("cluster" = km$cluster, cities_keep)
write_csv(df_explore1, "data_processed/select_msa_clusters_grad_inmove_rates.csv")

df_explore <- df_explore1 %>% 
  gather(key = DEGFIELD, value = inmove_per_thous, 
         -c(cluster, MET2013_chr, pop_total.1))

p <- 
  ggplot(df_explore %>% 
           dplyr::filter(DEGFIELD != "N/A")) + 
    geom_boxplot(aes(DEGFIELD, inmove_per_thous, 
                     group = interaction(DEGFIELD, cluster), 
                     color = factor(cluster))) + 
    coord_flip() + 
    theme(legend.position = "top")
pdf("plots/inmove_rates_by_cluster_major.pdf", width = 12, height = 12)
print(p)
dev.off()
  