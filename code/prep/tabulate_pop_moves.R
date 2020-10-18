#tabulate_pop_moves.R


#parameterized variable names.
varname_weight <- "PERWT"

varname_geo <- "MET2013_chr"
varname_geo.sym <- rlang::sym(varname_geo)

varname_time <- "YEAR"
varname_time.sym <- rlang::sym(varname_time)

varname_group.syms <- rlang::syms(c(varname_time, varname_geo))



#tabulate total population ---------------

pop <- primary1 %>% 
  group_by(!!!varname_group.syms) %>% 
  summarize(pop_total = sum(PERWT)) %>% 
  ungroup()
  
ggplot(pop %>% dplyr::filter(!!varname_geo.sym != 0)) + 
  geom_density(aes(pop_total))

quantile(pop %>% dplyr::filter(YEAR == 2013) %>% pull(pop_total), 
         probs = seq(0, 1, by = .1))
#~80% of metros have 1m or fewer people. 
#1m, nice round number, seems natural cutoff.


#subset to those metros with >1m people. cutoffs?
quantile(pop[["pop_total"]][pop[["pop_total"]] > 1e6], 
         probs = seq(0, 1, by = .05))

# ----------------






# tabulate inmoves for an age subset of scholars ------

age_min <- 21
age_max <- 25
varname_filter <- "AGE"
varname_filter.sym <- rlang::sym(varname_filter)
varname_educ <- "DEGFIELD_chr"
varname_educ.sym <- rlang::sym(varname_educ)

inmoves_age_sub <- primary1 %>% 
  dplyr::filter(!!varname_filter.sym >= age_min & 
                !!varname_filter.sym <= age_max) %>% 
  group_by(!!varname_time.sym, !!varname_geo.sym, !!varname_educ.sym) %>% 
  summarize(inmoves = sum(PERWT))

# -------------




# normalize inmove counts, prep for clustering
inmoves_scholars_age_sub1 <- inmoves_age_sub %>% 
  left_join(., 
            pop %>% 
              mutate(YEAR = YEAR + 1) %>% 
              rename(pop_total.1 = pop_total)) %>% 
  mutate(inmoves_per_thous = inmoves / (pop_total.1/1000) )

ggplot(inmoves_scholars_age_sub1 %>% 
         dplyr::filter(DEGFIELD_chr != "N/A" & pop_total.1 > 1e6)) + 
  geom_density_ridges(aes(x = inmoves_per_thous, y = DEGFIELD_chr))

inmoves_scholars_age_sub <- inmoves_scholars_age_sub1 %>% 
  dplyr::select(-inmoves) %>% 
  spread(key = DEGFIELD_chr, value = inmoves_per_thous)

#assume missing implies 0
inmoves_scholars_age_sub[is.na(inmoves_scholars_age_sub) ] <- 0




#aggregate to cross section
inmoves_scholars_age_sub_crs <- inmoves_scholars_age_sub %>% 
  group_by(!!varname_geo.sym) %>% 
  dplyr::filter(YEAR != 2010) %>% 
  dplyr::select(-YEAR) %>% 
  summarize_all(mean)
write_csv(inmoves_scholars_age_sub_crs, na = "#N/A",
          "data_processed/scholars_inmoves_per_thous_11-18avg.csv")
