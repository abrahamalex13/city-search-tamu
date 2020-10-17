

#parameterized variable names.
varname_weight <- "PERWT"

varname_geo <- "MET2013"
varname_geo.sym <- rlang::sym(varname_geo)

varname_time <- "YEAR"
varname_time.sym <- rlang::sym(varname_time)

varname_group.syms <- rlang::syms(c(varname_time, varname_geo))



#total population ---------------

pop <- data %>% 
  group_by(!!!varname_group.syms) %>% 
  summarize(pop_total = sum(PERWT))
  
ggplot(pop %>% dplyr::filter(!!varname_geo.sym != 0)) + 
  geom_density(aes(pop_total))

quantile(pop[["pop_total"]], probs = seq(0, 1, by = .1))
#~80% of metros have 1m or fewer people. 
#1m, nice round number, seems natural cutoff.


#subset to those metros with >1m people. cutoffs?
quantile(pop[["pop_total"]][pop[["pop_total"]] > 1e6], 
         probs = seq(0, 1, by = .05))

# ----------------
