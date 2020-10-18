rm(list=ls())

library(tidyverse)
library(ipumsr)


#load
setwd("data_raw/")
source("usa_00029.R")
setwd("..")

primary1 <- data %>% 
  dplyr::select(YEAR, MET2013, PERWT, AGE, DEGFIELD, MIGRATE1)





#sequence -------------

source("code/prep/edit_data_types.R")
source("code/prep/tabulate_pop_moves.R")

source("kmeans.R")

#end sequence -------