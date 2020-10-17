rm(list=ls())

library(tidyverse)
library(ipumsr)


#load
setwd("data_raw/")
source("usa_00029.R")
setwd("..")





#sequence -------------
source("code/prep/edit_data_types.R")

#end sequence -------