rm(list=ls())
source("Code/00_functions.R")
dts <- read_rds("data_inter/db_monthly_excess_deaths_bra_col_mex.rds")
bts <- read_rds("data_inter/db_monthly_excess_births_bra_col_mex.rds")

dts2 <- 
  dts %>% 
  mutate(country = case_when(country == "Brazil" ~ "BRA",
                             country == "Colombia" ~ "COL",
                             country == "Mexico" ~ "MEX"))


