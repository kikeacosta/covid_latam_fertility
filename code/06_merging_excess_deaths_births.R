rm(list=ls())
source("Code/00_functions.R")
dts <- read_rds("data_inter/db_monthly_excess_deaths_bra_col_mex.rds")
bts <- read_rds("data_inter/db_monthly_excess_births_bra_col_mex.rds")
cds <- read_rds("data_inter/geo_codes_bra_col_mex.rds")

dts2 <- 
  dts %>% 
  mutate(country = case_when(country == "Brazil" ~ "BRA",
                             country == "Colombia" ~ "COL",
                             country == "Mexico" ~ "MEX"))

bts2 <- 
  bts %>% 
  mutate(year = year %>% as.integer()) %>% 
  filter(date >= "2020-01-01") %>% 
  mutate(psc_bts_n = bts_n / bsn_n,
         psc_bts_i = bts_i / bsn_i,
         psc_bts_t = bts_t / bsn_t)

dts_bts <- 
  bts2 %>% 
  left_join(dts2) %>% 
  left_join(cds)

# saving outputs
write_rds(dts_bts, "data_inter/master_monthly_excess_deaths_births_bra_col_mex.rds")


test <- 
  dts_bts %>% 
  filter(is.na(raw_geolev1)) %>% 
  select(country, geo) %>% unique

