rm(list=ls())
source("Code/00_functions.R")

# loading data
# ~~~~~~~~~~~~
dts <- read_rds("data_inter/monthly_excess_deaths_bra_col_mex.rds")
dts_cum <- read_rds("data_inter/monthly_cumulative_pscores.rds")
bts <- read_rds("data_inter/monthly_excess_births_bra_col_mex.rds")
geo_codes <- read_csv("data_input/geo_codes_bra_col_mex.csv", 
                      locale = readr::locale(encoding = "latin1"))

# mortality p-scores
# ~~~~~~~~~~~~~~~~~~
dts2 <- 
  dts %>% 
  rename(dts_bsn = bsn,
         dts_psc = pscore)

# cumulative p-scores
# ~~~~~~~~~~~~~~~~~~~
dts_cum2 <- 
  dts_cum %>% 
  select(country, geo, date, dts_cum_psc = cum_pscore)

# fertility p-scores
# ~~~~~~~~~~~~~~~~~~
bts2 <- 
  bts %>% 
  rename(month = mth,
         bts_bsn = bsn,
         bts_bsn_lc = bsn_lc,
         bts_bsn_uc = bsn_uc,
         bts_bsn_lp = bsn_lp,
         bts_bsn_up = bsn_up) %>% 
  mutate(geo = case_when(geo == "total" ~ "Total",
                         TRUE ~ geo)) %>% 
  filter(date >= "2020-01-01") %>% 
  mutate(bts_psc = bts / bts_bsn)

# codes and names for subnational divisions
geo_codes2 <- 
  geo_codes %>% 
  select(country = ISO_Code,
         geo, raw_geolev1, region_shdi, geo_label)

# all together
# ~~~~~~~~~~~~
dts_bts <- 
  bts2 %>% 
  left_join(dts2) %>% 
  left_join(dts_cum2) %>% 
  left_join(geo_codes2)

# saving outputs
write_rds(dts_bts, "data_inter/master_monthly_excess_deaths_births_bra_col_mex.rds")

test <- 
  dts_bts %>% 
  filter(is.na(raw_geolev1)) %>% 
  select(country, geo) %>% unique

