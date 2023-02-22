rm(list=ls())
source("Code/00_functions.R")

# loading data
# ~~~~~~~~~~~~
dts <- read_rds("data_inter/monthly_excess_deaths_bra_col_mex.rds")
dts_cum <- read_rds("data_inter/monthly_cumulative_pscores.rds")
bts <- read_rds("data_inter/monthly_excess_births_bra_col_mex_edu03.rds")
geo_codes <- read_csv("data_input/geo_codes_bra_col_mex.csv", 
                      locale = readr::locale(encoding = "latin1"))

probs <- read_rds("data_inter/problematic_combinations_baseline_fertility.rds")

dts2 <- 
  dts %>% 
  mutate(geo = ifelse(geo == "Total", "total", geo))

# cumulative p-scores
# ~~~~~~~~~~~~~~~~~~~
dts_cum2 <- 
  dts_cum %>% 
  select(country, geo, date, dts_cum_psc = cum_psc) %>% 
  mutate(geo = ifelse(geo == "Total", "total", geo))

# fertility p-scores
# ~~~~~~~~~~~~~~~~~~
bts2 <- 
  bts %>% 
  rename(month = mth,
         bts_bsn = bsn,
         bts_bsn_lp = bsn_lp,
         bts_bsn_up = bsn_up) %>% 
  filter(date >= "2020-01-01") %>% 
  mutate(bts_psc = bts / bts_bsn)

# codes and names for subnational divisions
geo_codes2 <- 
  geo_codes %>% 
  select(country = ISO_Code,
         geo, raw_geolev1, region_shdi, geo_label, geo_type = type)

unique(geo_codes2$geo_type)

probs2 <- 
  probs %>% 
  mutate(flag_prob = 1)


# all together
# ~~~~~~~~~~~~
dts_bts <- 
  bts2 %>% 
  left_join(dts2) %>% 
  left_join(dts_cum2) %>% 
  left_join(geo_codes2) %>% 
  mutate(bts_psc_unc = ifelse(bts > bts_bsn_up | bts < bts_bsn_lp,
                              bts_psc, 1),
         dts_psc_unc = ifelse(dts > dts_bsn_up | dts < dts_bsn_lp,
                              dts_psc, 1)) %>% 
  select(country, geo, geo_label, region_shdi, raw_geolev1, geo_type, 
         everything(), -t, -w) %>% 
  left_join(probs2) %>% 
  mutate(flag_prob = ifelse(is.na(flag_prob), 0, 1))

# saving outputs
write_rds(dts_bts, "data_inter/master_monthly_excess_deaths_births_bra_col_mex_edu03.rds")

dts_bts <- read_rds("data_inter/master_monthly_excess_deaths_births_bra_col_mex_edu03.rds")

unique(dts_bts$geo)

test <- 
  dts_bts %>% 
  filter(is.na(raw_geolev1)) %>% 
  select(country, geo) %>% unique

