rm(list=ls())
source("Code/00_functions.R")

# loading data
# ~~~~~~~~~~~~
dts <- read_rds("data_inter/monthly_excess_deaths_bra_col_mex.rds")
dts_cum <- read_rds("data_inter/monthly_cumulative_pscores.rds")
bts <- read_rds("data_inter/monthly_excess_births_bra_col_mex_edu03.rds")
geo_codes <- read_csv("data_input/geo_codes_bra_col_mex.csv", 
                      locale = readr::locale(encoding = "latin1"))

# probs <- read_rds("data_inter/problematic_combinations_baseline_fertility.rds")

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

# identifying problematic cases
bts_issues <- 
  bts %>% 
  filter(date <= "2020-01-01") %>% 
  # periods with births
  mutate(bts_oc = ifelse(bts > 0, 1, 0)) %>% 
  group_by(country, geo, age, edu) %>% 
  mutate(bts_iss = ifelse(is.na(bsn_uc), 1, 0)) %>% 
  summarise(bts_avg = round(mean(bts), 1),
            bts_occ = mean(bts_oc),
            bts_iss = ifelse(any(is.na(bsn_uc)), 1, 0)) %>% 
  ungroup() 
  
# selecting prediction intervals of 60% and 80% for uncertainty
bts2 <- 
  bts %>% 
  rename(month = mth,
         bts_bsn = bsn,
         bts_bsn_lp60 = bsn_lp2,
         bts_bsn_up60 = bsn_up2,
         bts_bsn_lp80 = bsn_lp4,
         bts_bsn_up80 = bsn_up4) %>% 
  filter(date >= "2020-01-01") %>% 
  mutate(bts_psc = bts / bts_bsn) %>% 
  select(country, geo, date, year, month, age, edu, imp_type, bts, 
         bts_bsn, bts_psc, bts_bsn_lp60, bts_bsn_up60, bts_bsn_lp80, bts_bsn_up80)

# codes and names for subnational divisions
geo_codes2 <- 
  geo_codes %>% 
  select(country = ISO_Code,
         geo, raw_geolev1, region_shdi, geo_label, geo_type = type)

unique(geo_codes2$geo_type)


# all together
# ~~~~~~~~~~~~
dts_bts <- 
  bts2 %>% 
  left_join(dts2) %>% 
  left_join(dts_cum2) %>% 
  left_join(geo_codes2) %>% 
  mutate(bts_psc_unc60 = ifelse(bts > bts_bsn_up60 | bts < bts_bsn_lp60,
                                bts_psc, 1),
         bts_psc_unc80 = ifelse(bts > bts_bsn_up80 | bts < bts_bsn_lp80,
                                bts_psc, 1),
         dts_psc_unc = ifelse(dts > dts_bsn_up | dts < dts_bsn_lp,
                              dts_psc, 1)) %>% 
  select(country, geo, geo_label, region_shdi, raw_geolev1, geo_type, 
         everything()) 
# %>% 
  # left_join(probs2) %>% 
  # mutate(flag_prob = ifelse(is.na(flag_prob), 0, 1))

# saving outputs
write_rds(dts_bts, "data_inter/master_monthly_excess_deaths_births_bra_col_mex_edu03.rds")

dts_bts <- read_rds("data_inter/master_monthly_excess_deaths_births_bra_col_mex_edu03.rds")

unique(dts_bts$geo)

test <- 
  dts_bts %>% 
  filter(is.na(raw_geolev1)) %>% 
  select(country, geo) %>% unique

