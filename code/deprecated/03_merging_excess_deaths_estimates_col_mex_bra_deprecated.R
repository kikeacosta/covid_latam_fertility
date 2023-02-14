rm(list=ls())
source("Code/00_functions.R")
codes <- read_csv("data_input/iso_codes.csv")
codes <- 
  read_xlsx("data_input/codes_depto_states.xlsx") %>% 
  rename(country = ISO_Code) %>% 
  select(country, geo_std_gr, geo_iso) %>% 
  unique()

# ~~~~~~~~
# Colombia
# ~~~~~~~~
col_bsn <- read_rds("data_inter/colombia_baseline_weekly_2015_2021.rds")

col_bsn2 <- 
  col_bsn %>% 
  mutate(excess = dts - bsn) %>% 
  select(country, geo_std_gr = geo, date, isoweek, dts, bsn, ll, ul, exposure, p_score)

unique(col_bsn2$geo_std_gr)
# ~~~~~~
# Brazil
# ~~~~~~
bra_bsn <- read_rds("data_inter/brazil_baseline_weekly_2015_2021.rds")

bra_bsn2 <- 
  bra_bsn %>% 
  select(country, geo_iso = code, date, isoweek, dts, bsn, ll, ul, exposure, p_score) %>% 
  left_join(codes) %>% 
  select(country, geo_std_gr, date, isoweek, dts, bsn, ll, ul, exposure, p_score)

unique(bra_bsn2$geo_std_gr)


# ~~~~~~
# Mexico
# ~~~~~~
mex_bsn <- read_rds("data_inter/mexico_baseline_weekly_2015_2021.rds")

mex_bsn2 <- 
  mex_bsn %>% 
  rename(geo = div) %>% 
  mutate(country = "MEX") %>% 
  select(country, geo_std_gr = geo, date, isoweek, dts, bsn, ll, ul, exposure, p_score)

unique(mex_bsn2$geo_std_gr)

# ~~~~~~~~~~~~~~~~~~~~
# putting all together
# ~~~~~~~~~~~~~~~~~~~~

all <- 
  bind_rows(col_bsn2,
            bra_bsn2,
            mex_bsn2) %>% 
  filter(date >= "2020-01-01",
         date <= "2021-12-31") %>%
  mutate(bsn = ifelse(bsn > dts, dts, bsn),
         exc = dts - bsn,
         psc = dts / bsn) %>% 
  rename(geo = geo_std_gr)


write_rds(all, "data_inter/db_weekly_excess_deaths_bra_col_mex.rds")
# write_csv(all, "data_inter/weekly_excess_confirmed_brazil_colombia_mexico.csv")

# 
# # number of deaths between 2015 and 2021
# dts <- 
#   all %>% 
#   filter(date >= "2015-01-01",
#          date <= "2021-12-31",
#          geo == "Total") %>% 
#   group_by() %>% 
#   summarise(dts = sum(dts))
#   
# all %>% 
#   filter(date >= "2015-01-01",
#          date <= "2021-12-31",
#          geo == "Total") %>% 
#   group_by(country) %>% 
#   summarise(dts = sum(dts))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# from weekly to monthly deaths 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db2 <- 
  all %>% 
  mutate(ini_month = make_date(d  = 1, m = month(date), y = year(date)),
         days_in_mth = as.numeric(date - ini_month) + 1,
         frc = ifelse(days_in_mth >= 7, 1, days_in_mth/7),
         dts_mth_i = dts * frc,
         dts_mth_lag = dts * (1 - frc),
         bsn_mth_i = bsn * frc,
         bsn_mth_lag = bsn * (1 - frc))

test <- 
  db2 %>% 
  filter(date == "2020-01-05")

dts <- 
  db2 %>%
  select(country, geo, ini_month, dts_mth_i, dts_mth_lag) %>% 
  gather(dts_mth_i, dts_mth_lag, key = per, value = dts) %>% 
  mutate(date = case_when(per == "dts_mth_i" ~ ini_month,
                          TRUE ~ ini_month - months(1))) %>% 
  group_by(country, geo, date) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()

bsn <- 
  db2 %>% 
  select(country, geo, ini_month, bsn_mth_i, bsn_mth_lag) %>% 
  gather(bsn_mth_i, bsn_mth_lag, key = per, value = bsn) %>% 
  mutate(date = case_when(per == "bsn_mth_i" ~ ini_month,
                          TRUE ~ ini_month - months(1))) %>% 
  group_by(country, geo, date) %>% 
  summarise(bsn = sum(bsn)) %>% 
  ungroup()

pop <- 
  all %>% 
  filter(month(date) == 6) %>% 
  mutate(year = year(date)) %>% 
  select(country, geo, year, exposure) %>% 
  group_by(country, geo, year) %>% 
  summarise(pop = mean(exposure) * 52) %>% 
  ungroup()

db3 <- 
  dts %>% 
  mutate(year = year(date)) %>% 
  left_join(bsn) %>% 
  left_join(pop) %>% 
  filter(date >= "2020-01-01" & date <= "2021-12-31") %>% 
  mutate(pscore = dts / bsn,
         date = date + days(14))

# identifying regions with the highest average excess mortality

mth_out <- 
  db3 %>% 
  mutate(month = month(date)) %>% 
  select(country, geo, date, year, month, dts, pop, bsn, pscore)

write_rds(mth_out, "data_inter/db_monthly_excess_deaths_bra_col_mex.rds")
