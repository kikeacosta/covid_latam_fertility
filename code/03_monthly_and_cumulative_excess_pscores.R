rm(list=ls())
source("Code/00_functions.R")

bsn1 <- read_rds("data_inter/weekly_baseline_mortality_2015_2021.rds")

bsn2 <- 
  bsn1 %>% 
  filter(date >= "2020-01-01",
         date <= "2021-12-31") %>%
  mutate(bsn = ifelse(bsn > dts, dts, bsn),
         exc = dts - bsn,
         psc = dts / bsn)

write_rds(bsn2, "data_inter/weekly_excess_deaths_bra_col_mex.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# from weekly to monthly deaths 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bsn3 <- 
  bsn2 %>% 
  mutate(ini_month = make_date(d  = 1, m = month(date), y = year(date)),
         days_in_mth = as.numeric(date - ini_month) + 1,
         frc = ifelse(days_in_mth >= 7, 1, days_in_mth/7),
         dts_mth_i = dts * frc,
         dts_mth_lag = dts * (1 - frc),
         bsn_mth_i = bsn * frc,
         bsn_mth_lag = bsn * (1 - frc))

test <- 
  bsn3 %>% 
  filter(date == "2020-01-05")

dts <- 
  bsn3 %>%
  select(country, geo, ini_month, dts_mth_i, dts_mth_lag) %>% 
  gather(dts_mth_i, dts_mth_lag, key = per, value = dts) %>% 
  mutate(date = case_when(per == "dts_mth_i" ~ ini_month,
                          TRUE ~ ini_month - months(1))) %>% 
  group_by(country, geo, date) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()

bsn <- 
  bsn3 %>% 
  select(country, geo, ini_month, bsn_mth_i, bsn_mth_lag) %>% 
  gather(bsn_mth_i, bsn_mth_lag, key = per, value = bsn) %>% 
  mutate(date = case_when(per == "bsn_mth_i" ~ ini_month,
                          TRUE ~ ini_month - months(1))) %>% 
  group_by(country, geo, date) %>% 
  summarise(bsn = sum(bsn)) %>% 
  ungroup()

pop <- 
  bsn3 %>% 
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

mth_out <- 
  db3 %>% 
  mutate(month = month(date)) %>% 
  select(country, geo, date, year, month, dts, pop, bsn, pscore)

write_rds(mth_out, "data_inter/monthly_excess_deaths_bra_col_mex.rds")



# ~~~~~~~~~~~~~~~~~~~~~~~~
# cumulative p-scores ====
# ~~~~~~~~~~~~~~~~~~~~~~~~
cum_psc <- 
  mth_out %>% 
  mutate(dts_exc_pos = ifelse(dts > bsn, dts - bsn, 0)) %>% 
  group_by(country, geo) %>% 
  mutate(cum_bsn = cumsum(bsn),
         cum_dts_exc_pos = cumsum(dts_exc_pos),
         cum_avg_pscore = cumsum(pscore) / seq_along(pscore)) %>% 
  ungroup() %>% 
  mutate(cum_pscore = (cum_bsn + cum_dts_exc_pos) / cum_bsn)

cum_psc %>% 
  mutate(country_geo = paste0(country, "_", geo)) %>% 
  ggplot()+
  geom_point(aes(cum_pscore, country_geo, col = date))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()

cum_psc %>% 
  mutate(country_geo = paste0(country, "_", geo)) %>% 
  ggplot()+
  geom_point(aes(cum_avg_pscore, country_geo, col = date))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()

write_rds(cum_psc, "data_inter/monthly_cumulative_pscores.rds")



# trims <- 
#   cum_psc %>% 
#   mutate(trim_n = quarter(date),
#          year = year(date),
#          trimstr = case_when(year == 2020 & trim_n == 1 ~ "Jan-Mar\n2020",
#                              year == 2020 & trim_n == 2 ~ "Apr-Jun\n2020",
#                              year == 2020 & trim_n == 3 ~ "Jul-Sep\n2020",
#                              year == 2020 & trim_n == 4 ~ "Oct-Dec\n2020",
#                              year == 2021 & trim_n == 1 ~ "Jan-Mar\n2021",
#                              year == 2021 & trim_n == 2 ~ "Apr-Jun\n2021",
#                              year == 2021 & trim_n == 3 ~ "Jul-Sep\n2021",
#                              year == 2021 & trim_n == 4 ~ "Oct-Dec\n2021"),
#          trimstr = factor(trimstr, levels = c("Jan-Mar\n2020",
#                                               "Apr-Jun\n2020",
#                                               "Jul-Sep\n2020",
#                                               "Oct-Dec\n2020",
#                                               "Jan-Mar\n2021",
#                                               "Apr-Jun\n2021",
#                                               "Jul-Sep\n2021",
#                                               "Oct-Dec\n2021"))) %>% 
#   group_by(country, geo, code, year, trimstr) %>% 
#   summarise(cum_pscore = mean(cum_pscore),
#             cum_avg_pscore = mean(cum_avg_pscore)) %>% 
#   ungroup() 
# 


