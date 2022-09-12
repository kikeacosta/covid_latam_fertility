source("Code/00_functions.R")

db <- 
  read_rds("data_inter/weekly_excess_confirmed_brazil_colombia.rds")

# from weekly to monthly deaths 
db2 <- 
  db %>% 
  mutate(ini_month = make_date(d  = 1, m = month(date), y = year(date)),
         days_in_mth = as.numeric(date - ini_month) + 1,
         frc = ifelse(days_in_mth >= 7, 1, days_in_mth/7),
         dts_mth_i = dts * frc,
         dts_mth_lag = dts * (1 - frc),
         bsn_mth_i = bsn * frc,
         bsn_mth_lag = bsn * (1 - frc))

dts <- 
  db2 %>%
  select(country, div, code, ini_month, dts_mth_i, dts_mth_lag) %>% 
  gather(dts_mth_i, dts_mth_lag, key = per, value = dts) %>% 
  mutate(date = case_when(per == "dts_mth_i" ~ ini_month,
                          TRUE ~ ini_month - months(1))) %>% 
  group_by(country, div, code, date) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()

bsn <- 
  db2 %>% 
  select(country, div, code, ini_month, bsn_mth_i, bsn_mth_lag) %>% 
  gather(bsn_mth_i, bsn_mth_lag, key = per, value = bsn) %>% 
  mutate(date = case_when(per == "bsn_mth_i" ~ ini_month,
                          TRUE ~ ini_month - months(1))) %>% 
  group_by(country, div, code, date) %>% 
  summarise(bsn = sum(bsn)) %>% 
  ungroup()

db3 <- 
  dts %>% 
  mutate(year = year(date)) %>% 
  left_join(bsn) %>% 
  filter(date >= "2020-01-01" & date <= "2021-12-31",
         div != "Total") %>% 
  mutate(pscore = dts / bsn)

db4 <- 
  db3 %>% 
  mutate(dts_exc_pos = ifelse(dts > bsn, dts - bsn, 0)) %>% 
  group_by(country, div, code) %>% 
  mutate(cum_bsn = cumsum(bsn),
         cum_dts_exc_pos = cumsum(dts_exc_pos),
         cum_avg_pscore = cumsum(pscore) / seq_along(pscore)) %>% 
  ungroup() %>% 
  mutate(cum_pscore = (cum_bsn + cum_dts_exc_pos) / cum_bsn)

db4 %>% 
  mutate(country_div = paste0(country, "_", div)) %>% 
  ggplot()+
  geom_point(aes(cum_pscore, country_div, col = date))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()

db4 %>% 
  mutate(country_div = paste0(country, "_", div)) %>% 
  ggplot()+
  geom_point(aes(cum_avg_pscore, country_div, col = date))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()

db5 <- 
  db4 %>% 
  mutate(trim_n = quarter(date),
         year = year(date),
         trimstr = case_when(year == 2020 & trim_n == 1 ~ "Jan-Mar\n2020",
                             year == 2020 & trim_n == 2 ~ "Apr-Jun\n2020",
                             year == 2020 & trim_n == 3 ~ "Jul-Sep\n2020",
                             year == 2020 & trim_n == 4 ~ "Oct-Dec\n2020",
                             year == 2021 & trim_n == 1 ~ "Jan-Mar\n2021",
                             year == 2021 & trim_n == 2 ~ "Apr-Jun\n2021",
                             year == 2021 & trim_n == 3 ~ "Jul-Sep\n2021",
                             year == 2021 & trim_n == 4 ~ "Oct-Dec\n2021"),
         trimstr = factor(trimstr, levels = c("Jan-Mar\n2020",
                                              "Apr-Jun\n2020",
                                              "Jul-Sep\n2020",
                                              "Oct-Dec\n2020",
                                              "Jan-Mar\n2021",
                                              "Apr-Jun\n2021",
                                              "Jul-Sep\n2021",
                                              "Oct-Dec\n2021"))) %>% 
  group_by(country, div, code, year, trimstr) %>% 
  summarise(cum_pscore = mean(cum_pscore),
            cum_avg_pscore = mean(cum_avg_pscore)) %>% 
  ungroup() 

write_rds(db5, "data_inter/trimestral_cumulative_pscores.rds")
