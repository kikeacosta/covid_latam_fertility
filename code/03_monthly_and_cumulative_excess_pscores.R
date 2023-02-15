rm(list=ls())
source("Code/00_functions.R")

bsn1 <- read_rds("data_inter/monthly_baseline_mortality_2015_2021.rds")

mth_out <- 
  bsn1 %>% 
  filter(date >= "2020-01-01",
         date <= "2021-12-31") %>%
  mutate(bsn = ifelse(bsn > dts, dts, bsn),
         exc = dts - bsn,
         psc = dts / bsn,
         pop = exposure * 12,
         date = date + days(14)) %>% 
  select(country, geo, date, year, month, 
         dts, pop, 
         dts_bsn = bsn, 
         dts_bsn_lp = ll, 
         dts_bsn_up = ul, 
         dts_psc = psc)

write_rds(mth_out, "data_inter/monthly_excess_deaths_bra_col_mex.rds")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mth_out <- read_rds("data_inter/monthly_excess_deaths_bra_col_mex.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~
# cumulative p-scores ====
# ~~~~~~~~~~~~~~~~~~~~~~~~
cum_psc <- 
  mth_out %>% 
  mutate(dts_exc_pos = ifelse(dts > dts_bsn, dts - dts_bsn, 0)) %>% 
  group_by(country, geo) %>% 
  mutate(cum_bsn = cumsum(dts_bsn),
         cum_dts_exc_pos = cumsum(dts_exc_pos),
         cum_avg_psc = cumsum(dts_psc) / seq_along(dts_psc)) %>% 
  ungroup() %>% 
  mutate(cum_psc = (cum_bsn + cum_dts_exc_pos) / cum_bsn)

cum_psc %>% 
  mutate(country_geo = paste0(country, "_", geo)) %>% 
  ggplot()+
  geom_point(aes(cum_psc, country_geo, col = date))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()

cum_psc %>% 
  mutate(country_geo = paste0(country, "_", geo)) %>% 
  ggplot()+
  geom_point(aes(cum_avg_psc, country_geo, col = date))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()

write_rds(cum_psc, "data_inter/monthly_cumulative_pscores.rds")

