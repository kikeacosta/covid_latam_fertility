rm(list=ls())
source("Code/00_functions.R")

# monthly births
dt <- 
  readRDS("data_inter/master_births_for_baseline_estimation.rds") 


# codes 
codes <- 
  read_csv("data_input/geo_codes_bra_col_mex.csv", 
           locale = readr::locale(encoding = "latin1")) %>% 
  rename(country = ISO_Code) %>% 
  select(country, geo, raw_geolev1) %>% 
  unique()



# ~~~~~~~~~~~~~~~~~~~~~~~~
# baseline estimation ====
# ~~~~~~~~~~~~~~~~~~~~~~~~
# quick test with total national by educational level 
test <- 
  dt %>% 
  mutate(bts = bts + 1) %>% 
  filter(geo == "total",
         # edu == "8-11",
         age == "20-29") %>% 
  group_by(country, geo, age, edu, imp_type) %>%
  do(pred_births(chunk = .data)) %>% 
  ungroup()
%>% 
  mutate(bts = bts - 1,
         bsn = bsn - 1,
         bsn_lp = bsn_lp - 1,
         bsn_up = bsn_up - 1)

test %>% 
  filter(geo == "total",
         edu != "total",
         imp_type == "i") %>% 
  ggplot()+
  geom_line(aes(date, bts, linetype = edu, group = edu), 
            col = "black")+
  geom_ribbon(aes(date, ymin = bsn_lp, ymax = bsn_up, group = edu), fill = "red", alpha = 0.2)+
  geom_ribbon(aes(date, ymin = bsn_lc, ymax = bsn_uc, group = edu), fill = "red", alpha = 0.4)+
  geom_line(aes(date, bsn, linetype = edu), col = "red")+
  geom_vline(xintercept = c(ymd("2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~country, scales = "free_y")+
  theme_bw()

ggsave("figures/births_monthly_baseline_national_levels_all_ages.png",
       w = 10,
       h = 5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# estimating baselines for all combinations 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 23 mins at home
# 30 mins in hydra 11
# 15 mins at the office

bsn <- 
  dt %>% 
  mutate(bts = bts + 1) %>% 
  group_by(country, geo, age, edu, imp_type) %>%
  do(pred_births(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(bts = bts - 1,
         bsn = bsn - 1)

,
         bsn_lp = bsn_lp - 1,
         bsn_up = bsn_up - 1)

bsn_out <- 
  bsn %>% 
  mutate(bsn = ifelse(bsn < 0, 0, bsn),
         bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
         bsn_up = ifelse(bsn_up < 0, 0, bsn_up))

# saving outputs
write_rds(bsn_out, "data_inter/monthly_excess_births_bra_col_mex_edu03.rds")

bsn_out %>% 
  filter(geo == "total",
         age == "total",
         imp_type == "i") %>% 
  mutate(dts_r = dts / exposure,
         bsn_r = bsn / exposure,
         ll_r = ll / exposure,
         ul_r = ul / exposure) %>% 
  ggplot()+
  geom_line(aes(date, bts, linetype = edu, group = edu), 
            col = "black")+
  geom_ribbon(aes(date, ymin = bsn_lp, ymax = bsn_up, group = edu), fill = "red", alpha = 0.2)+
  geom_line(aes(date, bsn, linetype = edu), col = "red")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~country, scales = "free_y")+
  theme_bw()

ggsave("figures/births_monthly_baseline_national_levels_all_ages.png",
       w = 10,
       h = 5)

bsn_out %>% 
  filter(geo == "total",
         edu == "total",
         imp_type == "i") %>% 
  mutate(dts_r = dts / exposure,
         bsn_r = bsn / exposure,
         ll_r = ll / exposure,
         ul_r = ul / exposure) %>% 
  ggplot()+
  geom_line(aes(date, bts, linetype = age, group = age), 
            col = "black")+
  geom_ribbon(aes(date, ymin = bsn_lp, ymax = bsn_up, group = age), fill = "red", alpha = 0.2)+
  geom_line(aes(date, bsn, linetype = age), col = "red")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~country, scales = "free_y")+
  theme_bw()

ggsave("figures/births_monthly_baseline_national_levels_all_educ.png",
       w = 10,
       h = 5)

bsn_out %>% 
  filter(country == "MEX",
         geo == "total",
         edu != "total",
         age != "total",
         imp_type == "i") %>% 
  group_by(year) %>% 
  summarise(bts = sum(bts))

