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

all_geos_ages2 <- 
  dt %>% 
  filter(geo == "total",
         age == "total",
         edu != "total",
         imp_type == "i")

unique(all_geos_ages2$edu)
all_geos_ages2 %>%
  mutate(country = case_when(country == "BRA" ~ "Brazil",
                             country == "COL" ~ "Colombia",
                             country == "MEX" ~ "Mexico")) %>% 
  ggplot(aes(group = edu))+
  geom_line(aes(date, bts, col = edu, group = edu), alpha = .6)+
  geom_point(aes(date, bts, col = edu, group = edu), size = 0.6, alpha = .8)+
  # geom_ribbon(aes(date, ymin = bsn_lp1, ymax = bsn_up1, group = edu), fill = "#ef233c", alpha = 0.3)+
  # geom_ribbon(aes(date, ymin = bsn_lp3, ymax = bsn_up3, group = edu), fill = "red", alpha = 0.3)+
  # geom_ribbon(aes(date, ymin = bsn_lp5, ymax = bsn_up5, group = edu), fill = "red", alpha = 0.2)+
  # geom_ribbon(aes(date, ymin = bsn_lc, ymax = bsn_uc, group = edu), fill = "red", alpha = 0.2)+
  # geom_line(aes(date, bsn), col = "#ef233c")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")),
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  scale_color_manual(values = c("black", "grey60", "grey"))+
  facet_wrap(~country, scales = "free_y")+
  labs(y = "Births", x = "Months", col = "Education")+
  theme_bw()+
  theme()

ggsave("figures/births_monthly_national_levels_all_ages.png",
       w = 10,
       h = 5)
