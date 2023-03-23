rm(list=ls())
source("Code/00_functions.R")

# monthly births
dt <- read_rds("data_inter/monthly_excess_births_bra_col_mex_edu03.rds")

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
         imp_type == "i",
         country != "MEX") %>% 
  rename(bsn_lp = bsn_lp4,
         bsn_up = bsn_up4) %>% 
  mutate(country = case_when(country == "BRA" ~ "Brazil",
                             country == "COL" ~ "Colombia",
                             country == "MEX" ~ "Mexico"),
         bsn_lp = ifelse(date >= ymd("2020-03-01"), bsn_lp/1000, NA),
         bsn_up = ifelse(date >= ymd("2020-03-01"), bsn_up/1000, NA),
         bts = bts/1000,
         bsn = bsn/1000)
  

unique(all_geos_ages2$edu)

labels_perc <- 
  all_geos_ages2 %>% 
  group_by(country, edu) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(country, edu, date, bsn) %>% 
  mutate(label_perc = c("0.1%", "0.2%", "0.3%", "0.4%", "0.5%", "0.6%"))

c3 <- brewer.pal(9, "YlOrRd")[c(4, 6, 8)]

all_geos_ages2 %>%
  ggplot(aes(group = edu))+
  geom_point(aes(date, bts, col = edu, group = edu), size = 0.6, alpha = .8)+
  geom_ribbon(aes(date, ymin = bsn_lp, ymax = bsn_up, fill = edu), alpha = 0.3)+
  geom_line(aes(date, bsn, col = edu))+
  geom_vline(xintercept = ymd("2020-03-01"),
             linetype = "dashed")+
  geom_text(data = labels_perc, aes(x = date, y = bsn, label = label_perc), 
            hjust = -.2, size = 3)+
  scale_x_date(breaks = seq(ymd('2010-01-01'), ymd('2022-01-01'), by = '1 year'),
               limits = c(ymd('2015-01-01'), ymd('2022-04-15')),
               date_labels = "%Y")+
  scale_color_manual(values = c3)+
  scale_fill_manual(values = c3, guide = "none")+
  facet_wrap(~country, scales = "free_y")+
  labs(y = "Births (in thousands)", x = "Months", col = "Years of schooling")+
  theme_bw()+
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12, face = "bold"),
        strip.background = element_blank())

ggsave("figures/births_monthly_national_levels_all_ages.png",
       w = 10,
       h = 5,
       dpi = 600)
