rm(list=ls())
source("Code/00_functions.R")

# excess mortality estimates
db <- 
  read_rds("data_inter/monthly_excess_deaths_bra_col_mex.rds")

# standard codes
geo_codes <- 
  read_csv("data_input/geo_codes_bra_col_mex.csv", 
           locale = readr::locale(encoding = "latin1")) %>% 
  select(country = ISO_Code,
         geo, geo_label,
         type)


# excluding grouped departments from Colombia and Mexico
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db2 <- 
  db %>% 
  left_join(geo_codes) %>% 
  filter(type %in% c("single", "group")) 


# average monthly p-scores
# ~~~~~~~~~~~~~~~~~~~~~~~~
av_pscores <- 
  db2 %>% 
  group_by(country, geo, geo_label) %>% 
  summarise(av_psc = mean(dts_psc)) %>% 
  arrange(country, -av_psc) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(ord = 1:n()) %>% 
  filter(ord <= 4) %>% 
  ungroup() %>% 
  mutate(col_geo = paste0(geo_label, " (", country, ")")) %>% 
  select(-geo_label)

db3 <- 
  db2 %>% 
  left_join(av_pscores %>% select(-av_psc)) %>% 
  mutate(col_geo = ifelse(is.na(col_geo), "other", col_geo),
         ord = ifelse(col_geo == "other", "other", paste0(country, ord)),
         ident = ifelse(col_geo == "other", "other", "ident"))

cols <-
  c("BRA1" = "#6a040f",
    "BRA2" = "#d00000",
    "BRA3" = "#f77f00",
    "BRA4" = "#ffba08",
    "COL1" = "#6a040f",
    "COL2" = "#d00000",
    "COL3" = "#f77f00",
    "COL4" = "#ffba08",
    "MEX1" = "#6a040f",
    "MEX2" = "#d00000",
    "MEX3" = "#f77f00",
    "MEX4" = "#ffba08",
    "oth" = "black")

bks <- c(paste0("BRA", 1:4), paste0("COL", 1:4), paste0("MEX", 1:4))
lbs <- av_pscores %>% pull(col_geo)

tx <- 8

db3 %>% 
  filter(date <= "2021-12-31") %>% 
  ggplot(aes(date, dts_psc)) +
  geom_boxplot(aes(group = date), outlier.shape = NA, 
               lwd = 0.3, alpha = 0.01, col = "grey50")+
  geom_jitter(aes(date, dts_psc, col = ord, alpha = ident, size = pop),
              width = 3, height = 0)+
  scale_y_log10(labels = function(x) paste0((x - 1) * 100, "%"), 
                breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 5))+
  scale_x_date(breaks = seq(ymd('2020-01-01'), ymd('2021-12-01'), 
                            by = '2 month'), 
               date_labels = "%b\n%y")+
  scale_color_manual(values = cols,
                     breaks = bks,
                     labels = lbs)+
  scale_alpha_manual(values = c(0.8, 0.15), guide = "none")+
  scale_size_continuous(breaks = c(100000, 500000, 1000000, 
                                   5000000, 10000000, 40000000),
                        labels = c("100K", "500K", "1M", "5M", "10M", "40M"))+
  guides(color = guide_legend(order = 1,
                              nrow = 3, byrow = TRUE,
                              override.aes = list(size = 2.5, alpha = 0.8)),
         size = guide_legend(nrow = 2, byrow = T))+
  facet_wrap(~ country)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(y = "Excess p-score", x = "Month",
       col = "Subnational divisions\nwith extreme average\np-score value",
       size = "Population")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 1),
        legend.text = element_text(size = tx + 2),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = tx + 4))

ggsave("figures/monthly_pscores_boxplot.png",
       dpi = 600,
       w = 16,
       h = 8)

ggsave("figures/monthly_pscores_boxplot.pdf",
       w = 16,
       h = 8)


