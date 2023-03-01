rm(list=ls())
source("Code/00_functions.R")

# loading data
# ~~~~~~~~~~~~
bts <- read_rds("data_inter/monthly_excess_births_bra_col_mex_edu03.rds")
geo_codes <- read_csv("data_input/geo_codes_bra_col_mex.csv", 
                      locale = readr::locale(encoding = "latin1")) %>% 
  rename(country = ISO_Code) %>% 
  select(country, geo, type)

bts2 <- 
  bts %>% 
  left_join(geo_codes) %>% 
  filter(type %in% c("single", "group"),
         imp_type == "i") %>% 
  select(-imp_type, -t, -w) %>% 
  rename(bsn_lp = bsn_lp4,
         bsn_up = bsn_up4) %>% 
  mutate(psc_unc = ifelse((bts > bsn_up | bts < bsn_lp) & date >= "2020-02-01", bts / bsn, 1),
         psc = bts / bsn)
  

tttt <- 
  bts2 %>% 
  filter(date >= "2020-01-01")

extreme_psc <- 
  bts2 %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(country, geo, age, edu) %>% 
  summarise(min_psc_unc = min(psc_unc, na.rm = TRUE),
            max_psc_unc = max(psc_unc, na.rm = TRUE),
            min_psc = min(psc, na.rm = TRUE),
            max_psc = max(psc, na.rm = TRUE),
            lp_av = round(mean(bsn_lp), 2)) %>% 
  ungroup()

test <- 
  bts2 %>% 
  filter(date <= "2020-01-01") %>% 
  mutate(zero = ifelse(bts == 0, 1, 0)) %>% 
  group_by(country, geo, age, edu) %>% 
  summarise(bts_av = round(mean(bts), 1),
            zer_pr = round(sum(zero)/n(), 2) * 100) %>% 
  ungroup() %>% 
  left_join(extreme_psc)

comb_prob <- 
  test %>% 
  filter((bts_av < 4 | zer_pr > 15) | max_psc > 4 | lp_av < 0.5) %>% 
  filter(zer_pr != 100) %>% 
  arrange(bts_av)

data_prob <- 
  bts2 %>% 
  inner_join(comb_prob %>% 
               select(country, geo, age, edu)) %>% 
  left_join(comb_prob) %>% 
  mutate(exc = ifelse(date >= "2020-03-01" & (bts > bsn_up | bts < bsn_lp), "y", "n")) 

comb_prob[1:3,]
i <- 0
scaleFUN <- function(x) sprintf("%.2f", x)
unique(data_prob$geo)
length(unique(data_prob$geo))
geos <- unique(data_prob$geo)
n <- ceiling(length(unique(data_prob$geo))/10) - 1
for(i in 0:n){
  combs <- comb_prob[(i*10+1):((i+1)*10),]
  data_prob %>% 
    inner_join(combs %>% select(-bts_av, -zer_pr)) %>% 
    filter(bsn_up < 70000) %>% 
    ggplot()+
    geom_point(aes(date, bts, shape = exc))+
    geom_line(aes(date, bsn))+
    geom_ribbon(aes(ymin = bsn_lp, ymax = bsn_up, x = date), alpha = 0.2)+
    geom_text(aes(x = ymd("2015-01-01"), y = Inf-1, 
                  label = paste0("av = ", round(bts_av, 1), 
                                 "; prop_0s = ", round(zer_pr, 1), "%; ",
                                 "lp_av = ", round(lp_av, 2), 
                                 " ; max_psc = ", round(max_psc, 2))), 
              hjust = 0, vjust = 1, size = 4)+
    facet_wrap(edu+age~geo, scales = "free_y", ncol = 2)+
    geom_vline(xintercept = ymd("2020-03-01"), linetype = "dashed")+
    theme_bw()+
    scale_y_continuous(labels=scaleFUN)+
    scale_shape_manual(values = c(16, 17))+
    theme(legend.position = "none")
  
  
  ggsave(paste0("figures/problems_births_fitting_", i+1, ".png"),
         dpi = 600,
         h = 15, w = 10)

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique(bts2$age)
unique(bts2$edu)
bts2 %>% 
  filter(country == "MEX", age == "40-54") %>%
  ggplot()+
  geom_point(aes(date, bts))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(ymin = bsn_lp, ymax = bsn_up, x = date), alpha = 0.2)+
  facet_wrap(edu+age~country+geo, scales = "free_y", ncol = 5)+
  geom_vline(xintercept = ymd("2020-03-01"), linetype = "dashed")+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN)+
  scale_shape_manual(values = c(16, 17))+
  theme(legend.position = "none")

ggsave(paste0("figures/problems_mex_births_age_40_54.pdf"),
       dpi = 600,
       h = 160, w = 10,
       limitsize = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# all... too large!
bts2 %>% 
  ggplot()+
  geom_point(aes(date, bts))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(ymin = bsn_lp, ymax = bsn_up, x = date), alpha = 0.2)+
  facet_wrap(edu+age~country+geo, scales = "free_y", ncol = 5)+
  geom_vline(xintercept = ymd("2020-03-01"), linetype = "dashed")+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN)+
  scale_shape_manual(values = c(16, 17))+
  theme(legend.position = "none")

ggsave(paste0("figures/all_baselines.pdf"),
       dpi = 600,
       h = 300, w = 20,
       limitsize = FALSE)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tx <- 6

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Mexico
bts2 %>% 
  filter(country == "MEX") %>%
  ggplot()+
  geom_point(aes(date, bts), size = 0.5)+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(ymin = bsn_lp, ymax = bsn_up, x = date), alpha = 0.2)+
  facet_wrap(edu+age~geo, scales = "free_y", ncol = 5)+
  geom_vline(xintercept = ymd("2020-03-01"), linetype = "dashed")+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN)+
  scale_shape_manual(values = c(16, 17))+
  theme(legend.position = "none",
        axis.text = element_text(size = tx),
        axis.title = element_text(size = tx),
        strip.background = element_blank(),
        strip.text = element_text(size = tx, margin = margin (.5, .5, .5, .5)),
        panel.spacing = unit(0.1, "lines"))

ggsave(paste0("figures/baselines_mex_births_edu03.pdf"),
       dpi = 600,
       h = 160, w = 10,
       limitsize = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Colombia
bts2 %>% 
  filter(country == "COL") %>%
  ggplot()+
  geom_point(aes(date, bts), size = 0.5)+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(ymin = bsn_lp, ymax = bsn_up, x = date), alpha = 0.2)+
  facet_wrap(edu+age~geo, scales = "free_y", ncol = 5)+
  geom_vline(xintercept = ymd("2020-03-01"), linetype = "dashed")+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN)+
  scale_shape_manual(values = c(16, 17))+
  theme(legend.position = "none",
        axis.text = element_text(size = tx),
        axis.title = element_text(size = tx),
        strip.background = element_blank(),
        strip.text = element_text(size = tx, margin = margin (.5, .5, .5, .5)),
        panel.spacing = unit(0.1, "lines"))

ggsave(paste0("figures/baselines_col_births_edu03.pdf"),
       dpi = 600,
       h = 160, w = 10,
       limitsize = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Brazil
bts2 %>% 
  filter(country == "BRA") %>%
  ggplot()+
  geom_point(aes(date, bts), size = 0.5)+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(ymin = bsn_lp, ymax = bsn_up, x = date), alpha = 0.2)+
  facet_wrap(edu+age~geo, scales = "free_y", ncol = 5)+
  geom_vline(xintercept = ymd("2020-03-01"), linetype = "dashed")+
  theme_bw()+
  scale_y_continuous(labels=scaleFUN)+
  scale_shape_manual(values = c(16, 17))+
  theme(legend.position = "none",
        axis.text = element_text(size = tx),
        axis.title = element_text(size = tx),
        strip.background = element_blank(),
        strip.text = element_text(size = tx, margin = margin (.5, .5, .5, .5)),
        panel.spacing = unit(0.1, "lines"))

ggsave(paste0("figures/baselines_bra_births_edu03.pdf"),
       dpi = 600,
       h = 160, w = 10,
       limitsize = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~










comb_prob <- 
  test %>% 
  filter((bts_av < 1 | zer_pr > 30)) %>% 
  arrange(bts_av)

data_prob <- 
  bts2 %>% 
  inner_join(comb_prob %>% 
               select(country, geo, age, edu)) %>% 
  left_join(comb_prob) %>% 
  mutate(exc = ifelse(date >= "2020-03-01" & (bts > bsn_up | bts < bsn_lp), 
                      "y", "n")) 

# low[1:3,]
i <- 0
scaleFUN <- function(x) sprintf("%.2f", x)

data_prob %>% 
  select(country, geo, age, edu) %>% 
  unique()

comb_prob2 <- comb_prob %>% select(country, geo, age, edu)
comb_prob2
dim(comb_prob2)[1]
n <- ceiling(dim(comb_prob2)[1]/10) - 1

for(i in 0:n){
  combs <- comb_prob[(i*10+1):((i+1)*10),]
  data_prob %>% 
    inner_join(combs %>% select(-bts_av, -zer_pr)) %>% 
    filter(bsn_up < 70000) %>% 
    ggplot()+
    geom_point(aes(date, bts, shape = exc))+
    geom_line(aes(date, bsn))+
    geom_ribbon(aes(ymin = bsn_lp, ymax = bsn_up, x = date), alpha = 0.2)+
    geom_text(aes(x = ymd("2015-01-01"), y = Inf-1, 
                  label = paste0("av = ", round(bts_av, 1), 
                                 "; prop_0s = ", round(zer_pr, 1), "%; ",
                                 "lp_av = ", round(lp_av, 2), 
                                 " ; max_psc = ", round(max_psc, 2))), 
              hjust = 0, vjust = 1, size = 4)+
    facet_wrap(edu+age~geo, scales = "free_y", ncol = 2)+
    geom_vline(xintercept = ymd("2020-03-01"), linetype = "dashed")+
    theme_bw()+
    scale_y_continuous(labels=scaleFUN)+
    scale_shape_manual(values = c(16, 17))+
    theme(legend.position = "none")
  
  
  ggsave(paste0("figures/problems2_births_fitting_", i+1, "_v2.png"),
         dpi = 600,
         h = 15, w = 10)
  
}

write_rds(comb_prob2, "data_inter/problematic_combinations_baseline_fertility.rds")
