library(tidyverse)
library(lubridate)
library(mgcv)

db <- 
  read_rds("data_inter/db_trimester_bra_col.RDS") %>% 
  as_tibble()

typeof(db)

# testing imputation
# ~~~~~~~~~~~~~~~~~~
test <- 
  db %>% 
  group_by(ISO_Code, Region, raw_yearbir, raw_trimest) %>% 
  summarise(n = sum(raw_nbirth.current), 
            t = sum(raw_tbirth.current), 
            l = sum(raw_lbirth.current)) %>% 
  ungroup()
  
# adjusting data for baseline estimations
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db2 <- 
  db %>% 
  mutate(ctr_reg = paste(ISO_Code, Region, sep = "-")) %>% 
  select(ctr_reg, raw_yearbir, raw_trimest, raw_mothag6, raw_edumo04, 
         raw_nbirth.current, raw_tbirth.current, raw_lbirth.current) %>% 
  # excluding "unknown" ages
  filter(raw_mothag6 != "unknown") %>% 
  mutate(raw_mothag6 = factor(raw_mothag6, 
                              levels = c("10-19", "20-24", "25-29", 
                                         "30-34", "35-39", "40-54"))) %>% 
  # complete all possible combinations
  complete(ctr_reg, raw_yearbir, raw_trimest, raw_mothag6, raw_edumo04, 
           fill = list(raw_nbirth.current = 0, 
                       raw_tbirth.current = 0, 
                       raw_lbirth.current = 0)) %>% 
  mutate(trim = case_when(raw_trimest == "First" ~ 1,
                          raw_trimest == "Second" ~ 2,
                          raw_trimest == "Third" ~ 3,
                          raw_trimest == "Fourth" ~ 4),
         mth = trim * 3 - 1,
         date = make_date(d = 15, m = mth, y = raw_yearbir)) %>% 
  arrange(date, ctr_reg, raw_mothag6, raw_edumo04) %>% 
  group_by(ctr_reg, raw_mothag6, raw_edumo04) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  mutate(w = ifelse(raw_yearbir %in% 2015:2019 | 
                      (raw_yearbir == 2020 & raw_trimest == "First"), 1, 0),
         # increasing births in one to avoid 0s, it will be adjusted back later
         raw_nbirth.current = raw_nbirth.current + 1,
         raw_tbirth.current = raw_tbirth.current + 1,
         raw_lbirth.current = raw_lbirth.current + 1,
         # trimester dummies for the glm model
         trim_1 = ifelse(raw_trimest == "First", 1, 0),
         trim_2 = ifelse(raw_trimest == "Second", 1, 0),
         trim_3 = ifelse(raw_trimest == "Third", 1, 0),
         trim_4 = ifelse(raw_trimest == "Fourth", 1, 0)) %>% 
  filter(date <= "2021-12-31") %>% 
  gather(raw_nbirth.current, raw_tbirth.current, raw_lbirth.current,
         key = type, value = bts)

# testing that all combinations have complete data series
test <- 
  db2 %>% 
  group_by(ctr_reg, raw_mothag6, raw_edumo04) %>% 
  summarise(obs = n())


# visualizing births

ct <- "COL-Casanare"
ag <- "20-24"
ed <- "Prima-"

unique(db2$raw_edumo04)
unique(db2$raw_mothag6)
unique(db2$ctr_reg)

db2 %>% 
  filter(ctr_reg == ct,
         raw_mothag6 == ag, 
         raw_edumo04 == ed) %>%
  ggplot()+
  geom_line(aes(date, bts, col = type))+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2021-01-01'), by = '1 year'),
               date_labels = "%Y")+
  theme_bw()

test <- 
  db2 %>% 
  filter(ctr_reg == ct,
         raw_mothag6 == ag, 
         raw_edumo04 == ed)



# function for glm models, according to imputation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pred_births <- function(chunk){

  model_glm <- 
    glm(bts ~ t + trim_1 + trim_2 + trim_3 + trim_4, 
        weights = w,
        data = chunk, 
        family = quasipoisson(link = "log"))  
  
  chunk %>% 
    mutate(pred_glm = predict(model_glm, type = "response", newdata = chunk))

}

# births baseline estimation
db3 <- 
  db2 %>% 
  group_by(ctr_reg, raw_mothag6, raw_edumo04, type) %>% 
  do(pred_births(chunk = .)) %>% 
  ungroup()

# vis example
ct <- "COL-Bogota D.C."
ag <- "20-24"
ed <- "Prima-"

db3 %>% 
  filter(ctr_reg == ct, 
         # raw_edumo04 == ed,
         raw_mothag6 == ag) %>%
  ggplot()+
  geom_point(aes(date, bts, col = type), alpha = .5)+
  geom_line(aes(date, pred_glm, col = type), alpha = 0.6)+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2021-01-01'), by = '1 year'),
               limits = c(ymd('2015-01-01'), ymd('2022-01-01')),
               date_labels = "%Y")+
  facet_wrap(~raw_edumo04, scales = "free_y")+
  theme_bw()
  
# tibble with estimates to combine with original data
out <- 
  db3 %>% 
  mutate(ISO_Code = str_sub(ctr_reg, 1, 3),
         Region = str_sub(ctr_reg, 5, length(ctr_reg)),
         pred_glm = ifelse(date < "2015-01-01", NA, pred_glm),
         # decreasing again births in 1 to reverse the initial trick, and also 
         # in the baseline
         pred_glm = ifelse(pred_glm <= 1, 0, pred_glm - 1),
         bts = bts - 1) %>% 
  select(ISO_Code, Region, raw_yearbir, raw_trimest, raw_edumo04, raw_mothag6, 
         type, bts, pred_glm) %>% 
  pivot_wider(
    names_from = type,
    values_from = c(bts, pred_glm)) %>% 
  select(-bts_raw_nbirth.current, 
         -bts_raw_tbirth.current,
         -bts_raw_lbirth.current) %>% 
  rename(pred_n = pred_glm_raw_nbirth.current, 
         pred_t = pred_glm_raw_tbirth.current, 
         pred_l = pred_glm_raw_lbirth.current
         )
  
options(tibble.width = Inf)

# merging estimates in original data
db_out <- 
  db %>% 
  left_join(out, 
            by = c("raw_trimest", "raw_mothag6",
                   "raw_edumo04", "raw_yearbir", 
                   "ISO_Code", "Region")) %>% 
  arrange(raw_country, Region,
          raw_mothag6, raw_edumo04, raw_yearbir, raw_trimest)

write_rds(db_out, "data_inter/db_trimester_bra_col_ea.RDS")

test2 <- 
  db_out %>% 
  select(raw_country, Region, 
         raw_yearbir, raw_trimest, 
         raw_mothag6, raw_edumo04,
         raw_nbirth.current, 
         raw_tbirth.current, 
         raw_lbirth.current, 
         pred_n, pred_t, pred_l) %>% 
  filter(raw_yearbir >= 2015)


nal <- 
  db3 %>% 
  mutate(ISO_Code = str_sub(ctr_reg, 1, 3),
         Region = str_sub(ctr_reg, 5, length(ctr_reg)),
         pred_glm = ifelse(pred_glm <= 1, 0, pred_glm - 1),
         bts = bts - 1) %>% 
  group_by(ISO_Code, date, raw_yearbir, raw_trimest, raw_edumo04, type) %>% 
  summarise(bts = sum(bts),
            pred_glm = sum(pred_glm)) %>% 
  ungroup()


nal %>% 
  mutate(pred_glm = ifelse(date < "2015-01-01", NA, pred_glm)) %>% 
  ggplot()+
  geom_point(aes(date, bts, linetype = raw_edumo04, group = raw_edumo04, col = type))+
  geom_line(aes(date, pred_glm, linetype = raw_edumo04, col = type))+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(ISO_Code ~ type, scales = "free_y")+
  theme_bw()

ggsave("figures/births_baseline_national_levels_robustness.png",
       w = 10,
       h = 5)


# table for sample description
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test <- 
  db_out %>% 
  filter(raw_yearbir %in% 2020:2021,
         raw_mothag6 != "unknown")

avs1519 <- 
  db_out %>% 
  filter(raw_yearbir %in% 2015:2019,
         raw_mothag6 != "unknown") %>% 
  group_by(raw_country, Region) %>% 
  summarise(av_bts_1519 = round(mean(raw_nbirth.current), 1)) %>% 
  ungroup()

avs2021 <- 
  db_out %>% 
  filter(raw_yearbir %in% 2020:2021,
         raw_mothag6 != "unknown") %>% 
  group_by(raw_country, Region) %>% 
  summarise(av_bts_2021 = round(mean(raw_nbirth.current), 1)) %>% 
  ungroup()

preds <- 
  db_out %>% 
  filter(raw_yearbir %in% 2020:2021,
         raw_mothag6 != "unknown") %>% 
  group_by(raw_country, Region) %>% 
  summarise(av_prd = round(mean(pred_n), 1))

pscs <- 
  db_out %>% 
  filter(raw_yearbir %in% 2020:2021,
         raw_mothag6 != "unknown") %>% 
  mutate(psc = raw_nbirth.current / pred_n) %>% 
  group_by(raw_country, Region) %>% 
  summarise(av_psc = round(mean(psc), 2))


tb1 <- 
  avs1519 %>% 
  left_join(preds) %>% 
  left_join(avs2021) %>% 
  left_join(pscs) %>% 
  mutate(test = av_bts_2021 / av_prd)


all <- read_rds("data_inter/rec_bra_col_with_missing.RDS") %>% 
  as_tibble() %>% 
  select("raw_country",
         "raw_geolev1",
         "raw_yearbir", 
         "raw_trimest",
         "raw_mothag6",
         raw_edumo04,
         raw_nbirth) %>% 
  filter(raw_country == "COL")

mis_e <- 
  all %>% 
  filter(raw_edumo04 == "Missing") %>% 
  select(-raw_edumo04) %>% 
  rename(unk = raw_nbirth)

all_ed_c <- 
  all %>% 
  filter(raw_edumo04 != "Missing")

imp_ed <- 
  all_ed_c %>% 
  left_join(mis_e) %>% 
  group_by(raw_country,
           raw_geolev1,
           raw_yearbir, raw_trimest,
           raw_mothag6) %>% 
  mutate(bts2 = raw_nbirth + (unk * (raw_nbirth/sum(raw_nbirth)))) %>% 
  ungroup()
  
mis_a <- 
  imp_ed %>% 
  filter(raw_mothag6 == "unknown") %>% 
  select(-raw_mothag6, -unk) %>% 
  rename(unk2 = raw_nbirth)

all_ag_c <- 
  imp_ed %>% 
  filter(raw_mothag6 != "unknown")

imp_a <- 
  all_ag_c %>% 
  left_join(mis_a) %>% 
  replace_na(list(unk2 = 0)) %>% 
  group_by(raw_country,
           raw_geolev1,
           raw_yearbir, raw_trimest,
           raw_edumo04) %>% 
  mutate(bts3 = bts2 + (unk2 * (bts2/sum(bts2)))) %>% 
  ungroup()



