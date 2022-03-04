library(tidyverse)
library(lubridate)
library(mgcv)
db <- 
  read_rds("data_inter/db_trimester_bra_col.RDS") %>% 
  as_tibble()

db2 <- 
  db %>% 
  mutate(ctr_reg = paste(ISO_Code, Region, sep = "-")) %>% 
  select(ctr_reg, raw_yearbir, raw_trimest, raw_mothag6, 
         raw_edumo04, raw_nbirth.current) %>% 
  # excluding "unknown" ages
  filter(raw_mothag6 != "unknown") %>% 
  mutate(raw_mothag6 = factor(raw_mothag6, 
                              levels = c("10-19", "20-24", "25-29", 
                                         "30-34", "35-39", "40-54"))) %>% 
  # complete all possible combinations
  complete(ctr_reg, raw_yearbir, raw_trimest, raw_mothag6, raw_edumo04, 
           fill = list(raw_nbirth.current = 0)) %>% 
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
         # increasing births in one to avoid 0s
         raw_nbirth.current = raw_nbirth.current + 1,
         # trimester dummies for the glm model
         trim_1 = ifelse(raw_trimest == "First", 1, 0),
         trim_2 = ifelse(raw_trimest == "Second", 1, 0),
         trim_3 = ifelse(raw_trimest == "Third", 1, 0),
         trim_4 = ifelse(raw_trimest == "Fourth", 1, 0)) %>% 
  filter(date <= "2021-08-15")

# testing that all combinations have complete data series
test <- 
  db2 %>% 
  group_by(ctr_reg, raw_mothag6, raw_edumo04) %>% 
  summarise(obs = n())

# function for glm model
pred_births <- function(chunk){

  model_glm <- 
    glm(raw_nbirth.current ~ t + trim_1 + trim_2 + trim_3 + trim_4, 
        weights = w,
        data = chunk, 
        family = quasipoisson(link = "log"))  
  
  chunk %>% 
    mutate(pred_glm = predict(model_glm, type = "response", newdata = chunk))

}

# births baseline estimation
db3 <- 
  db2 %>% 
  group_by(ctr_reg, raw_mothag6, raw_edumo04) %>% 
  do(pred_births(chunk = .)) %>% 
  ungroup()

# visualiza example
ct <- "COL-Bogota D.C."
ag <- "20-24"
ed <- "Prima-"

db3 %>% 
  filter(ctr_reg == ct,
         raw_mothag6 == ag, 
         raw_edumo04 == ed) %>%
  ggplot()+
  geom_line(aes(date, raw_nbirth.current), col = "black")+
  geom_line(aes(date, pred_glm), col = "red", alpha = 0.6)+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2021-01-01'), by = '1 year'),
               date_labels = "%Y")+
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
         raw_nbirth.current = raw_nbirth.current - 1) %>% 
  select(ISO_Code, Region, raw_yearbir, raw_trimest, raw_edumo04, raw_mothag6, 
         pred_glm = pred_glm)
  
options(tibble.width = Inf)

# merging estimates in original data
db_out <- 
  db %>% 
  left_join(out)
  
write_rds(db_out, "data_inter/db_trimester_bra_col_ea.RDS")

  
nal <- 
  db3 %>% 
  mutate(ISO_Code = str_sub(ctr_reg, 1, 3),
         Region = str_sub(ctr_reg, 5, length(ctr_reg)),
         pred_glm = ifelse(pred_glm <= 1, 0, pred_glm - 1),
         raw_nbirth.current = raw_nbirth.current - 1) %>% 
  group_by(ISO_Code, date, raw_yearbir, raw_trimest, raw_edumo04) %>% 
  summarise(bts = sum(raw_nbirth.current),
            pred_glm = sum(pred_glm)) %>% 
  ungroup()


nal %>% 
  mutate(pred_glm = ifelse(date < "2015-01-01", NA, pred_glm)) %>% 
  ggplot()+
  geom_line(aes(date, bts, linetype = raw_edumo04, group = raw_edumo04), 
            col = "black")+
  geom_line(aes(date, pred_glm, linetype = raw_edumo04), col = "red")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2021-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~ISO_Code, scales = "free_y")+
  theme_bw()

ggsave("figures/births_baseline_national_levels.png",
       w = 10,
       h = 5)


