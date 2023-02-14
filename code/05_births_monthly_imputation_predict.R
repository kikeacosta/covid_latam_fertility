rm(list=ls())
source("Code/00_functions.R")

# codes 
codes <- 
  read_csv("data_input/geo_codes_bra_col_mex.csv", 
           locale = readr::locale(encoding = "latin1")) %>% 
  rename(country = ISO_Code) %>% 
  select(country, geo,raw_geolev1) %>% 
  unique()

# monthly births
dt <- 
  readRDS("data_inter/covid_tab_all.RDS") 



# MEX without state 
# ~~~~~~~~~~~~~~~~~
# births in MEX without state in 2020 and 2021
dt %>% 
  filter(is.na(raw_geo1nam),
         raw_country == "MEX",
         raw_yearbir > 2019) %>% 
  summarise(bts = sum(raw_nbirth))

# total births in MEX in 2020 and 2021
dt %>% 
  filter(!is.na(raw_geo1nam),
         raw_country == "MEX",
         raw_yearbir > 2019) %>% 
  summarise(bts = sum(raw_nbirth))

unique(dt$raw_geo1nam) %>% sort()




# grouping regions and ages together ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dt2 <- 
  dt %>% 
  as_tibble() %>% 
  # drop NA states in Mexico
  drop_na(raw_geo1nam) %>% 
  mutate(raw_geolev1 = raw_geolev1 %>% as.double()) %>% 
  left_join(codes) %>% 
  select(country = raw_country, 
         geo,
         year = raw_yearbir, 
         mth = raw_montbir,
         age = raw_mothag7, 
         edu = raw_edumo04,
         bts = raw_nbirth) %>% 
  mutate(age = case_when(age %in% c("10-14", "15-19") ~ "10-19",
                                 age %in% c("20-24", "25-29") ~ "20-29",
                                 age %in% c("30-34", "35-39") ~ "30-39",
                                 TRUE  ~ "40-54"),
         age = factor(age, levels = c("10-19", "20-29", "30-39", "40-54")))


# ~~~~~~~~~~~~~~~~~~
# imputing education
# ~~~~~~~~~~~~~~~~~~
tot_edu <-
  dt2 %>%
  group_by(country, year, mth, geo, age) %>%
  summarise(bts_tot = sum(bts, na.rm = T)) %>%
  ungroup()

mis_edu <-
  dt2 %>%
  filter(edu == "unknown") %>%
  select(everything(), -edu, bts_unk = bts)

dt3 <-
  dt2 %>%
  filter(edu != "unknown") %>%
  left_join(tot_edu) %>%
  left_join(mis_edu) %>%
  replace_na(list(bts_unk = 0)) %>%
  group_by(country, year, mth, geo, age) %>%
  # two different ways of imputation
  mutate(bts_i = bts_tot*bts/sum(bts),
         bts_t = ifelse(edu == "0-3", bts + bts_unk, bts)) %>%
  ungroup() %>% 
  select(-bts_tot, - bts_unk) %>% 
  filter(!is.na(geo))

# ~~~~~~~~~~~~~~~~~~
# imputing age
# ~~~~~~~~~~~~~~~~~~
tot_age <-
  dt3 %>%
  group_by(country, year, mth, geo, edu) %>%
  summarise(bts_i_tot = sum(bts_i, na.rm = T),
            bts_t_tot = sum(bts_t, na.rm = T)) %>%
  ungroup()

dt4 <-
  dt3 %>%
  filter(age != "unknown") %>%
  left_join(tot_age) %>%
  group_by(country, year, mth, geo, edu) %>%
  mutate(bts_i = bts_i_tot*bts_i/sum(bts_i),
         bts_t = bts_t_tot*bts_t/sum(bts_t)) %>%
  select(-bts_i_tot, -bts_t_tot) %>% 
  ungroup() %>% 
  droplevels()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# reshaping to long format and completing missing dates with 0s
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# to long format, by imputation type
dt5 <- 
  dt4 %>% 
  gather(bts, bts_i, bts_t, key = imp_type, value = bts) %>% 
  mutate(imp_type = case_when(imp_type == "bts" ~ "n",
                              imp_type == "bts_i" ~ "i",
                              imp_type == "bts_t" ~ "t")) %>% 
  mutate(ct_geo = paste(country, geo, sep = "_")) %>% 
  select(-country, -geo) %>% 
  complete(ct_geo, year, mth, age, edu, imp_type, fill = list(bts = 0)) %>% 
  filter(!(age == "10-14" & edu == "12+")) %>% 
  separate(ct_geo, c("country", "geo"), sep = "_")


# ~~~~~~~~~~~~~
# adding totals ====
# ~~~~~~~~~~~~~
# country, year, trim, geo, age, edu

dt6 <- 
  dt5 %>% 
  # adding total education
  bind_rows(
    dt5 %>% 
      group_by(country, year, mth, geo, age, imp_type) %>% 
      summarise(bts = sum(bts),
                edu = "total") %>% 
      ungroup()
  ) %>% 
  # adding total ages
  bind_rows(
    dt5 %>% 
      group_by(country, year, mth, geo, edu, imp_type) %>% 
      summarise(bts = sum(bts),
                age = "total") %>% 
      ungroup()
  ) %>% 
  # adding total country
  bind_rows(
    dt5 %>% 
      group_by(country, year, mth, age, edu, imp_type) %>% 
      summarise(bts = sum(bts),
                geo = "total") %>% 
      ungroup()
  ) %>% 
  # adding total education and age
  bind_rows(
    dt5 %>% 
      group_by(country, year, mth, geo, imp_type) %>% 
      summarise(bts = sum(bts),
                edu = "total",
                age = "total") %>% 
      ungroup()
  ) %>% 
  # adding total education and country
  bind_rows(
    dt5 %>% 
      group_by(country, year, mth, age, imp_type) %>% 
      summarise(bts = sum(bts),
                edu = "total",
                geo = "total") %>% 
      ungroup()
  ) %>% 
  # adding total age and country
  bind_rows(
    dt5 %>% 
      group_by(country, year, mth, edu, imp_type) %>% 
      summarise(bts = sum(bts),
                age = "total",
                geo = "total") %>% 
      ungroup()
  ) %>% 
  # adding total region, education, and age
  bind_rows(
    dt5 %>% 
      group_by(country, year, mth, imp_type) %>% 
      summarise(bts = sum(bts),
                geo = "total",
                edu = "total",
                age = "total") %>% 
      ungroup()
  ) %>% 
  mutate(date = make_date(d = 15, m = mth, y = year),
         edu = factor(edu, levels = c("0-3", "4-7", "8-11", "12+", "total")),
         age = factor(age, levels = c("10-19", "20-29", "30-39", "40-54", "total")))

unique(dt6$edu)
unique(dt6$age)
unique(dt6$geo) %>% sort
unique(dt6$imp_type)


# ~~~~~~~~~~~~~~~~~~~~~~~~~
# adding Colombian regions
# ~~~~~~~~~~~~~~~~~~~~~~~~~

# regions to group together
reg_amazon <- c("Amazonas",
                "Caquetá",
                "Caqueta",
                "Guainía",
                "Guainia",
                "Guainja",
                "Guaviare",
                "Putumayo",
                "Vaupés",
                "Vaupes")

reg_orinoq <- c("Arauca", 
                "Casanare", 
                "Meta", 
                "Vichada")

reg_ejecaf <- c("Caldas", 
                "Risaralda", 
                "Quindío",
                "Quindio")
dt7 <- 
  dt6 %>% 
  filter(country == "COL" & geo %in% c(reg_amazon, reg_orinoq, reg_ejecaf)) %>% 
  mutate(geo = case_when(country == "COL" & geo %in% reg_amazon ~ "Amazonia",
                         country == "COL" & geo %in% reg_orinoq ~ "Orinoquia",
                         country == "COL" & geo %in% reg_ejecaf ~ "Eje Cafetero",
                         TRUE ~ geo)) %>% 
  group_by(country, geo, date, year, mth, age, edu, imp_type) %>% 
  summarise(bts = sum(bts)) %>% 
  ungroup() %>% 
  bind_rows(dt6)


unique(dt7$geo) %>% sort


# ~~~~~~~~~~~~~~~
# master database
# ~~~~~~~~~~~~~~~

dt8 <- 
  dt7 %>% 
  group_by(country, geo, age, edu, imp_type) %>%
  arrange(date) %>%
  mutate(t = 1:n()) %>%
  ungroup() %>%
  mutate(w = ifelse(date < "2020-03-01", 1, 0))

write_rds(dt8, "data_inter/master_births_for_baseline_estimation.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~
# baseline estimation ====
# ~~~~~~~~~~~~~~~~~~~~~~~~

# quick test with total national by educational level 
test <- 
  dt8 %>% 
  filter(geo == "total",
         # edu == "8-11",
         age == "20-29") %>% 
  group_by(country, geo, age, edu, imp_type) %>%
  do(pred_births(chunk = .data)) %>% 
  ungroup()
  
test %>% 
  filter(geo == "total",
         edu != "total",
         imp_type == "i") %>% 
  ggplot()+
  geom_line(aes(date, bts, linetype = edu, group = edu), 
            col = "black")+
  geom_ribbon(aes(date, ymin = bsn_lp, ymax = bsn_up, group = edu), fill = "red", alpha = 0.2)+
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

dt9 <- 
  dt8 %>% 
  group_by(country, geo, age, edu, imp_type) %>%
  do(pred_births(chunk = .data)) %>% 
  ungroup()

# saving outputs
write_rds(dt9, "data_inter/monthly_excess_births_bra_col_mex.rds")

dt9 %>% 
  filter(geo == "total",
         age == "total",
         imp_type == "i") %>% 
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


dt9 %>% 
  filter(geo == "total",
         edu == "total") %>% 
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

dt9 %>% 
  filter(country == "MEX",
         geo == "total",
         edu != "total",
         age != "total") %>% 
  group_by(year) %>% 
  summarise(bts = sum(bts_n))

