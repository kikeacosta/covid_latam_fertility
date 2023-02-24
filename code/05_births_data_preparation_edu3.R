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
  readRDS("data_inter/covid_tab_all_edu03.RDS") 

<<<<<<< HEAD
=======
unique(dt$raw_country)
unique(dt$raw_geo1nam)

dt %>% 
  summarise(bts = sum(raw_nbirth))

# looking at adjustments
dt %>% 
  filter(raw_country == "MEX") %>%
  # filter(raw_yearbir > 2019) %>%
  group_by(raw_country, raw_yearbir) %>% 
  summarise(bts = sum(raw_nbirth))

dt %>% 
  filter(raw_country == "MEX") %>%
  # filter(raw_yearbir > 2019) %>% 
  group_by(raw_country, raw_yearbir) %>% 
  summarise(bts_adj = sum(raw_nbirth * cfactor))

# Mexico by year
dt %>% 
  filter(raw_country == "MEX") %>%
  # filter(raw_yearbir > 2019) %>%
  group_by(raw_country, raw_yearbir) %>% 
  summarise(bts = sum(raw_nbirth)) %>% 
  left_join(
    dt %>% 
      filter(raw_country == "MEX") %>%
      # filter(raw_yearbir > 2019) %>% 
      group_by(raw_country, raw_yearbir) %>% 
      summarise(bts_adj = sum(raw_nbirth * cfactor))
  ) %>% 
  mutate(adj = bts_adj / bts)


>>>>>>> ee9f835a4c359baf2a315f2541294caf1d7586c9
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
unique(dt$raw_mothag7)
unique(dt$raw_edumo03)

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
         edu = raw_edumo03,
         cfactor,
         bts = raw_nbirth) %>% 
  mutate(age = case_when(age %in% c("10-14", "15-19") ~ "10-19",
                                 age %in% c("20-24", "25-29") ~ "20-29",
                                 age %in% c("30-34", "35-39") ~ "30-39",
                                 TRUE  ~ "40-54"),
         age = factor(age, levels = c("10-19", "20-29", "30-39", "40-54")),
         # bts = bts * cfactor
         ) %>% 
  group_by(country, geo, age, edu, year, mth) %>% 
  summarise(bts = sum(bts)) %>% 
  ungroup()

# testing for duplicates
dt2 %>% 
  group_by(country, geo, age, edu, year, mth) %>% 
  filter(n() > 1)


# ~~~~~~~~~~~~~~~~~~
# imputing education
# ~~~~~~~~~~~~~~~~~~
tot_edu <-
  dt2 %>%
  group_by(country, geo, age, year, mth) %>%
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
imps <- 
  dt4 %>% 
  gather(bts, bts_i, bts_t, key = imp_type, value = bts) %>% 
  mutate(imp_type = case_when(imp_type == "bts" ~ "n",
                              imp_type == "bts_i" ~ "i",
                              imp_type == "bts_t" ~ "t")) %>% 
  mutate(ct_geo = paste(country, geo, sep = "_")) %>% 
  select(-country, -geo) %>% 
  arrange(ct_geo, year, mth, edu, age, imp_type) %>% 
  mutate(year = year %>% as.integer())

dt5 <- 
  imps %>% 
  complete(ct_geo = unique(imps$ct_geo),
           year = 2015:2021, 
           mth = 1:12, 
           age = unique(imps$age),
           edu = unique(imps$edu), 
           imp_type, 
           fill = list(bts = 0)) %>% 
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
         edu = factor(edu, levels = c("0-7", "8-11", "12+", "total")),
         age = factor(age, levels = c("10-19", "20-29", "30-39", "40-54", "total")))

unique(dt6$edu)
unique(dt6$age)
unique(dt6$geo) %>% sort
unique(dt6$imp_type)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# adding Colombian and Mexican regions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

reg_baja_cali <- c("Baja California",
                   "Baja California Sur")

dt7 <- 
  dt6 %>% 
  filter(country == "COL" & geo %in% c(reg_amazon, reg_orinoq, reg_ejecaf) | 
           country == "MEX" & geo %in% reg_baja_cali) %>% 
  mutate(geo = case_when(country == "COL" & geo %in% reg_amazon ~ "Amazonia",
                         country == "COL" & geo %in% reg_orinoq ~ "Orinoquia",
                         country == "COL" & geo %in% reg_ejecaf ~ "Eje Cafetero",
                         country == "MEX" & geo %in% reg_baja_cali ~ "Baja Californias",
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
  mutate(w = ifelse(date < "2020-03-01", 1, 0)) %>% 
  arrange(country, geo, date, edu, age, imp_type, date)

write_rds(dt8, "data_inter/master_births_for_baseline_estimation.rds")


test <- 
  dt8 %>% 
  group_by(country, geo, age, edu, year, mth, imp_type) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)

