# library(tidyverse)
# library(lubridate)
# library(readxl)
# library(ISOweek)
rm(list=ls())
source("Code/00_functions.R")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# deaths by state and week ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
des <- read_rds("data_input/mexico/mexico_region_codes.rds")

y <- 14
wk_dts <- tibble()
for(y in 14:21){
  temp <- 
    read_rds(paste0("data_input/mexico/mort20", y, ".rds")) %>% as_tibble()
  
  temp2 <- 
    temp %>% 
    rename_all(tolower) %>% 
    select(cod_reg = 1, 
           day = dia_ocurr, 
           mth = mes_ocurr,
           year = anio_ocur) %>% 
    mutate(cod_reg = as.character(cod_reg),
           date = make_date(d = day, m = mth, y = year),
           isoweek = date2ISOweek(date),
           isoweek = str_sub(isoweek, 1, 8)) %>% 
    group_by(cod_reg, isoweek, year) %>% 
    summarise(dts = n()) %>% 
    ungroup()
  
  wk_dts <- 
    wk_dts %>% 
    bind_rows(temp2)
}

wk_dts2 <- 
  wk_dts %>% 
  group_by(cod_reg, isoweek) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(epi_year = str_sub(isoweek, 1, 4) %>% as.integer()) %>% 
  filter(epi_year >= 2015) %>% 
  left_join(des) %>% 
  select(-cod_reg, -epi_year)

wk_dts3 <- 
  wk_dts2 %>% 
  bind_rows(wk_dts2 %>% 
              group_by(isoweek) %>% 
              summarise(dts = sum(dts)) %>% 
              ungroup() %>% 
              mutate(region = "Total")) %>% 
  mutate(isoweek = paste0(isoweek, "-7"),
         date = ISOweek2date(isoweek)) %>% 
  rename(div = region)

write_rds(wk_dts3, "data_inter/mexico_weekly_deaths_by_state_2015_2021.rds")

# ==============================================================================
# ==============================================================================
# ==============================================================================

# loading daily deaths by state ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wk_dts3 <- read_rds("data_inter/mexico_weekly_deaths_by_state_2015_2021.rds")


# ~~~~~~~~~~~~~~~
# population ====
# ~~~~~~~~~~~~~~~

# population by week for each state
locale=locale(encoding="latin1")
pop <- 
  read_csv("data_input/mexico/pob_mit_proyecciones_no_special_characters.csv",
           locale = locale(encoding = "UTF-8"))

pop2 <- 
  pop %>%
  select(year = ANO,
         div = ENTIDAD,
         age = EDAD,
         sex = SEXO,
         pop = POBLACION) %>% 
  mutate(div = case_when(div == "Republica Mexicana" ~ "Total",
                         TRUE ~ div)) %>% 
  group_by(year, div) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  filter(year %in% 2000:2025) 

unique(pop2$div)

# years with 53 weeks 2000-2025
leap_yrs <- 
  tibble(year = 2000:2025) %>% 
  mutate(isoweek = paste0(year, "-W53-7"),
         date = ISOweek2date(isoweek),
         isoweek2 = date2ISOweek(date),
         equal = ifelse(isoweek == isoweek2, "y", "n")) %>% 
  filter(equal == "y") %>% 
  pull(year)

# locate week at the mid-year 2000-2025 
wk_midyear <- 
  tibble(year = 2000:2025) %>% 
  mutate(wks = ifelse(year %in% leap_yrs, 53, 52),
         hlf = wks/2,
         t = cumsum(wks) - 26) %>% 
  select(year, t)


pop3 <- 
  pop2 %>% 
  left_join(wk_midyear) %>% 
  mutate(t = round(t))


# Interpolating exposures to weeks  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interpop <- function(db)
{
  xs <- db %>% drop_na() %>% pull(t)
  ys <- db %>% drop_na() %>% pull(pop)
  # smoothing using cubic splines
  ts <- db %>% pull(t) %>% sort()
  db %>% 
    mutate(pop2 = spline(xs, ys, xout = ts)$y)
}

pop_interpol <- 
  expand_grid(year = 2000:2022, week = 1:52, div = pop3$div %>% unique()) %>% 
  bind_rows(expand_grid(year = leap_yrs, week = 53, div = pop3$div %>% unique())) %>% 
  arrange(div, year, week) %>% 
  group_by(div) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  left_join(pop3) %>%  
  group_by(div) %>% 
  do(interpop(db = .data)) %>% 
  ungroup() %>% 
  mutate(isoweek = paste0(year, "-W", sprintf("%02d",week), "-7"),
         date = ISOweek2date(isoweek))
  
pop_interpol %>%
  filter(div == "Total") %>%
  ggplot()+
  geom_line(aes(t, pop2))+
  geom_point(aes(t, pop))

pop_int2 <- 
  pop_interpol %>% 
  select(div, date, pop = pop2)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# merging weekly deaths and population ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_pop <- 
  wk_dts3 %>% 
  left_join(pop_int2) %>% 
  mutate(week = str_sub(isoweek, 7, 8) %>% as.integer())

# standard codes
codes_std <- 
  read_csv("data_input/geo_codes_bra_col_mex.csv") %>% 
  filter(ISO_Code == "MEX") %>% 
  select(geo, geo_iso)

mex_out <- 
  dts_pop %>% 
  rename(geo = div) %>% 
  mutate(country = "MEX",
         geo = case_when(geo == "Ciudad de Mexico" ~ "Distrito Federal",
                         TRUE ~ geo),
         year = str_sub(isoweek, 1, 4) %>% as.double()) %>% 
  left_join(codes_std) %>% 
  select(country, geo, geo_iso, date, isoweek, year, week, dts, pop)

write_rds(mex_out, "data_inter/mexico_deaths_population_2015_2021.rds")

