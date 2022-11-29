library(tidyverse)
library(lubridate)
library(readxl)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# deaths by state and week ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
des <- read_rds("data_input/mexico/mexico_region_codes.rds")

# des <- 
#   des %>% mutate(region = ifelse(region == "Veracruz de Ignacio de la Llave", "Veracruz", region) )
# write_rds(des, "data_input/mexico/mexico_region_codes.rds")

temp <- read_rds("data_input/mexico/mort2021.rds") 

# mex_est <- 
#   des %>% 
#   as_tibble() %>% 
#   filter(CVE_MUN == "000",
#          !CVE_ENT %in% as.character(33:99)) %>% 
#   select(-CVE_MUN, -CVE_LOC, region = NOM_LOC, cod_reg = CVE_ENT) %>% 
#   mutate(region = as.character(region),
#          cod_reg = as.character(cod_reg)) %>% 
#   mutate(region = case_when(cod_reg == "09" ~ "Ciudad de Mexico",
#                             cod_reg == "15" ~ "Mexico",
#                             cod_reg == "16" ~ "Michoacan",
#                             cod_reg == "19" ~ "Nuevo Leon",
#                             cod_reg == "22" ~ "Queretaro",
#                             cod_reg == "24" ~ "San Luis Potosi",
#                             cod_reg == "3" ~ "Veracruz",
#                             cod_reg == "31" ~ "Yucatan",
#                             TRUE ~ region))
# 
# write_rds(mex_est, "data_input/mexico/mexico_region_codes.rds")
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
  mutate(isoweek = paste0(isoweek, "-1"),
         date = ISOweek2date(isoweek))
# %>% 
#   mutate(region = ifelse(region == "Veracruz de Ignacio de la Llave", "Veracruz", region))


# ~~~~~~~~~~~~~~~
# population ====
# ~~~~~~~~~~~~~~~

# population by week for each state
pop <- 
  read_xlsx("data_input/mexico/Poblacion_01.xlsx",
         skip = 4) %>% 
  select(region = 1,
         y2000 = 3,
         y2005 = 4,
         y2010 = 5,
         y2020 = 6) %>% 
  mutate(region = case_when(region == "Estados Unidos Mexicanos" ~ "Total",
                            region == "México" ~ "Mexico",
                            region == "Ciudad de México" ~ "Ciudad de Mexico",
                            region == "Michoacán de Ocampo" ~ "Michoacan",
                            region == "Nuevo León" ~ "Nuevo Leon",
                            region == "Querétaro" ~ "Queretaro",
                            region == "San Luis Potosí" ~ "San Luis Potosi",
                            region == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
                            region == "Yucatán" ~ "Yucatan",
                            TRUE ~ region)) %>% 
  drop_na() %>% 
  gather(-region, key = year, value = pop) %>% 
  mutate(year = str_replace(year, "y", "") %>% as.integer(),
         t = case_when(year == 2000 ~ 1,
                          year == 2005 ~ 262,
                          year == 2010 ~ 523,
                          year == 2020 ~ 1044))

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

leap_yrs <- 
  tibble(year = 2000:2025) %>% 
  mutate(isoweek = paste0(year, "-W53-7"),
         date = ISOweek2date(isoweek),
         isoweek2 = date2ISOweek(date),
         equal = ifelse(isoweek == isoweek2, "y", "n")) %>% 
  filter(equal == "y") %>% 
  pull(year)

pop_interpol <- 
  expand_grid(year = 2000:2022, week = 1:52, region = pop$region %>% unique()) %>% 
  bind_rows(expand_grid(year = leap_yrs, week = 53, region = pop$region %>% unique())) %>% 
  arrange(region, year, week) %>% 
  group_by(region) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  left_join(pop) %>% 
  group_by(region) %>% 
  do(interpop(db = .data)) %>% 
  ungroup() %>% 
  mutate(isoweek = paste0(year, "-W", sprintf("%02d",week), "-1"),
         date = ISOweek2date(isoweek))
  
pop_interpol %>%
  filter(region == "Total") %>%
  ggplot()+
  geom_line(aes(t, pop2))+
  geom_point(aes(t, pop))

pop_int2 <- 
  pop_interpol %>% 
  select(region, date, pop = pop2)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# merging weekly deaths and population ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_pop <- 
  wk_dts3 %>% 
  left_join(pop_int2) %>% 
  mutate(week = str_sub(isoweek, 7, 8) %>% as.integer())

write_rds(dts_pop, "data_inter/mexico_deaths_population_2015_2021.rds")
