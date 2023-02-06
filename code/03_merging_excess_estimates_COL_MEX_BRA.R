rm(list=ls())
source("Code/00_functions.R")

codes <- read_csv("data_input/iso_codes.csv")

# ~~~~~~~~
# Colombia
# ~~~~~~~~
col_bsn <- read_rds("data_inter/colombia_baseline_weekly_2015_2021.rds")

col_bsn2 <- 
  col_bsn %>% 
  rename(div = dpto) %>% 
  mutate(excess = dts - bsn,
         country = "Colombia",
         div = recode(div,
                      "Atlántico" = "Atlantico",
                      "Bogotá" = "Bogota",
                      "Bolívar" = "Bolivar",
                      "Boyacá" = "Boyaca",
                      "Caquetá" = "Caqueta",
                      "Chocó" = "Choco",
                      "Córdoba" = "Cordoba",
                      "Guainía" = "Guainia",
                      "La Guajira" = "Guajira",
                      "Nariño" = "Narino",
                      "Norte de Santander" = "Norte Santander",
                      "Quindío" = "Quindio",
                      "San Andrés y Providencia" = "San Andres",
                      "Total" = "Total",
                      "Valle del Cauca" = "Valle",
                      "Vaupés" = "Vaupes")) %>% 
  select(country, div, date, isoweek, dts, bsn, ll, ul, exposure) %>% 
  left_join(codes) 

unique(col_bsn2$div)
# ~~~~~~
# Brazil
# ~~~~~~
bra_bsn <- read_rds("data_inter/brazil_baseline_weekly_2015_2021.rds")

bra_bsn2 <- 
  bra_bsn %>% 
  rename(div = state) %>% 
  mutate(country = "Brazil",
         isoweek = paste0(year, "-W", sprintf("%02d", week), "-7"),
         div = str_trim(div),
         code = ifelse(div == "Total", state_iso, paste0("BR-", state_iso))) %>% 
  select(country, code, date, isoweek, dts, bsn, ll, ul, exposure) %>% 
  left_join(codes) 



# ~~~~~~
# Mexico
# ~~~~~~
mex_bsn <- read_rds("data_inter/mexico_baseline_weekly_2015_2021.rds")

mex_bsn2 <- 
  mex_bsn %>% 
  mutate(country = "Mexico") %>% 
  left_join(codes) %>% 
  select(country, code, div, date, isoweek, dts, bsn, ll, ul, exposure)


# ~~~~~~~~~~~~~~~~~~~~
# putting all together
# ~~~~~~~~~~~~~~~~~~~~

all <- 
  bind_rows(col_bsn2,
            bra_bsn2,
            mex_bsn2) %>% 
  filter(date >= "2020-01-01",
         date <= "2021-12-31") %>%
  mutate(bsn = ifelse(bsn > dts, dts, bsn),
         exc = dts - bsn,
         psc = dts / bsn) 


write_rds(all, "data_inter/weekly_excess_confirmed_brazil_colombia_mexico.rds")
write_csv(all, "data_inter/weekly_excess_confirmed_brazil_colombia_mexico.csv")


# number of deaths between 2015 and 2021
dts <- 
  all %>% 
  filter(date >= "2015-01-01",
         date <= "2021-12-31",
         div == "Total") %>% 
  group_by() %>% 
  summarise(dts = sum(dts))
  
all %>% 
  filter(date >= "2015-01-01",
         date <= "2021-12-31",
         div == "Total") %>% 
  group_by(country) %>% 
  summarise(dts = sum(dts))


