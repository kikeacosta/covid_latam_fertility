source("Code/00_functions.R")

# total confirmed cases and deaths by date and region
bra_conf <- read_rds("data_inter/confirmed_brazil_state.rds") %>% 
  mutate(div = recode(div, 
                      "Tocantis" = "Tocantins",
                      "All" = "Total"))

col_conf <- read_rds("data_inter/confirmed_colombia_dpto.rds") %>% 
  mutate(div = recode(div, 
                      "Cartagena" = "Bolivar",
                      "All" = "Total")) %>% 
  group_by(country, div, date) %>% 
  summarise(css = sum(css),
            dts = sum(dts)) %>% 
  ungroup()

week_conf <- 
  bind_rows(bra_conf,
            col_conf) %>% 
  mutate(isoweek = date2ISOweek(date)) %>% 
  replace_na(list(dts = 0, css = 0)) %>% 
  group_by(country, div, isoweek) %>% 
  summarise(dts = sum(dts),
            css = sum(css)) %>% 
  ungroup() %>% 
  mutate(css = ifelse(country == "Brazil", NA, css)) %>% 
  rename(css_conf = css,
         dts_conf = dts)

month_conf <- 
  bind_rows(bra_conf,
            col_conf) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  replace_na(list(dts = 0, css = 0)) %>% 
  group_by(country, div, year, month) %>% 
  summarise(dts = sum(dts),
            css = sum(css)) %>% 
  ungroup() %>% 
  mutate(css = ifelse(country == "Brazil", NA, css)) %>% 
  rename(css_conf = css,
         dts_conf = dts)


# excess estimates from Colombia
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
                      "Norte de Santander" = "Norte Santander",
                      "Quindío" = "Quindio",
                      "San Andrés y Providencia" = "San Andres",
                      "Total" = "Total",
                      "Valle del Cauca" = "Valle",
                      "Vaupés" = "Vaupes")) %>% 
  select(country, div, date, isoweek, dts, bsn, exposure)

# excess estimates from Brazil
bra_bsn <- read_rds("data_inter/brazil_baseline_weekly_2015_2021.rds")

bra_bsn2 <- 
  bra_bsn %>% 
  rename(div = state) %>% 
  mutate(country = "Brazil",
         isoweek = paste0(year, "-W", sprintf("%02d", week), "-7"),
         div = str_trim(div),
         country = "Brazil") %>% 
  select(country, div, date, isoweek, dts, bsn, exposure) 

all <- 
  bind_rows(col_bsn2,
            bra_bsn2) %>% 
  filter(date >= "2020-01-01") %>% 
  left_join(week_conf)


write_rds(all, "data_inter/weekly_excess_confirmed_brazil_colombia.rds")
write_csv(all, "data_inter/weekly_excess_confirmed_brazil_colombia.csv")