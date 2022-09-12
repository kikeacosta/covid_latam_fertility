source("Code/00_functions.R")

# total confirmed cases and deaths by date and region
bra_conf <- read_rds("data_inter/confirmed_brazil_state.rds") %>%
  mutate(div = recode(div,
                      "Tocantis" = "Tocantins",
                      "All" = "Total")) %>% 
  drop_na()

col_conf <- read_rds("data_inter/confirmed_colombia_dpto.rds") %>%
  mutate(div = recode(div,
                      "All" = "Total"))

week_conf <- 
  bind_rows(bra_conf, col_conf) %>% 
  rename(dts_conf = dts,
         css_conf = css)

month_conf <- 
  week_conf %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  replace_na(list(dts_conf = 0)) %>% 
  group_by(country, div, year, month) %>% 
  summarise(dts_conf = sum(dts_conf)) %>% 
  ungroup() 

codes <- 
  week_conf %>% 
  select(country, div, code) %>% unique()
  
  
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
  select(country, div, date, isoweek, dts, bsn, ll, ul, exposure) %>% 
  left_join(codes) 

# excess estimates from Brazil
bra_bsn <- read_rds("data_inter/brazil_baseline_weekly_2015_2021.rds")

bra_bsn2 <- 
  bra_bsn %>% 
  rename(div = state) %>% 
  mutate(country = "Brazil",
         isoweek = paste0(year, "-W", sprintf("%02d", week), "-7"),
         div = str_trim(div),
         country = "Brazil",
         code = ifelse(div == "Total", state_iso, paste0("BR-", state_iso))) %>% 
  select(country, code, date, isoweek, dts, bsn, ll, ul, exposure) %>% 
  left_join(codes) 

all <- 
  bind_rows(col_bsn2,
            bra_bsn2) %>% 
  filter(date >= "2020-01-01",
         date <= "2021-12-31") %>% 
  left_join(week_conf %>% select(-code)) %>% 
  mutate(dts_excs = dts - bsn) %>% 
  left_join(codes) %>% 
  mutate(css_conf = ifelse(country == "Colombia" & is.na(css_conf), 0, css_conf)) %>% 
  replace_na(list(dts_conf = 0))


write_rds(all, "data_inter/weekly_excess_confirmed_brazil_colombia.rds")
write_csv(all, "data_inter/weekly_excess_confirmed_brazil_colombia.csv")


# number of deaths between 2015 and 2021
dts <- 
  bind_rows(col_bsn2,
          bra_bsn2) %>% 
  filter(date >= "2015-01-01",
         date <= "2021-12-31",
         div == "Total") %>% 
  group_by() %>% 
  summarise(dts = sum(dts))
  

unique(dts$div)
