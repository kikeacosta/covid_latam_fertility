rm(list=ls())
source("Code/00_functions.R")

dts <- read_rds("data_inter/monthly_excess_deaths_bra_col_mex.rds")
bts <- read_rds("data_inter/monthly_excess_births_bra_col_mex.rds")
# cds <- read_rds("data_inter/geo_codes_bra_col_mex.rds")

geos_bts <- 
  bts %>% 
  select(country, geo) %>% 
  unique() %>% 
  mutate(source1 = "bts",
         geo = case_when(geo == "total" ~ "Total",
                         TRUE ~ geo))

geos_dts <- 
  dts %>% 
  select(country, geo) %>% 
  unique() %>% 
  mutate(source2 = "dts", 
         geo = case_when(geo == "Ciudad de Mexico" ~ "Distrito Federal",
                         geo == "Norte Santander" ~ "Norte de Santander",
                         TRUE ~ geo)) 

geos_dts_bts <- 
  full_join(geos_dts, geos_bts)

copy_this(geos_dts_bts)

dts2 <- 
  dts %>% 
  mutate(geo = case_when(
    geo == 'Ciudad de Mexico' ~ 'Distrito Federal',
    geo == 'Norte Santander' ~ 'Norte de Santander',
    TRUE ~ geo),
    country = case_when(country == "Brazil" ~ "BRA",
                             country == "Mexico" ~ "MEX",
                             country == "Colombia" ~ "COL",
                        TRUE ~ country))

bts2 <- 
  bts %>% 
  rename(month = mth) %>% 
  mutate(year = year %>% as.integer(),
         geo = case_when(geo == "total" ~ "Total",
                         TRUE ~ geo)) %>% 
  filter(date >= "2020-01-01") %>% 
  mutate(psc_bts_n = bts_n / bsn_n,
         psc_bts_i = bts_i / bsn_i,
         psc_bts_t = bts_t / bsn_t)

dts_bts <- 
  bts2 %>% 
  left_join(dts2)

%>% 
  # left_join(cds)

test <- 
  dts_bts %>%
  filter(is.na(code)) %>% 
  select(country, geo) %>% 
  unique()

test2 <-   
dts_bts2 %>% 
  select(country, geo) %>% 
  unique()

# saving outputs
write_rds(dts_bts, "data_inter/master_monthly_excess_deaths_births_bra_col_mex.rds")

test <- 
  dts_bts %>% 
  filter(is.na(raw_geolev1)) %>% 
  select(country, geo) %>% unique

