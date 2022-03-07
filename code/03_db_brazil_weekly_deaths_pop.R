

{
  source("Code/00_functions.R")
  # Source of data
  # https://opendatasus.saude.gov.br/dataset/sistema-de-informacao-sobre-mortalidade-sim-1979-a-2019
  # dictionnary
  # Source: https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Estrutura_SIM.pdf
  
  # Loading data ====
  # ~~~~~~~~~~~~~~~~~
  
  # States and municipality codes
  reg_codes <- 
    read_csv("data_input/brazil/bra_state_codes_names.csv", 
             locale = readr::locale(encoding = "latin1"))
  
  # loading deaths from microdata ====
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # deaths 2021
  # source: http://svs.aids.gov.br/dantps/centrais-de-conteudos/dados-abertos/sim/
  # # converting 2021 from dbf to csv
  library(foreign)
  d21 <- read.dbf("data_input/DO21OPEN.dbf")
  write_delim(d21, "data_input/brazil/Mortalidade_Geral_2021.csv",
              delim = ";")

  csv_files <- paste0("data_input/brazil/Mortalidade_Geral_", 2015:2021, ".csv")
  out <- list()
  for (i in csv_files){
    cat(i)
    out[[i]] <-
      read_delim(i,
                 delim = ";",
                 col_types = cols(.default = "c")) %>%
      filter(TIPOBITO == "2") %>%
      select(date = DTOBITO, mun_code = CODMUNOCOR) %>%
      mutate(date = dmy(date),
             isoweek = date2ISOweek(date),
             year = str_sub(isoweek, 1, 4) %>% as.double(),
             week = str_sub(isoweek, 7, 8) %>% as.double(),
             state_num = str_sub(mun_code, 1, 2)) %>%
      group_by(year, week, state_num) %>%
      summarise(dts = n()) %>%
      ungroup()
  }

  dts <-
    out %>%
    bind_rows() %>%
    mutate(isoweek = paste0(year, "-W", sprintf("%02d", week), "-7"),
           date = ISOweek2date(isoweek),
           state_num = state_num %>% as.double()) %>%
    filter(year >= 2015) %>%
    left_join(reg_codes) %>%
    select(-isoweek, -region, -state_num) %>%
    rename(state = state_name)

  dts_nal <-
    dts %>%
    group_by(year, week, date) %>%
    summarise(dts = sum(dts)) %>%
    ungroup() %>%
    mutate(state = "Total",
           state_iso = "BR") %>%
    bind_rows(dts) %>%
    arrange(state, date)


  # saving daily deaths by state
  write_rds(dts_nal, "data_inter/brazil_weekly_deaths_by_state_2015_2021.rds")
  
  # ====
  
}

# loading daily deaths by state ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_nal <- read_rds("data_inter/brazil_weekly_deaths_by_state_2015_2021.rds")

# reading population by state
pop <- 
  read_csv2("data_input/brazil/bra_population_state_2010_2025.csv",
            skip = 3) %>% 
  drop_na(RO) %>% 
  rename(year = Ano) %>% 
  gather(-year, key = state_iso, value = pop) %>% 
  mutate(week = 26, 
         year = year %>% as.double()) 

sts <- unique(pop$state_iso)

# Interpolating exposures to weeks  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop_interpol <- 
  expand_grid(year = 2014:2022, week = 1:52, state_iso = sts) %>% 
  bind_rows(expand_grid(year = c(2015, 2020), week = 53, state_iso = sts)) %>% 
  arrange(state_iso, year, week) %>% 
  left_join(pop) %>% 
  group_by(state_iso) %>% 
  mutate(t = 1:n()) %>% 
  do(interpop(db = .data)) %>% 
  ungroup()

# visual inspection
# ~~~~~~~~~~~~~~~~~

pop_interpol %>% 
  filter(state_iso == "AC") %>% 
  ggplot()+
  geom_line(aes(t, pop2))+
  geom_point(aes(t, pop), col = "red")

pop_interpol2 <- 
  pop_interpol %>% 
  select(-pop, -t) %>% 
  rename(pop = pop2)

pop_interpol3 <- 
  pop_interpol2 %>% 
  group_by(year, week) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(state_iso = "BR") %>% 
  bind_rows(pop_interpol2)

bra_dts_pop <- 
  dts_nal %>% 
  left_join(pop_interpol3) %>% 
  arrange(state, date)

write_rds(bra_dts_pop, "data_inter/brazil_deaths_population_2015_2021.rds")

  
  
