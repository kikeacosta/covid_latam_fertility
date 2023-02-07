source("Code/00_functions.R")

# data taken from coverage-db files
db_in <- read_rds("data_input/brazil/Brazil.rds")

db2 <- 
  db_in %>% 
  filter(Sex == "b") %>% 
  mutate(date = dmy(Date)) %>% 
  select(country = Country,
         div = Region,
         code = Code,
         date,
         dts_cum = Value) %>% 
  group_by(country, div, code, date) %>% 
  summarise(dts_cum = sum(dts_cum)) %>% 
  ungroup()

# weekly new deaths
db3 <- 
  db2 %>% 
  mutate(isoweek = date2ISOweek(date),
         daywk = str_sub(isoweek, 10, 10)) %>% 
  filter(daywk == "7") %>% 
  group_by(country, div, code) %>% 
  mutate(dts = dts_cum - lag(dts_cum)) %>% 
  ungroup() %>% 
  select(country, div, code, date, dts)

write_rds(db3, "data_inter/confirmed_brazil_state.rds")
