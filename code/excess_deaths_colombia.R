
library(tidyverse)
library(readxl)
library(ISOweek)

col <- read_xlsx("data/colombia/anexos-defunciones-covid-dept-semana-35-2021.xlsx",
                 sheet = 2,
                 skip = 11,
                 col_types = rep("text", 15))

pop_05_17 <- 
  read_xlsx("data/colombia/anexo-area-sexo-edad-proyecciones-poblacion-departamental_2005-2017.xlsx",
            skip = 11)

pop_18_50 <- 
  read_xlsx("data/colombia/anexo-proyecciones-poblacion-departamental_2018-2050.xlsx",
            skip = 11)


col2 <- 
  col %>% 
  rename(year = 1,
         dpto = 2,
         week = 3,
         t_nat = 4,
         m_nat = 5,
         f_nat = 6,
         u_nat = 7,
         t_vio = 8,
         m_vio = 9,
         f_vio = 10,
         u_vio = 11,
         t_ees = 12,
         m_ees = 13,
         f_ees = 14,
         u_ees = 15) %>% 
  fill(year) %>% 
  fill(dpto) %>% 
  drop_na(week) %>% 
  gather(-year, -dpto, -week, key = type, value = dts) %>% 
  separate(type, c("sex", "cause"), sep = "_") %>% 
  filter(week != "Total") %>% 
  mutate(year = str_replace(year, "pr", ""),
         week = str_replace(week, "Semana ", ""),
         year = year %>% as.double(),
         week = week %>% as.double(),
         dts = dts %>% as.double()) %>% 
  filter(sex == "t") %>% 
  group_by(year, week, dpto) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()
  
unique(col2$year)
unique(col2$dpto)
unique(col2$week)

col_sum <- 
  col2 %>% 
  group_by(dpto, year) %>% 
  summarise(dts = sum(dts))

chunk <- 
  col2 %>% 
  filter(year == 2015,
         week == 1)

dist_unk_dpto <- 
  function(chunk){
    tot <- 
      chunk %>% 
      summarise(dts = sum(dts)) %>% 
      pull(dts)
    
    chunk %>% 
      filter(!dpto %in% c("Sin información", "Extranjero")) %>% 
      mutate(dts = tot * dts/sum(dts))
  }

col3 <- 
  col2 %>% 
  group_by(year, week) %>% 
  do(dist_unk_dpto(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(isoweek = paste0(year, "-W", sprintf("%02d", week), "-7"),
         date = ISOweek2date(isoweek))

tot_nal <-
  col3 %>% 
  group_by(year, week, date, isoweek) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(dpto = "Total")

col4 <- 
  bind_rows(col3, tot_nal)


col4 %>% 
  filter(dpto == "Total") %>% 
  ggplot()+
  geom_line(aes(date, dts))

col4 %>% 
  filter(dpto == "Bogotá") %>% 
  ggplot()+
  geom_line(aes(date, dts))



# population
# ~~~~~~~~~~

pop_05_17_2 <- 
  pop_05_17 %>% 
  rename(dpto = 2,
         year = 3,
         area = 4) %>% 
  filter(area == "Total")

pop_18_50_2 <- 
  pop_18_50 %>% 
  rename(dpto = 2,
         year = 3,
         area = 4) %>% 
  filter(area == "Total") %>% 
  mutate(year = year %>% as.double())

pop <- 
  bind_rows(pop_05_17_2,
            pop_18_50_2) %>% 
  select(-area) %>% 
  gather(-DP, -dpto, -year, key = sex_age, value = pop) %>% 
  filter(!sex_age %in% c("Total Hombres", "Total Mujeres", "Total")) %>% 
  separate(sex_age, c("sex", "age"), sep = "_")

pop_all_age_sex <- 
  pop %>% 
  group_by(dpto, year) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

unique(pop$sex_age)
