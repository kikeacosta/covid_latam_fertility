Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
library(tidyverse)
library(osfr)
library(lubridate)

# Descarga de COVerAGE-DB InputDB de OSF
# Solo necesario una vez, a no ser que se quiera actualizar los datos 
# Ultima descarga hecha el 18.08.2021

# osf_retrieve_file("9dsfk") %>%
#   osf_download(path = "data/", conflicts = "overwrite")


# Paises de interes
cts <- c("Colombia", "Brazil", "Argentina", "Mexico", "Peru")

# Cargando los datos
# Filtrar paises de interes para evitar usar demasiada memoria en R
dat <-  read_csv("data/InputDB.zip",
                 skip = 1) %>% 
  filter(Country %in% cts) 
  
# Seleccionando variables de interes
# incluyendo algunas ciudades en Colombia
dat2 <- 
  dat %>% 
  mutate(Date = dmy(Date)) %>% 
  select(Country, Region, Short, Date, Sex, Age, Measure, Value) %>% 
  filter(Age != "TOT",
         Sex != "b",
         Measure %in% c("Cases", "Deaths")) %>% 
  mutate(Region = case_when(Country == "Colombia" & Region == "Barranquilla" ~ "Atlantico",
                            Country == "Colombia" & Region == "Cali" ~ "Valle del Cauca",
                            Country == "Colombia" & Region == "Cartagena" ~ "Bolivar",
                            Country == "Colombia" & Region == "Medellin" ~ "Antioquia",
                            Country == "Colombia" & Region == "Santa Marta" ~ "Magdalena",
                            TRUE ~ Region)) %>% 
  group_by(Country, Region, Date, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  spread(Measure, Value)

# Como la informacion de casos y muertes es cumulativa, no es necesario agregar data para 
# obtener el cumulativo por cuatrimestre. Basta con indicar las fechas de corte del cuatrimestre.
# Lo que si es necesario es desacumular para obtener casos y muertes por cuatrimestre 

# fechas de corte de cada cuatrimestre
dts <- c("2020-04-30", "2020-08-30", "2020-12-31", "2021-04-30")

dat3 <- 
  dat2 %>% 
  filter(Date %in% ymd(dts)) %>% 
  group_by(Country, Region) %>% 
  mutate(new_Cases = ifelse(Date == min(Date), Cases, Cases - lag(Cases)),
         new_Deaths = ifelse(Date == min(Date), Deaths, Deaths - lag(Deaths))) %>% 
  ungroup() %>% 
  mutate(Period = case_when(Date == "2020-04-30" ~ "2020_Q1", 
                            Date == "2020-08-30" ~ "2020_Q2", 
                            Date == "2020-12-31" ~ "2020_Q3", 
                            Date == "2021-04-30" ~ "2021_Q1")) %>% 
  select(-Cases, -Deaths)

# adjuntando codigos ISO
iso_codes <- 
  dat %>% 
  select(Country, Region, Short) %>% 
  unique() %>% 
  rename(IsoCode = Short) %>% 
  mutate(IsoCode = str_replace(IsoCode, "_", "-"))

dat4 <- 
  dat3 %>% 
  left_join(iso_codes)

# test de consistencia de datos
test <- 
  dat4 %>% 
  group_by(Country, Region) %>% 
  summarise(Tot_Cases = sum(new_Cases),
            Tot_Deaths = sum(new_Deaths)) %>% 
  left_join(dat2 %>% 
              filter(Date == "2021-04-30")) %>% 
  mutate(ok_cases = ifelse(Tot_Cases == Cases, "y", "n"),
         ok_deaths = ifelse(Tot_Deaths == Deaths, "y", "n"))

write_csv(dat4, "output/covid_casos_muertes_latam_regional_cuatrimestre.csv")
write_csv(iso_codes, "output/codigos_iso_regiones.csv")
