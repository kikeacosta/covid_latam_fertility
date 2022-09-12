
source("Code/00_functions.R")


# data_source_c <- paste0("data_input/colombia/cases_",today(), ".csv")

# # Downloading data from the web
# ###############################
# cases_url <- "https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD"
# 
# # EA: the files became too big and the direct reading from the links is vary unstable this way.
# # So better to download them first and read them directly from the local drive
# 
# 
# download.file(cases_url, destfile = data_source_c)


# Loading data in the session
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cases and deaths database
# db <- read_csv(data_source_c,
#                locale = locale(encoding = "UTF-8"))


db <- 
  read_csv("data_input/colombia/Casos_positivos_de_COVID-19_en_Colombia.csv",
           locale = locale(encoding = "UTF-8"))

unique(db$'Nombre departamento') %>% sort()

db2 <- 
  db %>% 
  rename(div = 'Nombre departamento',
         City = 'Nombre municipio',
         status = 'Estado') %>% 
  mutate(div = str_to_title(div),
         div = recode(div,
                      "Sta Marta D.e." = "Magdalena", 
                      "Barranquilla" = "Atlantico",
                      "Cartagena" = "Bolivar"),
         date = ymd(`fecha reporte web`)) 

# cases ----------------------------------------------
# three dates for cases, preferred in this order: diagnosed, symptoms, reported to web
db_cases <- 
  db2 %>% 
  rename(date_diag = 'Fecha de diagnóstico',
         date_repo = 'Fecha de notificación',
         date_repo2 = 'fecha reporte web',
         date_sint = 'Fecha de inicio de síntomas') %>% 
  mutate(date_diag = ymd(date_diag),
         date_sint = ymd(date_sint),
         date_repo = ymd(date_repo),
         date = case_when(!is.na(date_diag) ~ date_diag,
                            is.na(date_diag) & !is.na(date_sint) ~ date_sint,
                            is.na(date_diag) & is.na(date_sint) ~ date_repo)) %>%
  mutate(isoweek = date2ISOweek(date),
         isowk = str_sub(isoweek, 1, 8)) %>% 
  group_by(div, isowk) %>% 
  summarise(css = n()) %>% 
  ungroup()

# deaths -----------------------------------------------------------
db_deaths <- 
  db2 %>% 
  filter(status == "Fallecido") %>% 
  rename(date2 = 'Fecha de muerte') %>% 
  mutate(date = ymd(date2)) %>% 
  mutate(isoweek = date2ISOweek(date),
         isowk = str_sub(isoweek, 1, 8)) %>% 
  group_by(div, isowk) %>% 
  summarise(dts = n()) %>% 
  ungroup()


rm(db, db2)
gc()

# summarising new cases for each combination -----------------------
db3 <- 
  full_join(db_cases, db_deaths) %>% 
  replace_na(list(css = 0, dts = 0)) %>% 
  mutate(code = case_when(
    div == "All" ~ "CO",
    div == "Bogota" ~ "CO-DC",
    div == "Amazonas" ~ "CO-AMA",
    div == "Antioquia" ~ "CO-ANT",
    div == "Arauca" ~ "CO-ARA",
    div == "Atlantico" ~ "CO-ATL",
    div == "Barranquilla" ~ "CO-BARRANQUILLA+",
    div == "Bolivar" ~ "CO-BOL",
    div == "Boyaca" ~ "CO-BOY",
    div == "Caldas" ~ "CO-CAL",
    div == "Cali" ~ "CO-CALI+",
    div == "Caqueta" ~ "CO-CAQ",
    div == "Cartagena" ~ "CO-CARTAGENA+",
    div == "Casanare" ~ "CO-CAS",
    div == "Cauca" ~ "CO-CAU",
    div == "Cesar" ~ "CO-CES",
    div == "Cordoba" ~ "CO-COR",
    div == "Cundinamarca" ~ "CO-CUN",
    div == "Choco" ~ "CO-CHO",
    div == "Guainia" ~ "CO-GUA",
    div == "Guaviare" ~ "CO-GUV",
    div == "Huila" ~ "CO-HUI",
    div == "Guajira" ~ "CO-LAG",
    div == "Magdalena" ~ "CO-MAG",
    div == "Medellin" ~ "CO-MEDELLIN+",
    div == "Meta" ~ "CO-MET",
    div == "Narino" ~ "CO-NAR",
    div == "Norte Santander" ~ "CO-NSA",
    div == "Putumayo" ~ "CO-PUT",
    div == "Quindio" ~ "CO-QUI",
    div == "Risaralda" ~ "CO-RIS",
    div == "San Andres" ~ "CO-SAP",
    div == "Santa Marta" ~ "CO-SANTA_MARTA+",
    div == "Santander" ~ "CO-SAN",
    div == "Sucre" ~ "CO-SUC",
    div == "Tolima" ~ "CO-TOL",
    div == "Valle" ~ "CO-VAC",
    div == "Vaupes" ~ "CO-VAU",
    div == "Vichada" ~ "CO-VID",
    TRUE ~ "CO-UNK+"))

db_co <- 
  db3 %>% 
  group_by(isowk) %>% 
  summarise(dts = sum(dts),
            css = sum(css)) %>% 
  ungroup() %>% 
  mutate(div = "All",
         code = "CO")

out <- 
  bind_rows(db3, db_co) %>% 
  mutate(country = "Colombia",
         date = ISOweek2date(paste0(isowk, "-7"))) %>% 
  select(-isowk)

write_rds(out, "data_inter/confirmed_colombia_dpto.rds")
# out <- read_rds("data_inter/confirmed_colombia_dpto.rds")

out %>% 
  group_by(country, div) %>% 
  summarise(dts = sum(dts)) 

unique(out$code)
