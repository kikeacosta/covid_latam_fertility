

# Downloading data from the web
###############################
cases_url <- "https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD"

# EA: the files became too big and the direct reading from the links is vary unstable this way.
# So better to download them first and read them directly from the local drive

data_source_c <- paste0("data_input/colombia/cases_",today(), ".csv")

download.file(cases_url, destfile = data_source_c)


# Loading data in the session
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cases and deaths database
db <- read_csv(data_source_c,
               locale = locale(encoding = "UTF-8"))

unique(db$`Nombre departamento`)

# # compressing source files and cleanning stuff
# data_source <- c(data_source_c, data_source_t)
# 
# zipname <- paste0(dir_n, 
#                   "Data_sources/", 
#                   ctr,
#                   "/", 
#                   ctr,
#                   "_data_",
#                   today(), 
#                   ".zip")
# 
# zipr(zipname, 
#      data_source, 
#      recurse = TRUE, 
#      compression_level = 9,
#      include_directories = TRUE)
# 
# # clean up file chaff
# file.remove(data_source)

# data transformation for COVerAGE-DB
#####################################

unique(db$'Nombre departamento') %>% sort()

db2 <- db %>% 
  rename(Sex = Sexo,
         Region = 'Nombre departamento',
         City = 'Nombre municipio',
         status = 'Estado') %>% 
  mutate(Region = str_to_title(Region),
         Region = recode(Region,
                         "Sta Marta D.e." = "Magdalena", 
                         "Barranquilla" = "Atlantico")) 

# cases ----------------------------------------------
# three dates for cases, preferred in this order: diagnosed, symptoms, reported to web
db_cases <- db2 %>% 
  rename(date_diag = 'Fecha de diagnóstico',
         date_repo1 = 'Fecha de notificación',
         date_repo2 = 'fecha reporte web',
         date_sint = 'Fecha de inicio de síntomas') %>% 
  separate(date_diag, c("date_diag", "trash1"), sep = " ") %>% 
  separate(date_sint, c("date_sint", "trash2"), sep = " ") %>% 
  separate(date_repo1, c("date_repo", "trash3"), sep = " ") %>% 
  mutate(date_diag = dmy(date_diag),
         date_sint = dmy(date_sint),
         date_repo = dmy(date_repo),
         date_f = case_when(!is.na(date_diag) ~ date_diag,
                            is.na(date_diag) & !is.na(date_sint) ~ date_sint,
                            is.na(date_diag) & is.na(date_sint) ~ date_repo),
         Measure = "Cases") %>% 
  select(date_f, Region, Measure)

# deaths -----------------------------------------------------------
db_deaths <- 
  db2 %>% 
  filter(status == "Fallecido") %>% 
  rename(date = 'Fecha de muerte') %>% 
  separate(date, c("date_f", "trash1"), sep = " ") %>% 
  mutate(date_f = dmy(date_f),
         Measure = "Deaths") %>% 
  select(date_f, Region, Measure)

# summarising new cases for each combination -----------------------
db4 <- 
  bind_rows(db_cases, db_deaths) %>% 
  group_by(Region, date_f, Measure) %>% 
  summarise(new = n()) %>% 
  ungroup()






# expanding the database to all posible combinations and cumulating values -------------
all_dates <- 
  db4 %>% 
  filter(!is.na(date_f)) %>% 
  dplyr::pull(date_f)  
dates_f <- seq(min(all_dates), max(all_dates), by = '1 day')

db5 <- db4 %>% 
  tidyr::complete(Region, Measure, Sex, Age = ages, date_f = dates_f, fill = list(new = 0)) %>% 
  arrange(date_f, Region, Measure, Sex, suppressWarnings(as.integer(Age))) %>% 
  group_by(Region, Measure, Sex, Age) %>% 
  mutate(Value = cumsum(new)) %>% 
  select(-new) %>% 
  ungroup()

unique(db_deaths$Sex)

#######################
# template for database ------------------------------------------------------
#######################

# National data --------------------------------------------------------------
db_co <- 
  db5 %>% 
  group_by(date_f, Sex, Age, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Region = "All")

# 5-year age intervals for regional data -------------------------------------
db_regions <- 
  db5 %>% 
  mutate(Age2 = as.character(floor(as.numeric(Age)/5) * 5)) %>% 
  group_by(date_f, Region, Sex, Age2, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  arrange(date_f, Region, Measure, Sex, suppressWarnings(as.integer(Age2))) %>% 
  ungroup() %>% 
  rename(Age = Age2)

unique(db_regions$Region)

# merging national and regional data -----------------------------------
db_co_comp <- 
  bind_rows(db_regions, db_co)

# summarising totals by age and sex in each date -----------------------------------
db_tot_age <- 
  db_co_comp %>% 
  group_by(Region, date_f, Sex, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Age = "TOT")

db_tot <- 
  db_co_comp %>% 
  group_by(Region, date_f, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Sex = "b",
         Age = "TOT")

db_inc <- 
  db_tot %>% 
  filter(Measure == "Deaths",
         Value >= 10) %>% 
  group_by(Region) %>% 
  summarise(date_start = ymd(min(date_f))) %>% 
  ungroup()

# appending all data in one database ----------------------------------------------
db_all <- 
  bind_rows(db_co_comp, db_tot_age)

# filtering dates for each region (>50 deaths) -----------------------------------
db_all2 <- 
  db_all %>% 
  left_join(db_inc) %>% 
  mutate(date_start = ifelse(Age == "TOT", min(date_f), date_start)) %>% 
  drop_na() %>% 
  filter((Region == "All" & date_f >= "2020-03-20") | date_f >= date_start) %>% 
  select(-date_start) %>% 
  bind_rows(db_tot) %>% 
  arrange(date_f, Region, Sex, Age)
  

test <- 
  db_all2 %>% 
  filter(Age == "TOT")

unique(db_all2$Age)

# tests data -----------------------------------

db_inc2 <- 
  db_inc %>% dplyr::pull(Region)

db_m_reg <- 
  db_m %>% 
  mutate(date_f = ymd(str_sub(Fecha, 1, 10))) %>% 
  drop_na(date_f) %>% 
  rename(All = Acumuladas,
         t1 = 'Positivas acumuladas',
         t2 = "Negativas acumuladas",
         t3 = "Positividad acumulada",
         t4 = "Indeterminadas",
         t5 = "Procedencia desconocida") %>% 
  select(-c(Fecha, t1, t2, t3, t4, t5)) %>% 
  gather(-date_f, key = "Region", value = "Value") %>% 
  mutate(Measure = "Tests",
         Age = "TOT",
         Sex = "b",
         Region = case_when(Region == "Norte de Santander" ~ "Norte Santander",
                            Region == "Valle del Cauca" ~ "Valle",
                            Region == "Norte de Santander" ~ "Norte Santander",
                            TRUE ~ Region)) %>% 
  filter(Region %in% db_inc2,
         date_f >= "2020-03-20") %>% 
  select(Region, date_f, Sex, Age, Measure, Value) %>% 
  drop_na()

unique(db_m_reg$Region) %>% sort()
unique(db_all2$Region) %>% sort()

# all data together in COVerAGE-DB format -----------------------------------
out <- db_all2 %>%
  bind_rows(db_m_reg) %>% 
  mutate(Country = "Colombia",
         AgeInt = case_when(Region == "All" & !(Age %in% c("TOT", "100")) ~ 1,
                            Region != "All" & !(Age %in% c("0", "1", "TOT")) ~ 5,
                            Region != "All" & Age == "0" ~ 1,
                            Region != "All" & Age == "1" ~ 4,
                            Age == "100" ~ 5,
                            Age == "TOT" ~ NA_real_),
         Date = paste(sprintf("%02d",day(date_f)),
                      sprintf("%02d",month(date_f)),
                      year(date_f),
                      sep="."),
         Code = case_when(
           Region == "All" ~ paste0("CO", Date),
           Region == "Bogota" ~ paste0("CO_DC", Date),
           Region == "Amazonas" ~ paste0("CO_AMA", Date),
           Region == "Antioquia" ~ paste0("CO_ANT", Date),
           Region == "Arauca" ~ paste0("CO_ARA", Date),
           Region == "Atlantico" ~ paste0("CO_ATL", Date),
           Region == "Barranquilla" ~ paste0("CO_BQL", Date),
           Region == "Bolivar" ~ paste0("CO_BOL", Date),
           Region == "Boyaca" ~ paste0("CO_BOY", Date),
           Region == "Caldas" ~ paste0("CO_CAL", Date),
           Region == "Cali" ~ paste0("CO_CLI", Date),
           Region == "Caqueta" ~ paste0("CO_CAQ", Date),
           Region == "Cartagena" ~ paste0("CO_CAR", Date),
           Region == "Casanare" ~ paste0("CO_CAS", Date),
           Region == "Cauca" ~ paste0("CO_CAU", Date),
           Region == "Cesar" ~ paste0("CO_CES", Date),
           Region == "Cordoba" ~ paste0("CO_COR", Date),
           Region == "Cundinamarca" ~ paste0("CO_CUN", Date),
           Region == "Choco" ~ paste0("CO_CHO", Date),
           Region == "Guainia" ~ paste0("CO_GUA", Date),
           Region == "Guaviare" ~ paste0("CO_GUV", Date),
           Region == "Huila" ~ paste0("CO_HUI", Date),
           Region == "Guajira" ~ paste0("CO_LAG", Date),
           Region == "Magdalena" ~ paste0("CO_MAG", Date),
           Region == "Medellin" ~ paste0("CO_MLL", Date),
           Region == "Meta" ~ paste0("CO_MET", Date),
           Region == "Narino" ~ paste0("CO_NAR", Date),
           Region == "Norte Santander" ~ paste0("CO_NSA", Date),
           Region == "Putumayo" ~ paste0("CO_PUT", Date),
           Region == "Quindio" ~ paste0("CO_QUI", Date),
           Region == "Risaralda" ~ paste0("CO_RIS", Date),
           Region == "San Andres" ~ paste0("CO_SAP", Date),
           Region == "Santa Marta" ~ paste0("CO_SMT", Date),
           Region == "Santander" ~ paste0("CO_SAN", Date),
           Region == "Sucre" ~ paste0("CO_SUC", Date),
           Region == "Tolima" ~ paste0("CO_TOL", Date),
           Region == "Valle" ~ paste0("CO_VAC", Date),
           Region == "Vaupes" ~ paste0("CO_VAU", Date),
           TRUE ~ paste0("CO_Other", Date)
         ),
         Metric = "Count") %>% 
  arrange(Region, date_f, Measure, Sex, suppressWarnings(as.integer(Age))) %>% 
  select(Country, Region, Code,  Date, Sex, Age, AgeInt, Metric, Measure, Value)

unique(out$Region)
unique(out$Age)

############################################
#### saving database in N Drive ####
############################################
write_rds(out, paste0(dir_n, ctr, ".rds"))

# updating hydra dashboard
log_update(pp = ctr, N = nrow(out))

