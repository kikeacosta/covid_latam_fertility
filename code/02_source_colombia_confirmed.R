
source("Code/00_functions.R")

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

db_co <- 
  db4 %>% 
  group_by(date_f, Measure) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  mutate(Region = "All")

out <- 
  bind_rows(db4, db_co)

out2 <- 
  out %>% 
  spread(Measure, new) %>% 
  rename(date = date_f,
         div = Region,
         dts = Deaths,
         css = Cases) %>% 
  replace_na(list(css = 0,
                  dts = 0)) %>% 
  complete(div, date, fill = list(dts = 0, css = 0)) %>% 
  mutate(country = "Colombia") %>% 
  select(country, div, date, css, dts) %>% 
  arrange(country, div, date)

write_rds(out2, "data_inter/confirmed_colombia_dpto.rds")
out2 <- read_rds("data_inter/confirmed_colombia_dpto.rds")

