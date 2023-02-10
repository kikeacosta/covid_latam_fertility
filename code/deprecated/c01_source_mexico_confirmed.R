
library(tidyverse)
library(foreign)
mex_files <- unzip("data_input/mexico/defunciones_base_datos_2021_dbf.zip", list = TRUE)

temp <- 
  read.dbf(unz("data_input/mexico/defunciones_base_datos_2021_dbf.zip", mex_files[4,1]))

mort <- 
  read.dbf("data_input/mexico/defunciones_base_datos_2021/defun21.dbf")


des <- read.dbf("data_input/mexico/defunciones_base_datos_2021/CATEMLDE21.DBF")

write_rds(des, "data_input/catalog.rds")
des <- read_rds("data_input/catalog.rds")


mex_est <- 
  des %>% 
  as_tibble() %>% 
  filter(CVE_MUN == "000") %>% 
  select(-CVE_MUN, -CVE_LOC, region = NOM_LOC, cod_reg = CVE_ENT) %>% 
  mutate(region = case_when(cod_reg == "09" ~ "Ciudad de Mexico",
                            cod_reg == "15" ~ "Mexico",
                            cod_reg == "16" ~ "Michoacan",
                            cod_reg == "19" ~ "Nuevo Leon",
                            cod_reg == "22" ~ "Queretaro",
                            cod_reg == "24" ~ "San Luis Potosi",
                            cod_reg == "31" ~ "Yucatan",
                            TRUE ~ region))


test <- 
  des %>% 
  as_tibble() %>% 
  filter(CVE_MUN == "000") %>% 
  select(-CVE_MUN, -CVE_LOC, region = NOM_LOC, cod_reg = CVE_ENT) %>% 
  mutate(cod_reg = case_when(cod_reg == "09" ~ "Ciudad de Mexico",
                             cod_reg == "15" ~ "Mexico",
                             cod_reg == "16" ~ "Michoacan",
                             cod_reg == "19" ~ "Nuevo Leon",
                             cod_reg == "22" ~ "Queretaro",
                             cod_reg == "24" ~ "San Luis Potosi",
                             cod_reg == "31" ~ "Yucatan",
                             TRUE ~ cod_reg))


mex_est <- 
  des %>% 
  as_tibble() %>% 
  filter(CVE_MUN == "000") %>% 
  select(-CVE_MUN, -CVE_LOC, region = NOM_LOC, cod_reg = CVE_ENT) %>% 
  mutate(reg2 = region,
         region = case_when(cod_reg == "09" ~ "Ciudad de Mexico",
                            cod_reg == "15" ~ "Mexico",
                            cod_reg == "16" ~ "Michoacan",
                            cod_reg == "19" ~ "Nuevo Leon",
                            cod_reg == "22" ~ "Queretaro",
                            cod_reg == "24" ~ "San Luis Potosi",
                            cod_reg == "31" ~ "Yucatan",
                            TRUE ~ reg2))





yr <- 15
temp <- 
  read.dbf(paste0("data_input/mexico/defunciones_base_datos_20", yr, "/DEFUN", yr, ".dbf"))


write_rds(temp, paste0("data_input/mexico/mort20", yr, ".rds"))


temp <- read_rds("data_input/mexico/mort2021.rds") 
