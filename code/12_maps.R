library(cartography)
library(rgdal)
library(tmap)
library(sf)
library(tidyverse)
db1 <- 
  st_read("data_input/maps/sudamerica_adm2.shp")


db1 %>% 
  ggplot() + 
  geom_sf()
  
