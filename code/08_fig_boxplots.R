library(tidyverse)
all <- read_rds("data_inter/weekly_excess_confirmed_brazil_colombia.rds")

library(lubridate)

all2 <- 
  all %>% 
  mutate(ini_month = make_date(d = 1, m = month(date), y = year(date)),
         ds_in_mth = date - ini_month)


,
         dts_mth = ifelse())
         