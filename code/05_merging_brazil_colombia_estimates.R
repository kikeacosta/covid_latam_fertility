source("Code/00_functions.R")

col_bsn <- read_rds("data_inter/colombia_baseline_weekly_2015_2021.rds")
col_exc <- read_csv("data_inter/colombia_weekly_excess_2020_2021.csv")

bra_bsn <- read_rds("data_inter/brazil_baseline_weekly_2015_2021.rds")
bra_exc <- read_csv("data_inter/brazil_weekly_excess_2020_2021.csv")




col_bsn2 <- 
  col_bsn %>% 
  left_join() %>% 
  rename(div = dpto) 
  

bra_bsn2 <- 
  bra_bsn %>% 
  mutate(isoweek = paste0(year, "-W", sprintf("%02d", week), "-7")) %>% 
  rename(div = state,
         iso_code = state_iso)
  

  
  bsn <- 
  bind_rows(col_bsn,
            bra_bsn)


