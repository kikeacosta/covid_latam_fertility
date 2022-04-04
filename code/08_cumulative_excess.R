source("Code/00_functions.R")

db <- 
  read_rds("data_inter/weekly_excess_confirmed_brazil_colombia.rds")

db2 <- 
  db %>% 
  mutate(dts_exc_pos = ifelse(dts_excs > 0, dts_excs, 0),
         dts_conf = ifelse(is.na(dts_conf), 0, dts_conf),
         css_conf = ifelse(is.na(css_conf), 0, css_conf),
         out_lls = ifelse(dts < ll | dts > ul, 1, 0), 
         dts_exc_adj = ifelse(dts > ll, dts_excs, ll)) %>% 
  group_by(country, div) %>% 
  mutate(cum_bsn = cumsum(bsn),
         cum_dts_conf = cumsum(dts_conf),
         cum_dts_exc = cumsum(dts_exc_adj),
         cum_dts_exc_pos = cumsum(dts_exc_pos)) %>% 
  ungroup() %>% 
  mutate(cum_pscore = (cum_bsn + cum_dts_exc_pos) / cum_bsn,
         cum_pscore2 = (cum_bsn + cum_dts_exc) / cum_bsn)

  
db2 %>% 
  ggplot()+
  geom_point(aes(div, cum_pscore2, col = date))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10()+
  theme_bw()

db3 <- 
  db2 %>% 
  mutate(trim_n = quarter(date),
         year = year(date),
         trimstr = case_when(year == 2020 & trim_n == 1 ~ "Jan-Mar\n2020",
                             year == 2020 & trim_n == 2 ~ "Apr-Jun\n2020",
                             year == 2020 & trim_n == 3 ~ "Jul-Sep\n2020",
                             year == 2020 & trim_n == 4 ~ "Oct-Dec\n2020",
                             year == 2021 & trim_n == 1 ~ "Jan-Mar\n2021",
                             year == 2021 & trim_n == 2 ~ "Apr-Jun\n2021",
                             year == 2021 & trim_n == 3 ~ "Jul-Aug\n2021"),
         trimstr = factor(trimstr, levels = c("Jan-Mar\n2020",
                                              "Apr-Jun\n2020",
                                              "Jul-Sep\n2020",
                                              "Oct-Dec\n2020",
                                              "Jan-Mar\n2021",
                                              "Apr-Jun\n2021",
                                              "Jul-Aug\n2021"))) %>% 
  group_by(country, div, year, trimstr) %>% 
  summarise(cum_pscore = mean(cum_pscore)) %>% 
  ungroup() 

write_rds(db3, "data_inter/trimestral_cumulative_pscores.rds")