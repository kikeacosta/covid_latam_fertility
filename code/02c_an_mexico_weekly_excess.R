source("Code/00_functions.R")

mex_dts_pop <- 
  read_rds("data_inter/mexico_deaths_population_2015_2021.rds")

last_date <- "2020-03-15"

mex_dts_pop2 <- 
  mex_dts_pop %>% 
  mutate(exposure = pop / 52) %>% 
  arrange(div, date) %>% 
  group_by(div) %>% 
  mutate(t = 1:n(),
         w = ifelse(date <= "2020-03-15", 1, 0)) %>% 
  ungroup() %>% 
  mutate(isoweek = paste0(str_sub(isoweek, 1,8), "-7"),
         date = ISOweek2date(isoweek),
         year = year(date))

mex_bsn <- 
  mex_dts_pop2 %>% 
  group_by(div) %>% 
  do(est_baseline(db = .data)) %>% 
  ungroup()







write_rds(mex_bsn, "data_inter/mexico_baseline_weekly_2015_2021.rds")
mex_bsn <- read_rds("data_inter/mexico_baseline_weekly_2015_2021.rds")

mex_bsn %>% 
  mutate(excess = ifelse(date > last_date & dts > ul, "y", "n")) %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.2, fill = "#118ab2")+
  geom_line(aes(date, ll_r), linewidth = 0.1, alpha = 0.3, col = "#118ab2")+
  geom_line(aes(date, ul_r), linewidth = 0.1, alpha = 0.3, col = "#118ab2")+
  geom_line(aes(date, dts_r), linewidth = 0.2, alpha = 0.2)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.8)+
  geom_point(aes(date, dts_r, col = excess), size = 0.3, alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-02-15"), linetype = "dashed", 
             col = "#06d6a0",
             alpha = 0.5,
             linewidth = 0.3)+
  scale_color_manual(values = c("#073b4c", "#ef476f"))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  facet_wrap(~ div, scales = "free", ncol = 1)+
  theme_bw()+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 7),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 6),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 7),
    strip.background = element_blank()
  )
ggsave("figures/mexico_baseline_weekly_deaths_by_state.pdf",
       width = 10,
       height = 50,
       limitsize = FALSE)

# ====



# mortality excess ====
# ~~~~~~~~~~~~~~~~~~~~~
mex_exc <- 
  mex_bsn %>% 
  filter(year(date) >= 2020) %>% 
  select(div, date, isoweek, dts, bsn, exposure) %>% 
  mutate(excess = dts / bsn,
         pscore = dts / bsn)

write.csv(mex_exc, "data_inter/mexico_weekly_excess_2020_2021.csv")
