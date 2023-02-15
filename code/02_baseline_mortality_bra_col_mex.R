rm(list=ls())
source("Code/00_functions.R")

bra <- read_rds("data_inter/brazil_deaths_population_2015_2021.rds")
col <- read_rds("data_inter/colombia_deaths_population_2015_2021.rds")
mex <- read_rds("data_inter/mexico_deaths_population_2015_2021.rds")

dt <- 
  bind_rows(bra,
            col,
            mex)

last_date <- "2020-03-15"

# from weekly to monthly data ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dt2 <- 
  dt %>% 
  mutate(exposure = pop / 52) %>% 
  mutate(ini_month = make_date(d  = 1, m = month(date), y = year(date)),
         days_in_mth = as.numeric(date - ini_month) + 1,
         frc = ifelse(days_in_mth >= 7, 1, days_in_mth/7),
         dts_mth_i = dts * frc,
         dts_mth_lag = dts * (1 - frc),
         exp_mth_i = exposure * frc,
         exp_mth_lag = exposure * (1 - frc))

dts <- 
  dt2 %>% 
  select(country, geo, ini_month, dts_mth_i, dts_mth_lag) %>% 
  gather(dts_mth_i, dts_mth_lag, key = per, value = dts) %>% 
  mutate(date = case_when(per == "dts_mth_i" ~ ini_month,
                          TRUE ~ ini_month - months(1))) %>% 
  group_by(country, geo, date) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()

pop <- 
  dt2 %>% 
  select(country, geo, ini_month, exp_mth_i, exp_mth_lag) %>% 
  gather(exp_mth_i, exp_mth_lag, key = per, value = exposure) %>% 
  mutate(date = case_when(per == "exp_mth_i" ~ ini_month,
                          TRUE ~ ini_month - months(1))) %>% 
  group_by(country, geo, date) %>% 
  summarise(exposure = sum(exposure)) %>% 
  ungroup()

dt3 <- 
  dts %>%
  left_join(pop) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(year %in% 2015:2021) %>% 
  arrange(country, geo, date) %>% 
  group_by(country, geo) %>% 
  mutate(t = 1:n(),
         w = ifelse(date < "2020-03-01", 1, 0)) %>% 
  ungroup()

bsn <- 
  dt3 %>% 
  group_by(country, geo) %>% 
  do(est_baseline2(db = .data)) %>% 
  ungroup()

write_rds(bsn, "data_inter/monthly_baseline_mortality_2015_2021.rds")


bsn <- read_rds("data_inter/monthly_baseline_mortality_2015_2021.rds")
last_date <- "2020-03-15"
plot_baseline_mort <- function(ct){
  pt <- 
    bsn %>% 
    filter(country == ct) %>% 
    mutate(dts_r = dts / exposure,
           bsn_r = bsn / exposure,
           ll_r = ll / exposure,
           ul_r = ul / exposure) %>% 
    mutate(excess = ifelse(date > last_date & dts > ul, "y", "n")) %>% 
    ggplot()+
    geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.2, fill = "#118ab2")+
    geom_line(aes(date, dts_r), linewidth = 0.1, alpha = 0.3, col = "#118ab2")+
    geom_line(aes(date, ul_r), linewidth = 0.1, alpha = 0.3, col = "#118ab2")+
    geom_line(aes(date, dts_r), linewidth = 0.2, alpha = 0.2)+
    geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.8)+
    geom_point(aes(date, dts_r, col = excess), size = 0.3, alpha = 0.7)+
    geom_vline(xintercept = ymd("2020-02-15"), linetype = "dashed", 
               col = "#06d6a0",
               alpha = 0.5,
               size = 0.3)+
    scale_color_manual(values = c("#073b4c", "#ef476f"))+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    facet_wrap(~ geo, scales = "free", ncol = 1)+
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
  ggsave(pt, 
         filename=paste0("figures/baseline_mort_", ct, "_weekly_deaths_by_state.pdf"),
         device = "pdf",
         width = 10,
         height = 50,
         limitsize = FALSE)
  return(pt)
  }

pt_bra <- plot_baseline_mort("BRA")
pt_col <- plot_baseline_mort("COL")
pt_mex <- plot_baseline_mort("MEX")

# ====
