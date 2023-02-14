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

dt2 <- 
  dt %>% 
  mutate(exposure = pop / 52) %>% 
  arrange(country, geo, date) %>% 
  group_by(country, geo) %>% 
  mutate(t = 1:n(),
         w = ifelse(date <= last_date, 1, 0)) %>% 
  ungroup()

bsn <- 
  dt2 %>% 
  group_by(country, geo) %>% 
  do(est_baseline(db = .data)) %>% 
  ungroup()

write_rds(bsn, "data_inter/weekly_baseline_mortality_2015_2021.rds")


plot_baseline_mort <- function(ct){
  pt <- 
    bsn %>% 
    filter(country == ct) %>% 
    mutate(excess = ifelse(date > last_date & dts > ul, "y", "n")) %>% 
    ggplot()+
    geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.2, fill = "#118ab2")+
    geom_line(aes(date, ll_r), size = 0.1, alpha = 0.3, col = "#118ab2")+
    geom_line(aes(date, ul_r), size = 0.1, alpha = 0.3, col = "#118ab2")+
    geom_line(aes(date, dts_r), size = 0.2, alpha = 0.2)+
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


# # mortality excess ====
# # ~~~~~~~~~~~~~~~~~~~~~
# bra_exc <- 
#   bra_bsn %>% 
#   filter(year >= 2020) %>% 
#   select(state, state_iso, date, dts, bsn, exposure) %>% 
#   mutate(excess = dts - bsn,
#          pscore = dts / bsn)
# 
# write.csv(bra_exc, "data_inter/brazil_weekly_excess_2020_2021.csv")
# 
