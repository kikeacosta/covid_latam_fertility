source("Code/00_functions.R")

col_dts_pop <- read_rds("output/colombia_deaths_population_2015_2021.rds")


last_date <- "2020-03-15"

col_dts_pop2 <- 
  col_dts_pop %>% 
  mutate(exposure = pop / 52) %>% 
  arrange(dpto, date) %>% 
  group_by(dpto) %>% 
  mutate(t = 1:n(),
         w = ifelse(date <= "2020-03-15", 1, 0)) %>% 
  ungroup()


col_dts_pop3 <- 
  col_dts_pop2 %>% 
  group_by(dpto) %>% 
  do(est_baseline(db = .data))
  

col_dts_pop3 %>% 
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
  facet_wrap(~ dpto, scales = "free", ncol = 1)+
  # labs(title = paste0("age", age_gr, "_", age_cl, "plus_excl_", mths_exc))+
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
ggsave(here("figures", "test.pdf"),
       width = 10,
       height = 50,
       limitsize = FALSE)
