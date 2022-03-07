library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggridges)

db <- read_rds("data_inter/weekly_excess_confirmed_brazil_colombia.rds")

tx <- 8
db %>% 
  group_by(country, div) %>% 
  mutate(date_peak = date[which.max(dts_excs)] %>% as.numeric()) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = date, 
                       y = div,
                       height = dts_excs,
                       group = div)) + 
  geom_ridgeline(mapping = aes(y = reorder(div, -date_peak),
                               fill = as.factor(country)), alpha = .4) + 
  facet_wrap(~ country, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("#2a9d8f", "#e76f51")) +
  theme_bw()+
  theme(plot.title = element_text(size = tx + 1),
        axis.text.y = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx - 1),
        legend.position = "none",
        strip.background = element_rect(fill = "transparent"))


db2 <- 
  db %>% 
  mutate(ini_month = make_date(d  = 1, m = month(date), y = year(date)),
         days_in_mth = as.numeric(date - ini_month) + 1,
         frc = ifelse(days_in_mth >= 7, 1, days_in_mth/7),
         dts_mth_i = dts * frc,
         dts_mth_lag = dts * (1 - frc),
         bsn_mth_i = bsn * frc,
         bsn_mth_lag = bsn * (1 - frc))

dts <- 
  db2 %>%
  select(country, div, ini_month, dts_mth_i, dts_mth_lag) %>% 
  gather(dts_mth_i, dts_mth_lag, key = per, value = dts) %>% 
  mutate(date = case_when(per == "dts_mth_i" ~ ini_month,
                          TRUE ~ ini_month - months(1))) %>% 
  group_by(country, div, date) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()

bsn <- 
  db2 %>% 
  select(country, div, ini_month, bsn_mth_i, bsn_mth_lag) %>% 
  gather(bsn_mth_i, bsn_mth_lag, key = per, value = bsn) %>% 
  mutate(date = case_when(per == "bsn_mth_i" ~ ini_month,
                          TRUE ~ ini_month - months(1))) %>% 
  group_by(country, div, date) %>% 
  summarise(bsn = sum(bsn)) %>% 
  ungroup()

pop <- 
  db %>% 
  filter(month(date) == 6) %>% 
  mutate(year = year(date)) %>% 
  select(country, div, year, exposure) %>% 
  group_by(country, div, year) %>% 
  summarise(pop = mean(exposure) * 52) %>% 
  ungroup()

db3 <- 
  dts %>% 
  left_join(bsn) %>% 
  filter(date >= "2020-01-01") %>% 
  mutate(pscore = dts / bsn)

av_pscores <- 
  db3 %>% 
  group_by(country, div) %>% 
  summarise(av_pscore = mean(pscore)) %>% 
  arrange(country, -av_pscore)

db4 <- 
  db3 %>% 
  mutate(col_div = case_when(
    country == "Brazil" & div == "Amazonas" | 
      country == "Brazil" & div == "Rondônia" | 
      country == "Brazil" & div == "Mato Grosso" | 
      country == "Brazil" & div == "Goiás"  ~ paste0(div, " (Brazil)"),
    country == "Colombia" & div == "Amazonas" | 
      country == "Colombia" & div == "Atlantico" |
      country == "Colombia" & div == "Bogota" |
      country == "Colombia" & div == "Magdalena" ~ paste0(div, " (Colombia)"),
    TRUE ~ "other"),
    ident = ifelse(col_div == "other", "other", "ident"))

cols <- 
  c("Amazonas (Brazil)" = "#e41a1c",
    "Rondônia (Brazil)" = "#377eb8",
    "Mato Grosso (Brazil)" = "#4daf4a",
    "Goiás (Brazil)" = "#984ea3",
    "Amazonas (Colombia)" = "#e41a1c",
    "Atlantico (Colombia)" = "#377eb8",
    "Bogota (Colombia)" = "#4daf4a",
    "Magdalena (Colombia)" = "#984ea3",
    "other" = "black")
tx <- 8

db4 %>% 
  filter(date <= "2021-10-01") %>% 
  ggplot(aes(date, pscore)) +
  geom_boxplot(aes(group = date), outlier.shape = NA)+
  geom_jitter(aes(date, pscore, col = col_div, alpha = ident, size = ident),
              width = 5, height = 0)+
  scale_y_log10(labels = function(x) paste0((x - 1) * 100, "%"), 
                breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 5),
                limits = c(0.5, 5))+
  scale_x_date(breaks = seq(ymd('2020-01-01'), ymd('2021-09-01'), by = '3 months'), 
               date_labels = "%b\n%Y")+
  scale_color_manual(values = cols,
                     breaks = c("Amazonas (Brazil)",
                                "Rondônia (Brazil)",
                                "Mato Grosso (Brazil)",
                                "Goiás (Brazil)",
                                "Amazonas (Colombia)",
                                "Atlantico (Colombia)",
                                "Bogota (Colombia)",
                                "Magdalena (Colombia)"))+
  scale_alpha_manual(values = c(1, 0.3), guide = "none")+
  scale_size_manual(values = c(2, 1.5), guide = "none")+
  facet_wrap(~ country)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(y = "Excess p-score", x = "Date",
       col = "Subnational\ndivision")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 1),
        legend.text = element_text(size = tx + 2),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = tx + 4)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE,
                              override.aes = list(size = 2.5)))

ggsave("figures/pscores_boxplot.png", 
       dpi = 600,
       w = 12,
       h = 8)

ggsave("figures/pscores_boxplot.pdf", 
       w = 12,
       h = 8)


# data in trimesters ====
# ~~~~~~~~~~~~~~~~~~~~~~~

trims <- seq(ymd('2020-01-01'),ymd('2021-12-31'), by = '3 months')

db5 <- 
  dts %>% 
  mutate(year = year(date)) %>% 
  left_join(bsn) %>% 
  left_join(pop) %>% 
  filter(date >= "2020-01-01" & date < "2021-08-30") %>%
  mutate(trim_n = quarter(date),
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
  summarise(dts = sum(dts),
            bsn = sum(bsn),
            pop = mean(pop)) %>% 
  ungroup() %>% 
  mutate(pscore = dts / bsn)

divs_labs <- 
  db5 %>% 
  group_by(country, trimstr) %>% 
  arrange(-pscore) %>% 
  mutate(ord = 1:n()) %>% 
  ungroup() %>% 
  filter((country == "Colombia" & ord %in% c(1, 2, 33, 34))|
           (country == "Brazil" & ord %in% c(1, 2, 27, 28))) %>% 
  select(country, div, trimstr, pscore, ord)

db6 <- 
  db5 %>% 
  left_join(divs_labs) %>% 
  mutate(col_div = case_when(ord <= 2 ~ "top",
                             ord > 2 ~ "bottom",
                             TRUE ~ "other"),
         ident = ifelse(col_div == "other", "other", "ident"))

cols <- 
  c("top" = "#bb3e03",
    "bottom" = "#0a9396",
    "other" = "black")

tx <- 10

db6 %>% 
  ggplot(aes(trimstr, pscore)) +
  geom_boxplot(aes(group = trimstr), outlier.shape = NA)+
  geom_jitter(aes(trimstr, pscore, 
                  col = col_div, 
                  # shape = ident,
                  alpha = ident, 
                  size = pop),
              width = 0.05, height = 0)+
  geom_text_repel(data = divs_labs, aes(trimstr, pscore, label = div),
            size = tx / 3, 
            show.legend = FALSE,
            force = 0.1, 
            box.padding = 0.1,
            direction = "y",
            # nudge_x = 0.1,
            hjust = -0.3)+
  scale_y_log10(labels = function(x) paste0((x - 1) * 100, "%"), 
                breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 5))+
  # scale_x_date(breaks = seq(ymd('2020-01-01'), ymd('2021-09-01'), by = '1 months'),
  #              minor_breaks = NULL,
  #              date_labels = "%b\n%Y")+
  scale_color_manual(values = cols)+
  scale_alpha_manual(values = c(0.8, 0.2), guide = "none")+
  # scale_shape_manual(values = c(16, 1), guide = "none")+
  # scale_size_manual(values = c(2, 1.5), guide = "none")+
  # guides(color = guide_legend(nrow = 3))+
  facet_wrap(~ country)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(y = "Excess p-score", 
       x = "Trimester",
       col = "Extreme values",
       size = "Population")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(size = tx + 2),
        axis.title = element_text(size = tx + 3), 
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = tx + 4)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE,
                              override.aes = list(size = 3)))

ggsave("figures/pscores_boxplot_trim.pdf", 
       dpi = 600,
       w = 20,
       h = 8)

