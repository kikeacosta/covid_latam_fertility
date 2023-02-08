rm(list=ls())
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggridges)

db <- read_rds("data_inter/db_monthly_excess_deaths_bra_col_mex.rds")

av_pscores <- 
  db %>% 
  group_by(country, geo, code) %>% 
  summarise(av_pscore = mean(pscore)) %>% 
  arrange(country, -av_pscore) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(ord = 1:n()) %>% 
  filter(ord <= 4) %>% 
  ungroup() %>% 
  mutate(col_geo = paste0(geo, " (", country, ")"))

db2 <- 
  db %>% 
  left_join(av_pscores %>% select(-av_pscore)) %>% 
  mutate(col_geo = ifelse(is.na(col_geo), "other", col_geo),
         ord = ifelse(col_geo == "other", "other", paste0(str_sub(code, 1, 2), ord)),
         ident = ifelse(col_geo == "other", "other", "ident"))

cols <-
  c("BR1" = "#e41a1c",
    "BR2" = "#377eb8",
    "BR3" = "#4daf4a",
    "BR4" = "#984ea3",
    "CO1" = "#e41a1c",
    "CO2" = "#377eb8",
    "CO3" = "#4daf4a",
    "CO4" = "#984ea3",
    "MX1" = "#e41a1c",
    "MX2" = "#377eb8",
    "MX3" = "#4daf4a",
    "MX4" = "#984ea3",
    "oth" = "black")

bks <- c(paste0("BR", 1:4), paste0("CO", 1:4), paste0("MX", 1:4))
lbs <- av_pscores %>% pull(col_geo)

tx <- 8

db2 %>% 
  filter(date <= "2021-12-31") %>% 
  ggplot(aes(date, pscore)) +
  geom_boxplot(aes(group = date), outlier.shape = NA, 
               lwd = 0.3, alpha = 0.01, col = "grey50")+
  geom_jitter(aes(date, pscore, col = ord, alpha = ident, size = pop),
              width = 3, height = 0)+
  scale_y_log10(labels = function(x) paste0((x - 1) * 100, "%"), 
                breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 5))+
  scale_x_date(breaks = seq(ymd('2020-01-01'), ymd('2021-12-01'), 
                            by = '2 month'), 
               date_labels = "%b\n%y")+
  scale_color_manual(values = cols,
                     breaks = bks,
                     labels = lbs)+
  scale_alpha_manual(values = c(0.8, 0.15), guide = "none")+
  scale_size_continuous(breaks = c(100000, 500000, 1000000, 
                                   5000000, 10000000, 40000000),
                        labels = c("100K", "500K", "1M", "5M", "10M", "40M"))+
  guides(color = guide_legend(order = 1,
                              nrow = 3, byrow = TRUE,
                              override.aes = list(size = 2.5, alpha = 0.8)),
         size = guide_legend(nrow = 2, byrow = T))+
  facet_wrap(~ country)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(y = "Excess p-score", x = "Month",
       col = "Extreme values",
       size = "Population")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 1),
        legend.text = element_text(size = tx + 2),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = tx + 4))

ggsave("figures/pscores_boxplot_v2.png",
       dpi = 600,
       w = 16,
       h = 8)

ggsave("figures/pscores_boxplot_v2.pdf",
       w = 16,
       h = 8)

# 
# 
# 
# # data in trimesters ====
# # ~~~~~~~~~~~~~~~~~~~~~~~
# # reading cumulative pscores
# cum_pscores <- 
#   read_rds("data_inter/trimestral_cumulative_pscores.rds") %>% 
#   select(-div) 
# 
# # grouping into trimesters
# trims <- seq(ymd('2020-01-01'),ymd('2021-12-31'), by = '3 months')
# 
# db5 <- 
#   dts %>% 
#   mutate(year = year(date)) %>% 
#   left_join(bsn) %>% 
#   left_join(pop) %>% 
#   filter(date >= "2020-01-01" & date < "2021-12-31",
#          geo != "Total") %>%
#   mutate(trim_n = quarter(date),
#          trimstr = case_when(year == 2020 & trim_n == 1 ~ "Jan-Mar\n2020",
#                              year == 2020 & trim_n == 2 ~ "Apr-Jun\n2020",
#                              year == 2020 & trim_n == 3 ~ "Jul-Sep\n2020",
#                              year == 2020 & trim_n == 4 ~ "Oct-Dec\n2020",
#                              year == 2021 & trim_n == 1 ~ "Jan-Mar\n2021",
#                              year == 2021 & trim_n == 2 ~ "Apr-Jun\n2021",
#                              year == 2021 & trim_n == 3 ~ "Jul-Sep\n2021",
#                              year == 2021 & trim_n == 4 ~ "Oct-Dec\n2021"),
#          trimstr = factor(trimstr, levels = c("Jan-Mar\n2020",
#                                               "Apr-Jun\n2020",
#                                               "Jul-Sep\n2020",
#                                               "Oct-Dec\n2020",
#                                               "Jan-Mar\n2021",
#                                               "Apr-Jun\n2021",
#                                               "Jul-Sep\n2021",
#                                               "Oct-Dec\n2021"))) %>% 
#   group_by(country, geo, code, year, trimstr) %>% 
#   summarise(dts = sum(dts),
#             bsn = sum(bsn),
#             pop = mean(pop)) %>% 
#   ungroup() %>% 
#   mutate(pscore = dts / bsn) %>%
#   left_join(cum_pscores) %>%
#   mutate(geo = case_when(geo == "San Andres" ~ "San Andrés",
#                          geo == "Atlantico" ~ "Atlántico",
#                          geo == "Vaupes" ~ "Vaupés",
#                          geo == "Guainia" ~ "Guainía",
#                          geo == "Caqueta" ~ "Caquetá",
#                          geo == "Cordoba" ~ "Córdoba",
#                          TRUE ~ geo))
# 
# geos_labs <- 
#   db5 %>% 
#   group_by(country, trimstr) %>% 
#   arrange(-pscore) %>% 
#   mutate(ord = 1:n()) %>% 
#   ungroup() %>% 
#   filter((country == "Colombia" & ord %in% c(1, 2, 32, 33))|
#            (country == "Brazil" & ord %in% c(1, 2, 26, 27))) %>% 
#   select(country, geo, code, trimstr, pscore, ord)
# 
# geos_labs_cum <- 
#   db5 %>% 
#   group_by(country, trimstr) %>% 
#   arrange(-cum_pscore) %>% 
#   mutate(ord2 = 1:n()) %>% 
#   ungroup() %>% 
#   filter((country == "Colombia" & ord2 %in% c(1, 2, 32, 33))|
#            (country == "Brazil" & ord2 %in% c(1, 2, 26, 27))) %>% 
#   select(country, geo, code, trimstr, cum_pscore, ord2)
# 
# geos_labs_avg <- 
#   db5 %>% 
#   group_by(country, trimstr) %>% 
#   arrange(-cum_avg_pscore) %>% 
#   mutate(ord3 = 1:n()) %>% 
#   ungroup() %>% 
#   filter((country == "Colombia" & ord3 %in% c(1, 2, 32, 33))|
#            (country == "Brazil" & ord3 %in% c(1, 2, 26, 27))) %>% 
#   select(country, geo, code, trimstr, cum_avg_pscore, ord3) 
# 
# db6 <- 
#   db5 %>% 
#   left_join(geos_labs) %>% 
#   left_join(geos_labs_cum) %>% 
#   left_join(geos_labs_avg) %>% 
#   mutate(col_geo = case_when(ord <= 2 ~ "Highest p-score",
#                              ord > 2 ~ "Lowest p-score",
#                              TRUE ~ "other"),
#          col_geo = factor(col_geo, 
#                           levels = c("Lowest p-score", 
#                                      "Highest p-score", "other")),
#          ident = ifelse(col_geo == "other", "other", "ident"),
#          col_geo2 = case_when(ord2 <= 2 ~ "Highest p-score",
#                               ord2 > 2 ~ "Lowest p-score",
#                               TRUE ~ "other"),
#          col_geo2 = factor(col_geo2, 
#                            levels = c("Lowest p-score", 
#                                       "Highest p-score", "other")),
#          ident2 = ifelse(col_geo2 == "other", "other", "ident"),
#          col_geo3 = case_when(ord3 <= 2 ~ "Highest p-score",
#                               ord3 > 2 ~ "Lowest p-score",
#                               TRUE ~ "other"),
#          col_geo3 = factor(col_geo3, 
#                            levels = c("Lowest p-score", 
#                                       "Highest p-score", "other")),
#          ident3 = ifelse(col_geo3 == "other", "other", "ident")) 
# 
# 
# 
# # saving trimestral estimates
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# trim_out <-
#   db6 %>%
#   select(-ord, -col_geo, -ident, -ord2, -col_geo2, -ident2, -ord3, -col_geo3, -ident3)
# 
# write_rds(trim_out, "data_inter/trimestral_excess_confirmed_brazil_colombia.rds")
# write_csv(trim_out, "data_inter/trimestral_excess_confirmed_brazil_colombia.csv")
# 
# 
# 
# # plotting pscores ====
# # ~~~~~~~~~~~~~~~~~~~~~
# 
# cols <- 
#   c("Highest p-score" = "#bb3e03",
#     "Lowest p-score" = "#0a9396",
#     "other" = "black")
# 
# tx <- 12
# 
# # trimester p-scores
# # ~~~~~~~~~~~~~~~~~~
# db6 %>% 
#   ggplot(aes(trimstr, pscore)) +
#   geom_boxplot(aes(group = trimstr), outlier.shape = NA)+
#   geom_jitter(aes(trimstr, pscore, 
#                   col = col_geo, 
#                   # shape = ident,
#                   alpha = ident,
#                   size = pop),
#               # alpha = 0.9,
#               width = 0.025, 
#               height = 0)+
#   # geom_text_repel(data = geos_labs, aes(trimstr, pscore, label = geo),
#   #           size = tx / 2,
#   #           show.legend = FALSE,
#   #           force = 0.1,
#   #           box.padding = 0.1,
#   #           direction = "y",
#   #           # nudge_x = 0.1,
#   #           hjust = -0.15)+
#   # geom_text_repel(data = geos_labs, aes(trimstr, pscore, label = geo), size = 5)+
#   geom_text(data = geos_labs,
#             aes(trimstr, pscore, label = geo),
#             check_overlap = TRUE,
#             size = tx / 3,
#             hjust = -0.15)+
#   scale_y_log10(labels = function(x) paste0((x - 1) * 100, "%"), 
#                 breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 5))+
#   scale_color_manual(values = cols, breaks = c("Lowest p-score", 
#                                                "Highest p-score"))+
#   scale_alpha_manual(values = c(0.8, 0.2), guide = "none")+
#   scale_size_continuous(breaks = c(100000, 500000, 1000000, 
#                                    5000000, 10000000, 40000000),
#                         labels = c("100K", "500K", "1M", "5M", "10M", "40M"))+
#   guides(color = guide_legend(order = 1,
#                               nrow = 1,
#                               byrow = TRUE,
#                               override.aes = list(size = 3)),
#          size = guide_legend(nrow = 1))+
#   facet_wrap(~ country)+
#   geom_hline(yintercept = 1, linetype = "dashed")+
#   labs(y = "Excess p-score", 
#        x = "Trimester",
#        col = "Extreme values",
#        size = "Population")+
#   theme_bw()+
#   theme(legend.position = "bottom",
#         legend.title = element_text(size = tx + 7),
#         legend.text = element_text(size = tx + 6),
#         axis.text = element_text(size = tx + 3),
#         axis.title.y = element_text(size = tx + 4), 
#         axis.title.x = element_blank(), 
#         strip.background = element_rect(fill = "transparent"),
#         strip.text = element_text(size = tx + 10)) 
# 
# ggsave("figures/pscores_boxplot_trim.pdf",
#        dpi = 600,
#        w = 20,
#        h = 8)
# 
# ggsave("figures/pscores_boxplot_trim.png",
#        dpi = 600,
#        w = 20,
#        h = 8)
# 
# 
# # cumulative trimester p-scores
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# db6 %>% 
#   ggplot(aes(trimstr, cum_pscore)) +
#   geom_boxplot(aes(group = trimstr), outlier.shape = NA)+
#   geom_jitter(aes(trimstr, cum_pscore, 
#                   col = col_geo2, 
#                   # shape = ident,
#                   alpha = ident2, 
#                   size = pop),
#               width = 0.025, height = 0)+
#   # geom_text_repel(data = geos_labs_cum, aes(trimstr, cum_pscore, label = geo),
#   #                 size = tx / 3, 
#   #                 show.legend = FALSE,
#   #                 force = 0.1, 
#   #                 box.padding = 0.1,
#   #                 direction = "y",
#   #                 # nudge_x = 0.1,
#   #                 hjust = -0.15)+
#   geom_text(data = geos_labs_cum, 
#             aes(trimstr, cum_pscore, label = geo),
#             size = tx / 2, 
#                   # show.legend = FALSE,
#                   # force = 0.1, 
#                   # box.padding = 0.1,
#                   # direction = "y",
#                   # nudge_x = 0.1,
#             check_overlap = TRUE, 
#             hjust = -0.15)+
#   scale_y_log10(labels = function(x) paste0((x - 1) * 100, "%"), 
#                 breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 5))+
#   scale_color_manual(values = cols, breaks = c("Lowest p-score", 
#                                                "Highest p-score"))+
#   scale_alpha_manual(values = c(0.8, 0.2), guide = "none")+
#   scale_size_continuous(breaks = c(100000, 500000, 1000000, 
#                                    5000000, 10000000, 40000000),
#                         labels = c("100K", "500K", "1M", "5M", "10M", "40M"))+
#   guides(color = guide_legend(order = 1,
#                               nrow = 1, byrow = TRUE,
#                               override.aes = list(size = 3)),
#          size = guide_legend(nrow = 1))+
#   facet_wrap(~ country)+
#   geom_hline(yintercept = 1, linetype = "dashed")+
#   labs(y = "Cumulative p-score", 
#        x = "Trimester",
#        col = "Extreme values",
#        size = "Population")+
#   theme_bw()+
#   theme(legend.position = "bottom",
#         legend.title = element_text(size = tx + 3),
#         legend.text = element_text(size = tx + 2),
#         axis.text = element_text(size = tx + 2),
#         axis.title = element_text(size = tx + 3), 
#         strip.background = element_rect(fill = "transparent"),
#         strip.text = element_text(size = tx + 4)) 
# 
# ggsave("figures/cum_pscores_boxplot_trim.pdf",
#        dpi = 600,
#        w = 20,
#        h = 8)
# 
# ggsave("figures/cum_pscores_boxplot_trim.png",
#        dpi = 600,
#        w = 20,
#        h = 8)
# 
# 
# # cumulative average pscores
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# db6 %>% 
#   ggplot(aes(trimstr, cum_avg_pscore)) +
#   geom_boxplot(aes(group = trimstr), outlier.shape = NA)+
#   geom_jitter(aes(trimstr, cum_avg_pscore, 
#                   col = col_geo3, 
#                   # shape = ident,
#                   alpha = ident3, 
#                   size = pop),
#               width = 0.025, height = 0)+
#   # geom_text_repel(data = geos_labs_avg, 
#   #                 aes(trimstr, cum_avg_pscore, label = geo),
#   #                 size = tx / 3, 
#   #                 show.legend = FALSE,
#   #                 force = 0.1, 
#   #                 box.padding = 0.1,
#   #                 direction = "y",
#   #                 # nudge_x = 0.1,
#   #                 hjust = -0.15)+
#   geom_text(data = geos_labs_avg, 
#             aes(trimstr, cum_avg_pscore, label = geo),
#             size = tx / 3, 
#             # show.legend = FALSE,
#             # force = 0.1, 
#             # box.padding = 0.1,
#             # direction = "y",
#             # nudge_x = 0.1,
#             check_overlap = TRUE, 
#             hjust = -0.15)+
#   scale_y_log10(labels = function(x) paste0((x - 1) * 100, "%"), 
#                 breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 5))+
#   scale_color_manual(values = cols, breaks = c("Lowest p-score", 
#                                                "Highest p-score"))+
#   scale_alpha_manual(values = c(0.8, 0.2), guide = "none")+
#   scale_size_continuous(breaks = c(100000, 500000, 1000000, 
#                                    5000000, 10000000, 40000000),
#                         labels = c("100K", "500K", "1M", "5M", "10M", "40M"))+
#   guides(color = guide_legend(order = 1,
#                               nrow = 1, byrow = TRUE,
#                               override.aes = list(size = 3)),
#          size = guide_legend(nrow = 1))+
#   facet_wrap(~ country)+
#   geom_hline(yintercept = 1, linetype = "dashed")+
#   labs(y = "Average p-score", 
#        x = "Trimester",
#        col = "Extreme values",
#        size = "Population")+
#   theme_bw()+
#   theme(legend.position = "bottom",
#         legend.title = element_text(size = tx + 3),
#         legend.text = element_text(size = tx + 2),
#         axis.text = element_text(size = tx + 2),
#         axis.title = element_text(size = tx + 3), 
#         strip.background = element_rect(fill = "transparent"),
#         strip.text = element_text(size = tx + 4)) 
# 
# ggsave("figures/avg_pscores_boxplot_trim.pdf",
#        dpi = 600,
#        w = 20,
#        h = 8)
# 
# ggsave("figures/avg_pscores_boxplot_trim.png",
#        dpi = 600,
#        w = 20,
#        h = 8)
# 
