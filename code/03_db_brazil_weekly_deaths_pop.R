library(here)
source(here("Code", "00_functions.R"))
# Source of data
# https://opendatasus.saude.gov.br/dataset/sistema-de-informacao-sobre-mortalidade-sim-1979-a-2019
# dictionnary
# Source: https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Estrutura_SIM.pdf


# States and municipality codes
# =============================
reg_codes <- 
  read_csv("data/brazil/bra_state_codes_names.csv")

# loading deaths from microdata
# =============================

# files <- paste0(here("data", "brazil"), "/Mortalidade_Geral_", 2015:2021, ".csv")
# 
# out <- list()
# for (i in files){
#   cat(i)
#   out[[i]] <- read_delim(i, 
#                          delim = ";",
#                          col_types = cols(.default = "c")) %>%  
#     filter(TIPOBITO == "2") %>% 
#     select(date_e = DTOBITO, mun_code = CODMUNOCOR) %>% 
#     mutate(date_e = dmy(date_e),
#            mth = month(date_e),
#            year = year(date_e),
#            date = make_date(d = 15, m = mth, y = year),
#            state_num = str_sub(mun_code, 1, 2)) %>% 
#     group_by(date, state_num) %>% 
#     summarise(dts = n()) %>% 
#     ungroup()
# }
# 
# dts <- 
#   out %>% 
#   bind_rows()
# 
# # saving daily deaths by state
# write_rds(dts, "output/brazil_daily_deaths_by_state_2015_2021.rds")


# loading daily deaths by state
dts <- read_rds("output/brazil_daily_deaths_by_state_2015_2021.rds")


# reading population by state
pop <- 
  read_csv2("Data/Brazil/population/bra_population_state_2010_2025.csv",
            skip = 3) %>% 
  drop_na(RO) %>% 
  rename(year = Ano) %>% 
  gather(-year, key = state_iso, value = pop) %>% 
  mutate(date = make_date(d = 15, m = 7, y = year)) %>% 
  select(-year)

# interpolating annual to monthly population
sts <- unique(pop$state_iso)
mths <- seq(ymd("2010-01-15"), ymd("2022-12-15"), by = "month")

interpop <- function(chunk){
  xs <- chunk %>% drop_na() %>% pull(t)
  ys <- chunk %>% drop_na() %>% pull(pop)
  # smoothing using cubic splines
  ts <- chunk %>% pull(t)
  md2 <- smooth.spline(x = xs, y = ys)
  chunk %>% 
    left_join(tibble(t = ts, pop_int = predict(md2, ts)$y))
}

pop_mth <- 
  expand_grid(date = mths, state_iso = sts) %>% 
  left_join(pop) %>% 
  group_by(state_iso) %>% 
  mutate(t = 1:n()) %>% 
  do(interpop(chunk = .data)) %>% 
  select(state_iso, date, exposure = pop_int)
  
# merging deaths and population
dts_state <- 
  dts %>% 
  left_join(pop_mth) %>% 
  mutate(w = ifelse(date <= "2020-03-15", 1, 0),
         month = month(date)) %>% 
  group_by(state_iso) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  filter(date <= "2021-03-15")

# aggregation by region
dts_region <- 
  dts_state %>% 
  group_by(date, region) %>% 
  summarise(dts = sum(dts),
            exposure = sum(exposure)) %>% 
  ungroup() %>% 
  mutate(w = ifelse(date <= "2020-03-15", 1, 0),
         month = month(date)) %>% 
  group_by(region) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  filter(date <= "2021-03-15")

# =======================



# estimating mortality baseline
# ==============================
est_baseline <- function(db){
  
  base_gam <-
    gam(dts ~
          t +
          s(month, bs = 'cp') +
          offset(log(exposure)),
        weights = w,
        data = db,
        family = quasipoisson(link = "log"))
  
  resp <- predict(base_gam, newdata = db, type = "response", se.fit = TRUE)
  
  db %>% 
    mutate(bsn = resp$fit,
           # ul = bsn + 1.96 * resp$se.fit,
           # ll = bsn - 1.96 * resp$se.fit,
           p_score = dts / bsn,
           dts_r = dts / exposure,
           bsn_r = bsn / exposure) %>% 
    left_join(simul_intvals(base_gam, db, 200)) %>% 
    mutate(ll_r = ll / exposure,
           ul_r = ul / exposure)
}

# bootstrapping using Jonas' method 
simul_intvals <- function(model, db, nsim){
  # matrix model
  X_prd <- predict(model, newdata = db, type = 'lpmatrix')
  # estimated coefficients
  beta <- coef(model)
  # offsets
  offset_prd <- matrix(log(db$exposure))
  
  # applying Huber-White adjustment for robust estimators 
  # beta_sim <- MASS::mvrnorm(nsim, beta, sandwich::vcovHAC(model))
  beta_sim <- MASS::mvrnorm(nsim, coef(model), vcov(model))
  Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd%*%b + offset_prd))
  
  y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
    y <- mu <- Ey
    # NA's can't be passed to the simulation functions, so keep them out
    idx_na <- is.na(mu) 
    mu_ <- mu[!idx_na] 
    N <- length(mu_)
    phi <- summary(model)$dispersion
    # in case of under-dispersion, sample from Poisson
    if (phi < 1) { phi = 1 }
    y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
    return(y)
  })
  
  ints_simul <- 
    db %>% 
    select(date)
  
  colnames_y_sim <- paste0('deaths_sim', 1:nsim)
  
  ints_simul[,colnames_y_sim] <- y_sim
  
  ints_simul <-
    ints_simul %>%
    pivot_longer(cols = starts_with('deaths_sim'),
                 names_to = 'sim_id', values_to = 'deaths_sim') %>%
    group_by(date) %>%
    summarise(
      ll = quantile(deaths_sim, 0.05, na.rm = TRUE),
      ul = quantile(deaths_sim, 0.95, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(ints_simul)
}

state_bsln <- 
  dts_state %>% 
  group_by(state_iso) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(p_score_un = ifelse(dts > ul | dts < ll, p_score, 1))

region_bsln <- 
  dts_region %>% 
  group_by(region) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(p_score_un = ifelse(dts > ul | dts < ll, p_score, 1))

# ================


# saving estimates
# ================
write_rds(region_bsln, "Output/excess/brazil_monthly_excess_all_ages_region.rds")
write_rds(state_bsln, "Output/excess/brazil_monthly_excess_all_ages_state.rds")

region_bsln <- read_rds("Output/excess/brazil_monthly_excess_all_ages_region.rds")
state_bsln <- read_rds("Output/excess/brazil_monthly_excess_all_ages_state.rds")


# ==========

# plots
# =====

# states
# ~~~~~~~

# counts
state_bsln %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  facet_wrap(~ state_iso, scales = "free")+
  theme_bw()

# rates
state_bsln %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  ggplot()+
  geom_point(aes(date, dts_r, col = out), size = 0.5)+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  facet_wrap(region ~ state_iso, scales = "free", ncol = 7)+
  scale_color_manual(values = c("black", "#ef476f"))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 6),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 7)
  )
ggsave("Figures/excess/excess_state_all_age_deaths.png", dpi = 1000, 
       width = 8, height = 4)


# p-scores
state_bsln %>% 
  ggplot()+
  geom_point(aes(date, p_score_un), alpha = 0.5)+
  facet_wrap(region ~ state_iso, scales = "free")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()

# regions
# ~~~~~~~

# counts
region_bsln %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  facet_wrap(~ region, scales = "free")+
  theme_bw()

# rates
region_bsln %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  ggplot()+
  geom_point(aes(date, dts_r, col = out), size = 0.5)+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  facet_wrap(~ region, ncol = 5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/excess/excess_region_all_age_deaths.png", dpi = 1000, 
       width = 8, height = 2.5)

# p-scores
region_bsln %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  ggplot()+
  geom_point(aes(date, p_score, col = out), alpha = 0.5)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  scale_y_log10(breaks = c(0.9, 1, 1.2, 1.5, 1.8, 2))+
  facet_wrap( ~ region, ncol = 5)+
  geom_hline(yintercept = 1, linetype = "dashed", col = "#118ab2")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/excess/p_score_region_all_age_deaths.png", dpi = 1000, 
       width = 8, height = 2.5)


