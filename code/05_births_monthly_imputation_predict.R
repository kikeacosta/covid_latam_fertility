rm(list=ls())
library(lubridate)
library(tidyverse)
library(mgcv)
dt <- 
  readRDS("data_inter/covid_tab_all.RDS") %>% 
  as_tibble()


# grouping regions and ages together ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# regions to group together
# ~~~~~~~~~~~~~~~~~~~~~~~~~
reg_amazon <- c("Amazonas",
                "Caquetá",
                "Guainía",
                "Guaviare",
                "Putumayo",
                "Vaupés")

reg_orinoq <- c("Arauca", 
                "Casanare", 
                "Meta", 
                "Vichada")

reg_ejecaf <- c("Caldas", 
                "Risaralda", 
                "Quindío")

dt2 <- 
  dt %>% 
  as_tibble() %>% 
  select(country = raw_country, 
         geo = raw_geo1nam,
         year = raw_yearbir, 
         mth = raw_montbir,
         age = raw_mothag7, 
         edu = raw_edumo04,
         bts = raw_nbirth) %>% 
  mutate(age = case_when(age %in% c("10-14", "15-19") ~ "10-19",
                                 age %in% c("20-24", "25-29") ~ "20-29",
                                 age %in% c("30-34", "35-39") ~ "30-39",
                                 TRUE  ~ "40-54"),
         age = factor(age, levels = c("10-19", "20-29", "30-39", "40-54")),
         geo = case_when(country == "COL" & geo %in% reg_amazon ~ "Amazonía",
                                 country == "COL" & geo %in% reg_orinoq ~ "Orinoquía",
                                 country == "COL" & geo %in% reg_ejecaf ~ "Eje Cafetero",
                                 TRUE ~ geo)) %>% 
  group_by(country, geo, year, mth, age, edu) %>% 
  summarise(bts = sum(bts)) %>% 
  ungroup()

unk_mx <- 
  dt2 %>% 
  filter(is.na(geo),
         year > 2019) %>% 
  summarise(bts = sum(bts))

knn_mx <- 
  dt2 %>% 
  filter(!is.na(geo),
         year > 2019) %>% 
  summarise(bts = sum(bts))
# ~~~~~~~~~~~~~~~~~~
# imputing education
# ~~~~~~~~~~~~~~~~~~
tot_edu <-
  dt2 %>%
  group_by(country, year, mth, geo, age) %>%
  summarise(bts_tot = sum(bts, na.rm = T)) %>%
  ungroup()

mis_edu <-
  dt2 %>%
  filter(edu == "unknown") %>%
  select(everything(), -edu, bts_unk = bts)

dt3 <-
  dt2 %>%
  filter(edu != "unknown") %>%
  left_join(tot_edu) %>%
  left_join(mis_edu) %>%
  replace_na(list(bts_unk = 0)) %>%
  group_by(country, year, mth, geo, age) %>%
  # two different ways of imputation
  mutate(bts_i = bts_tot*bts/sum(bts),
         bts_t = ifelse(edu == "0-3", bts + bts_unk, bts)) %>%
  ungroup() %>% 
  select(-bts_tot, - bts_unk) %>% 
  filter(!is.na(geo))

# ~~~~~~~~~~~~~~~~~~
# imputing age
# ~~~~~~~~~~~~~~~~~~
tot_age <-
  dt3 %>%
  group_by(country, year, mth, geo, edu) %>%
  summarise(bts_i_tot = sum(bts_i, na.rm = T),
            bts_t_tot = sum(bts_t, na.rm = T)) %>%
  ungroup()

dt4 <-
  dt3 %>%
  filter(age != "unknown") %>%
  left_join(tot_age) %>%
  group_by(country, year, mth, geo, edu) %>%
  mutate(bts_i = bts_i_tot*bts_i/sum(bts_i),
         bts_t = bts_t_tot*bts_t/sum(bts_t)) %>%
  select(-bts_i_tot, -bts_t_tot)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~
# adding totals ====
# ~~~~~~~~~~~~~
# country, year, trim, geo, age, edu

dt5 <- 
  dt4 %>% 
  # adding total education
  bind_rows(
    dt4 %>% 
      group_by(country, year, mth, geo, age) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                edu = "total") %>% 
      ungroup()
  ) %>% 
  # adding total ages
  bind_rows(
    dt4 %>% 
      group_by(country, year, mth, geo, edu) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                age = "total") %>% 
      ungroup()
  ) %>% 
  # adding total country
  bind_rows(
    dt4 %>% 
      group_by(country, year, mth, age, edu) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                geo = "total") %>% 
      ungroup()
  ) %>% 
  # adding total education and age
  bind_rows(
    dt4 %>% 
      group_by(country, year, mth, geo) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                edu = "total",
                age = "total") %>% 
      ungroup()
  ) %>% 
  # adding total education and country
  bind_rows(
    dt4 %>% 
      group_by(country, year, mth, age) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                edu = "total",
                geo = "total") %>% 
      ungroup()
  ) %>% 
  # adding total age and country
  bind_rows(
    dt4 %>% 
      group_by(country, year, mth, edu) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                age = "total",
                geo = "total") %>% 
      ungroup()
  ) %>% 
  # adding total region, education, and age
  bind_rows(
    dt4 %>% 
      group_by(country, year, mth) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                geo = "total",
                edu = "total",
                age = "total") %>% 
      ungroup()
  ) %>% 
  mutate(date = make_date(d = 15, m = mth, y = year),
         edu = factor(edu, levels = c("0-3", "4-7", "8-11", "12+", "total")),
         age = factor(age, levels = c("10-19", "20-29", "30-39", "40-54", "total"))) %>% 
  rename(bts_n = bts)

unique(dt5$edu)
unique(dt5$age)
unique(dt5$geo)

# ~~~~~~~~~~~~~~~
# master database
# ~~~~~~~~~~~~~~~

dt6 <- 
  dt5 %>% 
  group_by(country, geo, age, edu) %>%
  arrange(date) %>%
  mutate(t = 1:n()) %>%
  ungroup() %>%
  mutate(w = ifelse(date < "2020-03-01", 1, 0))

# ~~~~~~~~~
# functions
# ~~~~~~~~~
# for testing the function
chunk <-
  dt4 %>%
  filter(geo == "Amazonía",
         edu == "8-11",
         age == "20-29") %>% 
  mutate(bts = bts_i)

pred_births <- function(chunk){
  
  try(
    model <- 
      gam(bts ~ t + s(mth, bs = 'cp'), 
          weights = w,
          data = chunk, 
          family = "quasipoisson")
  )
  
  test <- 
    try(
      pred <- 
        predict(model, 
                type = "response", 
                se.fit = T,
                newdata = chunk)
    )
  
  try(
    chunk2 <- 
      chunk %>% 
      mutate(bsn = pred$fit,
             bsn_lc = bsn - 1.96 * pred$se.fit,
             bsn_uc = bsn + 1.96 * pred$se.fit) %>% 
      left_join(simul_intvals_no_off(model, 
                                     model_type = "gam", 
                                     db = chunk, 
                                     nsim = 100,
                                     p = 0.95),
                by = "t")
  )
  
  if(class(test) == "try-error"){
    chunk2 <- 
      chunk %>% 
      mutate(bsn = NA,
             bsn_lc = NA,
             bsn_uc = NA,
             bsn_lp = NA,
             bsn_up = NA)
  }
  return(chunk2)
}

simul_intvals_no_off <-
  function(
    # fitted model
    model,
    # either GLM or GAM (needed for model matrix extraction step)
    model_type,
    # prediction data
    db,
    # number of iterations
    nsim,
    # prediction intervals' uncertainty level (between 0 and 1)
    p
  ){
    
    # defining upper and lower prediction quantiles
    lp <- (1 - p) / 2
    up <- 1 - lp
    
    # matrix model extraction
    if(model_type == "glm"){
      X_prd <- model.matrix(model, data = db, na.action = na.pass)
    }
    if(model_type == "gam"){
      X_prd <- predict(model, newdata = db, type = 'lpmatrix')
    }
    
    # estimated coefficients
    beta <- coef(model)
    
    # extracting variance covariance matrix
    beta_sim <- MASS::mvrnorm(nsim,
                              coef(model),
                              suppressWarnings(vcov(model)))
    
    # simulation process
    Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd %*% b))
    
    y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
      y <- mu <- Ey
      # NA's can't be passed to the simulation functions, so keep them out
      idx_na <- is.na(mu)
      mu_ <- mu[!idx_na]
      N <- length(mu_)
      phi <- suppressWarnings(summary(model)$dispersion)
      # in case of under-dispersion, sample from Poisson
      if (phi < 1) { phi = 1 }
      y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))
      return(y)
    })
    
    # from wide to tidy format
    ints_simul <-
      db %>%
      select(t)
    
    colnames_y_sim <- paste0('bsn_sim', 1:nsim)
    
    ints_simul[,colnames_y_sim] <- y_sim
    
    # prediction intervals output
    ints_simul <-
      ints_simul %>%
      pivot_longer(cols = starts_with('bsn_sim'),
                   names_to = 'sim_id', values_to = 'bsn_sim') %>%
      group_by(t) %>%
      summarise(
        bsn_lp = quantile(bsn_sim, lp, na.rm = TRUE),
        bsn_up = quantile(bsn_sim, up, na.rm = TRUE),
        .groups = 'drop'
      )
    
    return(ints_simul)
  }


# ~~~~~~~~~~~~~~~~~~~~~~~~
# baseline estimation ====
# ~~~~~~~~~~~~~~~~~~~~~~~~
test <- 
  dt6 %>% 
  filter(geo == "total",
         # edu == "8-11",
         age == "20-29") %>% 
  # using imputation i (bts_i)
  mutate(bts = bts_i) %>% 
  group_by(country, geo, age, edu) %>%
  do(pred_births(chunk = .data)) %>% 
  ungroup() %>% 
  rename(bsn_i = bsn,
         bsn_i_lc = bsn_lc,
         bsn_i_uc = bsn_uc,
         bsn_i_lp = bsn_lp,
         bsn_i_up = bsn_up) %>% 
  # using imputation t (bts_t)
  mutate(bts = bts_t) %>% 
  group_by(country, geo, edu, age) %>% 
  do(pred_births(chunk = .)) %>% 
  ungroup() %>% 
  rename(bsn_t = bsn,
         bsn_t_lc = bsn_lc,
         bsn_t_uc = bsn_uc,
         bsn_t_lp = bsn_lp,
         bsn_t_up = bsn_up) %>% 
  # no imputation, ignoring missing values (bts_n)
  mutate(bts = bts_n) %>% 
  group_by(country, geo, edu, age) %>% 
  do(pred_births(chunk = .)) %>% 
  ungroup() %>% 
  rename(bsn_n = bsn,
         bsn_n_lc = bsn_lc,
         bsn_n_uc = bsn_uc,
         bsn_n_lp = bsn_lp,
         bsn_m_up = bsn_up) %>% 
  select(-bts)

test %>% 
  filter(geo == "total",
         edu != "total") %>% 
  ggplot()+
  geom_line(aes(date, bts_i, linetype = edu, group = edu), 
            col = "black")+
  geom_ribbon(aes(date, ymin = bsn_i_lp, ymax = bsn_i_up, group = edu), fill = "red", alpha = 0.2)+
  geom_line(aes(date, bsn_i, linetype = edu), col = "red")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~country, scales = "free_y")+
  theme_bw()

ggsave("figures/births_monthly_baseline_national_levels_all_ages.png",
       w = 10,
       h = 5)

dt7 <- 
  dt6 %>% 
  # using imputation i (bts_i)
  mutate(bts = bts_i) %>% 
  group_by(country, geo, age, edu) %>%
  do(pred_births(chunk = .data)) %>% 
  ungroup() %>% 
  rename(bsn_i = bsn,
         bsn_i_lc = bsn_lc,
         bsn_i_uc = bsn_uc,
         bsn_i_lp = bsn_lp,
         bsn_i_up = bsn_up) %>% 
  # using imputation t (bts_t)
  mutate(bts = bts_t) %>% 
  group_by(country, geo, edu, age) %>% 
  do(pred_births(chunk = .)) %>% 
  ungroup() %>% 
  rename(bsn_t = bsn,
         bsn_t_lc = bsn_lc,
         bsn_t_uc = bsn_uc,
         bsn_t_lp = bsn_lp,
         bsn_t_up = bsn_up) %>% 
  # no imputation, ignoring missing values (bts_n)
  mutate(bts = bts_n) %>% 
  group_by(country, geo, edu, age) %>% 
  do(pred_births(chunk = .)) %>% 
  ungroup() %>% 
  rename(bsn_n = bsn,
         bsn_n_lc = bsn_lc,
         bsn_n_uc = bsn_uc,
         bsn_n_lp = bsn_lp,
         bsn_m_up = bsn_up) %>% 
  select(-bts)

# saving outputs
write_rds(dt7, "data_inter/db_monthly_excess_births_bra_col_mex.rds")

dt7 %>% 
  filter(geo == "total",
         age == "total") %>% 
  ggplot()+
  geom_line(aes(date, bts_i, linetype = edu, group = edu), 
            col = "black")+
  geom_ribbon(aes(date, ymin = bsn_i_lp, ymax = bsn_i_up, group = edu), fill = "red", alpha = 0.2)+
  geom_line(aes(date, bsn_i, linetype = edu), col = "red")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~country, scales = "free_y")+
  theme_bw()

ggsave("figures/births_monthly_baseline_national_levels_all_ages.png",
       w = 10,
       h = 5)


dt7 %>% 
  filter(geo == "total",
         edu == "total") %>% 
  ggplot()+
  geom_line(aes(date, bts_i, linetype = age, group = age), 
            col = "black")+
  geom_ribbon(aes(date, ymin = bsn_i_lp, ymax = bsn_i_up, group = age), fill = "red", alpha = 0.2)+
  geom_line(aes(date, bsn_i, linetype = age), col = "red")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~country, scales = "free_y")+
  theme_bw()

ggsave("figures/births_monthly_baseline_national_levels_all_educ.png",
       w = 10,
       h = 5)

dt6 %>% 
  filter(country == "MEX",
         geo == "total",
         edu != "total",
         age != "total") %>% 
  group_by(year) %>% 
  summarise(bts = sum(bts_n))

