library(tidyverse)
library(lubridate)
dt <- read_rds("data_inter/covid_master_subnational_births.RDS")


# dt<-m0[m0$ident==sample(levels(m0$ident), 1) & !is.na(m0$ident),]
dt

dt2 <- 
  dt %>% 
  as_tibble() %>% 
  select(country = raw_country, 
         year = raw_yearbir, 
         trim = raw_trimest,
         geo = raw_geolev1,
         age = raw_mothag6, 
         edu = raw_edumo04,
         bts = raw_nbirth,
         ident
         ) %>% 
  # creating date variable
  mutate(mth = case_when(trim == "First" ~ 2,
                         trim == "Second" ~ 5,
                         trim == "Third" ~ 8,
                         trim == "Fourth" ~ 11),
         date = make_date(d = 15, m = mth, y = year)) %>% 
  select(-mth)

unique(dt2$age)
unique(dt2$edu)

dt2 %>% 
  filter(age == "unknown") %>% 
  summarise(bts = sum(bts))

dt2 %>% 
  filter(edu == "unknown") %>% 
  summarise(bts = sum(bts))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# imputations of missing age and education ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~
# imputing education
# ~~~~~~~~~~~~~~~~~~
tot_edu <- 
  dt2 %>% 
  group_by(country, year, trim, geo, age) %>% 
  summarise(bts_tot = sum(bts, na.rm = T)) %>% 
  ungroup()

mis_edu <- 
  dt2 %>% 
  filter(edu == "unknown") %>% 
  select(everything(), -edu, -ident, bts_unk = bts)
  
dt3 <- 
  dt2 %>%
  filter(edu != "unknown") %>% 
  left_join(tot_edu) %>% 
  left_join(mis_edu) %>% 
  replace_na(list(bts_unk = 0)) %>% 
  group_by(country, year, trim, geo, age) %>% 
  # two different ways of imputation
  mutate(bts_i = bts_tot*bts/sum(bts),
         bts_t = ifelse(edu == "0-3", bts + bts_unk, bts)) %>% 
  ungroup()

dt4 <- 
  dt3 %>% 
  select(-bts_tot, - bts_unk)

# ~~~~~~~~~~~~~~~~~~
# imputing age
# ~~~~~~~~~~~~~~~~~~
tot_age <- 
  dt4 %>% 
  group_by(country, year, trim, geo, edu) %>% 
  summarise(bts_i_tot = sum(bts_i, na.rm = T),
            bts_t_tot = sum(bts_t, na.rm = T)) %>% 
  ungroup()

dt5 <- 
  dt4 %>%
  filter(age != "unknown") %>% 
  left_join(tot_age) %>% 
  group_by(country, year, trim, geo, edu) %>% 
  mutate(bts_i = bts_i_tot*bts_i/sum(bts_i),
         bts_t = bts_t_tot*bts_t/sum(bts_t)) %>% 
  select(-bts_i_tot, -bts_t_tot)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~
# adding totals ====
# ~~~~~~~~~~~~~
# country, year, trim, geo, age, edu

dt6 <- 
  dt5 %>% 
  # adding total education
  bind_rows(
    dt5 %>% 
      group_by(country, year, trim, geo, age) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                edu = "total") %>% 
      ungroup()
  ) %>% 
  # adding total ages
  bind_rows(
    dt5 %>% 
      group_by(country, year, trim, geo, edu) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                age = "total") %>% 
      ungroup()
  ) %>% 
  # adding total country
  bind_rows(
    dt5 %>% 
      group_by(country, year, trim, age, edu) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                geo = "total") %>% 
      ungroup()
  ) %>% 
  # adding total education and age
  bind_rows(
    dt5 %>% 
      group_by(country, year, trim, geo) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                edu = "total",
                age = "total") %>% 
      ungroup()
  ) %>% 
  # adding total region, education, and age
  bind_rows(
    dt5 %>% 
      group_by(country, year, trim) %>% 
      summarise(bts = sum(bts),
                bts_i = sum(bts_i),
                bts_t = sum(bts_t),
                geo = "total",
                edu = "total",
                age = "total") %>% 
      ungroup()
  )

unique(dt5$trim)

dt7 <- 
  dt6 %>% 
  mutate(mth = case_when(trim == "First" ~ 2,
                         trim == "Second" ~ 5,
                         trim == "Third" ~ 8,
                         trim == "Fourth" ~ 11),
         date = make_date(d = 15, m = mth, y = year)) %>% 
  select(-mth) %>% 
  ungroup()
    

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# evaluation of data for each possible combination
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# country, region, education, age

# total and average births by combination
temp1 <- 
  dt7 %>% 
  filter(date < "2020-03-15") %>% 
  mutate(ctr_geo = paste0(country, "_", geo)) %>% 
  select(ctr_geo, date, age, edu, bts)
  
# amount of observations for each combinations
comb_n <- 
  temp1 %>% 
  group_by(ctr_geo, age, edu) %>% 
  summarise(n = n()) 

# we have to fill with zeros for computing the average birth counts by trimester 
tot_avs <- 
  temp1 %>% 
  complete(ctr_geo, date, age, edu, fill = list(bts = 0)) %>% 
  group_by(ctr_geo, age, edu) %>% 
  summarise(bts_tot = sum(bts),
            bts_avg = mean(bts)) %>% 
  ungroup() %>% 
  left_join(comb_n)

# what about a criteria of minimum 1 births on average by month (3 by trimester)?
# combinations that would be excluded
exclude <- 
  tot_avs %>% 
  filter(bts_avg < 3) %>% 
  separate(ctr_geo, c("country", "geo")) %>%  
  select(-bts_tot, -bts_avg, -n)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~
# master database
# ~~~~~~~~~~~~~~~

dt8 <- 
  dt7 %>% 
  mutate(ctr_geo = paste0(country, "_", geo)) %>% 
  select(ctr_geo, date, age, edu, bts, bts_i, bts_t) %>% 
  # filling with zeros for missing periods
  complete(ctr_geo, date, age, edu, fill = list(bts = 0,
                                                bts_i = 0,
                                                bts_t = 0)) %>% 
  separate(ctr_geo, c("country", "geo")) %>% 
  # excluding combinations with insufficient data (1 monthly birth on average)
  anti_join(exclude) %>% 
  group_by(country, geo, age, edu) %>%
  arrange(date) %>%
  mutate(t = 1:n()) %>%
  ungroup() %>%
  mutate(w = ifelse(date < "2020-03-01", 1, 0),
         trim = case_when(month(date) == 2 ~ "First",
                          month(date) == 5 ~ "Second",
                          month(date) == 8 ~ "Third",
                          month(date) == 11 ~ "Fourth"))

test <- 
  dt8 %>% 
  group_by(country, geo, edu, age) %>% 
  summarise(n = n(),
            bts = sum(bts_i)) %>% 
  ungroup()


# ~~~~~~~~~
# functions
# ~~~~~~~~~
pred_births <- function(chunk){
  
  model <- 
    glm(bts_b ~ t + trim, 
        weights = w,
        data = chunk, 
        family = quasipoisson(link = "log"))  
  
  pred <- predict(model, 
                  type = "response", 
                  se.fit = T,
                  newdata = chunk)
  chunk %>% 
    mutate(bsn = pred$fit,
           bsn_ll = bsn - 1.96*pred$se.fit,
           bsn_ul = bsn + 1.96*pred$se.fit) %>% 
    left_join(simul_intvals_no_off(model,
                                   model_type = "glm",
                                   db = chunk,
                                   nsim = 500,
                                   p = 0.95),
              by = "t")
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


# ~~~~~~~~~~~~~~~~~~~
# baseline estimation
# ~~~~~~~~~~~~~~~~~~~
dt9 <- 
  dt8 %>% 
  # using imputation i
  mutate(bts_b = bts_i) %>% 
  group_by(country, geo, edu, age) %>% 
  do(pred_births(chunk = .)) %>% 
  ungroup() %>% 
  rename(bsn_i = bsn,
         bsn_i_ll = bsn_ll,
         bsn_i_ul = bsn_ul,
         bsn_i_lp = bsn_lp,
         bsn_i_up = bsn_up) %>% 
  # using imputation t
  mutate(bts_b = bts_t) %>% 
  group_by(country, geo, edu, age) %>% 
  do(pred_births(chunk = .)) %>% 
  ungroup() %>% 
  rename(bsn_t = bsn,
         bsn_t_ll = bsn_ll,
         bsn_t_ul = bsn_ul,
         bsn_t_lp = bsn_lp,
         bsn_t_up = bsn_up) %>% 
  # no imputation, ignoring missing values
  mutate(bts_b = bts) %>% 
  group_by(country, geo, edu, age) %>% 
  do(pred_births(chunk = .)) %>% 
  ungroup() %>% 
  rename(bsn_r = bsn,
         bsn_r_ll = bsn_ll,
         bsn_r_ul = bsn_ul,
         bsn_r_lp = bsn_lp,
         bsn_r_up = bsn_up) %>% 
  select(-bts_b)

# saving outputs
write_rds(dt9, "data_inter/db_trimester_bra_col_ea2.RDS")

# loading outputs
dt9 <- read_rds("data_inter/db_trimester_bra_col_ea2.RDS")


out %>% 
  filter(Region == "All",
         raw_mothag6 == "All") %>% 
  ggplot()+
  geom_line(aes(date, raw_nbirth.current, linetype = raw_edumo04, group = raw_edumo04), 
            col = "black")+
  geom_ribbon(aes(date, ymin = pred_glm_ll, ymax = pred_glm_ul, group = raw_edumo04), fill = "red", alpha = 0.2)+
  geom_line(aes(date, pred_glm, linetype = raw_edumo04), col = "red")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~ISO_Code, scales = "free_y")+
  theme_bw()

ggsave("figures/births_baseline_national_levels_all_ages.png",
       w = 10,
       h = 5)


# table for sample description
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test <- 
  db_out %>% 
  filter(raw_yearbir %in% 2020:2021,
         raw_mothag6 != "unknown")

avs1519 <- 
  db_out %>% 
  filter(raw_yearbir %in% 2015:2019,
         raw_mothag6 != "unknown") %>% 
  group_by(raw_country, Region) %>% 
  summarise(av_bts_1519 = round(mean(raw_nbirth.current), 1)) %>% 
  ungroup()

avs2021 <- 
  db_out %>% 
  filter(raw_yearbir %in% 2020:2021,
         raw_mothag6 != "unknown") %>% 
  group_by(raw_country, Region) %>% 
  summarise(av_bts_2021 = round(mean(raw_nbirth.current), 1)) %>% 
  ungroup()

preds <- 
  db_out %>% 
  filter(raw_yearbir %in% 2020:2021,
         raw_mothag6 != "unknown") %>% 
  group_by(raw_country, Region) %>% 
  summarise(av_prd = round(mean(pred_glm), 1))

pscs <- 
  db_out %>% 
  filter(raw_yearbir %in% 2020:2021,
         raw_mothag6 != "unknown") %>% 
  mutate(psc = raw_nbirth.current / pred_glm) %>% 
  group_by(raw_country, Region) %>% 
  summarise(av_psc = round(mean(psc), 2))

tb1 <- 
  avs1519 %>% 
  left_join(preds) %>% 
  left_join(avs2021) %>% 
  left_join(pscs) %>% 
  mutate(test = av_bts_2021 / av_prd)


