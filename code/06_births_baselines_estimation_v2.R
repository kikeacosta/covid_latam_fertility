rm(list=ls())
source("Code/00_functions.R")

# monthly births
dt <- 
  readRDS("data_inter/master_births_for_baseline_estimation.rds") 


# codes 
codes <- 
  read_csv("data_input/geo_codes_bra_col_mex.csv", 
           locale = readr::locale(encoding = "latin1")) %>% 
  rename(country = ISO_Code) %>% 
  select(country, geo,raw_geolev1) %>% 
  unique()



# ~~~~~~~~~~~~~~~~~~~~~~~~
# baseline estimation ====
# ~~~~~~~~~~~~~~~~~~~~~~~~

# quick test with total national by educational level 
test <- 
  dt %>% 
  filter(geo == "total",
         # edu == "8-11",
         age == "20-29") %>% 
  group_by(country, geo, age, edu, imp_type) %>%
  do(pred_births(chunk = .data)) %>% 
  ungroup()

test %>% 
  filter(geo == "total",
         edu != "total",
         imp_type == "i") %>% 
  ggplot()+
  geom_line(aes(date, bts, linetype = edu, group = edu), 
            col = "black")+
  geom_ribbon(aes(date, ymin = bsn_lp, ymax = bsn_up, group = edu), fill = "red", alpha = 0.2)+
  geom_line(aes(date, bsn, linetype = edu), col = "red")+
  geom_vline(xintercept = c(ymd("2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~country, scales = "free_y")+
  theme_bw()

ggsave("figures/births_monthly_baseline_national_levels_all_ages.png",
       w = 10,
       h = 5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# estimating baselines for all combinations 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 23 mins at home
# 30 mins in hydra 11
# 15 mins at the office
ct <- "BRA"
ge <- "total"

chunk <- 
  dt %>% 
  filter(country == ct,
         age == "10-19",
         edu == "0-3",
         geo == ge,
         imp_type == "i")



# pred_births <- function(chunk){
  
  step <- 
    paste(unique(chunk$country), 
          unique(chunk$geo),
          "edu",
          unique(chunk$edu), 
          "age",
          unique(chunk$age), 
          unique(chunk$imp_type),
          sep = "_")
  
  cat(paste0(step, "\n"))
  
  try(
    model1 <- 
      gam(bts ~ t + s(mth, bs = 'cp'), 
          weights = w,
          data = chunk, 
          family = "quasipoisson")
  )
  
  try(
    model2 <- 
      gam(bts ~ t, 
          weights = w,
          data = chunk, 
          family = "quasipoisson")
  )
  AIC(model1)
  AIC(model2)
  extractAIC(model1)
  extractAIC(model2)
  model1$aic
  model2$aic
  
  
  
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




















bsn <- 
  dt %>% 
  mutate(bts = bts + 1) %>% 
  group_by(country, geo, age, edu, imp_type) %>%
  do(pred_births(chunk = .data)) %>% 
  ungroup()

# saving outputs
write_rds(bsn, "data_inter/monthly_excess_births_bra_col_mex.rds")

bsn %>% 
  filter(geo == "total",
         age == "total",
         imp_type == "i") %>% 
  ggplot()+
  geom_line(aes(date, bts, linetype = edu, group = edu), 
            col = "black")+
  geom_ribbon(aes(date, ymin = bsn_lp, ymax = bsn_up, group = edu), fill = "red", alpha = 0.2)+
  geom_line(aes(date, bsn, linetype = edu), col = "red")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~country, scales = "free_y")+
  theme_bw()

ggsave("figures/births_monthly_baseline_national_levels_all_ages.png",
       w = 10,
       h = 5)


bsn %>% 
  filter(geo == "total",
         edu == "total") %>% 
  ggplot()+
  geom_line(aes(date, bts, linetype = age, group = age), 
            col = "black")+
  geom_ribbon(aes(date, ymin = bsn_lp, ymax = bsn_up, group = age), fill = "red", alpha = 0.2)+
  geom_line(aes(date, bsn, linetype = age), col = "red")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")), 
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(~country, scales = "free_y")+
  theme_bw()

ggsave("figures/births_monthly_baseline_national_levels_all_educ.png",
       w = 10,
       h = 5)

bsn %>% 
  filter(country == "MEX",
         geo == "total",
         edu != "total",
         age != "total") %>% 
  group_by(year) %>% 
  summarise(bts = sum(bts_n))

