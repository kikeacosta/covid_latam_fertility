# Description:
# Functions and preparation of environment script
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
set.seed(1234)
options(scipen=999)

# Installing missing packages
# ===========================
# install pacman to streamline further package installation
if(!require("pacman", character.only = TRUE)) {
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package pacman not found")
}
library(pacman)
# Required CRAN packages
pkgs <- c("tidyverse",
          "here",
          "lubridate",
          "mgcv",
          "ISOweek",
          "readxl",
          "ggrepel",
          "ggridges")

# Install required CRAN packages if not available yet
if(sum(!p_isinstalled(c(pkgs)))!=0) {
  for(i in 1:length(pkgs[!p_isinstalled(pkgs)])){
    p_install(
      package = pkgs[!p_isinstalled(pkgs)][i], 
      character.only = TRUE
    )
  }
}

# loading required packages
# =========================
p_load(pkgs, character.only = TRUE)

# ====
copy_this <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,
              file = paste0("clipboard-", object.size(x)),
              sep="\t",
              row.names=row.names,
              col.names=col.names,...)
}

# function for weekly population interpolation 
# ============================================
interpop <- function(db)
{
  xs <- db %>% drop_na() %>% pull(t)
  ys <- db %>% drop_na() %>% pull(pop)
  # smoothing using cubic splines
  ts <- db %>% pull(t)
  db2 <- 
    db %>% 
    mutate(pop2 = spline(xs, ys, xout = ts)$y)
  return(db2)
}
# ====

# functions for baseline mortality estimation
# ===========================================

# fitting the model
# ~~~~~~~~~~~~~~~~~
est_baseline <- 
  function(db, knots = NA){
  
  
  if(!is.na(knots)){
    gam_model <- 
      gam(dts ~ t + 
            s(week, bs = 'cp', k = knots) +
            offset(log(exposure)), 
          weights = w,
          data = db, 
          family = "quasipoisson")
  }else{
    gam_model <- 
      gam(dts ~ t + 
            s(week, bs = 'cp') +
            offset(log(exposure)), 
          weights = w,
          data = db, 
          family = "quasipoisson")
  }
  
  resp <- predict(gam_model, newdata = db, type = "response")
  
  db %>% 
    mutate(bsn = resp,
           p_score = dts / bsn,
           dts_r = dts / exposure,
           bsn_r = bsn / exposure) %>% 
    left_join(simul_intvals(gam_model, db, 1000),
              by = "date") %>% 
    mutate(ll_r = ll / exposure,
           ul_r = ul / exposure)
}


est_baseline2 <- 
  function(db, knots = NA){
    
    
    if(!is.na(knots)){
      gam_model <- 
        gam(dts ~ t + 
              s(month, bs = 'cp', k = knots) +
              offset(log(exposure)), 
            weights = w,
            data = db, 
            family = "quasipoisson")
    }else{
      gam_model <- 
        gam(dts ~ t + 
              s(month, bs = 'cp') +
              offset(log(exposure)), 
            weights = w,
            data = db, 
            family = "quasipoisson")
    }
    
    resp <- predict(gam_model, newdata = db, type = "response")
    
    db %>% 
      mutate(bsn = resp) %>% 
      left_join(simul_intvals(gam_model, db, 1000),
                by = "date") 
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

# function to estimate baseline mortality with user specifications
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
give_me_baseline <- 
  function(age_gr = 5,
           age_cl = 100,
           last_date = "2020-02-15",
           mths_exc = c(0),
           knots = 3){
    
    # preparing data to fit, aggregating age intervals, setting close age
    db_to_fit <- 
      db2 %>% 
      mutate(age_gr = age - age %% age_gr,
             age_gr = ifelse(age_gr > age_cl, age_cl, age_gr)) %>% 
      group_by(year, month, t_p, date, care, sex, age_gr) %>% 
      summarise(Dx = sum(Dx),
                Nx = sum(Nx)) %>% 
      ungroup() %>% 
      mutate(w = ifelse(date <= last_date & !month %in% mths_exc, 1, 0))
    
    # fitting baseline mortality
    db_bsn <- 
      db_to_fit %>% 
      arrange(care, sex, age_gr, date) %>% 
      group_by(care, sex, age_gr) %>% 
      do(est_baseline(db = .data, knots = knots)) %>% 
      ungroup() %>% 
      mutate(excess = ifelse(date > last_date & Dx > ul, "y", "n"))
    
    # saving estimates
    write_rds(db_bsn, here("output", paste0("baselines_age", age_gr, "_", age_cl, "plus_exc_", mths_exc, "_", knots, "knots.rds")))
    
    # plotting all estimates
    db_bsn %>% 
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
      facet_wrap(age_gr ~ care + sex, scales = "free", ncol = 6)+
      labs(title = paste0("age", age_gr, "_", age_cl, "plus_excl_", mths_exc))+
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
    ggsave(here("figures", paste0("baselines_age", age_gr, "_", age_cl, "plus_exc_", mths_exc, "_", knots, "knots.pdf")),
           width = 10,
           height = length(unique(db_to_fit$age_gr)) * 2.5,
           limitsize = FALSE)

  }


# chunk <- 
#   dt8 %>% 
#   filter(country == "COL",
#          geo == "Guainia",
#          edu == "8-11",
#          age == "40-54",
#          imp_type == "n")

# chunk <- 
#   dt %>% 
#   mutate(bts = bts + 1) %>% 
#   filter(country == "COL",
#          geo == "San Andres",
#          edu == "0-3",
#          age == "10-19",
#          imp_type == "i")

pred_births_round1 <- function(chunk, ns = 100){
  
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
  
  if(class(test) != "try-error" & model$outer.info$conv == "full convergence"){
    chunk2 <- 
      chunk %>% 
      mutate(bsn = pred$fit) %>% 
      left_join(simul_intvals_no_off(model, 
                                     model_type = "gam", 
                                     db = chunk, 
                                     nsim = ns,
                                     p = c(.5, .6, .7, .8, .9)),
                by = "t")
  }
  
  if(class(test) != "try-error" & model$outer.info$conv != "full convergence"){
    chunk2 <- 
      chunk %>% 
      mutate(bsn = pred$fit,
             bsn_lc = NA,
             bsn_uc = NA,
             bsn_lp1 = NA,
             bsn_up1 = NA,
             bsn_lp2 = NA,
             bsn_up2 = NA,
             bsn_lp3 = NA,
             bsn_up3 = NA,
             bsn_lp4 = NA,
             bsn_up4 = NA,
             bsn_lp5 = NA,
             bsn_up5 = NA)
  }
  
  if(class(test) == "try-error"){
    chunk2 <- 
      chunk %>% 
      mutate(bsn = NA,
             bsn_lc = NA,
             bsn_uc = NA,
             bsn_lp1 = NA,
             bsn_up1 = NA,
             bsn_lp2 = NA,
             bsn_up2 = NA,
             bsn_lp3 = NA,
             bsn_up3 = NA,
             bsn_lp4 = NA,
             bsn_up4 = NA,
             bsn_lp5 = NA,
             bsn_up5 = NA)
  }
  
  return(chunk2)
}


pred_births <- function(chunk, ns = 100){
  
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
  
  if(class(test) != "try-error" & model$outer.info$conv == "full convergence"){
    chunk2 <- 
      chunk %>% 
      mutate(bsn = pred$fit,
             bsn_lc = bsn - 1.96 * pred$se,
             bsn_uc = bsn + 1.96 * pred$se) %>% 
      left_join(simul_intvals_no_off(model, 
                                     model_type = "gam", 
                                     db = chunk, 
                                     nsim = ns,
                                     p = c(.5, .6, .7, .8, .9)),
                by = "t")
  }
 
  if(class(test) != "try-error" & model$outer.info$conv != "full convergence"){
    chunk2 <- 
      chunk %>% 
      mutate(bsn = pred$fit,
             bsn_lc = NA,
             bsn_uc = NA,
             bsn_lp1 = NA,
             bsn_up1 = NA,
             bsn_lp2 = NA,
             bsn_up2 = NA,
             bsn_lp3 = NA,
             bsn_up3 = NA,
             bsn_lp4 = NA,
             bsn_up4 = NA,
             bsn_lp5 = NA,
             bsn_up5 = NA)
  }
  
  if(class(test) == "try-error" | !exists("model") | !exists("pred")){
    chunk2 <- 
      chunk %>% 
      mutate(bsn = NA,
             bsn_lc = NA,
             bsn_uc = NA,
             bsn_lp1 = NA,
             bsn_up1 = NA,
             bsn_lp2 = NA,
             bsn_up2 = NA,
             bsn_lp3 = NA,
             bsn_up3 = NA,
             bsn_lp4 = NA,
             bsn_up4 = NA,
             bsn_lp5 = NA,
             bsn_up5 = NA)
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
        bsn_lp1 = quantile(bsn_sim, lp[1], na.rm = TRUE),
        bsn_up1 = quantile(bsn_sim, up[1], na.rm = TRUE),
        bsn_lp2 = quantile(bsn_sim, lp[2], na.rm = TRUE),
        bsn_up2 = quantile(bsn_sim, up[2], na.rm = TRUE),
        bsn_lp3 = quantile(bsn_sim, lp[3], na.rm = TRUE),
        bsn_up3 = quantile(bsn_sim, up[3], na.rm = TRUE),
        bsn_lp4 = quantile(bsn_sim, lp[4], na.rm = TRUE),
        bsn_up4 = quantile(bsn_sim, up[4], na.rm = TRUE),
        bsn_lp5 = quantile(bsn_sim, lp[5], na.rm = TRUE),
        bsn_up5 = quantile(bsn_sim, up[5], na.rm = TRUE),
        .groups = 'drop'
      )
    
    return(ints_simul)
  }
