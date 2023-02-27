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
  select(country, geo, raw_geolev1) %>% 
  unique()



# ~~~~~~~~~~~~~~~~~~~~~~~~
# baseline estimation ====
# ~~~~~~~~~~~~~~~~~~~~~~~~

# test function for 2-step estimation process
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test2step <- function(ct = "COL", 
                      ge = "Choco", 
                      ed = "total", 
                      ag = "total",
                      imp = "i",
                      plot = "n"){
  
  test_2step1 <- 
    dt %>% 
    mutate(bts = bts + 1) %>% 
    filter(country %in% ct,
           geo %in% ge,
           edu %in% ed,
           age %in% ag,
           imp_type == imp) %>% 
    group_by(country, geo, age, edu, imp_type) %>%
    do(pred_births(chunk = .data, ns = 1000)) %>% 
    ungroup() 
  
  test_2step2 <- 
    test_2step1 %>% 
    rename(old_bsn = bsn,
           old_bsn_up = bsn_up1,
           old_bsn_lp = bsn_lp1) %>% 
    mutate(w = ifelse(w == 0 | bts > old_bsn_up, 0, 1)) %>% 
    select(-starts_with("bsn")) %>% 
    group_by(country, geo, age, edu, imp_type) %>%
    do(pred_births(chunk = .data, ns = 1000)) %>% 
    ungroup() %>% 
    mutate(in_fitt = ifelse(w == 0, "exc", "inc"))
  
  if(plot == "y"){
    test_2step2 %>% 
      ggplot()+
      geom_point(aes(date, bts, shape = in_fitt), col = "black", size = 1)+
      geom_line(aes(date, bts), col = "black", alpha = 0.1, linewidth = 0.3)+
      geom_ribbon(aes(date, ymin = old_bsn_lp, ymax = old_bsn_up), fill = "grey80", alpha = 0.2)+
      geom_line(aes(date, old_bsn_up), col = "black", linetype = "dashed", alpha = 0.5, linewidth = 0.1)+
      geom_line(aes(date, old_bsn_lp), col = "black", linetype = "dashed", alpha = 0.5, linewidth = 0.1)+
      geom_line(aes(date, old_bsn), col = "black", linetype = "dashed", alpha = 0.5, linewidth = 0.3)+
      geom_ribbon(aes(date, ymin = bsn_lc, ymax = bsn_uc), fill = "#f72585", alpha = 0.2)+
      geom_ribbon(aes(date, ymin = bsn_lp1, ymax = bsn_up1), fill = "#d00000", alpha = 0.3)+
      geom_ribbon(aes(date, ymin = bsn_lp3, ymax = bsn_up3), fill = "#d00000", alpha = 0.1)+
      geom_ribbon(aes(date, ymin = bsn_lp5, ymax = bsn_up5), fill = "#d00000", alpha = 0.08)+
      geom_line(aes(date, bsn), col = "#780000", linewidth = 0.3)+
      geom_line(aes(date, bsn_lp1), col = "black", linetype = "dotted", alpha = 0.5, linewidth = 0.1)+
      geom_line(aes(date, bsn_up1), col = "black", linetype = "dotted", alpha = 0.5, linewidth = 0.1)+
      geom_vline(xintercept = c(ymd("2019-12-31")), linetype = "dashed")+
      scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
                   date_labels = "%Y")+
      scale_shape_manual(values = c(1, 16))+
      facet_wrap(age~edu, scales = "free_y")+
      labs(title = paste0("second-step fitting, test ", ge))+
      theme_bw()
    
    ggsave(paste0("figures/test_two_step_births_modelling_out_50_", ge, ".pdf"),
           w = 30, h = 10)
  }
  return(test_2step2)
}

unique(dt$age)
unique(dt$edu)

test2step(ct = "COL", ge = "Choco", 
          ed = c("0-7" , "8-11", "12+"), 
          ag = c("10-19", "20-29", "30-39", "40-54"),
          imp = "i", plot = "y")

test2step(ct = "COL", ge = "Guajira", 
          ed = c("0-7" , "8-11", "12+"), 
          ag = c("10-19", "20-29", "30-39", "40-54"),
          imp = "i", plot = "y")

test2step(ct = "COL", ge = "Atlantico", 
          ed = c("0-7" , "8-11", "12+"), 
          ag = c("10-19", "20-29", "30-39", "40-54"),
          imp = "i", plot = "y")

test2step(ct = "COL", ge = "Cesar", 
          ed = c("0-7" , "8-11", "12+"), 
          ag = c("10-19", "20-29", "30-39", "40-54"),
          imp = "i", plot = "y")

test2step(ct = "MEX", ge = "Nayarit", 
          ed = c("0-7" , "8-11", "12+"), 
          ag = c("10-19", "20-29", "30-39", "40-54"),
          imp = "i", plot = "y")

test2step(ct = "MEX", ge = "Durango", 
          ed = c("0-7" , "8-11", "12+"), 
          ag = c("10-19", "20-29", "30-39", "40-54"),
          imp = "i", plot = "y")


# plot for national levels and all ages by education
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_geos_ages <- 
  test2step(ct = c("COL", "MEX", "BRA"), 
            ge = "total", 
            ed = c("0-7" , "8-11", "12+"), 
            ag = c("total"),
            imp = "i", plot = "n")

all_geos_ages2 <- 
  all_geos_ages %>% 
  gather(bts, old_bsn, starts_with("bsn"), key = estimate, value = bts) %>%
  mutate(bts = ifelse(bts >= 1, bts -1, 0)) %>% 
  spread(estimate, bts)


all_geos_ages2 %>%
  mutate(country = case_when(country == "BRA" ~ "Brazil",
                             country == "COL" ~ "Colombia",
                             country == "MEX" ~ "Mexico")) %>% 
  ggplot(aes(group = edu))+
  geom_line(aes(date, bts, col = edu, group = edu), alpha = .6)+
  geom_point(aes(date, bts, col = edu, group = edu), size = 0.6, alpha = .8)+
  geom_ribbon(aes(date, ymin = bsn_lp1, ymax = bsn_up1, group = edu), fill = "#ef233c", alpha = 0.3)+
  # geom_ribbon(aes(date, ymin = bsn_lp3, ymax = bsn_up3, group = edu), fill = "red", alpha = 0.3)+
  # geom_ribbon(aes(date, ymin = bsn_lp5, ymax = bsn_up5, group = edu), fill = "red", alpha = 0.2)+
  # geom_ribbon(aes(date, ymin = bsn_lc, ymax = bsn_uc, group = edu), fill = "red", alpha = 0.2)+
  geom_line(aes(date, bsn), col = "#ef233c")+
  geom_vline(xintercept = c(ymd("2015-01-01", "2019-12-31")),
             linetype = "dashed")+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  scale_color_manual(values = c("black", "grey60", "grey"))+
  facet_wrap(~country, scales = "free_y")+
  labs(y = "Births", x = "Months", col = "Education")+
  theme_bw()+
  theme()

ggsave("figures/births_monthly_baseline_national_levels_all_ages1.png",
       w = 10,
       h = 5)



# estimating baselines for all combinations  ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# function for 2-step estimation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fitting2step <- 
  function(chunk = .data, ns = 200){
  
  fitting2step1 <- 
    chunk %>% 
    group_by(country, geo, age, edu, imp_type) %>%
    do(pred_births(chunk = .data, ns = ns)) %>% 
    ungroup() 
  
  fitting2step2 <- 
    fitting2step1 %>% 
    mutate(w = ifelse(w == 0 | bts > bsn_up1, 0, 1)) %>% 
    rename(old_bsn = bsn) %>% 
    select(-starts_with("bsn")) %>% 
    group_by(country, geo, age, edu, imp_type) %>%
    do(pred_births(chunk = .data, ns = ns)) %>% 
    ungroup() %>% 
    mutate(in_fitt = ifelse(w == 0, "exc", "inc"))
  
  return(fitting2step2)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~
# fitting the two-step ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~
bsn_2stp <- 
  dt %>% 
  # adjusting births and all estimates by increasing them in 1 unit, 
  # adjusted to actual levels after fitting the baseline 
  mutate(bts = bts + 1) %>% 
  group_by(country, geo, age, edu, imp_type) %>%
  do(fitting2step(chunk = .data, ns = 200)) %>% 
  ungroup()

bsn_out <- 
  bsn_2stp %>% 
  # adjusting births and all estimates by reducing them in 1 unit
  gather(bts, old_bsn, starts_with("bsn"), key = estimate, value = bts) %>%
  mutate(bts = ifelse(bts >= 1, bts -1, 0)) %>% 
  spread(estimate, bts)
  
# saving outputs ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_rds(bsn_out, "data_inter/monthly_excess_births_bra_col_mex_edu03.rds")

bsn_out %>%
  ggplot()+
  geom_point(aes(date, bts, shape = in_fitt), col = "black", size = 1)+
  geom_line(aes(date, bts), col = "black", alpha = 0.1, linewidth = 0.3)+
  geom_line(aes(date, old_bsn), col = "black", linetype = "dashed", alpha = 0.5, linewidth = 0.3)+
  geom_ribbon(aes(date, ymin = bsn_lc, ymax = bsn_uc), fill = "#f72585", alpha = 0.3)+
  geom_ribbon(aes(date, ymin = bsn_lp1, ymax = bsn_up1), fill = "red", alpha = 0.3)+
  geom_ribbon(aes(date, ymin = bsn_lp3, ymax = bsn_up3), fill = "red", alpha = 0.2)+
  geom_ribbon(aes(date, ymin = bsn_lp5, ymax = bsn_up5), fill = "red", alpha = 0.1)+
  geom_line(aes(date, bsn), col = "#780000", linewidth = 0.3)+
  geom_line(aes(date, bsn_lc), col = "black", linetype = "dotted", alpha = 0.5, linewidth = 0.1)+
  geom_line(aes(date, bsn_uc), col = "black", linetype = "dotted", alpha = 0.5, linewidth = 0.1)+
  geom_vline(xintercept = c(ymd("2019-12-31")), linetype = "dashed")+
  scale_shape_manual(values = c(1, 16))+
  scale_x_date(breaks = seq(ymd('2010-01-01'),ymd('2022-01-01'), by = '1 year'),
               date_labels = "%Y")+
  facet_wrap(edu~age, scales = "free_y")+
  theme_bw()

