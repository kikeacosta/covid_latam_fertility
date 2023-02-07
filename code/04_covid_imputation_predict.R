library(car); library(doBy); library(tidyverse)
##
# setwd("N:/lafecundidad/outputs/")
remove(list=ls())
tb<-readRDS("data_inter/covid_tab_all.RDS")

mis_edu<-summaryBy(
  raw_nbirth~raw_country+raw_yearbir+raw_montbir+raw_geo1nam+raw_mothag7,
  data=tb[tb$raw_edumo04=='unknown',], FUN=sum)
head(mis_edu); tail(mis_edu)

tot_edu<-summaryBy(
  raw_nbirth~raw_country+raw_yearbir+raw_montbir+raw_geo1nam+raw_mothag7,
  data=tb[tb$raw_edumo04!='unknown',], FUN=sum)
head(tot_edu); tail(tot_edu)

ident<-c('raw_country', 'raw_yearbir', 'raw_montbir', 'raw_geo1nam', 'raw_mothag7')

m0<-merge(merge(tb, mis_edu, all.x=T, by=ident), tot_edu, all.x=T, by=ident, suffixes=c('.m','.t'))
m0$ident<-droplevels(interaction(m0$raw_country, m0$raw_edumo04, m0$raw_mothag7, m0$raw_geo1nam))
m0[m0$ident==sample(levels(m0$ident), 1) & !is.na(m0$ident),]

m0$raw_contime<-as.numeric(as.factor(m0$raw_yearbir))*12+m0$raw_montbir-12
m0$raw_nbirth.sum.m[is.na(m0$raw_nbirth.sum.m)]<-0
m0$raw_ibirth<-m0$raw_nbirth + (m0$raw_nbirth/m0$raw_nbirth.sum.t * m0$raw_nbirth.sum.m)
m0$raw_tbirth<-m0$raw_nbirth + m0$raw_nbirth.sum.m
m0$raw_tbirth[m0$raw_edumo04!='0-3']<-m0$raw_nbirth[m0$raw_edumo04!='0-3']
head(m0)


dr <-
  m0 %>% 
  as_tibble() %>% 
  select(-ident, -raw_nbirth.sum.m, -raw_nbirth.sum.t, raw_contime) %>% 
  filter(!is.na(raw_geo1nam)) %>% 
  mutate(geo = paste0(raw_country,"_", raw_geo1nam)) %>% 
  select(-raw_country, -raw_geo1nam, -raw_geolev1)

geos <- unique(dr$geo)

dr2 <- 
  dr %>% 
  complete(geo = geos, 
           raw_yearbir = 2015:2021 %>% as.character(),
           raw_montbir = 1:12,
           raw_mothag7 = unique(dr$raw_mothag7),
           raw_edumo04 = unique(dr$raw_edumo04),
           fill = list(raw_nbirth = 0,
                       raw_ibirth = 0,
                       raw_tbirth = 0)) %>% 
  filter(!(raw_mothag7 == "10-14" & raw_edumo04 == "12+"),
         raw_mothag7 != "unknown",
         raw_edumo04 != "unknown") %>% 
  separate(geo, c("raw_country", "raw_geo1nam"), sep = "_")

dr2 <- droplevels(dr2)           
           
addmargins(xtabs(raw_nbirth ~ raw_mothag7 + raw_edumo04 + raw_country, data=dr2)           )
           
tmeans<-summaryBy(raw_ibirth~raw_geo1nam, data=dr2, 
                  FUN=c(min, mean, median, max))

# saveRDS(tmeans, "table_means_geo1nam.RDS")
# saveRDS(m0,"covid_master_subnational_births.RDS")


dr3 <- 
  dr2 %>% 
  mutate(raw_mothag3 = case_when(raw_mothag7 %in% c("10-14", "15-19") ~ "10-19",
                                 raw_mothag7 %in% c("20-24", "25-29") ~ "20-29",
                                 raw_mothag7 %in% c("30-34", "35-39") ~ "30-39",
                                 TRUE  ~ "40-54"),
         raw_mothag3 = factor(raw_mothag3, levels = c("10-19", "20-29", "30-39", "40-54"))) %>% 
  group_by(raw_country, raw_geo1nam, raw_yearbir, raw_montbir, raw_mothag3, raw_edumo04) %>% 
  summarise(raw_nbirth = sum(raw_nbirth),
            raw_ibirth = sum(raw_ibirth),
            raw_tbirth = sum(raw_tbirth)) %>% 
  ungroup()

test1 <- 
  dr2 %>% 
  group_by(raw_country, raw_geo1nam, raw_mothag7, raw_edumo04) %>% 
  summarise(mth_mean = mean(raw_ibirth),
            mth_medi = median(raw_ibirth)) %>% 
  ungroup()

test2 <- 
  dr3 %>% 
  group_by(raw_country, raw_geo1nam, raw_mothag3, raw_edumo04) %>% 
  summarise(mth_mean = mean(raw_ibirth),
            mth_medi = median(raw_ibirth)) %>% 
  ungroup()

geo_issues <- 
  test2 %>% 
  filter(mth_mean < 1)

dtos_issues <- 
  geo_issues %>% 
  filter(mth_mean <0.5,
         raw_mothag3 != "40-54") %>% 
  pull(raw_geo1nam) %>% 
  unique()

library(lubridate)
dr3 %>% 
  mutate(date = make_date(d = 15, m = raw_montbir, y = raw_yearbir)) %>% 
  filter(raw_country == "COL",
         raw_geo1nam %in% dtos_issues) %>% 
  ggplot()+
  geom_line(aes(date, raw_ibirth, col = raw_edumo04, linetype = raw_mothag3))+
  facet_wrap(~ raw_geo1nam)+
  coord_cartesian(ylim = c(0, 5))

saveRDS(geo_issues, "table_issues.RDS")

dr3 %>% filter(raw_country == "COL") %>% pull(raw_geo1nam) %>% unique() %>% sort

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

dr4 <- 
  dr3 %>% 
  mutate(raw_geo1nam = case_when(raw_country == "COL" & raw_geo1nam %in% reg_amazon ~ "Amazonía",
                                 raw_country == "COL" & raw_geo1nam %in% reg_orinoq ~ "Orinoquía",
                                 raw_country == "COL" & raw_geo1nam %in% reg_ejecaf ~ "Eje Cafetero",
                                 TRUE ~ raw_geo1nam)) %>% 
  group_by(raw_country, raw_geo1nam, raw_yearbir, raw_montbir, raw_mothag3, raw_edumo04) %>% 
  summarise(raw_nbirth = sum(raw_nbirth),
            raw_ibirth = sum(raw_ibirth),
            raw_tbirth = sum(raw_tbirth)) %>% 
  ungroup() %>% 
  mutate(date = make_date(d = 15, m = raw_montbir, y = raw_yearbir))
  


means <- 
  dr4 %>% 
  group_by(raw_country, raw_geo1nam, raw_mothag3, raw_edumo04) %>% 
  summarise(mth_mean = mean(raw_ibirth),
            mth_medi = median(raw_ibirth)) %>% 
  ungroup()


write_rds(dr4, "data_inter/db_monthly_bra_col_mex.RDS")






dt<-m0[m0$ident==sample(levels(m0$ident), 1) & !is.na(m0$ident),]
dt

dp<-NULL; j<-16

for(i in 1:length(levels(m0$ident))){
  
  if(j==16){
    #png(paste0("model_", round(runif(1, 100,999)), ".png"), width=2800, height=1800, res=320)
    par(mfrow=c(4,4), mar=c(2,2,2,.1))
    j<-0
  }
  
  dt<-m0[m0$ident==levels(m0$ident)[i] & !is.na(m0$ident),]
  
  if(nrow(dt)>5){
    m1<-glm(raw_nbirth~raw_montbir+raw_yearbir, weights=dt$weights, data=dt, family=quasipoisson())
    m2<-glm(raw_ibirth~raw_montbir, weights=dt$weights, data=dt, family=quasipoisson())
    #m3<-glm(raw_tbirth~raw_montbir+raw_montbir, weights=dt$weights, data=dt, family=poisson())
    
    dt$pre_nbirth<-predict(m1, newdata=dt[, 'raw_montbir','raw_yearbir'], type='response')
    dt$pre_ibirth<-predict(m2, newdata=dt[, c('raw_montbir')], type='response')
    #dt$pre_tbirth<-predict(m3, newdata=dt[, c('raw_montbir','raw_montbir')], type='response')
    
    dp<-rbind(dp, dt)
    
    if(runif(1,0,1)<.04){
      plot(dt$raw_nbirth, type='l', main=levels(m0$ident)[i], xlim=c(1,28), 
           ylim=range(c(dt$raw_nbirth, dt$pre_nbirth)))
      lines(dt$raw_montbir, dt$pre_nbirth, type='l', col=2, lty=2)
      abline(v=21, lty=2, col='grey')
      #lines(dt$pre_ibirth, type='b', pch=15)
      #lines(dt$pre_tbirth, type='b', col=2)
      j<-j+1
    }
  }
  #if(j==16)
    #dev.off()
}
dev.off()

save
