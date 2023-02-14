library(car); library(doBy)
##
# setwd("N:/lafecundidad/outputs/")
remove(list=ls())
tb<-readRDS("data_inter/covid_tab_all.RDS")

mis_edu<-summaryBy(
  raw_nbirth~raw_country+raw_yearbir+raw_trimest+raw_trimesc+raw_geolev1+raw_mothag6,
  data=tb[tb$raw_edumo04=='unknown',], FUN=sum)
head(mis_edu); tail(mis_edu)

tot_edu<-summaryBy(
  raw_nbirth~raw_country+raw_yearbir+raw_trimest+raw_trimesc+raw_geolev1+raw_mothag6,
  data=tb[tb$raw_edumo04!='unknown',], FUN=sum)
head(tot_edu); tail(tot_edu)

ident<-c('raw_country', 'raw_yearbir', 'raw_trimest', 'raw_trimesc', 'raw_geolev1', 'raw_mothag6')

m0<-merge(merge(tb, mis_edu, all.x=T, by=ident), tot_edu, all.x=T, by=ident, suffixes=c('.m','.t'))
m0$raw_nbirt 
m0$raw_tbirth[m0$raw_edumo04!='0-3']<-m0$raw_nbirth[m0$raw_edumo04!='0-3']

apply(m0[, grep('birth', colnames(m0))], 2, sum, na.rm=T)

m0$ident<-interaction(m0$raw_country, m0$raw_edumo04, m0$raw_mothag6, m0$raw_geolev1)
m0$ident[m0$raw_edumo04=='unknown' | m0$raw_mothag6=='unknown']<-NA

m0$ident<-droplevels(m0$ident)

m0$weights<-recode(m0$raw_yearbir, "2015:2019=1; else=0")
head(m0); tail(m0)
getwd()
saveRDS(m0,"covid_master_subnational_births.RDS")

library(tidyverse)
dt <- read_rds("data_inter/covid_master_subnational_births.RDS")


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
    m1<-glm(raw_nbirth~raw_trimesc+raw_trimest, weights=dt$weights, data=dt, family=quasipoisson())
    m2<-glm(raw_ibirth~raw_trimesc+raw_trimest, weights=dt$weights, data=dt, family=quasipoisson())
    #m3<-glm(raw_tbirth~raw_trimesc+raw_trimest, weights=dt$weights, data=dt, family=poisson())
    
    dt$pre_nbirth<-predict(m1, newdata=dt[, c('raw_trimesc','raw_trimest')], type='response')
    dt$pre_ibirth<-predict(m2, newdata=dt[, c('raw_trimesc','raw_trimest')], type='response')
    #dt$pre_tbirth<-predict(m3, newdata=dt[, c('raw_trimesc','raw_trimest')], type='response')
    
    dp<-rbind(dp, dt)
    
    if(runif(1,0,1)<.04){
      plot(dt$raw_nbirth, type='l', main=levels(m0$ident)[i], xlim=c(1,28), 
           ylim=range(c(dt$raw_nbirth, dt$pre_nbirth)))
      lines(dt$raw_trimesc, dt$pre_nbirth, type='l', col=2, lty=2)
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
