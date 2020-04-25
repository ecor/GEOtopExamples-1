##
## Emanuele Cordano 
##
## 2020 04 25 
## It retrieves wether data from RDA archive dataset 
##
rm(list=ls())

#####library(geotopbricks)
library(stringr)
library(purrr)
library(dplyr)
library(magrittr)
library(zoo)
###library(plyr)

###
##wpath <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/sims' 
wpath <- '/home/ecor/activity/2020/geotop_examples/GEOtopExamples-1/tests/ColdelaPorte/sims/'
wpaths <- wpath %>% list.files(pattern="sim",full.name=TRUE)
names(wpaths) <- wpath %>% list.files(pattern="sim",full.name=FALSE)
meteo_orig <- geotopbricks::get.geotop.inpts.keyword.value("MeteoFile",wpath=wpaths[1],data.frame=TRUE)
###

### OBSERVATION RDA FILE

##cdp_obs_rda <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/rda/cdp_obs.rda'
cdp_obs_rda <- '/home/ecor/activity/2020/geotop_examples/GEOtopExamples-1/tests/ColdelaPorte/obss/rda/cdp_obs.rda'

load(cdp_obs_rda)
#unique(cdp_obs$source)
#[1] CDP_met_insitu.dat CDP_met_safran.dat CDP_hor_eval.dat   CDP_daily_eval.dat
#Levels: CDP_met_insitu.dat CDP_met_safran.dat CDP_hor_eval.dat CDP_daily_eval.dat
cdp_obs <- cdp_obs %>% filter(source=="CDP_met_insitu.dat") %>% select(-unit,-source)  %>% dcast(Time ~ variable) ##
meteo_s <- cdp_obs %>% select(-Time) %>% as.zoo()
###stop("qui")
##%>% as.zoo()
index(meteo_s) <- cdp_obs$Time
###meteo_s <- meteo_s[,names(meteo_s)!="Time"]


### METEO FILE (template)

###
tz <- "GMT"
##wpath <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/sims' 
wpath <- '/home/ecor/activity/2020/geotop_examples/GEOtopExamples-1/tests/ColdelaPorte/sims/'
wpaths <- wpath %>% list.files(pattern="sim",full.name=TRUE)
names(wpaths) <- wpath %>% list.files(pattern="sim",full.name=FALSE)
meteo_orig <- geotopbricks::get.geotop.inpts.keyword.value("MeteoFileOld",wpath=wpaths[3],data.frame=TRUE,tz=tz)
###



## FROM kg m^-2 s^-1 water density: 1000 kg m^-3
water_density <- 1000
meteo_s$Prec <- (meteo_s$Rainf+meteo_s$Snowf)/water_density*1000*3600
meteo_s$SW <- meteo_s$DIR_SWdown+meteo_s$SCA_SWdown
meteo_s$LW <- meteo_s$LWdown
meteo_s$Tair <- meteo_s$Tair-273.15  ####T(C) = T(K) - 273.15 ...
meteo_s$Ws <- meteo_s$Wind
meteo_s$Wd <- 0 ## NO DATA
meteo_s$Pair <- meteo_s$PSurf/100 ## ffrom Pa to hPa see Manual (http://geotopmodel.github.io/geotop/materials/geotop_manuale.pdf)
meteo_orig$Pair <- as.numeric(NA)
## Qair to RH 
## source: https://earthscience.stackexchange.com/questions/2360/how-do-i-convert-specific-humidity-to-relative-humidity
## FUNCTION Qair2RH
##' Convert specific humidity to relative humidity
##'
##' converting specific humidity into relative humidity
##' NCEP surface flux data does not have RH
##' from Bolton 1980 The computation of Equivalent Potential Temperature 
##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @title qair2rh
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @export
##' @author David LeBauer
qair2rh <- function(qair, temp, press = 1013.25){
  es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}
meteo_s$RH <- qair2rh(qair=meteo_s$Qair,temp=meteo_s$Tair,press=meteo_s$Pair)*100

meteo_new <- meteo_s[,names(meteo_orig)]
####
head(meteo_orig[index(meteo_orig),])
head(meteo_orig[index(meteo_orig),])

##
summary(meteo_orig[index(meteo_orig),]-meteo_new[index(meteo_orig),])
#Index                          Prec                 Ws                 Wd        
#Min.   :1997-01-01 00:00:00   Min.   :-0.000001   Min.   :-2.62700   Min.   :  0.00  
#1st Qu.:1998-07-02 18:30:00   1st Qu.: 0.000000   1st Qu.: 0.00000   1st Qu.:  0.00  
#Median :2000-01-01 12:00:00   Median : 0.000000   Median : 0.00000   Median :  0.00  
#Mean   :2000-01-01 11:40:43   Mean   : 0.157747   Mean   : 0.01693   Mean   : 74.16  
#3rd Qu.:2001-07-02 06:30:00   3rd Qu.: 0.000000   3rd Qu.: 0.00000   3rd Qu.:190.00  
#Max.   :2003-01-01 00:00:00   Max.   :19.104762   Max.   : 4.02201   Max.   :350.00  
#NA's   :6                                              
#       RH                Tair                SW                LW                Pair      
# Min.   :-53.6901   Min.   :-3.56509   Min.   :-468.39   Min.   :-101.293   Min.   : NA    
# 1st Qu.: -1.1560   1st Qu.:-0.03000   1st Qu.:   0.00   1st Qu.: -10.011   1st Qu.: NA    
# Median : -0.4894   Median : 0.00000   Median :   0.00   Median :  -9.989   Median : NA    
# Mean   : -1.1321   Mean   : 0.02742   Mean   :  54.81   Mean   :  -7.376   Mean   :NaN    
# 3rd Qu.:  0.0000   3rd Qu.: 0.04000   3rd Qu.:  72.07   3rd Qu.:  -2.961   3rd Qu.: NA    
# Max.   : 45.5092   Max.   : 4.35141   Max.   : 964.07   Max.   : 107.439   Max.   : NA    
#                                       NA's   :658                          NA's   :52579  
#> 
##

for (i in (1:length(wpaths))) {
  
  wpath <- wpaths[i]
  file_prefix <- geotopbricks::get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,add_wpath=TRUE,data.frame=FALSE)
  print(file_prefix)
  geotopbricks::create.geotop.meteo.files(x=meteo_new,file_prefix=file_prefix)
}
## END 
