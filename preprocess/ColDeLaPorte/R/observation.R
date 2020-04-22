##
## Emanuele Cordano 
rm(list=ls())

#####library(geotopbricks)
library(stringr)
library(purrr)
library(dplyr)
library(magrittr)
library(zoo)
###library(plyr)

### ORIGINAL METEO DATA

meteo_insitu_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_met_insitu.dat'
meteo_safran_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_met_safran.dat'

##observations_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_hor_disk.dat' 
observations_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_hor_eval.dat' 
observations_daily_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_daily_eval.dat'
## INSERT DAILY OBS !!!
### SAFRAN
sep <- ","
meteo_safran_s <- meteo_insitu_file %>% (function(x,sep){readLines(meteo_insitu_file)[1] %>% str_split(sep)})(sep=sep) %>% extract2(1)
attr(meteo_safran_s,"unit") <-   meteo_safran_file %>% (function(x,sep){readLines(meteo_insitu_file)[2] %>% str_split(sep)})(sep=sep)
meteo_safran <- meteo_safran_file %>% read.table(header=FALSE,skip=2,sep=sep)
names(meteo_safran) <- meteo_safran_s[1:ncol(meteo_safran)]

### INSITU
sep <- ","
meteo_insitu_s <- meteo_insitu_file %>% (function(x,sep){readLines(meteo_insitu_file)[1] %>% str_split(sep)})(sep=sep) %>% extract2(1)
attr(meteo_insitu_s,"unit") <-   meteo_insitu_file %>% (function(x,sep){readLines(meteo_insitu_file)[2] %>% str_split(sep)})(sep=sep)
meteo_insitu <- meteo_insitu_file %>% read.table(header=FALSE,skip=2,sep=sep)
names(meteo_insitu) <- meteo_insitu_s

## 


### OBSERVATION
sep <- ","
observations_s <- observations_file %>% (function(x,sep){readLines(x)[1] %>% str_split(sep)})(sep=sep) %>% extract2(1)
attr(observations_s,"unit") <-  observations_file %>% (function(x,sep){readLines(meteo_insitu_file)[2] %>% str_split(sep)})(sep=sep)
observations <-observations_file %>% read.table(header=FALSE,skip=2,sep=sep)
names(observations) <- observations_s

observations <- observations ## %>% filter(!is.na(Snowdepth)) %>% filter(!is.na(Hdisk_01))

## OBSERVATION_DAILY
### OBSERVATION
sep <- ","
observations_daily_s <- observations_daily_file  %>% (function(x,sep){readLines(x)[1] %>% str_split(sep)})(sep=sep) %>% extract2(1)
attr(observations_daily_s,"unit") <-  observations_daily_file %>% (function(x,sep){readLines(meteo_insitu_file)[2] %>% str_split(sep)})(sep=sep)
observations_daily <-observations_daily_file %>% read.table(header=FALSE,skip=2,sep=sep)
names(observations_daily) <- observations_daily_s


stop("QUI")













tz <- "GMT"
time <- as.POSIXlt(observations$Time,tz=tz)
observations <- observations[,-1] %>% as.zoo() 
index(observations) <- time



tz <- "GMT"
time <- as.POSIXlt(observations_daily$Time,tz=tz)
observations_daily <- observations_daily[,-1] %>% as.zoo() 
index(observations_daily) <- time

##SWE_pit SWE_pit_south SWE_pit_north Runoff_5m2 Runoff_1m2 Albedo
##1993-08-01 12:00:00      NA            NA            NA         NA         NA     NA
##1993-08-02 12:00:00      NA            NA            NA         NA         NA     NA
##1993-08-03 12:00:00      NA            NA            NA         NA         NA     NA
##31993-08-04 12:00:00      NA            NA            NA         NA         NA     NA
##1993-08-05 12:00:00      NA            NA            NA         NA         NA     NA
##1993-08-06 12:00:00      NA            NA            NA         NA         NA     NA
##> plot(observations_daily)
plot(observations_daily[str_detect(names(observation_daily,"SWE"))])
u <- observations_daily$SWE_auto
rr <- which(!is.na(observations_daily$SWE_auto)) ##  %>% range()
plot(observations_daily[rr,]$SWE_auto)

##Error in type(pattern) : argument "pattern" is missing, with no default
##> plot(observations_daily[,str_detect(names(observation_daily,"SWE"))])
##Error in type(pattern) : argument "pattern" is missing, with no default
> plot(observations_daily[,str_detect(names(observation_daily),"SWE")])
Error in stri_detect_regex(string, pattern, negate = negate, opts_regex = opts(pattern)) : 
  object 'observation_daily' not found
> plot(observations_daily[,str_detect(names(observations_daily),"SWE")])
> 






stop("here")
###
wpath <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/sims' 
wpaths <- wpath %>% list.files(pattern="sim",full.name=TRUE)
names(wpaths) <- wpath %>% list.files(pattern="sim",full.name=FALSE)
meteo_orig <- geotopbricks::get.geotop.inpts.keyword.value("MeteoFile",wpath=wpaths[1],data.frame=TRUE)
###
# names(observations)
# [1] "Snowdepth"     "Tsoil_10cm"    "Tsoil_20cm"    "Tsoil_50cm"    "Runoff_1m2"    "Runoff_5m2"   
# [7] "Albedo"        "Tsurf"         "Ground_flux_1" "Ground_flux_2" "Ground_flux_3"
# 



### ORIGINAL METEO DATA

meteo_insitu_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_met_insitu.dat'
meteo_safran_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_met_safran.dat'

### SAFRAN
sep <- ","
meteo_safran_s <- meteo_insitu_file %>% (function(x,sep){readLines(meteo_insitu_file)[1] %>% str_split(sep)})(sep=sep) %>% extract2(1)
attr(meteo_safran_s,"unit") <-   meteo_safran_file %>% (function(x,sep){readLines(meteo_insitu_file)[2] %>% str_split(sep)})(sep=sep)
meteo_safran <- meteo_safran_file %>% read.table(header=FALSE,skip=2,sep=sep)
names(meteo_safran) <- meteo_safran_s[1:ncol(meteo_safran)]

### INSITU
sep <- ","
meteo_insitu_s <- meteo_insitu_file %>% (function(x,sep){readLines(meteo_insitu_file)[1] %>% str_split(sep)})(sep=sep) %>% extract2(1)
attr(meteo_insitu_s,"unit") <-   meteo_insitu_file %>% (function(x,sep){readLines(meteo_insitu_file)[2] %>% str_split(sep)})(sep=sep)
meteo_insitu <- meteo_insitu_file %>% read.table(header=FALSE,skip=2,sep=sep)
names(meteo_insitu) <- meteo_insitu_s

### METEO FILE (template)

###
tz <- "Etc/GMT-1"
wpath <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/sims' 
wpaths <- wpath %>% list.files(pattern="sim",full.name=TRUE)
names(wpaths) <- wpath %>% list.files(pattern="sim",full.name=FALSE)
meteo_orig <- geotopbricks::get.geotop.inpts.keyword.value("MeteoFileOld",wpath=wpaths[3],data.frame=TRUE,tz=tz)
###
meteo_ss <- meteo_insitu
####meteo_ss <- meteo_safran
ttime <- meteo_ss$Time %>% as.character() %>% as.POSIXlt(tz=tz)
meteo_s <- meteo_ss[,(names(meteo_ss)!="Time")] %>% as.zoo()
index(meteo_s) <- ttime
#index(meteo_s) <- meteo_s$Time %>% as.character() %>% as.POSIXlt(tz=tz)
##
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

# 
# summary(meteo_orig[index(meteo_orig),]-meteo_new[index(meteo_orig),])
# summary(meteo_orig[index(meteo_orig),"Prec"]-meteo_new[index(meteo_orig),"Prec"])
# plot(meteo_orig[index(meteo_orig),"Prec"]-meteo_new[index(meteo_orig),"Prec"])
# 
# plot(meteo_orig[index(meteo_orig),"Tair"]-meteo_new[index(meteo_orig),"Tair"])
# plot(meteo_new[index(meteo_orig),"SW"][],meteo_orig[index(meteo_orig),"SW"][])
# 
# 
# 
# 
# 
# 
# stop("HERE")
# # 
# # csv <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012/datasets/' %>% list.files(pattern=".tab",full.name=TRUE) 
# # ##
# # tz <- "Etc/GMT-1"
# # 
# # 
# # start_comment_delim <- fixed("/*")
# # end_comment_delim <- fixed("*/")
# # start_comment_line <- csv %>% map(readLines) %>%  map(str_detect,pattern=start_comment_delim) %>% map(which) %>% map(extract,1) %>% unlist()
# # end_comment_line <- csv %>% map(readLines) %>%  map(str_detect,pattern=end_comment_delim) %>% map(which) %>% map(extract,1) %>% unlist()
# # obs <- list()
# # sep <- "\t"
# # format_datetime <- "%Y-%m-%dT%H:%M"
# # format_date <- "%Y-%m-%d"
# # for (i in 1:length(csv)) {
# #   
# #   obs[[i]] <- read.table(csv[i],sep=sep,header=TRUE,skip=end_comment_line[i])
# #   attr(obs[[i]],"foreword") <- csv[i] %>% readLines() %>% extract(start_comment_line[i]:end_comment_line[i])
# #   attr(obs[[i]],"filepath") <- csv[i]
# #   if ("Date.Time" %in% names(obs[[i]])) {
# #     ##
# #     ##
# #     
# #     temp <- obs[[i]]$Date.Time %>% as.POSIXct(format=format_datetime,tz=tz)
# #     if (any(is.na(temp))) temp <- obs[[i]]$Date.Time %>% as.POSIXct(format=format_date,tz=tz)
# #     if (any(is.na(temp))) {
# #       message <- sprintf("DataTime issue on %s",csv[i])
# #       stop(message)
# #     }
# #     obs[[i]]$Date.Time <- temp
# #     
# #     ##
# #     ##
# #   }
# # }
# # ##
# # 
# # 
# # ##
# # 
# # ## GET METEO DATA
# # meteo <- obs[[5]]
# # 
# # ###
# # wpath <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/sims' 
# # wpaths <- wpath %>% list.files(pattern="sim",full.name=TRUE)
# # names(wpaths) <- wpath %>% list.files(pattern="sim",full.name=FALSE)
# # meteo_orig <- geotopbricks::get.geotop.inpts.keyword.value("MeteoFile",wpath=wpaths[3],data.frame=TRUE)
# # ###
# # 
# # str(obs[[2]])
# # 
# # ###
# # files <- obs[[2]]
# # files$URL <- files$URL.file %>% str_replace("hdl:","https://doi.pangaea.de/")
# # 
# # ###https://d##oi.pangaea.de/10013/epic.38613.d001
# # ## 
# # zips <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/%s.zip'
# # files$zip <- zips %>% sprintf(files$File.name)
# # 
# # for (i in 1:nrow(files)){
# #   
# #   download.file(files$URL[i],files$zip)
# # }
# #   
# #   
# # ###
# # stop("MI FERMO QUI")
# # ##
# # ##
# # ## SOIL TEMPERATURE
# # str(obs[[6]])
# # str(csv[[6]])
# # ## METEO DATA
# # str(obs[[5]])
# # str(meteo[[5]])
# # 
# # ## OBS 1
# # 
# # str(obs[[1]])
# # str(csv[[1]])
# # 
# # stop("HRERE")   