##
## Emanuele Cordano 
rm(list=ls())

#####library(geotopbricks)
library(stringr)
library(purrr)
library(dplyr)
library(magrittr)
library(zoo)
library(reshape2)
###library(plyr)

### ORIGINAL METEO DATA
wdir <- ''
meteo_insitu_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_met_insitu.dat'
meteo_safran_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_met_safran.dat'

##observations_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_hor_disk.dat' 
observations_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_hor_eval.dat' 
observations_daily_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_daily_eval.dat'


files <- c(meteo_insitu_file,meteo_safran_file,observations_file,observations_daily_file)
names(files) <- files %>% str_split("/") %>% map(rev) %>% map(extract2,1) %>% unlist()
cdp_obs <- NULL
tz ="GMT"

for (i in (1:length(files))) {
  
  sep <- ","
  it <- files[i]
  itn <- names(files)[i]
  
  it2 <- it %>% (function(x,sep){readLines(it)[1] %>% str_split(sep)})(sep=sep) %>% extract2(1)
  it3 <- it %>% (function(x,sep){readLines(it)[2] %>% str_split(sep)})(sep=sep) %>% extract2(1)
  it4 <- it %>% read.table(header=FALSE,skip=2,sep=sep)
  names(it4) <- it2[1:ncol(it4)]
  print(itn)
  print(it3)
  if (it3[1]!="UTC") it3 <- c("UTC",it3)
  names(it3) <- it2
  ###
  it4 <- it4 %>% melt(id="Time",na.rm=TRUE)
  it4$Time <- as.POSIXct(it4$Time,tz=tz)
  it4$unit <- it3[as.character(it4$variable)] %>% factor()
  it4$source <- itn %>% factor()
  cdp_obs <- rbind(cdp_obs,it4)

  
  
}

outfile <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/rda/cdp_obs.rda'

save(cdp_obs,file=outfile)

