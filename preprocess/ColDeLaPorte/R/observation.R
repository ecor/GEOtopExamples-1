##
## Emanuele Cordano 
##
## 2020 04 25
##
rm(list=ls())


library(stringr)
library(purrr)
library(dplyr)
library(magrittr)
library(zoo)
library(reshape2)


### ORIGINAL METEO DATA
wdir <- '/home/ecor/activity/2020/geotop_examples/GEOtopExamples-1/tests/ColdelaPorte/obss'
collapse="/"
meteo_insitu_file <- 'delivery/morin2012_allzips/CDP_ascii/CDP_met_insitu.dat' %>% c(wdir) %>% rev() %>% paste(collapse=collapse)##sprintf(fmt="%s%s",wdir) 
meteo_safran_file <- 'delivery/morin2012_allzips/CDP_ascii/CDP_met_safran.dat' %>% c(wdir) %>% rev() %>% paste(collapse=collapse)

##observations_file <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/CDP_ascii/CDP_hor_disk.dat' 
observations_file <- 'delivery/morin2012_allzips/CDP_ascii/CDP_hor_eval.dat' %>% c(wdir) %>% rev() %>% paste(collapse=collapse)
observations_daily_file <- 'delivery/morin2012_allzips/CDP_ascii/CDP_daily_eval.dat' %>% c(wdir) %>% rev() %>% paste(collapse=collapse)


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

outfile <- 'rda/cdp_obs.rda' %>% c(wdir) %>% rev() %>% paste(collapse=collapse)

save(cdp_obs,file=outfile,compress="xz")

