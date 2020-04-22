##
## Emanuele Cordano 
rm(list=ls())

#####library(geotopbricks)
library(stringr)
library(purrr)
library(dplyr)
library(magrittr)
###library(plyr)


csv <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012/datasets/' %>% list.files(pattern=".tab",full.name=TRUE) 
##
tz <- "Etc/GMT-1"
start_comment_delim <- fixed("/*")
end_comment_delim <- fixed("*/")
start_comment_line <- csv %>% map(readLines) %>%  map(str_detect,pattern=start_comment_delim) %>% map(which) %>% map(extract,1) %>% unlist()
end_comment_line <- csv %>% map(readLines) %>%  map(str_detect,pattern=end_comment_delim) %>% map(which) %>% map(extract,1) %>% unlist()
obs <- list()
sep <- "\t"
format_datetime <- "%Y-%m-%dT%H:%M"
format_date <- "%Y-%m-%d"
for (i in 1:length(csv)) {
  
  obs[[i]] <- read.table(csv[i],sep=sep,header=TRUE,skip=end_comment_line[i])
  attr(obs[[i]],"foreword") <- csv[i] %>% readLines() %>% extract(start_comment_line[i]:end_comment_line[i])
  attr(obs[[i]],"filepath") <- csv[i]
  if ("Date.Time" %in% names(obs[[i]])) {
    ##
    ##
    
    temp <- obs[[i]]$Date.Time %>% as.POSIXct(format=format_datetime,tz=tz)
    if (any(is.na(temp))) temp <- obs[[i]]$Date.Time %>% as.POSIXct(format=format_date,tz=tz)
    if (any(is.na(temp))) {
      message <- sprintf("DataTime issue on %s",csv[i])
      stop(message)
    }
    obs[[i]]$Date.Time <- temp
    
    ##
    ##
  }
}
##


##

## GET METEO DATA
meteo <- obs[[5]]

###
wpath <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/sims' 
wpaths <- wpath %>% list.files(pattern="sim",full.name=TRUE)
names(wpaths) <- wpath %>% list.files(pattern="sim",full.name=FALSE)
meteo_orig <- geotopbricks::get.geotop.inpts.keyword.value("MeteoFile",wpath=wpaths[3],data.frame=TRUE)
###

str(obs[[2]])

###
files <- obs[[2]]
files$URL <- files$URL.file %>% str_replace("hdl:","https://doi.pangaea.de/")

###https://d##oi.pangaea.de/10013/epic.38613.d001
## 
zips <- '/home/ecor/activity/2020/geotop_article/git/GEOtopExamples/tests/ColdelaPorte/obss/delivery/morin2012_allzips/%s.zip'
files$zip <- zips %>% sprintf(files$File.name)

for (i in 1:nrow(files)){
  
  download.file(files$URL[i],files$zip[i])
}
  
  
###
stop("MI FERMO QUI")
