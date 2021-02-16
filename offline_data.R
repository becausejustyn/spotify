#From offline data

library(jsonlite)
library(lubridate)
library(gghighlight)
library(spotifyr) #don't need for this
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)

streamHistory <- fromJSON("C:/Users/justy/Downloads/spotify-main/spotify-main/StreamingHistory0.json", flatten = TRUE)

head(streamHistory)

#tidy the data
#check for missing data

sum(is.na(streamHistory)) #no missing data

#Create column of counts, removed duplicates
mySpotify <- streamHistory %>%
  group_by(artistName) %>%
  add_count(trackName, name = "playCount") %>%
  distinct(trackName, .keep_all = TRUE) %>%
  dplyr::ungroup(artistName)

#Adding date, time
mySpotify <- mySpotify %>%
  mutate_at("endTime", ymd_hm) %>%
  mutate(endTime = endTime - hours(6)) %>%
  mutate(date = floor_date(endTime, "day") %>%
           as_date, seconds = msPlayed / 1000, minutes = seconds / 60)
  
#Moved columns  
mySpotify <- mySpotify %>%
  relocate(endTime, .after = minutes) %>%
  relocate(playCount, .after = trackName) %>%
  relocate(trackName, .before = artistName)
















